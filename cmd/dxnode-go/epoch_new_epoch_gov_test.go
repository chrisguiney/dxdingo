package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected values are derived by hand from Conway Rules/Epoch.hs, applied to
// the pulser's RATIFY verdict (Rules/Ratify.hs): the boundary into epoch 11
// completes the RATIFY the boundary into epoch 10 started over the request's
// proposals, votes and distributions, applies the enacted state, removes
// enacted and expired proposals with their subtrees, pays treasury
// withdrawals to registered accounts, and returns each removed deposit to its
// registered return account or, unclaimed, to the treasury. Same cases as
// haskell/dxnode-hs/test/Test/Epoch.hs.

// govNewEpochRequest is the boundary into epoch 11 over the gov test fixtures:
// DRep d1 (all the DRep stake), pool b1 (all the pool stake) and a three
// member committee with quorum 2/3, treasury 5000, and the registered
// accounts 0x61 (balance 7) and 0x62 (balance 11).
func govNewEpochRequest(t *testing.T, props []*v1.Proposal, votes ...[]*v1.VoteRecord) *v1.EpochNewEpochRequest {
	t.Helper()
	req := newEpochRequest(t)
	var all []*v1.VoteRecord
	for _, v := range votes {
		all = append(all, v...)
	}
	req.State.Accounts = []*v1.Account{{Credential: rotKey(1)}, {Credential: cred(0x61), Balance: 7}, {Credential: cred(0x62), Balance: 11}}
	req.State.Pools[0].RetiringEpoch = nil
	req.State.Pots = &v1.Pots{Reserves: 10_000_000_000, Treasury: 5000}
	req.State.Dreps = []*v1.DRepRecord{{Credential: cred(0xd1), Deposit: 500, Expiry: 100}}
	req.State.Committee = testCommittee(false)
	req.State.Proposals = props
	req.State.Votes = all
	req.PoolDistribution = []*v1.PoolStake{pool(0xb1, 100)}
	req.DrepDistribution = []*v1.DRepStake{{Drep: &v1.DRep{Target: &v1.DRep_Credential{Credential: cred(0xd1)}}, Stake: 100}}
	return req
}

func govProposal(id byte, action []byte, expires, deposit uint64, ret byte) *v1.Proposal {
	p := proposal(id, action)
	p.ExpiresEpoch, p.Deposit, p.ReturnAccount = expires, deposit, cred(ret)
	return p
}

func mustNewEpoch(t *testing.T, req *v1.EpochNewEpochRequest) *v1.EpochNewEpochResponse {
	t.Helper()
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	return got
}

func balances(as []*v1.Account) map[byte]uint64 {
	out := map[byte]uint64{}
	for _, a := range as {
		out[a.GetCredential().GetHash()[0]] = a.GetBalance()
	}
	return out
}

// Proposal 1 withdraws 1000 to 0x61 and is accepted by every voter. Proposal 2
// is an info action that expired at epoch 10 (expiry 9) and returns its 300
// to the registered account 0x62. Proposal 3 stays: its expiry is 15, so the
// DReps are not dormant. Treasury: 5000 - 1000 withdrawn + 400 (proposal 1's
// deposit, return account 0x50 not registered) = 4400. 0x61 holds 7 + 1000,
// 0x62 11 + 300.
func TestEpochNewEpochGovWithdrawalAndExpiry(t *testing.T) {
	t.Parallel()
	got := mustNewEpoch(t, govNewEpochRequest(t, []*v1.Proposal{
		govProposal(1, withdrawalAction([2]uint64{0x61, 1000}), 15, 400, 0x50),
		govProposal(2, actInfo, 9, 300, 0x62),
		govProposal(3, actInfo, 15, 0, 0x50),
	}, allVote(1, v1.Vote_VOTE_YES)))
	if g := idsOf(got.GetEnacted()); !bytes.Equal(g, []byte{1}) {
		t.Errorf("enacted %v, want [1]", g)
	}
	if g := idsOf(got.GetExpired()); !bytes.Equal(g, []byte{2}) {
		t.Errorf("expired %v, want [2]", g)
	}
	d := got.GetDelta()
	if g := idsOf(d.GetProposalsRemoved()); !bytes.Equal(g, []byte{1, 2}) {
		t.Errorf("proposals removed %v, want [1 2]", g)
	}
	if d.GetTreasuryDelta() != -600 {
		t.Errorf("treasury delta %d, want -600", d.GetTreasuryDelta())
	}
	if b := balances(d.GetAccountsChanged()); len(b) != 2 || b[0x61] != 1007 || b[0x62] != 311 {
		t.Errorf("accounts changed %v, want 0x61=1007 0x62=311", d.GetAccountsChanged())
	}
	if len(d.GetDrepsChanged()) != 0 {
		t.Errorf("dreps changed %v, want none", d.GetDrepsChanged())
	}
	if d.GetCommitteeAfter() != nil {
		t.Errorf("committee after %v, want unset", d.GetCommitteeAfter())
	}
}

// A parameter change accepted by every voter is enacted and becomes the root
// of its purpose; the proposal left behind keeps the DReps active.
func TestEpochNewEpochGovParameterChange(t *testing.T) {
	t.Parallel()
	got := mustNewEpoch(t, govNewEpochRequest(t, []*v1.Proposal{
		govProposal(1, pcAction(0), 15, 400, 0x62),
		govProposal(3, actInfo, 15, 0, 0x50),
	}, allVote(1, v1.Vote_VOTE_YES)))
	if g := idsOf(got.GetEnacted()); !bytes.Equal(g, []byte{1}) {
		t.Errorf("enacted %v, want [1]", g)
	}
	d := got.GetDelta()
	if root := d.GetEnactStateAfter().GetPrevPparamUpdate(); root == nil || root.GetTxId()[0] != 1 {
		t.Errorf("parameter root %v, want proposal 1", root)
	}
	if b := balances(d.GetAccountsChanged()); len(b) != 1 || b[0x62] != 411 {
		t.Errorf("accounts changed %v, want 0x62=411", d.GetAccountsChanged())
	}
	if d.GetTreasuryDelta() != 0 {
		t.Errorf("treasury delta %d, want 0", d.GetTreasuryDelta())
	}
}

// No confidence needs no committee vote, only the DRep and pool vote, and
// removes the committee.
func TestEpochNewEpochGovNoConfidence(t *testing.T) {
	t.Parallel()
	got := mustNewEpoch(t, govNewEpochRequest(t, []*v1.Proposal{
		govProposal(1, actNoConfidence, 15, 400, 0x62),
		govProposal(3, actInfo, 15, 0, 0x50),
	}, []*v1.VoteRecord{
		voteOn(1, v1.VoterRole_VOTER_ROLE_DREP, cred(0xd1), v1.Vote_VOTE_YES), spoVote(0xb1, v1.Vote_VOTE_YES, actionID(1)),
	}))
	if g := idsOf(got.GetEnacted()); !bytes.Equal(g, []byte{1}) {
		t.Errorf("enacted %v, want [1]", g)
	}
	d := got.GetDelta()
	if d.GetCommitteeAfter() == nil || len(d.GetCommitteeAfter().GetMembers()) != 0 {
		t.Errorf("committee after %v, want the empty committee", d.GetCommitteeAfter())
	}
	if root := d.GetEnactStateAfter().GetPrevCommittee(); root == nil || root.GetTxId()[0] != 1 {
		t.Errorf("committee root %v, want proposal 1", root)
	}
}

// Nothing is accepted: the committee is one vote short of 2/3, so the
// withdrawal stays, and no proposal expires.
func TestEpochNewEpochGovRejected(t *testing.T) {
	t.Parallel()
	votes := allVote(1, v1.Vote_VOTE_YES)[:3]
	got := mustNewEpoch(t, govNewEpochRequest(t, []*v1.Proposal{
		govProposal(1, withdrawalAction([2]uint64{0x61, 1000}), 15, 400, 0x50),
	}, votes))
	if len(got.GetEnacted()) != 0 || len(got.GetExpired()) != 0 {
		t.Errorf("enacted %v expired %v, want none", got.GetEnacted(), got.GetExpired())
	}
	d := got.GetDelta()
	if len(d.GetProposalsRemoved()) != 0 || d.GetTreasuryDelta() != 0 || len(d.GetAccountsChanged()) != 0 {
		t.Errorf("delta %v, want none", d)
	}
}

// A hard fork changes the protocol version and runs HARDFORK, which the
// response does not model; the pool distribution must hold stake; a
// proposal that left the set before the previous epoch is not representable.
func TestEpochNewEpochGovDomain(t *testing.T) {
	t.Parallel()
	unsupported := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
	for name, tc := range map[string]struct {
		mutate func(*v1.EpochNewEpochRequest)
		code   v1.HarnessErrorCode
	}{
		"hard fork proposal": {func(r *v1.EpochNewEpochRequest) {
			r.State.Proposals = []*v1.Proposal{govProposal(1, hfAction(0), 15, 0, 0x50)}
		}, unsupported},
		"no pool stake": {func(r *v1.EpochNewEpochRequest) { r.PoolDistribution = nil }, unsupported},
		"expired before the previous epoch": {func(r *v1.EpochNewEpochRequest) {
			r.State.Proposals = []*v1.Proposal{govProposal(1, actInfo, 7, 0, 0x50)}
		}, unsupported},
		"undecodable action": {func(r *v1.EpochNewEpochRequest) {
			r.State.Proposals = []*v1.Proposal{govProposal(1, []byte{0xff}, 15, 0, 0x50)}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := govNewEpochRequest(t, []*v1.Proposal{govProposal(3, actInfo, 15, 0, 0x50)})
			tc.mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}
