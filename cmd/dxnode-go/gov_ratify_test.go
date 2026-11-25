package main

import (
	"bytes"
	"context"
	"fmt"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected verdicts are derived by hand from Rules/Ratify.hs (ratifyTransition:
// an action is accepted when its parent is the purpose root, no delaying
// action was accepted before it, a withdrawal fits the treasury and the
// committee, pools and DReps all accept it) and Rules/Epoch.hs (the EPOCH
// rule that pays withdrawals and returns deposits). The same cases are in
// haskell/dxnode-hs/test/Test/Gov.hs.

// withdrawalAction is a TreasuryWithdrawals action paying each (key, amount)
// to the key-hash reward account made of key repeated.
func withdrawalAction(pairs ...[2]uint64) []byte {
	out := []byte{0x83, 0x02, 0xa0 + byte(len(pairs))}
	for _, p := range pairs {
		out = append(out, 0x58, 0x1d, 0xe0)
		out = append(out, bytes.Repeat([]byte{byte(p[0])}, 28)...)
		out = append(out, 0x1b)
		out = append(out, byte(p[1]>>56), byte(p[1]>>48), byte(p[1]>>40), byte(p[1]>>32), byte(p[1]>>24), byte(p[1]>>16), byte(p[1]>>8), byte(p[1]))
	}
	return append(out, 0xf6)
}

func voteOn(id byte, role v1.VoterRole, voter *v1.Credential, v v1.Vote) *v1.VoteRecord {
	return &v1.VoteRecord{ActionId: actionID(id), Voter: &v1.Voter{Role: role, Credential: voter}, Vote: v}
}

func allVote(id byte, v v1.Vote) []*v1.VoteRecord {
	out := []*v1.VoteRecord{
		voteOn(id, v1.VoterRole_VOTER_ROLE_DREP, cred(0xd1), v),
		spoVote(0xb1, v, actionID(id)),
	}
	for i := byte(1); i <= 3; i++ {
		out = append(out, voteOn(id, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, cred(0x20+i), v))
	}
	return out
}

func ratifyProposal(id byte, action []byte, expires uint64) *v1.Proposal {
	p := proposal(id, action)
	p.ExpiresEpoch = expires
	return p
}

func ratifyRequest(epoch uint64, treasury uint64, props []*v1.Proposal, votes ...[]*v1.VoteRecord) *v1.GovRatifyRequest {
	var all []*v1.VoteRecord
	for _, v := range votes {
		all = append(all, v...)
	}
	return &v1.GovRatifyRequest{
		Context: &v1.LedgerContext{Epoch: epoch},
		Pparams: govParams(10),
		State: &v1.LedgerState{
			Dreps:     []*v1.DRepRecord{{Credential: cred(0xd1), Expiry: 100}},
			Committee: testCommittee(false),
			Proposals: props,
			Votes:     all,
			Pots:      &v1.Pots{Treasury: treasury},
		},
		PoolDistribution: []*v1.PoolStake{pool(0xb1, 100)},
		DrepDistribution: []*v1.DRepStake{{Drep: &v1.DRep{Target: &v1.DRep_Credential{Credential: cred(0xd1)}}, Stake: 100}},
	}
}

func idsOf(xs []*v1.GovActionId) []byte {
	var out []byte
	for _, x := range xs {
		out = append(out, x.GetTxId()[0])
	}
	return out
}

func TestGovRatify(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		req     *v1.GovRatifyRequest
		enacted []byte
		expired []byte
		delayed bool
	}{
		// 61/100 of the DRep stake and 51/100 of the pool stake are needed;
		// both vote yes with all of it. No committee votes on no confidence.
		"no confidence accepted": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, actNoConfidence, 9)}, []*v1.VoteRecord{
				voteOn(1, v1.VoterRole_VOTER_ROLE_DREP, cred(0xd1), v1.Vote_VOTE_YES), spoVote(0xb1, v1.Vote_VOTE_YES, actionID(1)),
			}),
			[]byte{1}, nil, true,
		},
		"silent pool leaves the pool ratio at zero": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, actNoConfidence, 9)}, []*v1.VoteRecord{
				voteOn(1, v1.VoterRole_VOTER_ROLE_DREP, cred(0xd1), v1.Vote_VOTE_YES),
			}),
			nil, nil, false,
		},
		// Hard fork is accepted by 2/3 of three members, 54/100 of pools and
		// 65/100 of DReps; accepting it delays the parameter change behind it.
		"a delaying action blocks the actions after it": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, hfAction(0), 9), ratifyProposal(2, pcAction(0), 9)},
				allVote(1, v1.Vote_VOTE_YES), allVote(2, v1.Vote_VOTE_YES)),
			[]byte{1}, nil, true,
		},
		// One yes of three members is 1/3, below the 2/3 quorum.
		"committee below quorum": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, hfAction(0), 9)}, []*v1.VoteRecord{
				voteOn(1, v1.VoterRole_VOTER_ROLE_DREP, cred(0xd1), v1.Vote_VOTE_YES), spoVote(0xb1, v1.Vote_VOTE_YES, actionID(1)),
				voteOn(1, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, cred(0x21), v1.Vote_VOTE_YES),
			}),
			nil, nil, false,
		},
		// An expiry of 4 is below epoch 5: expired unless accepted at the
		// same boundary.
		"unaccepted action past its expiry expires": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, actInfo, 4), ratifyProposal(2, actInfo, 5)}),
			nil, []byte{1}, false,
		},
		"accepted action at its last chance is not expired": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, hfAction(0), 4)}, allVote(1, v1.Vote_VOTE_YES)),
			[]byte{1}, nil, true,
		},
		// Non-delaying parameter changes chain within one pass.
		"a parameter change chain is accepted in one pass": {
			ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, pcAction(0), 9), ratifyProposal(2, pcAction(1), 9)},
				allVote(1, v1.Vote_VOTE_YES), allVote(2, v1.Vote_VOTE_YES)),
			[]byte{1, 2}, nil, false,
		},
		// 1500 in the treasury pays one of two 1000 withdrawals: each
		// accepted withdrawal uses up the budget.
		"treasury withdrawals share one budget": {
			ratifyRequest(5, 1500, []*v1.Proposal{
				ratifyProposal(1, withdrawalAction([2]uint64{0x61, 1000}), 9),
				ratifyProposal(2, withdrawalAction([2]uint64{0x62, 1000}), 9),
			}, allVote(1, v1.Vote_VOTE_YES), allVote(2, v1.Vote_VOTE_YES)),
			[]byte{1}, nil, false,
		},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govRatify(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil {
				t.Fatalf("rejected: %v", got.GetRejection())
			}
			if g := idsOf(got.GetEnacted()); !bytes.Equal(g, tc.enacted) {
				t.Errorf("enacted %v, want %v", g, tc.enacted)
			}
			if g := idsOf(got.GetExpired()); !bytes.Equal(g, tc.expired) {
				t.Errorf("expired %v, want %v", g, tc.expired)
			}
			if got.GetDelayed() != tc.delayed {
				t.Errorf("delayed %v, want %v", got.GetDelayed(), tc.delayed)
			}
		})
	}
}

func TestGovRatifyDomain(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		mutate func(*v1.GovRatifyRequest)
		code   v1.HarnessErrorCode
	}{
		"epoch zero":    {func(r *v1.GovRatifyRequest) { r.Context.Epoch = 0 }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"no pool stake": {func(r *v1.GovRatifyRequest) { r.PoolDistribution = nil }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"expired before the previous epoch": {func(r *v1.GovRatifyRequest) {
			r.State.Proposals = []*v1.Proposal{ratifyProposal(1, actInfo, 2)}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"proposals out of submission order": {func(r *v1.GovRatifyRequest) {
			a, b := ratifyProposal(1, actInfo, 9), ratifyProposal(2, actInfo, 9)
			a.ProposedEpoch, b.ProposedEpoch = 3, 2
			r.State.Proposals = []*v1.Proposal{a, b}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"parent that is not the root": {func(r *v1.GovRatifyRequest) {
			r.State.Proposals = []*v1.Proposal{ratifyProposal(1, pcAction(9), 9)}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"undecodable action": {func(r *v1.GovRatifyRequest) {
			r.State.Proposals = []*v1.Proposal{ratifyProposal(1, []byte{0xff}, 9)}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := ratifyRequest(5, 0, []*v1.Proposal{ratifyProposal(1, actInfo, 9)})
			tc.mutate(req)
			_, err := govRatify(context.Background(), req)
			if err == nil {
				t.Fatal("accepted the request")
			}
			if code := dispatch.Code(err); code != tc.code {
				t.Fatalf("code %v (%v), want %v", code, err, tc.code)
			}
		})
	}
}

// With every threshold 1 and nothing delegated, only expiry removes a
// proposal. 1 and its child 2 leave together and 3 expires alone; the
// deposits of 1 (registered return account) and 2 and 3 (unregistered) are
// refunded or left to the treasury.
func TestGovProposalExpiry(t *testing.T) {
	t.Parallel()
	mk := func(id byte, action []byte, expires, deposit uint64, ret byte) *v1.Proposal {
		p := proposal(id, action)
		p.ExpiresEpoch, p.Deposit, p.ReturnAccount = expires, deposit, cred(ret)
		return p
	}
	got, err := govProposalExpiry(context.Background(), &v1.GovProposalExpiryRequest{
		Epoch: 5,
		Proposals: []*v1.Proposal{
			mk(1, pcAction(0), 3, 1000, 0xa1),
			mk(2, pcAction(1), 20, 500, 0xa2),
			mk(3, actInfo, 4, 300, 0xa3),
			mk(4, actInfo, 5, 200, 0xa1),
		},
		Accounts: []*v1.Account{{Credential: cred(0xa1), Balance: 10}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if g := idsOf(got.GetExpired()); !bytes.Equal(g, []byte{1, 2, 3}) {
		t.Errorf("expired %v, want [1 2 3]", g)
	}
	if len(got.GetRefunds()) != 1 || got.GetRefunds()[0].GetAmount() != 1000 || !bytes.Equal(got.GetRefunds()[0].GetCredential().GetHash(), ids(0xa1)) {
		t.Errorf("refunds %v, want 1000 to a1", got.GetRefunds())
	}
	if got.GetUnclaimedToTreasury() != 800 {
		t.Errorf("unclaimed %d, want 800", got.GetUnclaimedToTreasury())
	}
}

func TestGovProposalExpiryDomain(t *testing.T) {
	t.Parallel()
	for name, req := range map[string]*v1.GovProposalExpiryRequest{
		"epoch zero":        {Epoch: 0},
		"parent not a root": {Epoch: 5, Proposals: []*v1.Proposal{proposal(1, pcAction(9))}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			_, err := govProposalExpiry(context.Background(), req)
			if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
				t.Fatalf("code %v, err %v", code, err)
			}
		})
	}
	_ = fmt.Sprint
}
