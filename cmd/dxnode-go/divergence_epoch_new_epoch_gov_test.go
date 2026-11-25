package main

import (
	"context"
	"testing"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
)

// Confirmations of the divergences recorded in corpus/divergences/
// epoch_new_epoch_gov_*: each calls dingo's production boundary through
// dxoracle.NewEpoch, with no adapter in between. The reference side is in
// haskell/dxnode-hs/test/Test/Epoch.hs ("divergence: new epoch gov: ...").
//
// Dingo extends every active DRep's expiry by one at each boundary that leaves
// no active proposal, before RATIFY (governance.BumpDormantDRepExpiryAtEpochBoundary).
// The reference's EPOCH rule only counts the dormant epoch
// (updateNumDormantEpochs) and applies the count when a proposal next arrives,
// so a DRep's stored expiry is unchanged by it, and RATIFY runs over the
// expiries as they were.

func directGov(t *testing.T, expiry uint64, props []dxoracle.GovProposal, votes []dxoracle.GovVote) *dxoracle.NewEpochGovOutput {
	t.Helper()
	pp, err := govFullPParams(newEpochConwayPParams(t))
	if err != nil {
		t.Fatal(err)
	}
	gov := dxoracle.GovState{
		PParams:   pp,
		Treasury:  5000,
		Accounts:  []dxoracle.Account{{Tag: 0, Key: ids(0x62), Reward: 11, Active: true}},
		Dreps:     []dxoracle.DRep{{Tag: 0, Key: ids(0xd1), ExpiryEpoch: expiry, Active: true}},
		DRepStake: []dxoracle.GovDRepStake{{Delegation: dxoracle.Delegation{Type: dxoracle.DelegateKeyHash, Key: ids(0xd1)}, Stake: 100}},
		SPO:       &governance.SPOVotingState{Dist: []*models.PoolStakeSnapshot{{PoolKeyHash: ids(0xb1), TotalStake: 100}}, TotalStake: 100},
		Proposals: props,
		Votes:     votes,
	}
	got, err := dxoracle.NewEpoch(context.Background(), dxoracle.NewEpochInput{
		Epoch: 10, PParams: pp, Reserves: 10_000_000_000, Treasury: 5000,
		Accounts: []dxoracle.NewEpochAccount{{Key: ids(0x62), Reward: 11}},
		Gov:      &gov,
	})
	if err != nil {
		t.Fatal(err)
	}
	return got.Gov
}

func noConfidenceProposal() dxoracle.GovProposal {
	return dxoracle.GovProposal{
		ID: dxoracle.GovAction{TxHash: idBytes(1)}, ActionType: 3, Cbor: actNoConfidence,
		ProposedEpoch: 1, ExpiresEpoch: 9, Deposit: 400, Return: dxoracle.Credential{Tag: 0, Key: ids(0x62)},
	}
}

func TestDivergenceNewEpochGovDormantExpiry(t *testing.T) {
	t.Parallel()
	for name, props := range map[string][]dxoracle.GovProposal{
		"no proposals": nil,
		"the only proposal expires": {{
			ID: dxoracle.GovAction{TxHash: idBytes(1)}, ActionType: 6, Cbor: actInfo,
			ProposedEpoch: 1, ExpiresEpoch: 9, Deposit: 300, Return: dxoracle.Credential{Tag: 0, Key: ids(0x62)},
		}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got := directGov(t, 100, props, nil)
			if len(got.Dreps) != 1 || got.Dreps[0].ExpiryEpoch != 101 {
				t.Fatalf("dreps = %+v, want the expiry extended from 100 to 101", got.Dreps)
			}
		})
	}
}

func TestDivergenceNewEpochGovExpiredDRepVotes(t *testing.T) {
	t.Parallel()
	got := directGov(t, 9, []dxoracle.GovProposal{noConfidenceProposal()}, []dxoracle.GovVote{
		{Proposal: 0, Role: 1, Tag: 0, Key: ids(0xd1), Vote: 1},
		{Proposal: 0, Role: 2, Key: ids(0xb1), Vote: 1},
	})
	if len(got.Enacted) != 1 || got.Enacted[0].TxHash[0] != 1 || len(got.Expired) != 0 {
		t.Fatalf("enacted %v expired %v, want the action enacted on the vote of the DRep that expired at epoch 9", got.Enacted, got.Expired)
	}
}
