package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"math/big"
	"testing"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
)

// Divergence: a hard fork from protocol 10.0 to 10.1. The reference's HARDFORK
// rule matches on the new major version (Rules/HardFork.hs, `pvMajor newPv ==
// natVersion @10`), so it clears delegations to unregistered DReps again;
// dingo runs its protocol 10 step only when the major version changes
// (ledger/chainsync.go, `oldVer.Major != newVer.Major`) and keeps the
// delegation. This test calls dingo's rollover directly, bypassing the adapter.
// The Haskell half is the check "divergence: new epoch gov: HARDFORK clears
// dangling DRep delegations on a minor bump at 10" in test/Test/Epoch.hs.
func TestDivergenceDingoKeepsDanglingDRepOnMinorHardFork(t *testing.T) {
	t.Parallel()
	pp, err := govFullPParams(newEpochConwayPParams(t))
	if err != nil {
		t.Fatal(err)
	}
	pp.MinCommitteeSize = 1
	hf, _ := hex.DecodeString("8301f6820a01")
	tx := bytes.Repeat([]byte{1}, 32)
	k := func(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }
	gov := dxoracle.GovState{
		PParams:   pp,
		Treasury:  5_000_000,
		Accounts:  []dxoracle.Account{{Tag: 0, Key: k(1), Active: true}},
		Dreps:     []dxoracle.DRep{{Tag: 0, Key: k(0xd1), ExpiryEpoch: 30, Active: true}},
		DRepStake: []dxoracle.GovDRepStake{{Delegation: dxoracle.Delegation{Type: dxoracle.DelegateKeyHash, Key: k(0xd1)}, Stake: 100}},
		Committee: []dxoracle.CommitteeMember{{Tag: 0, Key: k(0xc1), Expiry: 40, Hot: &dxoracle.Credential{Tag: 0, Key: k(0xc2)}}},
		Quorum:    big.NewRat(1, 1),
		SPO:       &governance.SPOVotingState{Dist: []*models.PoolStakeSnapshot{{PoolKeyHash: k(0xa0), TotalStake: 100}}, TotalStake: 100},
		Proposals: []dxoracle.GovProposal{{
			ID: dxoracle.GovAction{TxHash: tx}, ActionType: 1, Cbor: hf, ProposedEpoch: 1, ExpiresEpoch: 15,
			Return: dxoracle.Credential{Tag: 0, Key: k(0xa2)},
		}},
		Votes: []dxoracle.GovVote{
			{Proposal: 0, Role: 1, Tag: 0, Key: k(0xd1), Vote: 1},
			{Proposal: 0, Role: 0, Tag: 0, Key: k(0xc2), Vote: 1},
			{Proposal: 0, Role: 2, Key: k(0xa0), Vote: 1},
		},
	}
	got, err := dxoracle.NewEpoch(context.Background(), dxoracle.NewEpochInput{
		Epoch: 10, PParams: pp, Reserves: 10_000_000_000, Treasury: 5_000_000,
		Accounts: []dxoracle.NewEpochAccount{
			{Key: k(1)},
			{Key: k(0x72), Drep: dxoracle.Delegation{Type: dxoracle.DelegateKeyHash, Key: k(0xdd)}},
		},
		Gov: &gov,
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Gov.Enacted) != 1 {
		t.Fatalf("enacted %v, want the hard fork", got.Gov.Enacted)
	}
	if d := got.Accounts[1].Drep; d.Type != dxoracle.DelegateKeyHash || !bytes.Equal(d.Key, k(0xdd)) {
		t.Errorf("delegation %v, want it kept (dingo's behaviour)", d)
	}
}
