package main

import (
	"bytes"
	"context"
	"math/big"
	"testing"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
	"github.com/blinklabs-io/gouroboros/cbor"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
)

// Divergence confirmations call dingo directly, bypassing the dxnode-go
// adapters, so a reported mismatch cannot come from request translation.

// gov.thresholds, ParameterChange with an update that touches no parameter.
// The reference takes the maximum over the touched groups, which is zero for
// an empty set (Governance/Internal.hs pparamsUpdateThreshold). Dingo falls
// back to the governance group threshold. The ledger rejects an empty update
// as malformed (ppuWellFormed requires ppu /= emptyPParamsUpdate), so no
// enacted chain reaches it.
func TestDivergenceEmptyParameterChangeDRepThreshold(t *testing.T) {
	t.Parallel()
	pp := &conway.ConwayProtocolParameters{}
	pp.DRepVotingThresholds.PpGovGroup = cbor.Rat{Rat: big.NewRat(69, 100)}
	pp.DRepVotingThresholds.PpNetworkGroup = cbor.Rat{Rat: big.NewRat(66, 100)}

	got := governance.DRepThreshold(
		lcommon.GovActionTypeParameterChange, pp, &conway.ConwayParameterChangeGovAction{}, true,
	)
	if got == nil || got.Cmp(big.NewRat(69, 100)) != 0 {
		t.Fatalf("dingo threshold = %v, want the governance group fallback 69/100; re-derive the divergence", got)
	}
	if got.Sign() == 0 {
		t.Fatal("dingo now matches the reference; move the corpus case out of corpus/divergences")
	}
}

// gov.drep-expiry, an expired DRep during dormancy. The reference keeps an
// expiry whose sum with the dormant count is still below the current epoch
// (Rules/Certs.hs updateDormantDRepExpiry). Dingo's store bumps every nonzero
// expiry by one per dormant epoch and has no epoch input, so the expired
// DRep's expiry advances toward the current epoch and can eventually reach it,
// reviving the DRep. A chain reaches this with a DRep that expires and then
// sees dormant epochs.
func TestDivergenceExpiredDRepIsBumpedDuringDormancy(t *testing.T) {
	oracle, err := scratchOracle()
	if err != nil {
		t.Fatal(err)
	}
	got, err := oracle.DRepDormancy(context.Background(), dxoracle.DRepDormancyInput{
		Dreps:         []dxoracle.DRep{{Tag: 0, Key: bytes.Repeat([]byte{0xd2}, 28), ExpiryEpoch: 10, Active: true}},
		DormantEpochs: 3,
	})
	if err != nil {
		t.Fatal(err)
	}
	if got[0].ExpiryEpoch != 13 {
		t.Fatalf("dingo expiry = %d, want 13 (bumped although 13 < current epoch 20); re-derive the divergence", got[0].ExpiryEpoch)
	}
}

// gov.drep-ratio, a DRep whose expiry is zero. The reference counts a DRep
// only while the epoch is not past its expiry (Rules/Ratify.hs
// dRepAcceptedRatio: reCurrentEpoch > drepExpiry is expired), so expiry 0 is
// expired from epoch 1. Dingo reads a zero expiry as "no activity bound
// recorded" and keeps the DRep active at every epoch, in the voting-state
// loader and in the dormancy bump alike. Dingo's registration writes a zero
// expiry and renews it in the same transaction, so a chain reaches a stored
// zero only when the renewal is epoch 0 plus a zero DRep activity period.
func TestDivergenceZeroExpiryDRepStaysActive(t *testing.T) {
	t.Parallel()
	drep := &models.Drep{CredentialTag: 0, Credential: bytes.Repeat([]byte{0xd1}, 28), ExpiryEpoch: 0, Active: true}
	if !governance.DRepActiveAtEpoch(drep, 1) {
		t.Fatal("dingo now treats expiry 0 as expired at epoch 1; move the corpus case out of corpus/divergences")
	}
	if !governance.DRepActiveAtEpoch(drep, 1_000_000) {
		t.Fatal("dingo's zero expiry is no longer unbounded; re-derive the divergence")
	}
	drep.ExpiryEpoch = 1
	if governance.DRepActiveAtEpoch(drep, 2) {
		t.Fatal("control: a nonzero expiry below the epoch must be expired")
	}
}

// gov.ratify, a final-chance ratification at a boundary that leaves no live
// proposal. The reference counts a DRep only while the epoch is not past its
// expiry and moves expiries only when a proposal arrives (updateDormantDRepExpiry),
// so a DRep that expired one epoch earlier counts nothing and the action
// expires. Dingo's boundary extends every nonzero expiry by one when no
// proposal is live in the new epoch, before RATIFY reads the DReps
// (ProcessEpoch: BumpDormantDRepExpiryAtEpochBoundary), so the same DRep is
// active again and the action is accepted. A chain reaches this whenever the
// last live proposals expire at a boundary where a DRep expired one epoch
// before.
func TestDivergenceDormantBumpBeforeRatify(t *testing.T) {
	t.Parallel()
	oracle, err := scratchOracle()
	if err != nil {
		t.Fatal(err)
	}
	pp := expiryPParams()
	pp.DRepVotingThresholds.MotionNoConfidence = cbor.Rat{Rat: big.NewRat(1, 2)}
	pp.PoolVotingThresholds.MotionNoConfidence = cbor.Rat{Rat: big.NewRat(1, 2)}
	state := func(drepExpiry, proposalExpires uint64) dxoracle.GovState {
		return dxoracle.GovState{
			PParams: pp,
			Dreps:   []dxoracle.DRep{{Tag: 0, Key: bytes.Repeat([]byte{0xd1}, 28), ExpiryEpoch: drepExpiry, Active: true}},
			DRepStake: []dxoracle.GovDRepStake{{
				Delegation: dxoracle.Delegation{Type: dxoracle.DelegateKeyHash, Key: bytes.Repeat([]byte{0xd1}, 28)}, Stake: 100,
			}},
			SPO: &governance.SPOVotingState{
				Dist:       []*models.PoolStakeSnapshot{{PoolKeyHash: bytes.Repeat([]byte{0xb1}, 28), TotalStake: 100}},
				TotalStake: 100,
			},
			Proposals: []dxoracle.GovProposal{{
				ID: dxoracle.GovAction{TxHash: bytes.Repeat([]byte{1}, 32)}, ActionType: uint8(lcommon.GovActionTypeNoConfidence),
				Cbor: mustHex("8203f6"), ProposedEpoch: 1, ExpiresEpoch: proposalExpires,
				Return: dxoracle.Credential{Key: bytes.Repeat([]byte{0xa1}, 28)},
			}},
			Votes: []dxoracle.GovVote{
				{Role: 1, Key: bytes.Repeat([]byte{0xd1}, 28), Vote: 1},
				{Role: 2, Key: bytes.Repeat([]byte{0xb1}, 28), Vote: 1},
			},
		}
	}
	ratified := func(drepExpiry, proposalExpires uint64) int {
		got, err := oracle.Ratify(context.Background(), state(drepExpiry, proposalExpires), 5)
		if err != nil {
			t.Fatal(err)
		}
		return len(got.Ratified)
	}
	// The DRep expired at 4 and the only proposal expires at 4: dingo accepts.
	if ratified(4, 4) != 1 {
		t.Fatal("dingo no longer accepts after the dormancy extension; move the corpus case out of corpus/divergences")
	}
	// Control: a live proposal in the new epoch keeps the expiry, the DRep
	// counts nothing and nothing is accepted.
	if n := ratified(4, 9); n != 0 {
		t.Fatalf("control: ratified %d with a live proposal and an expired DRep, want 0", n)
	}
	// Control: a DRep expiring at the epoch is active in both.
	if n := ratified(5, 4); n != 1 {
		t.Fatalf("control: ratified %d with an active DRep, want 1", n)
	}
}
