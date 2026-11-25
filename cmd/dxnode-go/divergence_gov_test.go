package main

import (
	"bytes"
	"context"
	"math/big"
	"testing"

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
