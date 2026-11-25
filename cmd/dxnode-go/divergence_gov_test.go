package main

import (
	"math/big"
	"testing"

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
