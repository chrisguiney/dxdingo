package main

import (
	"bytes"
	"context"
	"testing"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/eras"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Divergence: dingo's Shelley update decoder accepts protocol version 4 (a
// Mary version). The reference decodes a ShelleyEra update with the era's
// protocol-version range, so the same bytes are a DeserialiseFailure there;
// see "divergence: pparam update" in haskell/dxnode-hs/test/Test/Epoch.hs.
// corpus/divergences holds the replayable case.
func TestDivergenceShelleyUpdateProtocolVersionOutsideEraRange(t *testing.T) {
	t.Parallel()
	if _, err := eras.DecodePParamsUpdateShelley(hexbEpoch(pv40)); err != nil {
		t.Fatalf("dingo no longer accepts the update: %v; the divergence is gone", err)
	}
}

// Divergence: HardForkAllegra raises a protocol major below 3 to 3. The
// reference translateEra keeps the version unchanged, so a ledger translated
// before the pending major is enacted stays at major 2. A chain enacts the
// version first, so only a schedule-driven fork (a test network) reaches it.
func TestDivergenceAllegraForkBumpsProtocolMajor(t *testing.T) {
	t.Parallel()
	got, err := epochHardFork(context.Background(), forkReq(2, 3, 2))
	if err != nil {
		t.Fatal(err)
	}
	if major := got.GetDelta().GetPparamsAfter().GetProtocolVersion().GetMajor(); major != 3 {
		t.Fatalf("major = %d; dingo no longer bumps it, the divergence is gone", major)
	}
}

// Divergence: HardForkBabbage inserts DefaultPlutusV2CostModel when the
// previous parameters carry no Plutus V2 model. The reference translation adds
// no cost model, so a V2 script has no model to run under until an update
// supplies one. Whether a chain's fork epoch leaves that window open was not
// checked against mainnet history.
func TestDivergenceBabbageForkInsertsPlutusV2CostModel(t *testing.T) {
	t.Parallel()
	req := forkReq(6, 7, 6)
	req.Pparams = alonzoPParamsProto()
	req.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 6}
	got := run(t, req)
	if len(got.GetCostModelPlutusV2()) != len(eras.DefaultPlutusV2CostModel) {
		t.Fatalf("v2 model length %d; dingo no longer inserts it, the divergence is gone", len(got.GetCostModelPlutusV2()))
	}
}

// Divergence: dingo's SNAP capture drops stake of an account that delegates
// to a pool with no registration. The reference resolves active stake from
// registered, delegated credentials and never consults the pool set; see
// "divergence: snapshot rotate" in haskell/dxnode-hs/test/Test/Epoch.hs. A
// chain cannot reach it: a delegation needs a registered pool, and POOLREAP
// clears delegations to the pools it retires after SNAP has run.
func TestDivergenceSnapshotDropsStakeDelegatedToUnregisteredPool(t *testing.T) {
	t.Parallel()
	oracle, err := scratchOracle()
	if err != nil {
		t.Fatal(err)
	}
	got, err := oracle.SnapshotRotate(context.Background(), dxoracle.SnapshotRotateInput{
		Epoch:    rotateEpoch,
		Stake:    []dxoracle.SnapshotStake{{Key: bytes.Repeat([]byte{4}, 28), Amount: 42}},
		Accounts: []dxoracle.LiveStakeAccount{{Key: bytes.Repeat([]byte{4}, 28), Active: true, Pool: bytes.Repeat([]byte{0xa0}, 28)}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Mark) != 0 {
		t.Fatalf("mark = %v; dingo now keeps the stake, the divergence is gone", got.Mark)
	}
}
