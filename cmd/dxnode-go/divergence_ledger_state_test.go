package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/blinklabs-io/dingo/ledgerstate"

	"github.com/chrisguiney/dxdingo/internal/lsfixture"
)

// Divergence confirmations call dingo directly, bypassing dxnode-go, so a
// reported mismatch cannot come from request translation. The counterpart
// checks are the "divergence:" entries in haskell/dxnode-hs/test/Test/Codec.hs.

// pool_and_drep.json registers one DRep with expiry 999 and a dormant-epoch
// count in VState; the reference keeps the stored expiry, dingo reports the
// expiry with the dormant epochs added.
func TestDivergenceLedgerStateDRepExpiry(t *testing.T) {
	t.Parallel()
	raw, err := ledgerstate.ParseNewEpochState(conwayEraIndex, newEpochState(t, "pool_and_drep.json", lsfixture.Options{Epoch: 7}))
	if err != nil {
		t.Fatal(err)
	}
	cs, err := ledgerstate.ParseCertState(raw.CertStateData)
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("stored expiry 999, dormant epochs %d, dingo ExpiryEpoch %d", cs.DormantEpochs, cs.DReps[0].ExpiryEpoch)
	if cs.DReps[0].ExpiryEpoch != 999+cs.DormantEpochs {
		t.Fatalf("ExpiryEpoch = %d, want stored 999 plus %d dormant epochs", cs.DReps[0].ExpiryEpoch, cs.DormantEpochs)
	}
}

// dingo writes a MemPack output as the map format; the reference writes the
// two-element array when the output has no datum or script.
func TestDivergenceMempackTxOutIsMapFormat(t *testing.T) {
	t.Parallel()
	addr := append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...)
	b := append(append([]byte{0x00, 0x1d}, addr...), 0x00, 0xbd, 0x84, 0x40)
	row, err := ledgerstate.ParseMempackTxOut(b)
	if err != nil {
		t.Fatal(err)
	}
	want := append([]byte{0xa2, 0x00, 0x58, 0x1d}, addr...)
	want = append(want, 0x01, 0x1a, 0x00, 0x0f, 0x42, 0x40)
	if !bytes.Equal(row.Cbor, want) {
		t.Fatalf("Cbor = %x, want the map format %x", row.Cbor, want)
	}
}

// dingo reads the fields it needs and ignores the rest of the encoding: a
// byte after the state, or a six-element array where NewEpochState has seven,
// still decodes.
func TestDivergenceLedgerStateLenient(t *testing.T) {
	t.Parallel()
	good := newEpochState(t, "minimal.json", lsfixture.Options{Epoch: 1})
	if _, err := ledgerstate.ParseNewEpochState(conwayEraIndex, good); err != nil {
		t.Fatal(err)
	}
	if _, err := ledgerstate.ParseNewEpochState(conwayEraIndex, append(bytes.Clone(good), 0x00)); err != nil {
		t.Fatalf("trailing byte: %v", err)
	}
	if _, err := ledgerstate.ParseNewEpochState(conwayEraIndex, append([]byte{0x86}, good[1:]...)); err != nil {
		t.Fatalf("six elements: %v", err)
	}
}

// dingo keys a snapshot by credential hash alone, so a key and a script
// credential with one hash share an entry.
func TestDivergenceLedgerStateSnapshotSharedHash(t *testing.T) {
	t.Parallel()
	h := bytes.Repeat([]byte{0x01}, 28)
	pool := bytes.Repeat([]byte{0x77}, 28)
	raw, err := ledgerstate.ParseNewEpochState(conwayEraIndex, newEpochState(t, "minimal.json", lsfixture.Options{
		Epoch: 1,
		Mark:  []lsfixture.Stake{{Hash: h, Amount: 4, Pool: pool}, {Script: true, Hash: h, Amount: 5, Pool: pool}},
	}))
	if err != nil {
		t.Fatal(err)
	}
	snaps, err := ledgerstate.ParseSnapShots(raw.SnapShotsData)
	if err != nil {
		t.Fatal(err)
	}
	if len(snaps.Mark.Stake) != 1 {
		t.Fatalf("%d snapshot entries for two credentials, want 1", len(snaps.Mark.Stake))
	}
}

// A pinned-ledger Shelley..Babbage state carries the protocol parameters as
// 17, 17, 17, 24 and 22 elements with the protocol version as one nested
// pair; gouroboros's per-era structs, which dingo decodes into, flatten the
// version into two elements, so dingo cannot read a state of these eras.
func TestDivergenceLedgerStatePreConwayPParams(t *testing.T) {
	t.Parallel()
	for name, era := range map[string]int{
		"shelley": ledgerstate.EraShelley, "allegra": ledgerstate.EraAllegra, "mary": ledgerstate.EraMary,
		"alonzo": ledgerstate.EraAlonzo, "babbage": ledgerstate.EraBabbage,
	} {
		_, err := ledgerstate.ParseNewEpochState(era, eraState(t, name+"_nes"))
		if err == nil || !strings.Contains(err.Error(), "protocol parameters") {
			t.Fatalf("%s: err = %v, want a protocol parameters decode failure", name, err)
		}
	}
}

// The pinned ledger packs the output index of a UTxO key in little-endian
// order, [txid, 0x01 0x00] for index 1; dingo reads the two bytes as
// big-endian and reports index 256.
func TestDivergenceLedgerStateTxInIndexByteOrder(t *testing.T) {
	t.Parallel()
	raw, err := ledgerstate.ParseNewEpochState(ledgerstate.EraConway+1, eraState(t, "dijkstra_utxo_ix1_nes"))
	if err != nil {
		t.Fatal(err)
	}
	var indexes []uint32
	if _, err := ledgerstate.ParseUTxOsStreaming(raw.UTxOData, func(batch []ledgerstate.ParsedUTxO) error {
		for _, u := range batch {
			indexes = append(indexes, u.OutputIndex)
		}
		return nil
	}); err != nil {
		t.Fatal(err)
	}
	if len(indexes) != 1 || indexes[0] != 256 {
		t.Fatalf("output indexes %v, want [256] for the packed index 1", indexes)
	}
}
