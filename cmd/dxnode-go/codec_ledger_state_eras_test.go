package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"os"
	"strings"
	"testing"

	"golang.org/x/crypto/blake2b"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// The era states are built by cardano-ledger's own encoder, in the Test.Codec
// builder sampleNes (haskell/dxnode-hs/test/Test/Codec.hs): epoch 7, treasury
// 1000, reserves 2000, fees 194490, one pool 0x77.. (pledge 71343432,
// cost 400249652, deposit 500000000), one mark snapshot entry of 40 for the
// key credential 0x01.. delegated to that pool, and blocks made 3 and 5.
// The *_utxo states also hold one output of 1500000 lovelace under the key
// hash 0x11, spent by the input 0x42.. at index 0 (dijkstra_utxo_ix1 at
// index 1). Shelley through Babbage also hold one registered account with a deposit of
// 2000000 delegated to the pool.

func eraState(t *testing.T, name string) []byte {
	t.Helper()
	raw, err := os.ReadFile("testdata/ledger_state/eras/" + name + ".hex")
	if err != nil {
		t.Fatal(err)
	}
	b, err := hex.DecodeString(strings.TrimSpace(string(raw)))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func eraLedgerState(t *testing.T, era v1.Era, format v1.LedgerStateFormat, b []byte) *v1.CodecLedgerStateResponse {
	t.Helper()
	got, err := codecLedgerState(context.Background(), &v1.CodecLedgerStateRequest{Era: era, Format: format, Cbor: b})
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func TestCodecDijkstraNewEpochState(t *testing.T) {
	t.Parallel()
	got := eraLedgerState(t, v1.Era_ERA_DIJKSTRA, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, eraState(t, "dijkstra_nes"))
	mustAccept(t, got.GetRejection())
	st, pots := got.GetState(), got.GetState().GetPots()
	if got.GetEpoch() != 7 || pots.GetTreasury() != 1000 || pots.GetReserves() != 2000 || pots.GetFees() != 194490 || pots.GetDonations() != 0 {
		t.Fatalf("epoch %d pots %v", got.GetEpoch(), pots)
	}
	if len(st.GetUtxo()) != 0 {
		t.Fatalf("utxo %v", st.GetUtxo())
	}
	if len(st.GetPools()) != 1 {
		t.Fatalf("pools %v", st.GetPools())
	}
	p := st.GetPools()[0]
	if !bytes.Equal(p.GetPoolId(), bytes.Repeat([]byte{0x77}, 28)) || p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT ||
		p.GetPledge() != 71343432 || p.GetCost() != 400249652 || p.GetDeposit() != 500000000 {
		t.Fatalf("pool %v", p)
	}
	if len(got.GetSnapshots()) != 1 || got.GetSnapshots()[0].GetSnapshot() != v1.SnapshotKind_SNAPSHOT_KIND_MARK || got.GetSnapshots()[0].GetAmount() != 40 ||
		!bytes.Equal(got.GetSnapshots()[0].GetPoolId(), bytes.Repeat([]byte{0x77}, 28)) {
		t.Fatalf("snapshots %v", got.GetSnapshots())
	}
	if len(got.GetBlocksPrevious()) != 1 || got.GetBlocksPrevious()[0].GetCount() != 3 || len(got.GetBlocksCurrent()) != 1 || got.GetBlocksCurrent()[0].GetCount() != 5 {
		t.Fatalf("blocks %v %v", got.GetBlocksPrevious(), got.GetBlocksCurrent())
	}
	if got.GetPparams().GetProtocolVersion().GetMajor() != 12 {
		t.Fatalf("pparams %v", got.GetPparams())
	}
}

// Byron has no ledger state of this shape.
func TestCodecLedgerStateByronUnsupported(t *testing.T) {
	t.Parallel()
	_, err := codecLedgerState(context.Background(), &v1.CodecLedgerStateRequest{
		Era: v1.Era_ERA_BYRON, Format: v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, Cbor: eraState(t, "shelley_nes"),
	})
	if err == nil {
		t.Fatal("Byron ledger state accepted")
	}
}

func TestCodecDijkstraNewEpochStateRejections(t *testing.T) {
	t.Parallel()
	good := eraState(t, "dijkstra_nes")
	for name, b := range map[string][]byte{
		"truncated":    good[:len(good)-1],
		"not an array": {0x01},
	} {
		got := eraLedgerState(t, v1.Era_ERA_DIJKSTRA, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, b)
		mustReject(t, got.GetRejection(), name)
	}
}

// Shelley through Babbage states, written by the pinned ledger: dingo reads
// the protocol parameters with gouroboros's per-era layouts, which are not the
// ledger's (TestDivergenceLedgerStatePreConwayPParams), so the adapter reports
// a decode failure where the reference reads the state.
func TestCodecPreConwayNewEpochStateIsRejected(t *testing.T) {
	t.Parallel()
	for era, name := range map[v1.Era]string{
		v1.Era_ERA_SHELLEY: "shelley", v1.Era_ERA_ALLEGRA: "allegra", v1.Era_ERA_MARY: "mary",
		v1.Era_ERA_ALONZO: "alonzo", v1.Era_ERA_BABBAGE: "babbage",
	} {
		got := eraLedgerState(t, era, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, eraState(t, name+"_nes"))
		mustReject(t, got.GetRejection(), name)
	}
}

// A MemPack output decodes under every era; dingo writes the Babbage map
// format (TestDivergenceMempackTxOutIsMapFormat), so the rows are read back
// through the Babbage output decoder.
func TestCodecMempackTxOutEras(t *testing.T) {
	t.Parallel()
	addr := append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...)
	policy := bytes.Repeat([]byte{0x22}, 28)
	for _, c := range []struct {
		era    v1.Era
		file   string
		coin   uint64
		assets bool
		datum  []byte
	}{
		{v1.Era_ERA_SHELLEY, "shelley_ada", 1500000, false, nil},
		{v1.Era_ERA_ALLEGRA, "allegra_ada", 1500000, false, nil},
		{v1.Era_ERA_MARY, "mary_ada", 1500000, false, nil},
		{v1.Era_ERA_MARY, "mary_assets", 2000000, true, nil},
		{v1.Era_ERA_ALONZO, "alonzo_ada", 1500000, false, nil},
		{v1.Era_ERA_ALONZO, "alonzo_datum_hash", 1500000, false, bytes.Repeat([]byte{0x33}, 32)},
		{v1.Era_ERA_BABBAGE, "babbage_ada", 1500000, false, nil},
		{v1.Era_ERA_BABBAGE, "babbage_datum_hash", 1500000, false, bytes.Repeat([]byte{0x33}, 32)},
		{v1.Era_ERA_DIJKSTRA, "dijkstra_ada", 1500000, false, nil},
	} {
		got := eraLedgerState(t, c.era, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT, eraState(t, "mempack_"+c.file))
		mustAccept(t, got.GetRejection())
		out, err := ledger.NewBabbageTransactionOutputFromCbor(got.GetCbor())
		if err != nil {
			t.Fatalf("%s: %v", c.file, err)
		}
		gotAddr, err := out.Address().Bytes()
		if err != nil {
			t.Fatalf("%s: %v", c.file, err)
		}
		if !bytes.Equal(gotAddr, addr) || out.Amount().Uint64() != c.coin || (out.Assets() != nil) != c.assets {
			t.Fatalf("%s: address %x amount %v assets %v", c.file, gotAddr, out.Amount(), out.Assets())
		}
		if c.assets && out.Assets().Asset(common.NewBlake2b224(policy), []byte("aa")).Uint64() != 5 {
			t.Fatalf("%s: assets %v", c.file, out.Assets())
		}
		if h := out.DatumHash(); (c.datum == nil) != (h == nil) || (h != nil && !bytes.Equal(h[:], c.datum)) {
			t.Fatalf("%s: datum hash %v, want %x", c.file, h, c.datum)
		}
	}
}

// An output of the state: dingo reports the Babbage map format where the
// reference writes the array (TestDivergenceMempackTxOutIsMapFormat); the
// input is the one the state holds.
func TestCodecDijkstraNewEpochStateUtxo(t *testing.T) {
	t.Parallel()
	got := eraLedgerState(t, v1.Era_ERA_DIJKSTRA, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, eraState(t, "dijkstra_utxo_nes"))
	mustAccept(t, got.GetRejection())
	u := got.GetState().GetUtxo()
	if len(u) != 1 || u[0].GetTxin().GetIndex() != 0 || !bytes.Equal(u[0].GetTxin().GetTxId(), bytes.Repeat([]byte{0x42}, 32)) {
		t.Fatalf("utxo %v", u)
	}
	out, err := ledger.NewBabbageTransactionOutputFromCbor(u[0].GetTxout())
	if err != nil {
		t.Fatal(err)
	}
	if out.Amount().Uint64() != 1500000 {
		t.Fatalf("amount %v", out.Amount())
	}
}

// eraStateDigests are the blake2b-256 digests of the states Test.Codec builds
// (nesDigests there). A state whose bytes differ was not written by the
// reference.
var eraStateDigests = map[string]string{
	"allegra_nes":           "6c01ce0779d32300e603ff5f94832f52d832e4a3500db01f2506f032a08a8c96",
	"allegra_utxo_nes":      "b58034ac0a0c79b820581cfbec540dd75527900b8c3101c9cf952e2bd0533306",
	"alonzo_nes":            "2d8d512ef1dc808a64d5cb83072452bbf682bb2448badbce03e2b55bb54dffc1",
	"alonzo_utxo_nes":       "107b3fd2a1de92dab2d31dcf829646c240a6b97a203760ae61b0f1e4f5fc4da9",
	"babbage_nes":           "86091a4c5ba84f948d5d9931ba85ea094e422f882575075fe68e1d242a7e1b83",
	"babbage_utxo_nes":      "a729f1c7730dab5058812d3e4907536b63b9c774169adde97a1e5a973df89381",
	"dijkstra_nes":          "c0ca711ca381d19e96e377864f2ea5fd9ff87f309e95aad4c942f27aaf14fefd",
	"dijkstra_utxo_ix1_nes": "721e1471862bfcaccacd5e87aa70d5eccb10ecade5295639aa9591394edf8ec4",
	"dijkstra_utxo_nes":     "b172cb4b0db3ac81cc952494c470850d814a147686cb8f8d794a4cd3657cbfb9",
	"mary_nes":              "f44145f0aad0599a27095afdbff2ebc0386c1d9633500162a64c5bdc63fa4b14",
	"mary_utxo_nes":         "5c453f511095564e7ed987123ca0e9ef393ba9dd996da41f75b492f22aa91814",
	"shelley_nes":           "9bcaf14a81bd1ce1784cd1968e550f883f26e6eb0ef6f70904b39d66ce509296",
	"shelley_utxo_nes":      "f061636f4889aa9ca976552a0b3ef941a457e722b7364374fdd7c2a3b168af7c",
}

func TestEraStatesAreTheReferencesBytes(t *testing.T) {
	t.Parallel()
	for name, want := range eraStateDigests {
		sum := blake2b.Sum256(eraState(t, name))
		if got := hex.EncodeToString(sum[:]); got != want {
			t.Errorf("%s: digest %s, want %s", name, got, want)
		}
	}
}
