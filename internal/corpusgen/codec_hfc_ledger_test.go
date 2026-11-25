package corpusgen

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"encoding/hex"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/lsfixture"
)

// hfcTag is the tag of Cardano's encodeDiskHfcBlock: Byron blocks keep their
// own encoding (boundary 0, regular 1), every later era follows.
var hfcTags = map[string]uint{"shelley": 2, "allegra": 3, "mary": 4, "alonzo": 5, "babbage": 6, "conway": 7}

func hfcWire(tag uint, inner []byte) []byte {
	return append([]byte{0x82, byte(tag)}, inner...)
}

func hfcCase(t *testing.T, version uint32, b []byte) *v1.Call {
	return &v1.Call{Request: &v1.Call_CodecHfcBlock{CodecHfcBlock: &v1.CodecHfcBlockRequest{NodeToNodeVersion: version, Cbor: b}}}
}

// hfcCases wraps the real mainnet blocks, one per era, in the consensus disk
// form [tag, block], and in the node-to-node form (that array in CBOR tag
// 24), with the malformed variants of each rule.
func hfcCases(t *testing.T, dir string, out []entry) []entry {
	add := func(name, why string, call *v1.Call) {
		out = append(out, entry{"codec_hfc_block_" + name, why, call})
	}
	for _, e := range codecEras {
		blk := readHex(t, dir, e.file)
		wire := hfcWire(hfcTags[e.name], blk)
		add(e.name, "real mainnet "+e.name+" block in the disk form", hfcCase(t, 0, wire))
		add(e.name+"_n2n", "the same block in the node-to-node form", hfcCase(t, 14, mustCbor(t, cbor.WrappedCbor(wire))))
	}
	conway := readHex(t, dir, "conway_block.hex")
	good := hfcWire(7, conway)
	tr, tl := malformed(good)
	add("conway_truncated", "disk form cut by one byte is rejected", hfcCase(t, 0, tr))
	add("conway_trailing", "disk form with a trailing byte is rejected", hfcCase(t, 0, tl))
	add("conway_wrong_tag", "a Conway block under the Babbage tag does not decode", hfcCase(t, 0, hfcWire(6, conway)))
	add("conway_unknown_tag", "tag 9 names no era", hfcCase(t, 0, hfcWire(9, conway)))
	add("conway_n2n_truncated", "node-to-node form cut by one byte is rejected", hfcCase(t, 14, mustCbor(t, cbor.WrappedCbor(good))[:len(mustCbor(t, cbor.WrappedCbor(good)))-1]))
	add("conway_n2n_unwrapped", "the disk form is no node-to-node form: tag 24 is missing", hfcCase(t, 14, good))
	add("conway_n2n_trailing", "node-to-node form with a trailing byte is rejected", hfcCase(t, 14, append(mustCbor(t, cbor.WrappedCbor(good)), 0)))
	add("three_elements", "an envelope of three elements is rejected", hfcCase(t, 0, append([]byte{0x83, 0x07}, append(bytes.Clone(conway), 0x00)...)))
	// The header is the one of the consensus golden Dijkstra block.
	djHeader := readHex(t, "../../cmd/dxnode-go/testdata", "dijkstra_header.hex")
	djBlock := func(body ...byte) []byte { return append(append([]byte{0x82}, djHeader...), body...) }
	add("dijkstra_golden_layout", "the consensus golden Dijkstra block has the older body layout (bodies, witnesses, auxiliary data, invalid transactions) and both nodes reject it", hfcCase(t, 0, hfcWire(8, append(append([]byte{0x86}, djHeader...), 0x80, 0x80, 0xa0, 0x80))))
	add("dijkstra_golden_layout_n2n", "the same block in the node-to-node form", hfcCase(t, 14, mustCbor(t, cbor.WrappedCbor(hfcWire(8, append(append([]byte{0x86}, djHeader...), 0x80, 0x80, 0xa0, 0x80))))))
	add("dijkstra_empty_payload", "tag 8 followed by an empty list is no block", hfcCase(t, 0, hfcWire(8, []byte{0x80})))
	add("dijkstra_truncated", "a Dijkstra block cut by one byte is rejected", hfcCase(t, 0, hfcWire(8, djBlock(0x84, 0xf6, 0x80, 0xf6, 0xf6)[:len(djHeader)+5])))
	add("dijkstra_body_three_components", "a Dijkstra body of [transactions, leios, peras]: gouroboros accepts it, the reference needs four components", hfcCase(t, 0, hfcWire(8, djBlock(0x83, 0x80, 0xf6, 0xf6))))
	add("dijkstra_body_four_components", "a Dijkstra body of [invalid transactions, transactions, leios, peras]: the reference accepts it, gouroboros needs three components", hfcCase(t, 0, hfcWire(8, djBlock(0x84, 0xf6, 0x80, 0xf6, 0xf6))))

	byron := readHex(t, dir, "byron_block.hex")
	add("byron_main", "real mainnet Byron regular block: tag 1, Byron's own encoding", hfcCase(t, 0, hfcWire(1, byron)))
	add("byron_main_n2n", "the Byron block in the node-to-node form", hfcCase(t, 14, mustCbor(t, cbor.WrappedCbor(hfcWire(1, byron)))))
	add("byron_as_boundary", "a regular Byron block under the boundary tag does not decode", hfcCase(t, 0, hfcWire(0, byron)))
	return out
}

// blueprintVectors locates the Cardano Blueprint ledger corpus that the
// ouroboros-mock conformance package vendors, in the module cache. Set
// DXDINGO_BLUEPRINT_VECTORS to use another copy.
func blueprintVectors(t *testing.T) string {
	out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", "github.com/blinklabs-io/ouroboros-mock").Output()
	if err != nil {
		t.Fatalf("locate ouroboros-mock: %v", err)
	}
	return filepath.Join(strings.TrimSpace(string(out)), "conformance", "blueprint-vectors.tar.gz")
}

type vectorArchive struct {
	files map[string][]byte
}

func openVectors(t *testing.T) *vectorArchive {
	path := os.Getenv("DXDINGO_BLUEPRINT_VECTORS")
	if path == "" {
		path = blueprintVectors(t)
	}
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	zr, err := gzip.NewReader(f)
	if err != nil {
		t.Fatal(err)
	}
	a := &vectorArchive{files: map[string][]byte{}}
	tr := tar.NewReader(zr)
	for {
		h, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			t.Fatal(err)
		}
		if h.Typeflag != tar.TypeReg || !strings.Contains(h.Name, "/dump/") {
			continue
		}
		b, err := io.ReadAll(tr)
		if err != nil {
			t.Fatal(err)
		}
		a.files[h.Name[strings.Index(h.Name, "/dump/")+len("/dump/"):]] = b
	}
	return a
}

func (a *vectorArchive) nes(t *testing.T, vector, state string, opt lsfixture.Options) []byte {
	t.Helper()
	raw, ok := a.files["Conway.Imp.ConwayImpSpec - Version 10."+vector]
	if !ok {
		raw, ok = a.files[vector]
	}
	if !ok {
		t.Fatalf("no vector %q", vector)
	}
	b, err := lsfixture.FromVector(raw, func(hash string) ([]byte, error) {
		p, ok := a.files["pparams-by-hash/"+hash]
		if !ok {
			return nil, io.EOF
		}
		return p, nil
	}, state, opt)
	if err != nil {
		t.Fatalf("%s: %v", vector, err)
	}
	return b
}

func ledgerStateCall(format v1.LedgerStateFormat, era v1.Era, b []byte) *v1.Call {
	return &v1.Call{Request: &v1.Call_CodecLedgerState{CodecLedgerState: &v1.CodecLedgerStateRequest{Era: era, Format: format, Cbor: b}}}
}

// ledgerStateCases reads Conway ledger states from Cardano Blueprint
// vectors, each wrapped as a NewEpochState (lsfixture), and MemPack outputs.
func ledgerStateCases(t *testing.T, out []entry) []entry {
	a := openVectors(t)
	nesFormat, mpFormat := v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT
	conway := v1.Era_ERA_CONWAY
	add := func(name, why string, call *v1.Call) {
		out = append(out, entry{"codec_ledger_state_" + name, why, call})
	}
	pool := rep(0x77, 28)
	snap := []lsfixture.Stake{
		{Hash: rep(0x01, 28), Amount: 4_000_000, Pool: pool},
		{Script: true, Hash: rep(0x03, 28), Amount: 5_000_000, Pool: pool},
		{Hash: rep(0x02, 28), Amount: 1, Pool: rep(0x78, 28)},
	}
	shared := []lsfixture.Stake{
		{Hash: rep(0x01, 28), Amount: 4_000_000, Pool: pool},
		{Script: true, Hash: rep(0x01, 28), Amount: 5_000_000, Pool: pool},
	}
	blocks := lsfixture.Blocks{string(pool): 12, string(rep(0x78, 28)): 3}
	full := lsfixture.Options{Epoch: 612, Treasury: 7_000_000_000, Reserves: 9_000_000_000_000, Prev: blocks, Cur: lsfixture.Blocks{string(pool): 4}, Mark: snap, Set: snap[:2], Go: snap[:1]}

	for _, c := range []struct{ name, why, vector, state string }{
		{"minimal", "a single output and no accounts, pools or proposals", "EPOCH.Treasury.TreasuryWithdrawal/1", "newLedgerState"},
		{"before", "the state before the transaction of a vector, not the one after it", "EPOCH.Treasury.TreasuryWithdrawal/1", "oldLedgerState"},
		{"pool_retire_reregister", "a pool with a staged re-registration, accounts delegating to it and a DRep", "DELEG.Delegate both stake and vote - separated out for conformance mismatch.Delegate, retire and re-register pool/10", "newLedgerState"},
		{"spo_ratify", "three pools and a ratification vector", "RATIFY.ParameterChange affects existing proposals.SPO.Decreasing the threshold ratifies a hitherto-unratifiable proposal/32", "newLedgerState"},
		{"drep_ratify", "two DReps with delegations and proposals", "RATIFY.ParameterChange affects existing proposals.DRep.Decreasing the threshold ratifies a hitherto-unratifiable proposal/31", "newLedgerState"},
		{"many_utxo", "nineteen outputs, eighteen accounts and ten proposals", "GOV.Proposals.Consistency.Subtrees are pruned when proposals expire over multiple rounds/53", "newLedgerState"},
		{"proposal_forest", "eleven proposals chained under enacted roots", "GOV.Proposals.Consistency.Subtrees are pruned when competing proposals are enacted over multiple rounds/34", "newLedgerState"},
		{"treasury_withdrawals", "treasury withdrawal proposals and a DRep", "ENACT.Treasury withdrawals.Withdrawals exceeding treasury submitted in several proposals within the same epoch/60", "newLedgerState"},
		{"drep_registered", "a registered DRep with a deposit", "GOVCERT.fails for.DRep already registered/0", "newLedgerState"},
		{"info_event", "a pool, accounts and parameter-change proposals", "EPOCH.Events.emits event.GovInfoEvent/15", "newLedgerState"},
		{"plutus_mint", "an Alonzo-era vector: a minting script in the UTxO", "../AlonzoImpSpec.UTXOW.Valid transactions.PlutusV1.Validating MINT script/1", "newLedgerState"},
	} {
		vec := c.vector
		if strings.HasPrefix(vec, "../") {
			vec = "Conway.Imp." + strings.TrimPrefix(vec, "../")
		}
		add("nes_"+c.name, c.why, ledgerStateCall(nesFormat, conway, a.nes(t, vec, c.state, lsfixture.Options{Epoch: 612, NoUTxO: true})))
	}
	const poolVec = "EPOCH.Events.emits event.GovInfoEvent/15"
	full.NoUTxO = true
	rich := a.nes(t, poolVec, "newLedgerState", full)
	add("nes_snapshot_shared_hash", "a key credential and a script credential with the same hash in one snapshot", ledgerStateCall(nesFormat, conway, a.nes(t, poolVec, "newLedgerState", lsfixture.Options{Epoch: 612, NoUTxO: true, Mark: shared})))
	add("nes_utxo_minimal", "the minimal vector with its output: the state as the vector holds it", ledgerStateCall(nesFormat, conway, a.nes(t, "EPOCH.Treasury.TreasuryWithdrawal/1", "newLedgerState", lsfixture.Options{Epoch: 612})))
	add("nes_utxo_many", "nineteen outputs with their MemPack values", ledgerStateCall(nesFormat, conway, a.nes(t, "GOV.Proposals.Consistency.Subtrees are pruned when proposals expire over multiple rounds/53", "newLedgerState", lsfixture.Options{Epoch: 612})))
	add("nes_snapshots_and_blocks", "stake snapshots with key and script credentials, blocks made by two pools, and non-zero pots", ledgerStateCall(nesFormat, conway, rich))
	tr, tl := malformed(rich)
	add("nes_truncated", "state cut by one byte is rejected", ledgerStateCall(nesFormat, conway, tr))
	add("nes_trailing", "state with a trailing byte is rejected", ledgerStateCall(nesFormat, conway, tl))
	add("nes_short_array", "a six-element array is no NewEpochState", ledgerStateCall(nesFormat, conway, append([]byte{0x86}, rich[1:]...)))
	add("nes_integer", "an integer is no state", ledgerStateCall(nesFormat, conway, []byte{0x01}))
	add("nes_conway_bytes_as_babbage", "a Conway state under the Babbage era: the parameters have the wrong shape, rejected on both nodes", ledgerStateCall(nesFormat, v1.Era_ERA_BABBAGE, rich))

	// MemPack outputs: the byte-string values of a vector's UTxO map, and
	// outputs written by hand for each tag.
	vals, err := lsfixture.TxOutBytes(a.nes(t, "GOV.Proposals.Consistency.Subtrees are pruned when proposals expire over multiple rounds/53", "newLedgerState", lsfixture.Options{}))
	if err != nil {
		t.Fatal(err)
	}
	for i, v := range vals {
		if i >= 4 {
			break
		}
		add("mempack_vector_"+string(rune('0'+i)), "an output from the UTxO map of a ledger vector", ledgerStateCall(mpFormat, conway, v))
	}
	addr := append([]byte{0x61}, rep(0x11, 28)...)
	ada := append(append([]byte{0x00, 0x1d}, addr...), 0x00, 0xbd, 0x84, 0x40)
	add("mempack_ada_only", "tag 0: address and an ada-only value", ledgerStateCall(mpFormat, conway, ada))
	tr, tl = malformed(ada)
	add("mempack_truncated", "output cut by one byte is rejected", ledgerStateCall(mpFormat, conway, tr))
	add("mempack_trailing", "output with a trailing byte is rejected", ledgerStateCall(mpFormat, conway, tl))
	add("mempack_bad_tag", "tag 9 is no output", ledgerStateCall(mpFormat, conway, append([]byte{0x09}, ada[1:]...)))
	add("mempack_empty", "no bytes", ledgerStateCall(mpFormat, conway, nil))
	for name, h := range map[string]string{
		"datum_hash":     "01391067f33146617a5e61936081db3b2117cbf59bd21237" + "48f58ac9678656895789b05dc94c007bd5fdf766585c8d96c7d8352238941d2c641d7200fa8900923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
		"address_hash28": "0301895789b05dc94c007bd5fdf766585c8d96c7d8352238941d2c641d72615e7a614631f367cb17213bdb8160938af5483712d29bf500000000568667c900fa8900c343bf03e418399204eeb28eb4f64e4bb9d1d82073d1bebaec446fe886d09aff",
	} {
		b, err := hex.DecodeString(h)
		if err != nil {
			t.Fatal(err)
		}
		add("mempack_"+name, "a MemPack output from the dingo importer tests (tag "+name+")", ledgerStateCall(mpFormat, conway, b))
	}
	add("mempack_alonzo_conway_row", "the ada-only row under the Alonzo era: the same bytes decode", ledgerStateCall(mpFormat, v1.Era_ERA_ALONZO, ada))
	return eraLedgerStateCases(t, out)
}

// eraLedgerStateCases adds the Shelley through Dijkstra states and MemPack
// rows the pinned ledger wrote (the builders in Test.Codec: sampleNes and
// mempackFixtures), read from the files the Go tests read.
func eraLedgerStateCases(t *testing.T, out []entry) []entry {
	const dir = "../../cmd/dxnode-go/testdata/ledger_state/eras"
	eras := []struct {
		name string
		era  v1.Era
	}{
		{"shelley", v1.Era_ERA_SHELLEY}, {"allegra", v1.Era_ERA_ALLEGRA}, {"mary", v1.Era_ERA_MARY},
		{"alonzo", v1.Era_ERA_ALONZO}, {"babbage", v1.Era_ERA_BABBAGE}, {"dijkstra", v1.Era_ERA_DIJKSTRA},
	}
	nes, mp := v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT
	add := func(name, why string, format v1.LedgerStateFormat, e v1.Era, file string) {
		out = append(out, entry{"codec_ledger_state_" + name, why, ledgerStateCall(format, e, readHex(t, dir, file+".hex"))})
	}
	for _, e := range eras {
		add("nes_"+e.name, "a "+e.name+" state from the ledger's own encoder: epoch, pots, a pool, a mark snapshot entry, blocks made", nes, e.era, e.name+"_nes")
		add("nes_"+e.name+"_utxo", "the same state with one output", nes, e.era, e.name+"_utxo_nes")
	}
	add("nes_dijkstra_utxo_ix1", "the Dijkstra state with its output at index 1: the ledger packs the index little-endian", nes, v1.Era_ERA_DIJKSTRA, "dijkstra_utxo_ix1_nes")
	for _, r := range []struct {
		era   string
		label string
	}{
		{"shelley", "ada"}, {"allegra", "ada"}, {"mary", "ada"}, {"mary", "assets"}, {"alonzo", "ada"}, {"alonzo", "datum_hash"},
		{"babbage", "ada"}, {"babbage", "datum_hash"}, {"dijkstra", "ada"},
	} {
		var e v1.Era
		for _, x := range eras {
			if x.name == r.era {
				e = x.era
			}
		}
		add("mempack_"+r.era+"_"+r.label, "a "+r.era+" MemPack output ("+r.label+") from the ledger's own packer", mp, e, "mempack_"+r.era+"_"+r.label)
	}
	return out
}
