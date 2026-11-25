package corpusgen

import (
	"bytes"
	"encoding/hex"
	"errors"
	"math/big"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/blinklabs-io/dingo/ledger/hardfork"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"github.com/blinklabs-io/plutigo/syn"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// goModDir is the directory of a module the go.work resolves.
func goModDir(t *testing.T, module string) string {
	t.Helper()
	out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", module).Output()
	if err != nil {
		t.Fatal(err)
	}
	return strings.TrimSpace(string(out))
}

// mainnetBlockTxs decodes the era's mainnet block shipped in gouroboros'
// test data and returns its transactions.
func mainnetBlockTxs(t *testing.T, era string) []common.Transaction {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(goModDir(t, "github.com/blinklabs-io/gouroboros"), "internal", "testdata", era+"_block.hex"))
	if err != nil {
		t.Fatal(err)
	}
	raw, err := hex.DecodeString(strings.TrimSpace(string(b)))
	if err != nil {
		t.Fatal(err)
	}
	var blk ledger.Block
	switch era {
	case "alonzo":
		blk, err = ledger.NewAlonzoBlockFromCbor(raw)
	case "babbage":
		blk, err = ledger.NewBabbageBlockFromCbor(raw)
	default:
		blk, err = ledger.NewConwayBlockFromCbor(raw)
	}
	if err != nil {
		t.Fatal(err)
	}
	return blk.Transactions()
}

// mainnetEraHistory is the mainnet hard-fork history: Byron's 20-second
// slots, then one-second slots from epoch 208, with the epoch of each
// transition. Conway is the open era.
func mainnetEraHistory() *v1.EraHistory {
	const shelleyStartSlot, byronEndTime = 4492800, 89856000
	type era struct {
		era                  v1.Era
		startEpoch, endEpoch uint64 // endEpoch 0: open
	}
	shelleyEpochs := []era{
		{v1.Era_ERA_SHELLEY, 208, 236}, {v1.Era_ERA_ALLEGRA, 236, 251}, {v1.Era_ERA_MARY, 251, 290},
		{v1.Era_ERA_ALONZO, 290, 365}, {v1.Era_ERA_BABBAGE, 365, 507}, {v1.Era_ERA_CONWAY, 507, 0},
	}
	h := &v1.EraHistory{SystemStartUnix: 1506203091}
	h.Eras = append(h.Eras, &v1.EraSummary{
		Era: v1.Era_ERA_BYRON, StartTime: rat("0", "1"),
		EndSlot: u64(shelleyStartSlot), EndEpoch: u64(208), EndTime: rat("89856000", "1"),
		EpochLength: 21600, SlotLength: rat("20", "1"), SafeZone: 4320,
	})
	for _, e := range shelleyEpochs {
		start := uint64(shelleyStartSlot) + (e.startEpoch-208)*432000
		es := &v1.EraSummary{
			Era: e.era, StartSlot: start, StartEpoch: e.startEpoch,
			StartTime:   rat(itoa(byronEndTime+start-shelleyStartSlot), "1"),
			EpochLength: 432000, SlotLength: rat("1", "1"), SafeZone: 129600,
		}
		if e.endEpoch != 0 {
			end := uint64(shelleyStartSlot) + (e.endEpoch-208)*432000
			es.EndSlot, es.EndEpoch, es.EndTime = u64(end), u64(e.endEpoch), rat(itoa(byronEndTime+end-shelleyStartSlot), "1")
		}
		h.Eras = append(h.Eras, es)
	}
	return h
}

func u64(v uint64) *uint64 { return &v }

func itoa(v uint64) string {
	if v == 0 {
		return "0"
	}
	var b []byte
	for ; v > 0; v /= 10 {
		b = append([]byte{byte('0' + v%10)}, b...)
	}
	return string(b)
}

// plutusScriptHashes lists the hashes of the transaction's witness Plutus
// scripts, V1 then V2 then V3.
func plutusScriptHashes(tx common.Transaction) [][]byte {
	var out [][]byte
	w := tx.Witnesses()
	if w == nil {
		return nil
	}
	for _, s := range w.PlutusV1Scripts() {
		h := s.Hash()
		out = append(out, h.Bytes())
	}
	for _, s := range w.PlutusV2Scripts() {
		h := s.Hash()
		out = append(out, h.Bytes())
	}
	for _, s := range w.PlutusV3Scripts() {
		h := s.Hash()
		out = append(out, h.Bytes())
	}
	return out
}

// cborHead writes a CBOR head: major type and argument.
func cborHead(major byte, n uint64) []byte {
	m := major << 5
	switch {
	case n < 24:
		return []byte{m | byte(n)}
	case n < 1<<8:
		return []byte{m | 24, byte(n)}
	case n < 1<<16:
		return []byte{m | 25, byte(n >> 8), byte(n)}
	}
	return []byte{m | 26, byte(n >> 24), byte(n >> 16), byte(n >> 8), byte(n)}
}

func cborBytes(b []byte) []byte { return append(cborHead(2, uint64(len(b))), b...) }

func cat(parts ...[]byte) []byte {
	var out []byte
	for _, p := range parts {
		out = append(out, p...)
	}
	return out
}

// syntheticScript is a Plutus script of the language that ignores its
// arguments and returns unit (or fails with error), wrapped as the ledger stores it (the flat bytes
// as a CBOR byte string). The block data carries no reference scripts, so
// the transactions that use them are given one of these.
func syntheticScript(t *testing.T, lang uint, fails bool) []byte {
	t.Helper()
	// V3 scripts must return unit; V1 and V2 accept any result.
	var body syn.Term[syn.DeBruijn] = &syn.Constant{Con: &syn.Unit{}}
	if fails {
		body = &syn.Error{}
	}
	args, version := 1, [3]uint32{1, 1, 0}
	if lang < 3 {
		args, version = 3, [3]uint32{1, 0, 0}
	}
	for range args {
		body = &syn.Lambda[syn.DeBruijn]{Body: body}
	}
	flat, err := syn.Encode(&syn.Program[syn.DeBruijn]{Version: version, Term: body})
	if err != nil {
		t.Fatal(err)
	}
	return cborBytes(flat)
}

func scriptHash(lang uint, wrapped []byte) []byte {
	h, err := blake2b.New(28, nil)
	if err != nil {
		panic(err)
	}
	h.Write(append([]byte{byte(lang)}, wrapped...))
	return h.Sum(nil)
}

// chainTx is a transaction from the block data with the UTxO it needs.
type chainTx struct {
	name string
	era  v1.Era
	// pv is the protocol major version the transaction ran under.
	pv  uint32
	raw []byte
	// utxo holds the resolved inputs and reference inputs.
	utxo []*v1.UtxoEntry
}

// madeUpUtxos gives a block-data transaction the UTxO it needs. The block
// data carries no parent outputs, so each is made up: a spending redeemer's
// input sits at the address of scriptHash, carrying the first witness
// datum's hash, and every other input at a key address. refScript, when set,
// is the reference script on the first reference input.
func madeUpUtxos(t *testing.T, tx common.Transaction, babbage bool, scriptHash, refScript []byte, refLang uint) []*v1.UtxoEntry {
	t.Helper()
	spend := map[uint32]bool{}
	if w := tx.Witnesses(); w != nil && w.Redeemers() != nil {
		for k := range w.Redeemers().Iter() {
			if k.Tag == common.RedeemerTagSpend {
				spend[k.Index] = true
			}
		}
	}
	var datumHash []byte
	if w := tx.Witnesses(); w != nil {
		for _, d := range w.PlutusData() {
			h := d.Hash()
			datumHash = h.Bytes()
			break
		}
	}
	sorted := append([]common.TransactionInput(nil), tx.Inputs()...)
	sortInputs(sorted)
	var out []*v1.UtxoEntry
	add := func(in common.TransactionInput, sh, dh, ref []byte) {
		out = append(out, &v1.UtxoEntry{
			Txin:  &v1.TxIn{TxId: in.Id().Bytes(), Index: in.Index()},
			Txout: madeUpTxOut(sh, dh, ref, refLang, babbage),
		})
	}
	for i, in := range sorted {
		if spend[uint32(i)] && scriptHash != nil {
			add(in, scriptHash, datumHash, nil)
		} else {
			add(in, nil, nil, nil)
		}
	}
	for i, in := range tx.ReferenceInputs() {
		if i == 0 {
			add(in, nil, nil, refScript)
		} else {
			add(in, nil, nil, nil)
		}
	}
	return out
}

func sortInputs(in []common.TransactionInput) {
	slices.SortFunc(in, func(a, b common.TransactionInput) int {
		if c := bytes.Compare(a.Id().Bytes(), b.Id().Bytes()); c != 0 {
			return c
		}
		return int(a.Index()) - int(b.Index())
	})
}

// madeUpTxOut encodes a 5 ADA output at the script's enterprise address with
// a datum hash, or at a key address, with an optional reference script.
func madeUpTxOut(scriptHash, datumHash, refScript []byte, refLang uint, babbage bool) []byte {
	addr := append([]byte{0x61}, rep(0x11, 28)...)
	if scriptHash != nil {
		addr = append([]byte{0x71}, scriptHash...)
	}
	coin := cborHead(0, 5_000_000)
	if !babbage {
		if datumHash != nil && scriptHash != nil {
			return cat(cborHead(4, 3), cborBytes(addr), coin, cborBytes(datumHash))
		}
		return cat(cborHead(4, 2), cborBytes(addr), coin)
	}
	n := uint64(2)
	body := cat(cborHead(0, 0), cborBytes(addr), cborHead(0, 1), coin)
	if datumHash != nil && scriptHash != nil {
		n++
		body = cat(body, cborHead(0, 2), cborHead(4, 2), cborHead(0, 0), cborBytes(datumHash))
	}
	if refScript != nil {
		n++
		// script_ref = #6.24(bytes .cbor [language, script])
		inner := cat(cborHead(4, 2), cborHead(0, uint64(refLang)), cborBytes(refScript))
		body = cat(body, cborHead(0, 3), cborHead(6, 24), cborBytes(inner))
	}
	return cat(cborHead(5, n), body)
}

// plutusChainTxs are the Plutus transactions of the mainnet blocks, plus the
// one-shot mint transaction with its real UTxO.
func plutusChainTxs(t *testing.T) []chainTx {
	t.Helper()
	var out []chainTx
	pick := func(era string, idx int) common.Transaction { return mainnetBlockTxs(t, era)[idx] }
	raw := func(tx common.Transaction) []byte { return tx.Cbor() }

	// Alonzo: the witness set carries the PlutusV1 script.
	a := pick("alonzo", 0)
	out = append(out, chainTx{"alonzo_v1_spend", v1.Era_ERA_ALONZO, 6, raw(a),
		madeUpUtxos(t, a, false, plutusScriptHashes(a)[0], nil, 0)})

	// Babbage and Conway: the scripts were reference scripts the block data
	// lacks, so a synthetic one of the language sits on the first reference
	// input.
	ref := func(name string, era v1.Era, pv uint32, tx common.Transaction, lang uint) chainTx {
		s := syntheticScript(t, lang, false)
		return chainTx{name, era, pv, raw(tx), madeUpUtxos(t, tx, true, scriptHash(lang, s), s, lang)}
	}
	failing := func(name string, era v1.Era, pv uint32, tx common.Transaction, lang uint) chainTx {
		s := syntheticScript(t, lang, true)
		return chainTx{name, era, pv, raw(tx), madeUpUtxos(t, tx, true, scriptHash(lang, s), s, lang)}
	}
	out = append(out,
		failing("babbage_v2_failing_script", v1.Era_ERA_BABBAGE, 8, pick("babbage", 3), 2),
		failing("babbage_v1_failing_script", v1.Era_ERA_BABBAGE, 8, pick("babbage", 12), 1),
		ref("babbage_v2_spend_ref_script", v1.Era_ERA_BABBAGE, 8, pick("babbage", 3), 2),
		ref("babbage_v1_spend_ref_script", v1.Era_ERA_BABBAGE, 8, pick("babbage", 9), 1),
		ref("conway_v3_two_spends_ref_script", v1.Era_ERA_CONWAY, 9, pick("conway", 1), 3),
		// Three redeemers, one a script withdrawal whose script the UTxO does not hold.
		ref("conway_v3_spends_and_withdrawal", v1.Era_ERA_CONWAY, 9, pick("conway", 4), 3),
		oneShotMint(t),
	)
	return out
}

// mainnetParams carries what evaluation reads: the cost models, the
// transaction execution-unit bound and the protocol version.
func mainnetParams(t *testing.T, major uint32) *v1.ProtocolParameters {
	m1, m2, m3 := mainnetCostModels(t)
	maxMem, maxSteps := uint64(14_000_000), uint64(10_000_000_000)
	return &v1.ProtocolParameters{
		ProtocolVersion:   &v1.ProtocolVersion{Major: major},
		CostModelPlutusV1: m1,
		CostModelPlutusV2: m2,
		CostModelPlutusV3: m3,
		MaxTxExMem:        &maxMem,
		MaxTxExSteps:      &maxSteps,
	}
}

func ledgerContext(c chainTx) *v1.LedgerContext {
	return &v1.LedgerContext{Era: c.era, ProtocolVersion: &v1.ProtocolVersion{Major: c.pv}, NetworkId: 1}
}

// withdrawalTx is the case whose script withdrawal has no script in the UTxO,
// which dingo's evaluator skips and the reference reports; it is a divergence.
const withdrawalTx = "conway_v3_spends_and_withdrawal"

func evalEntry(t *testing.T, c chainTx, hist *v1.EraHistory) entry {
	return entry{"plutus_eval_ex_units_" + c.name, "execution units of every redeemer under mainnet cost models",
		&v1.Call{Request: &v1.Call_PlutusEvalExUnits{PlutusEvalExUnits: &v1.PlutusEvalExUnitsRequest{
			Context: ledgerContext(c), Pparams: mainnetParams(t, c.pv), Tx: c.raw, Utxo: c.utxo, EraHistory: hist}}}}
}

// plutusEvalDivergences are the evaluation cases the two nodes answer
// differently; each has direct tests on both sides.
func plutusEvalDivergences(t *testing.T) []entry {
	t.Helper()
	for _, c := range plutusChainTxs(t) {
		if c.name == withdrawalTx {
			e := evalEntry(t, c, mainnetEraHistory())
			e.comment = "reference reports MissingScript for the withdrawal redeemer; dingo's evaluator skips a redeemer whose script is absent (TestDivergenceMissingScriptIsSkipped)"
			return []entry{e}
		}
	}
	t.Fatal("withdrawal transaction missing")
	return nil
}

func plutusChainCases(t *testing.T) []entry {
	t.Helper()
	hist := mainnetEraHistory()
	var out []entry
	langs := map[v1.Era][]v1.PlutusVersion{
		v1.Era_ERA_ALONZO:  {v1.PlutusVersion_PLUTUS_VERSION_V1},
		v1.Era_ERA_BABBAGE: {v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2},
		v1.Era_ERA_CONWAY:  {v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2, v1.PlutusVersion_PLUTUS_VERSION_V3},
	}
	for _, c := range plutusChainTxs(t) {
		if c.name != withdrawalTx {
			out = append(out, evalEntry(t, c, hist))
		}
		out = append(out,
			entry{"plutus_script_purpose_" + c.name, "purposes of a mainnet transaction with a made-up UTxO",
				&v1.Call{Request: &v1.Call_PlutusScriptPurpose{PlutusScriptPurpose: &v1.PlutusScriptPurposeRequest{
					Context: ledgerContext(c), Tx: c.raw, Utxo: c.utxo}}}},
		)
		for _, l := range langs[c.era] {
			out = append(out, entry{
				"plutus_tx_info_" + c.name + "_v" + itoa(uint64(l)), "the TxInfo of a mainnet transaction in each language its era has",
				&v1.Call{Request: &v1.Call_PlutusTxInfo{PlutusTxInfo: &v1.PlutusTxInfoRequest{
					Context: ledgerContext(c), PlutusVersion: l, Tx: c.raw, Utxo: c.utxo, EraHistory: hist}}}})
		}
	}
	return out
}

// oneShotMint is a mainnet Conway PlutusV3 minting transaction with its real
// inputs, outputs and reference input, from gouroboros' test data.
func oneShotMint(t *testing.T) chainTx {
	t.Helper()
	dir := filepath.Join(goModDir(t, "github.com/blinklabs-io/gouroboros"), "internal", "testdata")
	read := func(name string) []byte {
		b, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil {
			t.Fatal(err)
		}
		raw, err := hex.DecodeString(strings.TrimSpace(string(b)))
		if err != nil {
			t.Fatal(err)
		}
		return raw
	}
	pairs := func(ins, outs string) []*v1.UtxoEntry {
		var inputs []shelley.ShelleyTransactionInput
		var outputs []cbor.RawMessage
		if _, err := cbor.Decode(read(ins), &inputs); err != nil {
			t.Fatal(err)
		}
		if _, err := cbor.Decode(read(outs), &outputs); err != nil {
			t.Fatal(err)
		}
		var out []*v1.UtxoEntry
		for i := range inputs {
			out = append(out, &v1.UtxoEntry{
				Txin:  &v1.TxIn{TxId: inputs[i].Id().Bytes(), Index: inputs[i].Index()},
				Txout: outputs[i],
			})
		}
		return out
	}
	utxo := append(pairs("oneshot_mint_inputs.hex", "oneshot_mint_input_outputs.hex"),
		pairs("oneshot_mint_ref_input.hex", "oneshot_mint_ref_output.hex")...)
	return chainTx{"conway_v3_one_shot_mint", v1.Era_ERA_CONWAY, 10, read("mainnet_tx_oneshot_mint.hex"), utxo}
}

// plutusEdgeCases are inputs the reference rejects or reports on that a
// well-formed transaction does not reach.
func plutusEdgeCases(t *testing.T) []entry {
	t.Helper()
	hist := mainnetEraHistory()
	var txs = map[string]chainTx{}
	for _, c := range plutusChainTxs(t) {
		txs[c.name] = c
	}
	conway := txs["conway_v3_one_shot_mint"]
	babbage := txs["babbage_v2_spend_ref_script"]
	v3, v2 := v1.PlutusVersion_PLUTUS_VERSION_V3, v1.PlutusVersion_PLUTUS_VERSION_V2
	info := func(name, why string, c chainTx, l v1.PlutusVersion, utxo []*v1.UtxoEntry, h *v1.EraHistory) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PlutusTxInfo{PlutusTxInfo: &v1.PlutusTxInfoRequest{
			Context: ledgerContext(c), PlutusVersion: l, Tx: c.raw, Utxo: utxo, EraHistory: h}}}}
	}
	// Byron's era alone: no slot of a Conway transaction converts to time.
	byronOnly := &v1.EraHistory{SystemStartUnix: hist.SystemStartUnix, Eras: hist.Eras[:1]}
	noModel := mainnetParams(t, babbage.pv)
	noModel.CostModelPlutusV2 = nil
	return []entry{
		info("plutus_tx_info_missing_input", "an input absent from the UTxO", conway, v3, conway.utxo[1:], hist),
		info("plutus_tx_info_past_horizon", "the validity slot is beyond the era history", conway, v3, conway.utxo, byronOnly),
		info("plutus_tx_info_language_not_in_era", "PlutusV3 does not exist in Babbage", babbage, v3, babbage.utxo, hist),
		info("plutus_tx_info_v2_in_alonzo", "PlutusV2 does not exist in Alonzo", txs["alonzo_v1_spend"], v2, txs["alonzo_v1_spend"].utxo, hist),
		{"plutus_eval_ex_units_missing_cost_model", "the script's language has no cost model in the parameters",
			&v1.Call{Request: &v1.Call_PlutusEvalExUnits{PlutusEvalExUnits: &v1.PlutusEvalExUnitsRequest{
				Context: ledgerContext(babbage), Pparams: noModel, Tx: babbage.raw, Utxo: babbage.utxo, EraHistory: hist}}}},
		{"plutus_script_purpose_missing_input", "a spending input absent from the UTxO has no purpose",
			&v1.Call{Request: &v1.Call_PlutusScriptPurpose{PlutusScriptPurpose: &v1.PlutusScriptPurposeRequest{
				Context: ledgerContext(babbage), Tx: babbage.raw, Utxo: babbage.utxo[:1]}}}},
	}
}

// slotState converts slots with the era history, as dxnode-go does.
type slotState struct {
	common.LedgerState
	utxos   common.ResolvedUtxos
	summary *hardfork.Summary
}

func (s slotState) UtxoById(in common.TransactionInput) (common.Utxo, error) {
	return s.utxos.UtxoById(in)
}
func (s slotState) SlotToTime(slot uint64) (time.Time, error) { return s.summary.SlotToTime(slot) }
func (s slotState) TimeToSlot(time.Time) (uint64, error)      { return 0, errors.New("unsupported") }

// txInfoBytes is the PlutusData of the transaction's TxInfo in the language,
// from gouroboros' own constructors, for the script-context cases.
func txInfoBytes(t *testing.T, c chainTx, lang v1.PlutusVersion) []byte {
	t.Helper()
	txType := map[v1.Era]uint{v1.Era_ERA_ALONZO: ledger.TxTypeAlonzo, v1.Era_ERA_BABBAGE: ledger.TxTypeBabbage, v1.Era_ERA_CONWAY: ledger.TxTypeConway}[c.era]
	tx, err := ledger.NewTransactionFromCbor(txType, c.raw)
	if err != nil {
		t.Fatal(err)
	}
	var utxos common.ResolvedUtxos
	for _, e := range c.utxo {
		in, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(e.Txin.TxId), int(e.Txin.Index))
		if err != nil {
			t.Fatal(err)
		}
		out, err := ledger.NewTransactionOutputFromCbor(e.Txout)
		if err != nil {
			t.Fatal(err)
		}
		utxos = append(utxos, common.Utxo{Id: in, Output: out})
	}
	// The history's seconds are whole, so the summary can be built directly.
	h := mainnetEraHistory()
	summary := &hardfork.Summary{SystemStart: time.Unix(h.SystemStartUnix, 0).UTC()}
	for _, e := range h.Eras {
		sec := func(r *v1.Rational) time.Duration {
			n, _ := new(big.Int).SetString(r.Numerator, 10)
			return time.Duration(n.Int64()) * time.Second
		}
		era := hardfork.EraSummary{
			Start:  hardfork.Bound{RelativeTime: sec(e.StartTime), Slot: e.StartSlot, Epoch: e.StartEpoch},
			Params: hardfork.EraParams{EpochSize: e.EpochLength, SlotLength: sec(e.SlotLength), SafeZoneSlots: e.SafeZone},
		}
		if e.EndSlot != nil {
			era.End = &hardfork.Bound{RelativeTime: sec(e.EndTime), Slot: *e.EndSlot, Epoch: *e.EndEpoch}
		}
		summary.Eras = append(summary.Eras, era)
	}
	ls := slotState{utxos: utxos, summary: summary}
	var resolved []common.Utxo
	for _, in := range append(append([]common.TransactionInput(nil), tx.Inputs()...), tx.ReferenceInputs()...) {
		u, err := utxos.UtxoById(in)
		if err != nil {
			t.Fatal(err)
		}
		resolved = append(resolved, u)
	}
	strict := c.era == v1.Era_ERA_CONWAY
	var info script.TxInfo
	switch lang {
	case v1.PlutusVersion_PLUTUS_VERSION_V1:
		info, err = infoOf(script.NewTxInfoV1FromTransaction(ls, tx, resolved, strict, uint(c.pv)))
	case v1.PlutusVersion_PLUTUS_VERSION_V2:
		info, err = infoOf(script.NewTxInfoV2FromTransaction(ls, tx, resolved, strict, uint(c.pv)))
	default:
		info, err = infoOf(script.NewTxInfoV3FromTransaction(ls, tx, resolved, uint(c.pv)))
	}
	if err != nil {
		t.Fatal(err)
	}
	b, err := cbor.Encode(info.ToPlutusData())
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func infoOf[T script.TxInfo](info T, err error) (script.TxInfo, error) { return info, err }

// plutusContextCases build script contexts for every purpose the corpus
// transactions have, and for purposes no transaction here has.
func plutusContextCases(t *testing.T) []entry {
	t.Helper()
	txs := map[string]chainTx{}
	for _, c := range plutusChainTxs(t) {
		txs[c.name] = c
	}
	v1c, v2c, v3c := v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2, v1.PlutusVersion_PLUTUS_VERSION_V3
	unitRedeemer := []byte{0xd8, 0x79, 0x80} // Constr 0 []
	datum := []byte{0x18, 0x2a}              // 42
	key28, script28 := rep(0x11, 28), rep(0x22, 28)
	hash32 := rep(0x33, 32)
	build := func(name, why string, c chainTx, lang v1.PlutusVersion, p *v1.ScriptPurpose, redeemer, datum []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PlutusScriptContext{PlutusScriptContext: &v1.PlutusScriptContextRequest{
			PlutusVersion: lang, TxInfo: txInfoBytes(t, c, lang), Purpose: p, Redeemer: redeemer, Datum: datum,
			Context: ledgerContext(c)}}}}
	}
	spend := func(c chainTx, ix uint32) *v1.ScriptPurpose {
		in := c.utxo[ix].Txin
		return &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_SPEND, Index: ix, SpendInput: in}
	}
	mint := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT, MintPolicyId: script28}
	// A stake registration certificate: [0, [0, key hash]].
	cert := cat(cborHead(4, 2), cborHead(0, 0), cborHead(4, 2), cborHead(0, 0), cborBytes(key28))
	certPurpose := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_CERT, Index: 1, Certificate: cert}
	reward := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_REWARD, Index: 0,
		RewardAccount: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: script28}}
	vote := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE, Index: 0,
		Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_DREP, Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: script28}}}
	// A proposal: [deposit, reward account, [6] (Info), [url, hash]].
	proposal := cat(cborHead(4, 4), cborHead(0, 1_000_000), cborBytes(append([]byte{0xe1}, key28...)),
		cborHead(4, 1), cborHead(0, 6), cborHead(4, 2), cborHead(3, 3), []byte("url"), cborBytes(hash32))
	propose := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_PROPOSE, Index: 0, Proposal: proposal}

	alonzo, babbage, conway := txs["alonzo_v1_spend"], txs["babbage_v2_spend_ref_script"], txs["conway_v3_one_shot_mint"]
	return []entry{
		build("plutus_script_context_v1_spend", "V1 spending: datum, redeemer and context", alonzo, v1c, spend(alonzo, 0), unitRedeemer, datum),
		build("plutus_script_context_v2_spend", "V2 spending: datum, redeemer and context", babbage, v2c, spend(babbage, 0), unitRedeemer, datum),
		build("plutus_script_context_v3_spend", "V3 spending: the context alone, carrying the datum in its ScriptInfo", conway, v3c, spend(conway, 0), unitRedeemer, datum),
		build("plutus_script_context_v3_spend_no_datum", "V3 spending without a datum", conway, v3c, spend(conway, 0), unitRedeemer, nil),
		build("plutus_script_context_v1_spend_no_datum", "V1 spending without a datum: two arguments", alonzo, v1c, spend(alonzo, 0), unitRedeemer, nil),
		build("plutus_script_context_v3_mint", "V3 minting", conway, v3c, mint, unitRedeemer, nil),
		build("plutus_script_context_v2_mint", "V2 minting: redeemer and context", babbage, v2c, mint, unitRedeemer, nil),
		build("plutus_script_context_v3_cert", "V3 certifying: the certificate and its index", conway, v3c, certPurpose, unitRedeemer, nil),
		build("plutus_script_context_v1_cert", "V1 certifying", alonzo, v1c, certPurpose, unitRedeemer, nil),
		build("plutus_script_context_v3_reward", "V3 withdrawal", conway, v3c, reward, unitRedeemer, nil),
		build("plutus_script_context_v2_reward", "V2 withdrawal", babbage, v2c, reward, unitRedeemer, nil),
		build("plutus_script_context_v3_vote", "V3 voting", conway, v3c, vote, unitRedeemer, nil),
		build("plutus_script_context_v3_propose", "V3 proposing", conway, v3c, propose, unitRedeemer, nil),
	}
}

// plutusContextDivergences are script-context cases the nodes answer
// differently.
func plutusContextDivergences(t *testing.T) []entry {
	t.Helper()
	var conway chainTx
	for _, c := range plutusChainTxs(t) {
		if c.name == "conway_v3_one_shot_mint" {
			conway = c
		}
	}
	vote := &v1.ScriptPurpose{Tag: v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE, Index: 0,
		Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_DREP, Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: rep(0x22, 28)}}}
	lang := v1.PlutusVersion_PLUTUS_VERSION_V2
	return []entry{{"plutus_script_context_v2_vote",
		"reference rejects a voting purpose in V2 with PlutusPurposeNotSupported; gouroboros' V1/V2 context panics on it (TestDivergenceVotingPurposeInV2Panics)",
		&v1.Call{Request: &v1.Call_PlutusScriptContext{PlutusScriptContext: &v1.PlutusScriptContextRequest{
			PlutusVersion: lang, TxInfo: txInfoBytes(t, conway, lang), Purpose: vote,
			Redeemer: []byte{0xd8, 0x79, 0x80}, Context: ledgerContext(conway)}}}}}
}
