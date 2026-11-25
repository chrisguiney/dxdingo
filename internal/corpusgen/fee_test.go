package corpusgen

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// The fee corpus uses real mainnet blocks and the one-shot mint transaction
// from gouroboros testdata, so the sizes, redeemers and datums are what the
// chain carries. Set DX_THIRD_PARTY to the third_party directory when it is not
// at ../../third_party.

func TestGenerateFeeCorpus(t *testing.T) {
	if os.Getenv("DXDINGO_GEN_CORPUS") == "" {
		t.Skip("set DXDINGO_GEN_CORPUS=1 to regenerate corpus/")
	}
	fx := loadFeeFixtures(t)
	writeCorpus(t, "", fx.cases(t))
	writeCorpus(t, "divergences", fx.divergenceCases(t))
}

func feeThirdParty() string {
	if v := os.Getenv("DX_THIRD_PARTY"); v != "" {
		return v
	}
	return filepath.Join("..", "..", "third_party")
}

func feeRead(t *testing.T, rel string) []byte {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(feeThirdParty(), rel))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func feeHex(t *testing.T, name string) []byte {
	t.Helper()
	b := feeRead(t, "gouroboros/internal/testdata/"+name+".hex")
	raw, err := hex.DecodeString(strings.TrimSpace(string(b)))
	if err != nil {
		t.Fatal(name, err)
	}
	return raw
}

type feeFixtures struct {
	// Raw transactions from the blocks, by name.
	tx map[string][]byte
	// Decoded copies, for inputs and outputs.
	dec map[string]lcommon.Transaction
	// Every transaction of each era's block, decoded.
	all map[uint][]lcommon.Transaction
	// Mainnet cost models: PlutusV1 from the Alonzo genesis, PlutusV3 at epoch
	// 653. PlutusV2 is a valid-length model with small values.
	v1, v2, v3 []int64
}

func loadFeeFixtures(t *testing.T) *feeFixtures {
	t.Helper()
	fx := &feeFixtures{tx: map[string][]byte{}, dec: map[string]lcommon.Transaction{}, all: map[uint][]lcommon.Transaction{}}
	blocks := []struct {
		file  string
		typ   uint
		era   uint
		names map[int]string
	}{
		{"shelley_block", ledger.BlockTypeShelley, ledger.TxTypeShelley, map[int]string{0: "shelley0"}},
		{"allegra_block", ledger.BlockTypeAllegra, ledger.TxTypeAllegra, map[int]string{0: "allegra0", 4: "allegra4"}},
		{"mary_block", ledger.BlockTypeMary, ledger.TxTypeMary, map[int]string{0: "mary0", 5: "mary5"}},
		{"alonzo_block", ledger.BlockTypeAlonzo, ledger.TxTypeAlonzo, map[int]string{0: "alonzo0", 3: "alonzo3"}},
		{"babbage_block", ledger.BlockTypeBabbage, ledger.TxTypeBabbage, map[int]string{3: "babbage3", 6: "babbage6", 9: "babbage9", 12: "babbage12", 13: "babbage13"}},
		{"conway_block", ledger.BlockTypeConway, ledger.TxTypeConway, map[int]string{0: "conway0", 1: "conway1", 4: "conway4", 7: "conway7"}},
	}
	for _, b := range blocks {
		blk, err := ledger.NewBlockFromCbor(b.typ, feeHex(t, b.file))
		if err != nil {
			t.Fatal(b.file, err)
		}
		txs := blk.Transactions()
		for _, tx := range txs {
			dec, err := ledger.NewTransactionFromCbor(b.era, tx.Cbor())
			if err != nil {
				t.Fatal(b.file, err)
			}
			fx.all[b.era] = append(fx.all[b.era], dec)
		}
		for i, name := range b.names {
			if i >= len(txs) {
				t.Fatalf("%s has %d transactions, want index %d", b.file, len(txs), i)
			}
			fx.tx[name] = bytes.Clone(txs[i].Cbor())
			tx, err := ledger.NewTransactionFromCbor(b.era, fx.tx[name])
			if err != nil {
				t.Fatal(name, err)
			}
			fx.dec[name] = tx
		}
	}
	oneshot := feeHex(t, "mainnet_tx_oneshot_mint")
	fx.tx["oneshot"] = oneshot
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, oneshot)
	if err != nil {
		t.Fatal(err)
	}
	fx.dec["oneshot"] = tx

	var genesis struct {
		CostModels  map[string][]int64 `json:"costModels"`
		ExtraConfig struct {
			CostModels map[string][]int64 `json:"costModels"`
		} `json:"extraConfig"`
	}
	if err := json.Unmarshal(feeRead(t, "gouroboros/ledger/alonzo/testdata/alonzo-genesis-extra-config.json"), &genesis); err != nil {
		t.Fatal(err)
	}
	fx.v1 = genesis.CostModels["PlutusV1"]
	fx.v3 = genesis.ExtraConfig.CostModels["PlutusV3"]
	var golden struct {
		CostModels map[string][]int64 `json:"costModels"`
	}
	if err := json.Unmarshal(feeRead(t, "cardano-ledger/eras/babbage/impl/golden/pparams.json"), &golden); err != nil {
		t.Fatal(err)
	}
	fx.v2 = golden.CostModels["PlutusV2"]
	if len(fx.v1) != 166 || len(fx.v2) != 175 || len(fx.v3) != 350 {
		t.Fatalf("cost model lengths %d %d %d", len(fx.v1), len(fx.v2), len(fx.v3))
	}
	return fx
}

func feeU64(x uint64) *uint64 { return &x }

// findOutput returns the raw bytes of the first output in the era's block
// that satisfies pred.
func (fx *feeFixtures) findOutput(t *testing.T, era uint, pred func(lcommon.TransactionOutput) bool) []byte {
	t.Helper()
	for _, tx := range fx.all[era] {
		for _, o := range tx.Outputs() {
			if pred(o) {
				return bytes.Clone(o.Cbor())
			}
		}
	}
	t.Fatalf("no output in era %d satisfies the predicate", era)
	return nil
}

func feeCall(name, why string, set func(*v1.Call)) entry {
	c := &v1.Call{}
	set(c)
	return entry{name, why, c}
}

func mustCborFee(t *testing.T, v any) []byte {
	t.Helper()
	b, err := cbor.Encode(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// feePParams are the protocol parameters most fee cases share.
func feePParams() *v1.ProtocolParameters {
	return &v1.ProtocolParameters{
		MinFeeA: feeU64(44), MinFeeB: feeU64(155381),
		PriceMem: rat("577", "10000"), PriceSteps: rat("721", "10000000"),
		MinFeeRefScriptCostPerByte: rat("15", "1"),
	}
}

func feeAddr() []byte { return append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...) }

func feeTxIn(b byte, idx uint) []any { return []any{bytes.Repeat([]byte{b}, 32), idx} }

// feeScriptOut is a map-form output with a script reference holding a Plutus V2
// script of n feeFiller bytes (the size is what the fee depends on).
func feeScriptOut(t *testing.T, coin uint64, script []byte) []byte {
	return feeScriptOutLang(t, coin, 2, script)
}

// feeScriptOutLang is feeScriptOut for a script language (1, 2 or 3).
func feeScriptOutLang(t *testing.T, coin uint64, lang uint, script []byte) []byte {
	out := mustCborFee(t, map[uint]any{0: feeAddr(), 1: coin})
	if script == nil {
		return out
	}
	inner := mustCborFee(t, []any{lang, script})
	ref := append([]byte{0xd8, 0x18}, mustCborFee(t, inner)...)
	out[0]++
	return append(append(out, 0x03), ref...)
}

// feeInlineDatumOut is a map-form output with an inline datum, a Plutus list of
// n small integers (datum option [1, #6.24(bytes)]).
func feeInlineDatumOut(t *testing.T, n int) []byte {
	out := mustCborFee(t, map[uint]any{0: feeAddr(), 1: uint64(2_000_000)})
	datum := feeHdr(4, n)
	for i := 0; i < n; i++ {
		datum = append(datum, byte(i%24))
	}
	opt := append([]byte{0x82, 0x01, 0xd8, 0x18}, mustCborFee(t, datum)...)
	out[0]++
	return append(append(out, 0x02), opt...)
}

func feeFiller(n int, seed byte) []byte {
	b := make([]byte, n)
	for i := range b {
		b[i] = seed + byte(i%7)
	}
	return b
}

// feeRefTx is a Conway transaction body with the given spending and reference
// inputs, in the 4-element wire form.
func feeRefTx(t *testing.T, spend, refs [][]any) []byte {
	body := map[uint]any{
		0:  spend,
		1:  []any{[]any{feeAddr(), uint64(2_000_000)}},
		2:  uint64(200_000),
		18: refs,
	}
	return append(append([]byte{0x84}, mustCborFee(t, body)...), 0xa0, 0xf5, 0xf6)
}

// feeTxParts returns the top-level elements of a transaction.
func feeTxParts(t *testing.T, raw []byte) []cbor.RawMessage {
	t.Helper()
	var parts []cbor.RawMessage
	if _, err := cbor.Decode(raw, &parts); err != nil {
		t.Fatal(err)
	}
	return parts
}

// feeSetIsValid rewrites the IsValid element of a 4-element transaction.
func feeSetIsValid(t *testing.T, raw []byte, valid bool) []byte {
	t.Helper()
	parts := feeTxParts(t, raw)
	if len(parts) != 4 {
		t.Fatalf("transaction has %d elements", len(parts))
	}
	flag := byte(0xf4)
	if valid {
		flag = 0xf5
	}
	return bytes.Join([][]byte{{0x84}, parts[0], parts[1], {flag}, parts[3]}, nil)
}

func feeUtxo(b byte, idx uint32, txout []byte) *v1.UtxoEntry {
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{b}, 32), Index: idx}, Txout: txout}
}

func (fx *feeFixtures) cases(t *testing.T) []entry {
	var es []entry
	add := func(e ...entry) { es = append(es, e...) }

	// tx size: one real transaction per era.
	sizeCase := func(name, why string, era v1.Era, tx []byte) entry {
		return feeCall("fee_tx_size_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeTxSize{FeeTxSize: &v1.FeeTxSizeRequest{Era: era, Tx: tx}}
		})
	}
	add(
		sizeCase("shelley", "Shelley: the transaction bytes", v1.Era_ERA_SHELLEY, fx.tx["shelley0"]),
		sizeCase("allegra", "Allegra: the transaction bytes, two inputs", v1.Era_ERA_ALLEGRA, fx.tx["allegra4"]),
		sizeCase("mary", "Mary: the transaction bytes, multi-asset outputs", v1.Era_ERA_MARY, fx.tx["mary5"]),
		sizeCase("alonzo_plutus", "Alonzo wire form carries IsValid; the size does not count that byte", v1.Era_ERA_ALONZO, fx.tx["alonzo0"]),
		sizeCase("babbage_plutus", "Babbage with a reference input and a redeemer", v1.Era_ERA_BABBAGE, fx.tx["babbage3"]),
		sizeCase("babbage_large", "Babbage, 2.7 KB", v1.Era_ERA_BABBAGE, fx.tx["babbage13"]),
		sizeCase("conway_plain", "Conway without scripts", v1.Era_ERA_CONWAY, fx.tx["conway0"]),
		sizeCase("conway_redeemers", "Conway with three redeemers", v1.Era_ERA_CONWAY, fx.tx["conway4"]),
		sizeCase("conway_oneshot_mint", "Conway Plutus V3 mint, 9.5 KB", v1.Era_ERA_CONWAY, fx.tx["oneshot"]),
		sizeCase("conway_truncated", "A transaction cut short fails to decode on both nodes", v1.Era_ERA_CONWAY, fx.tx["conway0"][:60]),
		sizeCase("shelley_wrong_era", "A Shelley transaction in Conway's decoder", v1.Era_ERA_CONWAY, fx.tx["shelley0"]),
	)
	add(sizeCase("conway_phase2_invalid", "IsValid false does not change the size", v1.Era_ERA_CONWAY, feeSetIsValid(t, fx.tx["conway4"], false)))

	// min fee.
	feeCase := func(name, why string, era v1.Era, pp *v1.ProtocolParameters, tx []byte, ref uint64) entry {
		return feeCall("fee_min_fee_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeMinFee{FeeMinFee: &v1.FeeMinFeeRequest{Era: era, Pparams: pp, Tx: tx, RefScriptSize: ref}}
		})
	}
	add(
		feeCase("shelley", "a*size + b", v1.Era_ERA_SHELLEY, feePParams(), fx.tx["shelley0"], 0),
		feeCase("allegra", "a*size + b", v1.Era_ERA_ALLEGRA, feePParams(), fx.tx["allegra0"], 0),
		feeCase("mary", "a*size + b, multi-asset", v1.Era_ERA_MARY, feePParams(), fx.tx["mary5"], 0),
		feeCase("alonzo_plutus", "size fee plus the price of the declared execution units", v1.Era_ERA_ALONZO, feePParams(), fx.tx["alonzo0"], 0),
		feeCase("babbage_plutus", "Babbage ignores the reference-script size input", v1.Era_ERA_BABBAGE, feePParams(), fx.tx["babbage3"], 5000),
		feeCase("conway_plain", "no scripts, no reference scripts", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway0"], 0),
		feeCase("conway_redeemers", "three redeemers priced by execution units", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway4"], 0),
		feeCase("conway_ref_small", "reference scripts inside the first tier", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway4"], 100),
		feeCase("conway_ref_tier_edge", "exactly one 25600-byte tier", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway4"], 25600),
		feeCase("conway_ref_past_tier", "one byte into the second tier, priced 6/5", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway4"], 25601),
		feeCase("conway_ref_three_tiers", "60000 bytes across three tiers", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway4"], 60000),
		feeCase("conway_oneshot_mint", "Plutus V3 mint with a reference script", v1.Era_ERA_CONWAY, feePParams(), fx.tx["oneshot"], 8000),
		feeCase("conway_zero_prices", "execution units are free", v1.Era_ERA_CONWAY, &v1.ProtocolParameters{
			MinFeeA: feeU64(44), MinFeeB: feeU64(155381), PriceMem: rat("0", "1"), PriceSteps: rat("0", "1"),
			MinFeeRefScriptCostPerByte: rat("0", "1"),
		}, fx.tx["conway4"], 30000),
		feeCase("conway_truncated", "an undecodable transaction", v1.Era_ERA_CONWAY, feePParams(), fx.tx["conway0"][:60], 0),
	)

	// reference script size.
	refSize := func(name, why string, era v1.Era, tx []byte, utxos ...*v1.UtxoEntry) entry {
		return feeCall("fee_ref_script_size_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeRefScriptSize{FeeRefScriptSize: &v1.FeeRefScriptSizeRequest{Era: era, Tx: tx, Utxo: utxos}}
		})
	}
	conwayRef := fx.dec["conway1"]
	var spendIDs, refIDs []lcommon.TransactionInput
	spendIDs = append(spendIDs, conwayRef.Inputs()...)
	refIDs = append(refIDs, conwayRef.ReferenceInputs()...)
	entryFor := func(in lcommon.TransactionInput, out []byte) *v1.UtxoEntry {
		id := in.Id()
		return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: out}
	}
	resolved := func(spend, ref []int) []*v1.UtxoEntry {
		var u []*v1.UtxoEntry
		for i, in := range spendIDs {
			var s []byte
			if i < len(spend) && spend[i] > 0 {
				s = feeFiller(spend[i], 0x10)
			}
			u = append(u, entryFor(in, feeScriptOut(t, 5_000_000, s)))
		}
		for i, in := range refIDs {
			var s []byte
			if i < len(ref) && ref[i] > 0 {
				s = feeFiller(ref[i], 0x40)
			}
			u = append(u, entryFor(in, feeScriptOut(t, 5_000_000, s)))
		}
		return u
	}
	add(
		refSize("none", "inputs resolve to outputs with no script", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved(nil, nil)...),
		refSize("reference_inputs_only", "scripts only on the reference inputs", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved(nil, []int{3000, 5000})...),
		refSize("spending_and_reference", "spending-input scripts count too", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved([]int{100, 0, 250}, []int{3000, 5000})...),
		refSize("tier_boundary", "total of exactly 25600 bytes", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved([]int{600}, []int{20000, 5000})...),
		refSize("past_tier_boundary", "total of 25601 bytes", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved([]int{601}, []int{20000, 5000})...),
		refSize("large_scripts", "three 25 KiB scripts", v1.Era_ERA_CONWAY, fx.tx["conway1"], resolved([]int{25000}, []int{25000, 25000})...),
		refSize("babbage_reference_input", "Babbage transaction with one reference input", v1.Era_ERA_BABBAGE, fx.tx["babbage3"], feeBabbageRefEntries(t, fx, "babbage3")...),
		refSize("truncated_tx", "an undecodable transaction", v1.Era_ERA_CONWAY, fx.tx["conway1"][:60], resolved(nil, nil)...),
	)
	// An input in both sets counts once; equal scripts at two inputs count twice.
	script := feeFiller(1000, 0x70)
	shared := feeRefTx(t, [][]any{feeTxIn(0x01, 0)}, [][]any{feeTxIn(0x01, 0)})
	twoSame := feeRefTx(t, [][]any{feeTxIn(0x01, 0)}, [][]any{feeTxIn(0x02, 0)})
	add(
		refSize("shared_input_once", "an input that is both spent and referenced is counted once", v1.Era_ERA_CONWAY, shared, feeUtxo(0x01, 0, feeScriptOut(t, 5_000_000, script))),
		refSize("equal_scripts_counted_twice", "two inputs with equal scripts count twice (non-distinct)", v1.Era_ERA_CONWAY, twoSame,
			feeUtxo(0x01, 0, feeScriptOut(t, 5_000_000, script)), feeUtxo(0x02, 0, feeScriptOut(t, 5_000_000, script))),
		refSize("unrelated_utxo_ignored", "entries the transaction does not use are ignored", v1.Era_ERA_CONWAY, twoSame,
			feeUtxo(0x01, 0, feeScriptOut(t, 5_000_000, script)), feeUtxo(0x02, 0, feeScriptOut(t, 5_000_000, nil)), feeUtxo(0x09, 0, feeScriptOut(t, 5_000_000, feeFiller(7000, 0x11)))),
	)
	add(fx.oneshotRefSize(t), fx.oneshotRealScriptRefSize(t))

	// reference script fee.
	refFee := func(name, why string, cost *v1.Rational, size uint64) entry {
		return feeCall("fee_ref_script_fee_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeRefScriptFee{FeeRefScriptFee: &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_CONWAY, CostPerByte: cost, Size: size}}
		})
	}
	add(
		refFee("zero", "no reference scripts cost nothing", rat("15", "1"), 0),
		refFee("one_byte", "one byte in the first tier", rat("15", "1"), 1),
		refFee("below_tier", "25599 bytes, still the first tier", rat("15", "1"), 25_599),
		refFee("tier_edge", "exactly one tier", rat("15", "1"), 25_600),
		refFee("past_tier", "one byte at 6/5 of the base price", rat("15", "1"), 25_601),
		refFee("two_tiers", "exactly two tiers", rat("15", "1"), 51_200),
		refFee("three_tiers_and_one", "second tier boundary plus one byte", rat("15", "1"), 51_201),
		refFee("eight_tiers", "a very large script total, 204800 bytes", rat("15", "1"), 204_800),
		refFee("fractional_price", "floor applied once to the exact rational total", rat("1", "3"), 25_601),
		refFee("fractional_price_many_tiers", "floor over several tiers of a fractional price", rat("7", "9"), 130_000),
		refFee("zero_price", "a zero base price", rat("0", "1"), 100_000),
		refFee("mainnet_typical", "mainnet price 15 with a 30000-byte total", rat("15", "1"), 30_000),
		refFee("negative_price", "a negative price is not a NonNegativeInterval", rat("-1", "1"), 100),
		refFee("saturating", "the result would not fit a Word64", rat("1000000000000000000", "1"), 100_000_000),
		refFee("above_modelled_bound", "a size past the bound both nodes accept is UNSUPPORTED", rat("15", "1"), 1<<29),
	)

	// script fee.
	scFee := func(name, why string, pm, ps *v1.Rational, mem, steps uint64) entry {
		return feeCall("fee_script_fee_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeRequest{PriceMem: pm, PriceSteps: ps, ExMem: mem, ExSteps: steps}}
		})
	}
	mem, steps := rat("577", "10000"), rat("721", "10000000")
	add(
		scFee("mainnet_typical", "mainnet prices, a mid-sized script", mem, steps, 2_500_000, 1_000_000_000),
		scFee("maximum_tx_budget", "the mainnet per-transaction budget, 14M memory and 10G steps", mem, steps, 14_000_000, 10_000_000_000),
		scFee("zero_units", "no execution units", mem, steps, 0, 0),
		scFee("zero_prices", "free execution", rat("0", "1"), rat("0", "1"), 14_000_000, 10_000_000_000),
		scFee("ceiling", "1/3 rounds up to 1", rat("1", "3"), rat("0", "1"), 1, 0),
		scFee("one_ceiling_over_the_sum", "1/2 + 1/2 is 1, not 2", rat("1", "2"), rat("1", "2"), 1, 1),
		scFee("exact_integer", "an exact total needs no rounding", rat("1", "1"), rat("2", "1"), 5, 7),
		scFee("unreduced_prices", "unreduced rationals are the same value", rat("1154", "20000"), rat("1442", "20000000"), 2_500_000, 1_000_000_000),
		scFee("large_units", "units near 2^62 with small prices", rat("1", "1000000000"), rat("1", "1000000000"), 1<<62, 1<<62),
		scFee("near_word64_limit", "2^64 - 2: the largest total that still fits", rat("1", "1"), rat("1", "1"), 1<<63-1, 1<<63-1),
		scFee("overflow", "a result beyond a Word64 is not representable", rat("2", "1"), rat("1", "1"), 1<<63-1, 1<<63-1),
		scFee("negative_price", "a negative price is not a NonNegativeInterval", rat("-1", "2"), rat("1", "2"), 1, 1),
	)

	// min UTxO.
	minUtxo := func(name, why string, era v1.Era, pp *v1.ProtocolParameters, out []byte) entry {
		return feeCall("fee_min_utxo_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeMinUtxo{FeeMinUtxo: &v1.FeeMinUtxoRequest{Era: era, Pparams: pp, Txout: out}}
		})
	}
	shelleyPP := &v1.ProtocolParameters{MinUtxoValue: feeU64(1_000_000)}
	alonzoPP := &v1.ProtocolParameters{CoinsPerUtxoWord: feeU64(34_482)}
	babbagePP := &v1.ProtocolParameters{CoinsPerUtxoByte: feeU64(4_310)}
	out := func(name string, i int) []byte { return fx.dec[name].Outputs()[i].Cbor() }
	plainAda := func(o lcommon.TransactionOutput) bool {
		return o.Assets() == nil && o.DatumHash() == nil && o.Datum() == nil && o.ScriptRef() == nil
	}
	withAssets := func(o lcommon.TransactionOutput) bool { return o.Assets() != nil }
	withInlineDatum := func(o lcommon.TransactionOutput) bool { return o.Datum() != nil }
	add(
		minUtxo("shelley", "Shelley: the flat parameter", v1.Era_ERA_SHELLEY, shelleyPP, out("shelley0", 0)),
		minUtxo("allegra", "Allegra: the flat parameter", v1.Era_ERA_ALLEGRA, shelleyPP, out("allegra0", 0)),
		minUtxo("mary_ada_only", "Mary pure-ADA output: the flat parameter", v1.Era_ERA_MARY, shelleyPP, out("mary0", 0)),
		minUtxo("alonzo_ada_only", "Alonzo output without a datum: 29 words", v1.Era_ERA_ALONZO, alonzoPP, fx.findOutput(t, ledger.TxTypeAlonzo, plainAda)),
		minUtxo("alonzo_multi_asset", "Alonzo multi-asset output prices the value size", v1.Era_ERA_ALONZO, alonzoPP, fx.findOutput(t, ledger.TxTypeAlonzo, withAssets)),
		minUtxo("alonzo_datum_hash", "Alonzo output with a datum hash adds 10 words", v1.Era_ERA_ALONZO, alonzoPP,
			feeCat([]byte{0x83}, mustCborFee(t, feeAddr()), mustCborFee(t, uint64(2_000_000)), mustCborFee(t, bytes.Repeat([]byte{0x99}, 32)))),
		minUtxo("babbage_inline_datum", "Babbage output with an inline datum, a list of 200 integers", v1.Era_ERA_BABBAGE, babbagePP, feeInlineDatumOut(t, 200)),
		minUtxo("conway_inline_datum", "real Conway output with an inline datum", v1.Era_ERA_CONWAY, babbagePP, fx.findOutput(t, ledger.TxTypeConway, withInlineDatum)),
		minUtxo("babbage_multi_asset_real", "real Babbage multi-asset output", v1.Era_ERA_BABBAGE, babbagePP, fx.findOutput(t, ledger.TxTypeBabbage, withAssets)),
		minUtxo("conway_multi_asset_real", "real Conway multi-asset output", v1.Era_ERA_CONWAY, babbagePP, fx.findOutput(t, ledger.TxTypeConway, withAssets)),
		minUtxo("babbage_plain", "Babbage output: (160 + size) * coinsPerUTxOByte", v1.Era_ERA_BABBAGE, babbagePP, out("babbage6", 0)),
		minUtxo("babbage_map_form", "map-form output with no datum or script, priced by its wire size", v1.Era_ERA_BABBAGE, babbagePP, feeScriptOut(t, 1_000_000, nil)),
		minUtxo("babbage_script_ref", "output carrying a 7000-byte reference script", v1.Era_ERA_BABBAGE, babbagePP, feeScriptOut(t, 1_000_000, feeFiller(7000, 0x20))),
		minUtxo("conway_plain", "Conway output", v1.Era_ERA_CONWAY, babbagePP, out("conway0", 0)),
		minUtxo("conway_many_outputs", "Conway output from a six-output transaction", v1.Era_ERA_CONWAY, babbagePP, out("conway7", 2)),
		minUtxo("conway_large_multi_asset", "64 policies of 32-byte asset names", v1.Era_ERA_CONWAY, babbagePP, feeMultiAssetOut(t, 64, 1, 32, true)),
		minUtxo("conway_many_assets_one_policy", "one policy with 200 assets", v1.Era_ERA_CONWAY, babbagePP, feeMultiAssetOut(t, 1, 200, 12, true)),
		minUtxo("babbage_large_multi_asset", "Babbage output with 64 policies", v1.Era_ERA_BABBAGE, babbagePP, feeMultiAssetOut(t, 64, 1, 32, true)),
		minUtxo("alonzo_large_multi_asset", "Alonzo output with 64 policies", v1.Era_ERA_ALONZO, alonzoPP, feeMultiAssetOut(t, 64, 1, 32, false)),
		minUtxo("zero_cost", "a zero coinsPerUTxOByte", v1.Era_ERA_CONWAY, &v1.ProtocolParameters{CoinsPerUtxoByte: feeU64(0)}, out("conway0", 0)),
	)

	// script integrity hash.
	add(fx.integrityCases(t)...)
	add(fx.valueBalanceCases(t)...)
	return es
}

// feeBabbageRefEntries resolves a Babbage transaction's inputs: the spending
// inputs carry no script and each reference input a 4000-byte one.
func feeBabbageRefEntries(t *testing.T, fx *feeFixtures, name string) []*v1.UtxoEntry {
	t.Helper()
	tx := fx.dec[name]
	var u []*v1.UtxoEntry
	for _, in := range tx.Inputs() {
		id := in.Id()
		u = append(u, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOut(t, 5_000_000, nil)})
	}
	for _, in := range tx.ReferenceInputs() {
		id := in.Id()
		u = append(u, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOut(t, 5_000_000, feeFiller(4000, 0x30))})
	}
	return u
}

func feeCborHeader(major byte, n int) []byte {
	switch {
	case n < 24:
		return []byte{major<<5 | byte(n)}
	case n < 256:
		return []byte{major<<5 | 24, byte(n)}
	default:
		return []byte{major<<5 | 25, byte(n >> 8), byte(n)}
	}
}

// feeMultiAssetOut is an output holding the given numbers of policies and asset
// names. mapForm selects the Babbage map encoding, otherwise the Mary and
// Alonzo array encoding. Maps are written by hand because asset names and
// policy ids are byte-string keys.
func feeMultiAssetOut(t *testing.T, policies, assets, nameLen int, mapForm bool) []byte {
	t.Helper()
	var ma []byte
	ma = append(ma, feeCborHeader(5, policies)...)
	for p := 0; p < policies; p++ {
		ma = append(ma, mustCborFee(t, bytes.Repeat([]byte{byte(p + 1)}, 28))...)
		ma = append(ma, feeCborHeader(5, assets)...)
		for a := 0; a < assets; a++ {
			name := make([]byte, nameLen)
			for i := range name {
				name[i] = byte(a>>8) ^ byte(a) ^ byte(i)
			}
			if nameLen >= 2 {
				name[0], name[1] = byte(a>>8), byte(a)
			}
			ma = append(ma, mustCborFee(t, name)...)
			ma = append(ma, byte(1+a%20))
		}
	}
	value := append(append([]byte{0x82}, mustCborFee(t, uint64(5_000_000))...), ma...)
	if mapForm {
		out := []byte{0xa2, 0x00}
		out = append(out, mustCborFee(t, feeAddr())...)
		out = append(out, 0x01)
		return append(out, value...)
	}
	out := []byte{0x82}
	out = append(out, mustCborFee(t, feeAddr())...)
	return append(out, value...)
}

// oneshotRefSize resolves the one-shot mint transaction against its real
// spent and reference outputs from gouroboros testdata.
func (fx *feeFixtures) oneshotRefSize(t *testing.T) entry {
	t.Helper()
	read := func(ins, outs string) []*v1.UtxoEntry {
		var inputs [][]any
		if _, err := cbor.Decode(feeHex(t, ins), &inputs); err != nil {
			t.Fatal(err)
		}
		var outputs []cbor.RawMessage
		if _, err := cbor.Decode(feeHex(t, outs), &outputs); err != nil {
			t.Fatal(err)
		}
		if len(inputs) != len(outputs) {
			t.Fatalf("%s: %d inputs, %d outputs", ins, len(inputs), len(outputs))
		}
		var u []*v1.UtxoEntry
		for i := range inputs {
			id := inputs[i][0].([]byte)
			idx, _ := inputs[i][1].(uint64)
			u = append(u, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id, Index: uint32(idx)}, Txout: bytes.Clone(outputs[i])})
		}
		return u
	}
	utxos := append(read("oneshot_mint_inputs", "oneshot_mint_input_outputs"), read("oneshot_mint_ref_input", "oneshot_mint_ref_output")...)
	return feeCall("fee_ref_script_size_oneshot_mint", "the mainnet one-shot mint transaction against its real spent outputs (Alonzo array form) and reference output",
		func(c *v1.Call) {
			c.Request = &v1.Call_FeeRefScriptSize{FeeRefScriptSize: &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: fx.tx["oneshot"], Utxo: utxos}}
		})
}

// feeRedeemersAndDatums are the witness-set bytes as the chain carries them.
func feeRedeemersAndDatums(t *testing.T, raw []byte) (redeemers, datums []byte) {
	t.Helper()
	parts := feeTxParts(t, raw)
	var wits map[uint]cbor.RawMessage
	if _, err := cbor.Decode(parts[1], &wits); err != nil {
		t.Fatal(err)
	}
	return bytes.Clone(wits[5]), bytes.Clone(wits[4])
}

func (fx *feeFixtures) integrityCases(t *testing.T) []entry {
	t.Helper()
	call := func(name, why string, era v1.Era, pp *v1.ProtocolParameters, rdmrs, dats []byte, langs ...v1.PlutusVersion) entry {
		return feeCall("fee_script_integrity_hash_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeScriptIntegrityHash{FeeScriptIntegrityHash: &v1.FeeScriptIntegrityHashRequest{
				Era: era, Pparams: pp, Redeemers: rdmrs, Datums: dats, Languages: langs,
			}}
		})
	}
	v1l, v2l, v3l := v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2, v1.PlutusVersion_PLUTUS_VERSION_V3
	models := func(m1, m2, m3 bool) *v1.ProtocolParameters {
		pp := &v1.ProtocolParameters{}
		if m1 {
			pp.CostModelPlutusV1 = fx.v1
		}
		if m2 {
			pp.CostModelPlutusV2 = fx.v2
		}
		if m3 {
			pp.CostModelPlutusV3 = fx.v3
		}
		return pp
	}

	// The one-shot mint hash is on chain: confirm the inputs reproduce it
	// before the case is written.
	oneshotR, oneshotD := feeRedeemersAndDatums(t, fx.tx["oneshot"])
	got, err := lcommon.ScriptDataHash(oneshotR, oneshotD, map[uint]struct{}{2: {}}, map[uint][]int64{2: fx.v3})
	if err != nil {
		t.Fatal(err)
	}
	if want := fx.dec["oneshot"].ScriptDataHash(); want == nil || *want != got {
		t.Fatalf("one-shot mint script data hash %x does not match the declared %v; the cost model or witness bytes are wrong", got, want)
	}
	alonzoR, alonzoD := feeRedeemersAndDatums(t, fx.tx["alonzo0"])
	conwayR, conwayD := feeRedeemersAndDatums(t, fx.tx["conway4"])
	babbageR, babbageD := feeRedeemersAndDatums(t, fx.tx["babbage3"])
	babbage9R, babbage9D := feeRedeemersAndDatums(t, fx.tx["babbage9"])
	datumOnly := []byte{0x81, 0x01}
	return []entry{
		call("conway_oneshot_mint", "on-chain: the hash declared by the mainnet Plutus V3 mint, real cost model", v1.Era_ERA_CONWAY, models(false, false, true), oneshotR, oneshotD, v3l),
		call("alonzo_plutus_v1", "Alonzo redeemer array with datums and the PlutusV1 view (double-serialised tag)", v1.Era_ERA_ALONZO, models(true, false, false), alonzoR, alonzoD, v1l),
		call("babbage_plutus_v2", "Babbage redeemers and datums, PlutusV2 view", v1.Era_ERA_BABBAGE, models(false, true, false), babbageR, babbageD, v2l),
		call("babbage_plutus_v1_and_v2", "two languages: views are sorted by tag", v1.Era_ERA_BABBAGE, models(true, true, false), babbage9R, babbage9D, v2l, v1l),
		call("conway_three_redeemers", "Conway with three redeemers", v1.Era_ERA_CONWAY, models(false, false, true), conwayR, conwayD, v3l),
		call("conway_all_languages", "all three languages: sorted by tag, V1 first", v1.Era_ERA_CONWAY, models(true, true, true), conwayR, conwayD, v3l, v2l, v1l),
		call("conway_duplicate_languages", "a language listed twice is one view", v1.Era_ERA_CONWAY, models(false, false, true), conwayR, conwayD, v3l, v3l),
		call("conway_redeemers_only", "no datums: nothing is added between redeemers and views", v1.Era_ERA_CONWAY, models(false, false, true), conwayR, nil, v3l),
		call("babbage_datums_only", "datums without redeemers hash the era's empty redeemers", v1.Era_ERA_BABBAGE, models(false, true, false), nil, datumOnly, v2l),
		call("conway_datums_only", "datums without redeemers, Conway", v1.Era_ERA_CONWAY, models(false, false, true), nil, datumOnly, v3l),
		call("alonzo_datums_only", "datums without redeemers, Alonzo", v1.Era_ERA_ALONZO, models(true, false, false), nil, datumOnly, v1l),
		call("conway_no_languages", "redeemers with no language views: an empty map", v1.Era_ERA_CONWAY, models(false, false, false), conwayR, conwayD),
		call("conway_nothing", "nothing to hash: no hash", v1.Era_ERA_CONWAY, models(false, false, false), nil, nil),
	}
}

// divergenceCases are confirmed mismatches, kept out of corpus/ so the
// default replay stays green.
func (fx *feeFixtures) divergenceCases(t *testing.T) []entry {
	t.Helper()
	shelleyPP := &v1.ProtocolParameters{MinUtxoValue: feeU64(1_000_000)}
	maryOut := fx.findOutput(t, ledger.TxTypeMary, func(o lcommon.TransactionOutput) bool { return o.Assets() != nil })
	syntheticMary := feeMultiAssetOut(t, 1, 1, 1, false)
	conwayRef := fx.dec["conway1"]
	var refIDs []lcommon.TransactionInput
	refIDs = append(refIDs, conwayRef.ReferenceInputs()...)
	var spend []*v1.UtxoEntry
	for _, in := range conwayRef.Inputs() {
		id := in.Id()
		spend = append(spend, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOut(t, 5_000_000, nil)})
	}
	conwayR, conwayD := feeRedeemersAndDatums(t, fx.tx["conway4"])
	vb := fx.valueBalanceDivergences(t)
	es := []entry{
		feeCall("fee_min_utxo_mary_multi_asset_real", "Mary prices a multi-asset output by value size; dingo returns the flat minUTxOValue", func(c *v1.Call) {
			c.Request = &v1.Call_FeeMinUtxo{FeeMinUtxo: &v1.FeeMinUtxoRequest{Era: v1.Era_ERA_MARY, Pparams: shelleyPP, Txout: maryOut}}
		}),
		feeCall("fee_min_utxo_mary_multi_asset_synthetic", "one policy, one one-byte asset name", func(c *v1.Call) {
			c.Request = &v1.Call_FeeMinUtxo{FeeMinUtxo: &v1.FeeMinUtxoRequest{Era: v1.Era_ERA_MARY, Pparams: shelleyPP, Txout: syntheticMary}}
		}),
		feeCall("fee_script_fee_units_above_int64", "execution units are Word64 in the reference and int64 in gouroboros", func(c *v1.Call) {
			c.Request = &v1.Call_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeRequest{PriceMem: rat("1", "1000000000"), PriceSteps: rat("0", "1"), ExMem: 1 << 63}}
		}),
		feeCall("fee_ref_script_size_missing_reference_input", "the reference reads an absent input as no scripts; dingo fails", func(c *v1.Call) {
			c.Request = &v1.Call_FeeRefScriptSize{FeeRefScriptSize: &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: fx.tx["conway1"], Utxo: spend}}
		}),
		feeCall("fee_ref_script_size_missing_spending_input", "an absent spending input", func(c *v1.Call) {
			var u []*v1.UtxoEntry
			for _, in := range refIDs {
				id := in.Id()
				u = append(u, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOut(t, 5_000_000, feeFiller(100, 0x10))})
			}
			c.Request = &v1.Call_FeeRefScriptSize{FeeRefScriptSize: &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: fx.tx["conway1"], Utxo: u}}
		}),
		feeCall("fee_script_integrity_hash_missing_cost_model", "a language without a cost model: the reference hashes a null view; dingo fails", func(c *v1.Call) {
			c.Request = &v1.Call_FeeScriptIntegrityHash{FeeScriptIntegrityHash: &v1.FeeScriptIntegrityHashRequest{
				Era: v1.Era_ERA_CONWAY, Pparams: &v1.ProtocolParameters{}, Redeemers: conwayR, Datums: conwayD,
				Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V3},
			}}
		}),
	}
	return append(es, vb...)
}

// Value balance fixtures. Outputs are in the Alonzo array form.

func feeHdr(major byte, n int) []byte {
	if n < 24 {
		return []byte{major<<5 | byte(n)}
	}
	return []byte{major<<5 | 24, byte(n)}
}

func feeCat(parts ...[]byte) []byte { return bytes.Join(parts, nil) }

func feeEntries(entries ...[2][]byte) []byte {
	out := feeHdr(5, len(entries))
	for _, e := range entries {
		out = append(out, e[0]...)
		out = append(out, e[1]...)
	}
	return out
}

func feeAssets(t *testing.T, items ...any) []byte {
	// items: policy byte, name string, quantity int64, ...; policies are
	// distinct and in ascending order.
	t.Helper()
	type one struct {
		p byte
		n string
		q int64
	}
	var list []one
	for i := 0; i < len(items); i += 3 {
		list = append(list, one{items[i].(byte), items[i+1].(string), items[i+2].(int64)})
	}
	out := feeHdr(5, len(list))
	for _, o := range list {
		out = append(out, mustCborFee(t, bytes.Repeat([]byte{o.p}, 28))...)
		out = append(out, feeHdr(5, 1)...)
		out = append(out, mustCborFee(t, []byte(o.n))...)
		out = append(out, mustCborFee(t, o.q)...)
	}
	return out
}

func feeOutput(t *testing.T, coin uint64, assets []byte) []byte {
	value := mustCborFee(t, coin)
	if assets != nil {
		value = feeCat([]byte{0x82}, value, assets)
	}
	return feeCat([]byte{0x82}, mustCborFee(t, feeAddr()), value)
}

func feeCred(kind byte, b byte) []byte {
	return feeCat([]byte{0x82, kind}, mustCborFee0(bytes.Repeat([]byte{b}, 28)))
}

func mustCborFee0(v any) []byte {
	b, err := cbor.Encode(v)
	if err != nil {
		panic(err)
	}
	return b
}

func (fx *feeFixtures) valueBalanceBuilders(t *testing.T) *vbBuilders {
	t.Helper()
	pp := &v1.ProtocolParameters{
		KeyDeposit: feeU64(2_000_000), PoolDeposit: feeU64(500_000_000),
		DrepDeposit: feeU64(500_000_000), GovActionDeposit: feeU64(100_000_000_000),
	}
	call := func(name, why string, state *v1.LedgerState, tx []byte) entry {
		return feeCall("fee_value_balance_"+name, why, func(c *v1.Call) {
			c.Request = &v1.Call_FeeValueBalance{FeeValueBalance: &v1.FeeValueBalanceRequest{
				Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Slot: 1000, Epoch: 5},
				Pparams: pp, State: state, Tx: tx,
			}}
		})
	}
	txid := bytes.Repeat([]byte{0x01}, 32)
	input := func(coin uint64, assets []byte) *v1.LedgerState {
		return &v1.LedgerState{Utxo: []*v1.UtxoEntry{feeUtxo(0x01, 0, feeOutput(t, coin, assets))}}
	}
	// body builds a transaction spending the one input with the given extra
	// entries (keys in ascending order) and wraps it in the wire form.
	body := func(outputs []byte, fee uint64, extra ...[2][]byte) []byte {
		entries := [][2][]byte{
			{{0x00}, mustCborFee(t, []any{[]any{txid, uint(0)}})},
			{{0x01}, outputs},
			{{0x02}, mustCborFee(t, fee)},
		}
		entries = append(entries, extra...)
		return feeCat([]byte{0x84}, feeEntries(entries...), []byte{0xa0, 0xf5, 0xf6})
	}
	outs := func(coin uint64, assets []byte) []byte {
		return feeCat([]byte{0x81}, feeOutput(t, coin, assets))
	}
	certs := func(cs ...[]byte) [2][]byte {
		return [2][]byte{{0x04}, feeCat(feeHdr(4, len(cs)), feeCat(cs...))}
	}
	cbor1 := func(v any) []byte { return mustCborFee(t, v) }
	hash := func(b byte) []byte { return cbor1(bytes.Repeat([]byte{b}, 28)) }
	account := func(kind v1.CredentialKind, b byte, deposit uint64) *v1.Account {
		return &v1.Account{Credential: &v1.Credential{Kind: kind, Hash: bytes.Repeat([]byte{b}, 28)}, Deposit: deposit}
	}
	withAccounts := func(st *v1.LedgerState, as ...*v1.Account) *v1.LedgerState {
		st.Accounts = as
		return st
	}
	poolCertWith := func(operator byte, pledge uint64) []byte {
		return feeCat([]byte{0x8a, 0x03}, hash(operator), cbor1(bytes.Repeat([]byte{0x55}, 32)),
			cbor1(pledge), cbor1(uint64(340_000_000)), []byte{0xd8, 0x1e, 0x82, 0x01, 0x18, 0x64},
			cbor1(append([]byte{0xe1}, bytes.Repeat([]byte{operator}, 28)...)), []byte{0x81}, hash(operator), []byte{0x80}, []byte{0xf6})
	}
	poolCert := func(operator byte) []byte { return poolCertWith(operator, 1_000_000) }
	pools := func(stage v1.PoolStage, retiring *uint64, ids ...byte) *v1.LedgerState {
		st := input(100_000_000_000, nil)
		for _, id := range ids {
			st.Pools = append(st.Pools, &v1.Pool{PoolId: bytes.Repeat([]byte{id}, 28), Stage: stage, RetiringEpoch: retiring})
		}
		return st
	}
	assets := func(items ...any) []byte { return feeAssets(t, items...) }
	rewardAddr := cbor1(append([]byte{0xe1}, bytes.Repeat([]byte{0x22}, 28)...))
	oneshotState := func() *v1.LedgerState {
		// The real spent outputs of the mainnet one-shot mint.
		var ins [][]any
		if _, err := cbor.Decode(feeHex(t, "oneshot_mint_inputs"), &ins); err != nil {
			t.Fatal(err)
		}
		var outputs []cbor.RawMessage
		if _, err := cbor.Decode(feeHex(t, "oneshot_mint_input_outputs"), &outputs); err != nil {
			t.Fatal(err)
		}
		st := &v1.LedgerState{}
		for i := range ins {
			idx, _ := ins[i][1].(uint64)
			st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: ins[i][0].([]byte), Index: uint32(idx)}, Txout: bytes.Clone(outputs[i])})
		}
		return st
	}
	anchor := feeCat([]byte{0x82}, cbor1("https://example.com/a"), cbor1(bytes.Repeat([]byte{0x77}, 32)))
	return &vbBuilders{
		call: call, input: input, body: body, outs: outs, certs: certs, cbor1: cbor1, hash: hash,
		account: account, withAccounts: withAccounts, poolCertWith: poolCertWith, poolCert: poolCert,
		pools: pools, assets: assets, rewardAddr: rewardAddr, oneshotState: oneshotState, anchor: anchor,
	}
}

type vbBuilders struct {
	call         func(name, why string, state *v1.LedgerState, tx []byte) entry
	input        func(coin uint64, assets []byte) *v1.LedgerState
	body         func(outputs []byte, fee uint64, extra ...[2][]byte) []byte
	outs         func(coin uint64, assets []byte) []byte
	certs        func(cs ...[]byte) [2][]byte
	cbor1        func(v any) []byte
	hash         func(b byte) []byte
	account      func(kind v1.CredentialKind, b byte, deposit uint64) *v1.Account
	withAccounts func(st *v1.LedgerState, as ...*v1.Account) *v1.LedgerState
	poolCertWith func(operator byte, pledge uint64) []byte
	poolCert     func(operator byte) []byte
	pools        func(stage v1.PoolStage, retiring *uint64, ids ...byte) *v1.LedgerState
	assets       func(items ...any) []byte
	rewardAddr   []byte
	oneshotState func() *v1.LedgerState
	anchor       []byte
}

func (fx *feeFixtures) valueBalanceCases(t *testing.T) []entry {
	t.Helper()
	b := fx.valueBalanceBuilders(t)
	call, input, body, outs, certs, cbor1 := b.call, b.input, b.body, b.outs, b.certs, b.cbor1
	account, withAccounts, poolCertWith, poolCert := b.account, b.withAccounts, b.poolCertWith, b.poolCert
	pools, assets, rewardAddr, oneshotState, anchor, hash := b.pools, b.assets, b.rewardAddr, b.oneshotState, b.anchor, b.hash
	key := v1.CredentialKind_CREDENTIAL_KIND_KEY
	script := v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	return []entry{
		call("oneshot_mint", "the mainnet one-shot mint against its real spent outputs: inputs, a positive mint and a balanced fee", oneshotState(), fx.tx["oneshot"]),
		call("plain_transfer", "one input, one output, a fee", input(10_000_000, nil), body(outs(9_800_000, nil), 200_000)),
		call("multi_asset_mint_burn", "consumed gains the positive mint, produced gains the burn",
			input(10_000_000, assets(byte(0xaa), "a", int64(5), byte(0xbb), "b", int64(4))),
			body(outs(9_800_000, assets(byte(0xaa), "a", int64(8), byte(0xbb), "b", int64(2))), 200_000,
				[2][]byte{{0x09}, assets(byte(0xaa), "a", int64(3), byte(0xbb), "b", int64(-2))})),
		call("burn_everything", "all input assets burned: nothing remains in the outputs", input(10_000_000, assets(byte(0xaa), "a", int64(5))),
			body(outs(9_800_000, nil), 200_000, [2][]byte{{0x09}, assets(byte(0xaa), "a", int64(-5))})),
		call("withdrawal", "a withdrawal is consumed", input(10_000_000, nil), body(outs(10_800_000, nil), 200_000,
			[2][]byte{{0x05}, feeCat(feeHdr(5, 1), rewardAddr, cbor1(uint64(1_000_000)))})),
		call("donation", "a treasury donation is produced", input(10_000_000, nil), body(outs(9_000_000, nil), 200_000, [2][]byte{{0x16}, cbor1(uint64(800_000))})),
		call("unresolved_input", "an input missing from the UTxO counts nothing", &v1.LedgerState{}, body(outs(9_800_000, nil), 200_000)),
		call("stake_registration_legacy", "cert 0: key deposit from the parameters", input(10_000_000, nil),
			body(outs(7_800_000, nil), 200_000, certs(feeCat([]byte{0x82, 0x00}, feeCred(0, 0x33))))),
		call("stake_registration_explicit", "cert 7 carries the deposit", input(10_000_000, nil),
			body(outs(7_800_000, nil), 200_000, certs(feeCat([]byte{0x83, 0x07}, feeCred(0, 0x33), cbor1(uint64(2_000_000)))))),
		call("stake_registration_script", "a script credential is registered like a key", input(10_000_000, nil),
			body(outs(7_800_000, nil), 200_000, certs(feeCat([]byte{0x83, 0x07}, feeCred(1, 0x33), cbor1(uint64(2_000_000)))))),
		call("stake_deregistration_legacy", "cert 1: the refund is the account's deposit",
			withAccounts(input(5_000_000, nil), account(key, 0x33, 2_000_000)),
			body(outs(6_800_000, nil), 200_000, certs(feeCat([]byte{0x82, 0x01}, feeCred(0, 0x33))))),
		call("stake_deregistration_explicit", "cert 8: the refund matches the account's deposit",
			withAccounts(input(5_000_000, nil), account(key, 0x33, 2_000_000)),
			body(outs(6_800_000, nil), 200_000, certs(feeCat([]byte{0x83, 0x08}, feeCred(0, 0x33), cbor1(uint64(2_000_000)))))),
		call("stake_deregistration_old_deposit", "the refund is the deposit paid at registration, not the current parameter",
			withAccounts(input(5_000_000, nil), account(key, 0x33, 3_000_000)),
			body(outs(7_800_000, nil), 200_000, certs(feeCat([]byte{0x83, 0x08}, feeCred(0, 0x33), cbor1(uint64(3_000_000)))))),
		call("stake_deregistration_script", "a script credential refunds like a key; a key with the same hash does not",
			withAccounts(input(5_000_000, nil), account(script, 0x33, 2_000_000), account(key, 0x34, 9_000_000)),
			body(outs(6_800_000, nil), 200_000, certs(feeCat([]byte{0x82, 0x01}, feeCred(1, 0x33))))),
		call("register_then_deregister", "registration and deregistration in one transaction: the refund is the key deposit",
			input(10_000_000, nil),
			body(outs(9_800_000, nil), 200_000, certs(feeCat([]byte{0x82, 0x00}, feeCred(0, 0x33)), feeCat([]byte{0x82, 0x01}, feeCred(0, 0x33))))),
		call("stake_registration_and_delegation", "cert 11 registers and delegates", input(10_000_000, nil),
			body(outs(7_800_000, nil), 200_000, certs(feeCat([]byte{0x84, 0x0b}, feeCred(0, 0x33), hash(0x44), cbor1(uint64(2_000_000)))))),
		call("pool_registration_new", "a new pool pays the pool deposit", input(600_000_000, nil),
			body(outs(99_800_000, nil), 200_000, certs(poolCert(0x44)))),
		call("pool_registration_existing", "re-registering a registered pool pays no deposit", pools(v1.PoolStage_POOL_STAGE_CURRENT, nil, 0x44),
			body(outs(99_800_000_000, nil), 200_000, certs(poolCert(0x44)))),
		call("pool_registration_duplicate", "the same new pool twice (different parameters) in one transaction pays once", input(600_000_000, nil),
			body(outs(99_800_000, nil), 200_000, certs(poolCertWith(0x44, 1_000_000), poolCertWith(0x44, 2_000_000)))),
		call("pool_registration_retiring", "a pool retiring in a later epoch is still registered", pools(v1.PoolStage_POOL_STAGE_CURRENT, feeU64(9), 0x44),
			body(outs(99_800_000_000, nil), 200_000, certs(poolCert(0x44)))),
		call("pool_registration_two_pools", "two different new pools pay two deposits", input(1_100_000_000, nil),
			body(outs(99_800_000, nil), 200_000, certs(poolCert(0x44), poolCert(0x45)))),
		call("drep_registration", "cert 16 pays the DRep deposit", input(600_000_000, nil),
			body(outs(99_800_000, nil), 200_000, certs(feeCat([]byte{0x84, 0x10}, feeCred(0, 0x66), cbor1(uint64(500_000_000)), []byte{0xf6})))),
		call("drep_deregistration", "cert 17 refunds the amount in the certificate", input(5_000_000, nil),
			body(outs(505_000_000-200_000, nil), 200_000, certs(feeCat([]byte{0x83, 0x11}, feeCred(0, 0x66), cbor1(uint64(500_000_000)))))),
		call("proposal_deposit", "a governance action pays the proposal deposit", input(200_000_000_000, nil),
			body(outs(99_000_000_000, nil), 200_000, [2][]byte{{0x14}, feeCat([]byte{0x81, 0x84}, cbor1(uint64(100_000_000_000)), rewardAddr, []byte{0x81, 0x06}, anchor)})),
		call("certs_and_assets", "stake registration, a withdrawal, a donation and a burn together",
			withAccounts(input(10_000_000, assets(byte(0xaa), "a", int64(5))), account(key, 0x33, 2_000_000)),
			body(outs(8_000_000, assets(byte(0xaa), "a", int64(3))), 200_000,
				[2][]byte{{0x04}, feeCat([]byte{0x82}, feeCat([]byte{0x82, 0x00}, feeCred(0, 0x34)), feeCat([]byte{0x82, 0x01}, feeCred(0, 0x33)))},
				[2][]byte{{0x05}, feeCat(feeHdr(5, 1), rewardAddr, cbor1(uint64(1_000_000)))},
				[2][]byte{{0x09}, assets(byte(0xaa), "a", int64(-2))},
				[2][]byte{{0x16}, cbor1(uint64(500_000))})),
		call("truncated_tx", "an undecodable transaction", input(10_000_000, nil), body(outs(9_800_000, nil), 200_000)[:40]),
	}
}

// oneshotRealScriptRefSize resolves the one-shot mint's reference input to an
// output holding the transaction's own Plutus V3 script, so the size is a real
// script's.
func (fx *feeFixtures) oneshotRealScriptRefSize(t *testing.T) entry {
	t.Helper()
	parts := feeTxParts(t, fx.tx["oneshot"])
	var wits map[uint]cbor.RawMessage
	if _, err := cbor.Decode(parts[1], &wits); err != nil {
		t.Fatal(err)
	}
	var scripts [][]byte
	if _, err := cbor.Decode(wits[7], &scripts); err != nil || len(scripts) == 0 {
		t.Fatalf("one-shot mint carries no PlutusV3 script: %v", err)
	}
	tx := fx.dec["oneshot"]
	var utxos []*v1.UtxoEntry
	for _, in := range tx.Inputs() {
		id := in.Id()
		utxos = append(utxos, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOut(t, 5_000_000, nil)})
	}
	for _, in := range tx.ReferenceInputs() {
		id := in.Id()
		utxos = append(utxos, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id.Bytes(), Index: in.Index()}, Txout: feeScriptOutLang(t, 5_000_000, 3, scripts[0])})
	}
	return feeCall("fee_ref_script_size_oneshot_real_script", "the mint's real PlutusV3 script as a reference script",
		func(c *v1.Call) {
			c.Request = &v1.Call_FeeRefScriptSize{FeeRefScriptSize: &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: fx.tx["oneshot"], Utxo: utxos}}
		})
}

// valueBalanceDivergences are inputs where dingo validates deposits and
// registrations inside the balance and the reference only sums.
func (fx *feeFixtures) valueBalanceDivergences(t *testing.T) []entry {
	t.Helper()
	b := fx.valueBalanceBuilders(t)
	return []entry{
		b.call("deregister_unregistered", "a deregistration of a credential with no account: the reference refunds nothing; dingo fails",
			b.input(5_000_000, nil), b.body(b.outs(5_800_000, nil), 200_000, b.certs(feeCat([]byte{0x82, 0x01}, feeCred(0, 0x33))))),
		b.call("deregister_wrong_refund", "the certificate names a refund unlike the account deposit: the reference refunds the deposit; dingo fails",
			b.withAccounts(b.input(5_000_000, nil), b.account(v1.CredentialKind_CREDENTIAL_KIND_KEY, 0x33, 2_000_000)),
			b.body(b.outs(5_800_000, nil), 200_000, b.certs(feeCat([]byte{0x83, 0x08}, feeCred(0, 0x33), mustCborFee(t, uint64(1_000_000)))))),
		b.call("registration_wrong_deposit", "the certificate names a deposit unlike keyDeposit: the reference charges keyDeposit; dingo fails",
			b.input(10_000_000, nil), b.body(b.outs(7_800_000, nil), 200_000, b.certs(feeCat([]byte{0x83, 0x07}, feeCred(0, 0x33), mustCborFee(t, uint64(1_000_000)))))),
		b.call("proposal_wrong_deposit", "a proposal deposit unlike govActionDeposit: the reference charges govActionDeposit; dingo fails",
			b.input(200_000_000_000, nil), b.body(b.outs(99_000_000_000, nil), 200_000, [2][]byte{{0x14}, feeCat([]byte{0x81, 0x84}, mustCborFee(t, uint64(1_000_000)), b.rewardAddr, []byte{0x81, 0x06}, b.anchor)})),
	}
}
