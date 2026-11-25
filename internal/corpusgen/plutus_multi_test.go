package corpusgen

import (
	"bytes"
	"slices"
	"testing"

	"github.com/blinklabs-io/plutigo/syn"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// mintScript is one Plutus script a synthetic minting transaction witnesses:
// its language and whether it fails.
type mintScript struct {
	lang  uint
	fails bool
}

// mintScriptBytes is a Plutus script of the language for a minting purpose,
// wrapped as the ledger stores it: it takes the arguments a mint applies it to
// (the redeemer and the context for V1 and V2, the context for V3) and returns
// unit, or fails with error. A script applied to fewer arguments than it
// binds returns a function, which V1 and V2 accept, so the arity matters.
func mintScriptBytes(t *testing.T, lang uint, fails bool) []byte {
	t.Helper()
	var body syn.Term[syn.DeBruijn] = &syn.Constant{Con: &syn.Unit{}}
	if fails {
		body = &syn.Error{}
	}
	args, version := 1, [3]uint32{1, 1, 0}
	if lang < 3 {
		args, version = 2, [3]uint32{1, 0, 0}
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

// multiMintTx is a Conway transaction that mints one token under each
// script's policy and witnesses every script, with a redeemer per policy.
// A mint redeemer counts policies in hash order, so redeemer i runs the i-th
// policy by hash. drop, when not negative, omits the redeemer of that policy.
// Each redeemer declares distinct units so a collection can be told apart.
func multiMintTx(t *testing.T, name string, pv uint32, scripts []mintScript, drop int) chainTx {
	t.Helper()
	type policy struct {
		hash, wrapped []byte
		lang          uint
	}
	var policies []policy
	for _, s := range scripts {
		w := mintScriptBytes(t, s.lang, s.fails)
		policies = append(policies, policy{scriptHash(s.lang, w), w, s.lang})
	}
	slices.SortFunc(policies, func(a, b policy) int { return bytes.Compare(a.hash, b.hash) })

	var mint, redeemers []byte
	mint = cborHead(5, uint64(len(policies)))
	var redeemerCount uint64
	witnessScripts := map[uint][]byte{}
	counts := map[uint]uint64{}
	for i, p := range policies {
		mint = cat(mint, cborBytes(p.hash), cborHead(5, 1), cborBytes(nil), cborHead(0, 1))
		witnessScripts[p.lang] = cat(witnessScripts[p.lang], cborBytes(p.wrapped))
		counts[p.lang]++
		if i == drop {
			continue
		}
		redeemerCount++
		// key [tag, index], value [data, [mem, steps]]
		redeemers = cat(redeemers,
			cborHead(4, 2), cborHead(0, 1), cborHead(0, uint64(i)),
			cborHead(4, 2), cborHead(0, uint64(i)), cborHead(4, 2), cborHead(0, uint64(1000+i)), cborHead(0, uint64(2000+i)))
	}
	// Witness set keys: 3 PlutusV1, 5 redeemers, 6 PlutusV2, 7 PlutusV3.
	var wits []byte
	var entries uint64
	for _, k := range []struct {
		key  uint64
		lang uint
	}{{3, 1}, {6, 2}, {7, 3}} {
		if counts[k.lang] > 0 {
			entries++
			wits = cat(wits, cborHead(0, k.key), cborHead(4, counts[k.lang]), witnessScripts[k.lang])
		}
	}
	entries++
	wits = cat(wits, cborHead(0, 5), cborHead(5, redeemerCount), redeemers)
	wits = cat(cborHead(5, entries), wits)

	inTx := rep(0x44, 32)
	body := cat(cborHead(5, 4),
		cborHead(0, 0), cborHead(4, 1), cborHead(4, 2), cborBytes(inTx), cborHead(0, 0),
		cborHead(0, 1), cborHead(4, 1), madeUpTxOut(nil, nil, nil, 0, true),
		cborHead(0, 2), cborHead(0, 200_000),
		cborHead(0, 9), mint)
	raw := cat(cborHead(4, 4), body, wits, []byte{0xf5}, []byte{0xf6})
	return chainTx{
		name: name, era: v1.Era_ERA_CONWAY, pv: pv, raw: raw,
		utxo: []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: inTx}, Txout: madeUpTxOut(nil, nil, nil, 0, true)}},
	}
}

// multiMintTxs are the synthetic multi-redeemer transactions. Every script
// is a Plutus script in a different language, so the policies differ.
func multiMintTxs(t *testing.T) map[string]chainTx {
	t.Helper()
	ok := []mintScript{{1, false}, {2, false}, {3, false}}
	out := map[string]chainTx{}
	for _, c := range []chainTx{
		multiMintTx(t, "conway_three_languages", 9, ok, -1),
		multiMintTx(t, "conway_two_failing_one_ok", 9, []mintScript{{1, true}, {2, false}, {3, true}}, -1),
		multiMintTx(t, "conway_all_failing", 9, []mintScript{{1, true}, {2, true}, {3, true}}, -1),
		multiMintTx(t, "conway_no_redeemer", 9, ok, 0),
	} {
		out[c.name] = c
	}
	return out
}

func evalCall(t *testing.T, c chainTx, pp *v1.ProtocolParameters) *v1.Call {
	return &v1.Call{Request: &v1.Call_PlutusEvalExUnits{PlutusEvalExUnits: &v1.PlutusEvalExUnitsRequest{
		Context: ledgerContext(c), Pparams: pp, Tx: c.raw, Utxo: c.utxo, EraHistory: mainnetEraHistory()}}}
}

func collectCall(c chainTx, pp *v1.ProtocolParameters) *v1.Call {
	return &v1.Call{Request: &v1.Call_PlutusCollectScripts{PlutusCollectScripts: &v1.PlutusCollectScriptsRequest{
		Context: ledgerContext(c), Pparams: pp, Tx: c.raw, Utxo: c.utxo, EraHistory: mainnetEraHistory()}}}
}

// plutusMultiEvalCases are transactions with several redeemers, which the
// reference evaluates each on its own.
func plutusMultiEvalCases(t *testing.T) []entry {
	t.Helper()
	txs := multiMintTxs(t)
	return []entry{
		{"plutus_eval_ex_units_conway_three_languages", "three redeemers, one script per language, all succeed",
			evalCall(t, txs["conway_three_languages"], mainnetParams(t, 9))},
		{"plutus_eval_ex_units_conway_two_failing_one_ok", "two of three redeemers fail: each is reported, and the third still has its units",
			evalCall(t, txs["conway_two_failing_one_ok"], mainnetParams(t, 9))},
		{"plutus_eval_ex_units_conway_all_failing", "every redeemer fails: all are reported",
			evalCall(t, txs["conway_all_failing"], mainnetParams(t, 9))},
	}
}

// plutusMultiEvalDivergences are multi-redeemer cases the nodes answer
// differently.
func plutusMultiEvalDivergences(t *testing.T) []entry {
	t.Helper()
	pp := mainnetParams(t, 9)
	pp.CostModelPlutusV2 = nil
	return []entry{
		{"plutus_eval_ex_units_conway_mixed_missing_cost_model",
			"only the PlutusV2 model is absent: the reference fails the V2 redeemer alone; dingo's cost-model rule rejects the whole transaction (TestDivergenceCostModelRuleIsPerTransaction)",
			evalCall(t, multiMintTxs(t)["conway_three_languages"], pp)},
	}
}

// plutusCollectCases collect the scripts of the chain transactions and of the
// synthetic ones, and fail the collection in each way the reference has.
func plutusCollectCases(t *testing.T) []entry {
	t.Helper()
	var out []entry
	for _, c := range plutusChainTxs(t) {
		if c.name == withdrawalTx {
			continue
		}
		out = append(out, entry{"plutus_collect_scripts_" + c.name,
			"the script, datum, redeemer, context and declared units of every redeemer of a mainnet transaction", collectCall(c, mainnetParams(t, c.pv))})
	}
	txs := multiMintTxs(t)
	noV2 := mainnetParams(t, 9)
	noV2.CostModelPlutusV2 = nil
	noV1V2 := mainnetParams(t, 9)
	noV1V2.CostModelPlutusV1, noV1V2.CostModelPlutusV2 = nil, nil
	return append(out,
		entry{"plutus_collect_scripts_conway_three_languages", "three scripts, one per language, each with its declared units",
			collectCall(txs["conway_three_languages"], mainnetParams(t, 9))},
		entry{"plutus_collect_scripts_conway_no_redeemer", "a script the transaction needs and witnesses has no redeemer",
			collectCall(txs["conway_no_redeemer"], mainnetParams(t, 9))},
		entry{"plutus_collect_scripts_conway_missing_cost_model", "the PlutusV2 script's language has no cost model",
			collectCall(txs["conway_three_languages"], noV2)},
		entry{"plutus_collect_scripts_conway_two_missing_cost_models", "two languages have no cost model: one failure kind, however many scripts",
			collectCall(txs["conway_three_languages"], noV1V2)},
	)
}
