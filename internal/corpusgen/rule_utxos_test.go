package corpusgen

import (
	"fmt"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

func TestGenerateRuleUtxosCorpus(t *testing.T) {
	writeCorpus(t, "", utxosCases(t, false))
	writeCorpus(t, "divergences", utxosCases(t, true))
}

// utxosDivergent names the cases kept out of corpus/ because the nodes
// disagree; see divergence_rule_utxos_test.go in dxnode-go.
var utxosDivergent = map[string]bool{
	// gouroboros refuses a spending script with no datum; the reference runs it
	// on the remaining arguments. UTXOW rejects the transaction first.
	"rule_utxos_alonzo_v1_no_datum":  true,
	"rule_utxos_babbage_v2_no_datum": true,
	"rule_utxos_conway_v2_no_datum":  true,
}

type utxosCase struct {
	name, why string
	era       v1.Era
	tx        []byte
	st        *v1.LedgerState
	mutate    func(*v1.RuleUtxosRequest)
}

func (c utxosCase) entry() entry {
	ctx, g, pp, eh := rulefixture.UtxosEnv(c.era)
	req := &v1.RuleUtxosRequest{Context: ctx, Globals: g, Pparams: pp, State: c.st, EraHistory: eh, Tx: c.tx}
	if c.mutate != nil {
		c.mutate(req)
	}
	return entry{c.name, c.why, &v1.Call{Request: &v1.Call_RuleUtxos{RuleUtxos: req}}}
}

func utxosCases(t *testing.T, divergences bool) []entry {
	var cases []utxosCase
	add := func(c utxosCase) { cases = append(cases, c) }

	type era struct {
		name  string
		era   v1.Era
		langs []int
	}
	for _, e := range []era{
		{"alonzo", v1.Era_ERA_ALONZO, []int{1}},
		{"babbage", v1.Era_ERA_BABBAGE, []int{1, 2}},
		{"conway", v1.Era_ERA_CONWAY, []int{1, 2, 3}},
	} {
		for _, lang := range e.langs {
			for _, mint := range []bool{false, true} {
				purpose := map[bool]string{false: "spend", true: "mint"}[mint]
				for _, fail := range []bool{false, true} {
					for _, valid := range []bool{true, false} {
						script := map[bool]string{false: "pass", true: "fail"}[fail]
						flag := map[bool]string{true: "valid", false: "invalid"}[valid]
						tx, st := rulefixture.PlutusCase{
							Era: e.era, Script: rulefixture.NewScript(lang, fail, mint), Mint: mint, IsValid: valid,
							CollateralReturn: e.era != v1.Era_ERA_ALONZO,
							// A PlutusV1 context cannot carry reference inputs under Babbage.
							NoRefInput: e.era == v1.Era_ERA_BABBAGE && lang == 1,
						}.Build()
						why := fmt.Sprintf("Plutus V%d %s script that %ses, transaction flagged %s: ", lang, purpose, script, flag)
						switch {
						case fail && !valid:
							why += "phase-2 failure the transaction declared, accepted, collateral forfeited"
						case fail:
							why += "script fails though the flag says it passes: tag mismatch"
						case !valid:
							why += "script passes though the flag says it fails: tag mismatch"
						default:
							why += "the ordinary accepted transaction"
						}
						add(utxosCase{
							name: fmt.Sprintf("rule_utxos_%s_v%d_%s_%s_%s", e.name, lang, purpose, script, flag),
							why:  why, era: e.era, tx: tx, st: st,
						})
					}
				}
			}
		}
	}

	// Babbage and later: where the script and datum come from, and the
	// collateral return.
	for _, e := range []struct {
		name string
		era  v1.Era
		lang int
	}{{"babbage", v1.Era_ERA_BABBAGE, 2}, {"conway", v1.Era_ERA_CONWAY, 3}} {
		for _, v := range []struct {
			tag, why string
			pc       rulefixture.PlutusCase
		}{
			{"refscript", "script supplied by a reference input", rulefixture.PlutusCase{RefScript: true, IsValid: true, CollateralReturn: true}},
			{"inline_datum", "datum carried inline by the locked output, none in the witness set", rulefixture.PlutusCase{InlineDatum: true, OmitDatum: true, IsValid: true, CollateralReturn: true}},
			{"no_collateral_return", "failing script flagged invalid with no collateral return: all 5 ada forfeited", rulefixture.PlutusCase{IsValid: false}},
		} {
			pc := v.pc
			pc.Era = e.era
			pc.Script = rulefixture.NewScript(e.lang, v.tag == "no_collateral_return", false)
			tx, st := pc.Build()
			add(utxosCase{name: fmt.Sprintf("rule_utxos_%s_v%d_%s", e.name, e.lang, v.tag), why: v.why, era: e.era, tx: tx, st: st})
		}
	}

	// Inputs the script cannot be run without.
	for _, e := range []struct {
		name string
		era  v1.Era
		lang int
	}{{"alonzo", v1.Era_ERA_ALONZO, 1}, {"babbage", v1.Era_ERA_BABBAGE, 2}, {"conway", v1.Era_ERA_CONWAY, 2}, {"conway", v1.Era_ERA_CONWAY, 3}} {
		for _, v := range []struct {
			tag, why string
			pc       rulefixture.PlutusCase
			mutate   func(*v1.RuleUtxosRequest)
		}{
			{"no_datum", "spending a script input whose datum is not supplied", rulefixture.PlutusCase{OmitDatum: true, IsValid: true}, nil},
			{"no_script", "script neither in the witness set nor referenced", rulefixture.PlutusCase{OmitScript: true, IsValid: true}, nil},
			{"no_cost_model", "the language's cost model is absent from the parameters", rulefixture.PlutusCase{IsValid: true}, func(r *v1.RuleUtxosRequest) {
				r.Pparams.CostModelPlutusV1, r.Pparams.CostModelPlutusV2, r.Pparams.CostModelPlutusV3 = nil, nil, nil
			}},
		} {
			pc := v.pc
			pc.Era = e.era
			pc.Script = rulefixture.NewScript(e.lang, false, false)
			pc.CollateralReturn = e.era != v1.Era_ERA_ALONZO
			tx, st := pc.Build()
			add(utxosCase{name: fmt.Sprintf("rule_utxos_%s_v%d_%s", e.name, e.lang, v.tag), why: v.why, era: e.era, tx: tx, st: st, mutate: v.mutate})
		}
	}

	// PlutusV1 with a reference input under Babbage (protocol version 8).
	{
		tx, st := rulefixture.PlutusCase{
			Era: v1.Era_ERA_BABBAGE, Script: rulefixture.NewScript(1, false, false), IsValid: true, CollateralReturn: true,
		}.Build()
		add(utxosCase{
			name: "rule_utxos_babbage_v1_ref_input", era: v1.Era_ERA_BABBAGE, tx: tx, st: st,
			why: "PlutusV1 script in a transaction with a reference input, protocol version 8",
		})
	}

	// Real transactions.
	if tx, st, err := rulefixture.AlonzoPlutusV1(); err != nil {
		t.Fatal(err)
	} else {
		add(utxosCase{
			name: "rule_utxos_alonzo_mainnet_v1", era: v1.Era_ERA_ALONZO, tx: tx, st: st,
			why: "the PlutusV1 transaction of the Alonzo mainnet block against a synthesised input at its script address",
		})
	}
	tx, st, err := rulefixture.OneshotMint()
	if err != nil {
		t.Fatal(err)
	}
	conwayV11 := func(r *v1.RuleUtxosRequest) {
		r.Context.Slot = 196791729
		r.Context.ProtocolVersion = &v1.ProtocolVersion{Major: 11}
		r.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 11}
	}
	add(utxosCase{
		name: "rule_utxos_conway_mainnet_oneshot_mint_v3", era: v1.Era_ERA_CONWAY, tx: tx, st: st, mutate: conwayV11,
		why: "real mainnet one-shot mint: PlutusV3 policy, 7 inputs, a reference input with an inline datum, epoch 653 cost model",
	})
	flipped := append([]byte(nil), tx...)
	for i := len(flipped) - 1; i >= 0; i-- {
		if flipped[i] == 0xf5 {
			flipped[i] = 0xf4
			break
		}
	}
	add(utxosCase{
		name: "rule_utxos_conway_mainnet_oneshot_mint_v3_flagged_invalid", era: v1.Era_ERA_CONWAY, tx: flipped, st: st, mutate: conwayV11,
		why: "the same transaction with is_valid cleared: the script passes, so the flag is a mismatch",
	})
	add(utxosCase{
		name: "rule_utxos_conway_mainnet_oneshot_mint_v3_tight_budget", era: v1.Era_ERA_CONWAY, tx: tx, st: st,
		mutate: func(r *v1.RuleUtxosRequest) {
			conwayV11(r)
			r.Pparams.CostModelPlutusV3 = r.Pparams.CostModelPlutusV3[:len(r.Pparams.CostModelPlutusV3)-1]
		},
		why: "epoch 653 PlutusV3 cost model one parameter short",
	})

	var out []entry
	for _, c := range cases {
		if utxosDivergent[c.name] == divergences {
			out = append(out, c.entry())
		}
	}
	return out
}
