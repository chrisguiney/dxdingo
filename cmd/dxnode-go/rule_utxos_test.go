package main

import (
	"context"
	"encoding/hex"
	"strings"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// Two transactions each for Alonzo (PlutusV1) and Conway (PlutusV3), built by
// rulefixture.PlutusCase: one input locked by an always-succeeding or an
// always-failing script, flagged valid. The same hex runs in dxnode-hs
// (test/Test/Rule.hs); TestUtxosFixtureMatchesHex keeps the two in step.
type utxosFixture struct {
	era     v1.Era
	lang    int
	fail    bool
	tx      string
	utxo    [][2]string // txin id byte (hex, repeated 32 times) and txout
	return_ bool
}

var utxosFixtures = map[string]utxosFixture{
	"alonzo_v1_pass": {
		era: v1.Era_ERA_ALONZO, lang: 1,
		tx: "84a40081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a100018182581d61111111111111111111111111111111111111111111111111111111111a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b200a3038147460100002224990481d879800581840000d87980821a000f42401a1dcd6500f5f6",
		utxo: [][2]string{
			{"a1", "83581d71fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"},
			{"b2", "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40"},
		},
	},
	"alonzo_v1_fail": {
		era: v1.Era_ERA_ALONZO, lang: 1, fail: true,
		tx: "84a40081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a100018182581d61111111111111111111111111111111111111111111111111111111111a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b200a3038147460100002226010481d879800581840000d87980821a000f42401a1dcd6500f5f6",
		utxo: [][2]string{
			{"a1", "83581d716a09cb22defaf4a96a6be1ef6c07467ac9923d1750a79214a06c503a1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"},
			{"b2", "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40"},
		},
	},
	"conway_v3_pass": {
		era: v1.Era_ERA_CONWAY, lang: 3, return_: true,
		tx: "84a70081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1000181a200581d6111111111111111111111111111111111111111111111111111111111011a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b20010a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480111a002dc6c01281825820c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c300a30481d879800581840000d87980821a000f42401a1dcd6500078146450101002499f5f6",
		utxo: [][2]string{
			{"a1", "a300581d71186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4011a009896800282005820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"},
			{"b2", "a200581d6111111111111111111111111111111111111111111111111111111111011a004c4b40"},
			{"c3", "a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480"},
		},
	},
	"conway_v3_fail": {
		era: v1.Era_ERA_CONWAY, lang: 3, fail: true, return_: true,
		tx: "84a70081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1000181a200581d6111111111111111111111111111111111111111111111111111111111011a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b20010a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480111a002dc6c01281825820c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c300a30481d879800581840000d87980821a000f42401a1dcd6500078146450101002601f5f6",
		utxo: [][2]string{
			{"a1", "a300581d71ebb18a12777410738fdeaa77ec0fd582685d677b6b34de9a6e3b6d7e011a009896800282005820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"},
			{"b2", "a200581d6111111111111111111111111111111111111111111111111111111111011a004c4b40"},
			{"c3", "a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480"},
		},
	},
}

func (f utxosFixture) request(t *testing.T, flag bool) *v1.RuleUtxosRequest {
	t.Helper()
	ctx, g, pp, eh := rulefixture.UtxosEnv(f.era)
	tx, err := hex.DecodeString(f.tx)
	if err != nil {
		t.Fatal(err)
	}
	if !flag {
		// The is_valid flag is the byte before the trailing null auxiliary data.
		tx[len(tx)-2] = 0xf4
	}
	st := &v1.LedgerState{}
	for _, u := range f.utxo {
		id, _ := hex.DecodeString(strings.Repeat(u[0], 32))
		out, _ := hex.DecodeString(u[1])
		st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id}, Txout: out})
	}
	return &v1.RuleUtxosRequest{Context: ctx, Globals: g, Pparams: pp, State: st, EraHistory: eh, Tx: tx}
}

func TestUtxosFixtureMatchesHex(t *testing.T) {
	t.Parallel()
	for name, f := range utxosFixtures {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			tx, st := rulefixture.PlutusCase{
				Era: f.era, Script: rulefixture.NewScript(f.lang, f.fail, false), IsValid: true, CollateralReturn: f.return_,
			}.Build()
			if got := hex.EncodeToString(tx); got != f.tx {
				t.Fatalf("tx = %s", got)
			}
			if len(st.Utxo) != len(f.utxo) {
				t.Fatalf("%d utxo entries, want %d", len(st.Utxo), len(f.utxo))
			}
			for i, e := range st.Utxo {
				if got := hex.EncodeToString(e.Txout); got != f.utxo[i][1] {
					t.Fatalf("utxo %d = %s", i, got)
				}
			}
		})
	}
}

// Expected outcomes follow Cardano.Ledger.Alonzo.Rules.Utxos
// (alonzoEvalScriptsTxValid / alonzoEvalScriptsTxInvalid) and the UTXO rule's
// collateral balance (Babbage.Collateral.collAdaBalance; all of the 5 ada
// collateral in Alonzo, 5 ada less the 2 ada return from Babbage on).
func TestRuleUtxos(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		fixture    string
		flag       bool
		want       string
		scriptsOK  bool
		collateral uint64
	}{
		"alonzo pass, flagged valid":   {"alonzo_v1_pass", true, "", true, 0},
		"alonzo pass, flagged invalid": {"alonzo_v1_pass", false, "UTXOS/ValidationTagMismatch", true, 0},
		"alonzo fail, flagged valid":   {"alonzo_v1_fail", true, "UTXOS/ValidationTagMismatch", false, 0},
		"alonzo fail, flagged invalid": {"alonzo_v1_fail", false, "", false, 5_000_000},
		"conway pass, flagged valid":   {"conway_v3_pass", true, "", true, 0},
		"conway pass, flagged invalid": {"conway_v3_pass", false, "UTXOS/ValidationTagMismatch", true, 0},
		"conway fail, flagged valid":   {"conway_v3_fail", true, "UTXOS/ValidationTagMismatch", false, 0},
		"conway fail, flagged invalid": {"conway_v3_fail", false, "", false, 3_000_000},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			resp, err := ruleUtxos(context.Background(), utxosFixtures[tc.fixture].request(t, tc.flag))
			if err != nil {
				t.Fatal(err)
			}
			if got := strings.Join(failuresOf(resp.GetRejection()), ","); got != tc.want {
				t.Fatalf("failures = %q, want %q", got, tc.want)
			}
			if resp.GetScriptsValid() != tc.scriptsOK || resp.GetCollateralConsumed() != tc.collateral {
				t.Fatalf("scripts_valid=%v collateral=%d, want %v %d", resp.GetScriptsValid(), resp.GetCollateralConsumed(), tc.scriptsOK, tc.collateral)
			}
		})
	}
}

func TestRuleUtxosUnsupportedEra(t *testing.T) {
	t.Parallel()
	r := utxosFixtures["alonzo_v1_pass"].request(t, true)
	r.Context.Era = v1.Era_ERA_MARY
	if _, err := ruleUtxos(context.Background(), r); err == nil {
		t.Fatal("Mary accepted")
	}
}

// A missing cost model is a collect error, not a script failure.
func TestRuleUtxosMissingCostModel(t *testing.T) {
	t.Parallel()
	r := utxosFixtures["alonzo_v1_pass"].request(t, true)
	r.Pparams.CostModelPlutusV1 = nil
	resp, err := ruleUtxos(context.Background(), r)
	if err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(failuresOf(resp.GetRejection()), ","); got != "UTXOS/CollectErrors" || resp.GetScriptsValid() {
		t.Fatalf("failures = %q scripts_valid=%v", got, resp.GetScriptsValid())
	}
}

// UTXO does not embed UTXOW: a redeemer for a key-locked input is the
// reference's ExtraRedeemers, owned by UTXOW, so a UTXO-only run reports
// none of it, nor a UTXOS failure for the evaluation error it causes.
func TestRuleUtxoLeavesUtxowFailuresToUtxow(t *testing.T) {
	t.Parallel()
	raw, st := rulefixture.PlutusCase{
		Era: v1.Era_ERA_CONWAY, Script: rulefixture.NewScript(3, false, false), IsValid: true,
		OmitScript: true, KeyInput: true, NoRefInput: true, CollateralReturn: true,
	}.Build()
	ctx, g, pp, _ := rulefixture.UtxosEnv(v1.Era_ERA_CONWAY)
	resp, err := ruleUtxo(context.Background(), &v1.RuleUtxoRequest{Context: ctx, Globals: g, Pparams: pp, State: st, Tx: raw})
	if err != nil {
		t.Fatal(err)
	}
	for _, f := range failuresOf(resp.GetRejection()) {
		if strings.HasPrefix(f, "UTXOW/") || strings.HasPrefix(f, "UTXOS/") {
			t.Fatalf("failure %s in %v", f, failuresOf(resp.GetRejection()))
		}
	}
	uw, err := ruleUtxow(context.Background(), &v1.RuleUtxowRequest{Context: ctx, Globals: g, Pparams: pp, State: st, Tx: raw})
	if err != nil {
		t.Fatal(err)
	}
	if got := failuresOf(uw.GetRejection()); !strings.Contains(strings.Join(got, ","), "UTXOW/ExtraRedeemers") {
		t.Fatalf("utxow failures = %v, want ExtraRedeemers", got)
	}
}
