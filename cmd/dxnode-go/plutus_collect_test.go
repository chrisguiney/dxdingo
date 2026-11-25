package main

import (
	"context"
	"slices"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func evalResults(t *testing.T, name string) []*v1.ScriptEvaluation {
	t.Helper()
	got, err := evalExUnits(context.Background(), loadCorpusCall(t, name).GetPlutusEvalExUnits())
	if err != nil {
		t.Fatal(err)
	}
	return got.GetResults()
}

// Every redeemer of the transaction has a result: a script that fails does not
// hide the others, as the reference's evalTxExUnits reports each redeemer. The
// transaction mints under three scripts, V1 and V3 failing and V2 not.
func TestEvalExUnitsReportsEveryFailingRedeemer(t *testing.T) {
	t.Parallel()
	results := evalResults(t, "plutus_eval_ex_units_conway_two_failing_one_ok")
	if len(results) != 3 {
		t.Fatalf("results = %v, want one per redeemer", results)
	}
	var failed, succeeded int
	for i, r := range results {
		if r.GetPurpose().GetTag() != v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT || r.GetPurpose().GetIndex() != uint32(i) {
			t.Fatalf("result %d is for %v", i, r.GetPurpose())
		}
		switch {
		case r.GetOk() && r.GetExMem() > 0 && r.GetExSteps() > 0:
			succeeded++
		case !r.GetOk() && r.GetFailure() == "ValidationFailure":
			failed++
		default:
			t.Fatalf("result %d = %v", i, r)
		}
	}
	if failed != 2 || succeeded != 1 {
		t.Fatalf("%d failed, %d succeeded, want 2 and 1", failed, succeeded)
	}
}

func TestEvalExUnitsAllRedeemersFailing(t *testing.T) {
	t.Parallel()
	results := evalResults(t, "plutus_eval_ex_units_conway_all_failing")
	if len(results) != 3 {
		t.Fatalf("results = %v, want one per redeemer", results)
	}
	for _, r := range results {
		if r.GetOk() || r.GetFailure() != "ValidationFailure" {
			t.Fatalf("result = %v, want ValidationFailure", r)
		}
	}
}

func TestEvalExUnitsAllRedeemersSucceeding(t *testing.T) {
	t.Parallel()
	results := evalResults(t, "plutus_eval_ex_units_conway_three_languages")
	if len(results) != 3 {
		t.Fatalf("results = %v, want one per redeemer", results)
	}
	for _, r := range results {
		if !r.GetOk() || r.GetExMem() == 0 || r.GetExSteps() == 0 {
			t.Fatalf("result = %v, want units", r)
		}
	}
}

func collected(t *testing.T, name string) *v1.PlutusCollectScriptsResponse {
	t.Helper()
	got, err := collectScripts(context.Background(), loadCorpusCall(t, name).GetPlutusCollectScripts())
	if err != nil {
		t.Fatal(err)
	}
	return got
}

// Three mint scripts, one per language, redeemer i holding the integer i and
// declaring 1000+i memory and 2000+i steps. A V1 or V2 script is applied to
// the redeemer and the context (no datum for a mint); a V3 script to the
// context alone, which holds the redeemer.
func TestCollectScriptsThreeLanguages(t *testing.T) {
	t.Parallel()
	got := collected(t, "plutus_collect_scripts_conway_three_languages")
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	scripts := got.GetScripts()
	if len(scripts) != 3 {
		t.Fatalf("scripts = %v, want one per redeemer", scripts)
	}
	var languages []v1.PlutusVersion
	for i, s := range scripts {
		ix := uint32(i)
		if s.GetPurpose().GetTag() != v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT || s.GetPurpose().GetIndex() != ix {
			t.Fatalf("script %d purpose = %v", i, s.GetPurpose())
		}
		if len(s.GetPurpose().GetMintPolicyId()) != 28 || !slices.Equal(s.GetPurpose().GetMintPolicyId(), s.GetScriptHash()) {
			t.Fatalf("script %d: policy %x, script hash %x", i, s.GetPurpose().GetMintPolicyId(), s.GetScriptHash())
		}
		if s.GetExMem() != uint64(1000+i) || s.GetExSteps() != uint64(2000+i) {
			t.Fatalf("script %d declares %d, %d", i, s.GetExMem(), s.GetExSteps())
		}
		if len(s.GetDatum()) != 0 || len(s.GetContext()) == 0 {
			t.Fatalf("script %d: datum %x, context %d bytes", i, s.GetDatum(), len(s.GetContext()))
		}
		wantRedeemer := []byte{byte(i)}
		if s.GetPlutusVersion() == v1.PlutusVersion_PLUTUS_VERSION_V3 {
			wantRedeemer = nil
		}
		if !slices.Equal(s.GetRedeemer(), wantRedeemer) {
			t.Fatalf("script %d (%v) redeemer = %x, want %x", i, s.GetPlutusVersion(), s.GetRedeemer(), wantRedeemer)
		}
		languages = append(languages, s.GetPlutusVersion())
	}
	slices.Sort(languages)
	if !slices.Equal(languages, []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2, v1.PlutusVersion_PLUTUS_VERSION_V3}) {
		t.Fatalf("languages = %v", languages)
	}
}

// A V1 spending script is applied to the datum the output carries by hash,
// found in the witness set.
func TestCollectScriptsSpendingDatum(t *testing.T) {
	t.Parallel()
	got := collected(t, "plutus_collect_scripts_alonzo_v1_spend")
	if got.GetRejection() != nil || len(got.GetScripts()) != 1 {
		t.Fatalf("response = %v", got)
	}
	s := got.GetScripts()[0]
	if s.GetPurpose().GetTag() != v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_SPEND || s.GetPlutusVersion() != v1.PlutusVersion_PLUTUS_VERSION_V1 {
		t.Fatalf("script = %v", s)
	}
	if len(s.GetDatum()) == 0 || len(s.GetRedeemer()) == 0 || len(s.GetContext()) == 0 {
		t.Fatalf("a spending script is applied to a datum, a redeemer and a context: %v", s)
	}
}

// The reference's collection errors, by the redeemer set and the cost models.
func TestCollectScriptsErrors(t *testing.T) {
	t.Parallel()
	for name, want := range map[string]string{
		// A needed and witnessed script with no redeemer.
		"plutus_collect_scripts_conway_no_redeemer": "NoRedeemer",
		// The PlutusV2 script's language has no cost model.
		"plutus_collect_scripts_conway_missing_cost_model": "NoCostModel",
		// Two languages without one: still one failure kind.
		"plutus_collect_scripts_conway_two_missing_cost_models": "NoCostModel",
		// V1 cannot translate a reference input in Babbage.
		"plutus_collect_scripts_babbage_v1_spend_ref_script": "BadTranslation",
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got := collected(t, name)
			if rejectionName(got.GetRejection()) != want || got.GetRejection().GetFailures()[0].GetRule() != "CollectError" {
				t.Fatalf("rejection = %v, want CollectError %s", got.GetRejection(), want)
			}
			if len(got.GetScripts()) != 0 {
				t.Fatalf("a rejected collection lists scripts: %v", got.GetScripts())
			}
		})
	}
}
