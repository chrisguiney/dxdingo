package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"google.golang.org/protobuf/encoding/prototext"

	"github.com/blinklabs-io/plutigo/lang"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func costParams(version lang.LanguageVersion, n int, v int64) []int64 {
	if n < 0 {
		n = len(lang.GetParamNamesForVersion(version))
	}
	out := make([]int64, n)
	for i := range out {
		out[i] = v
	}
	return out
}

// plutus_cost_model: the parameters are stored as supplied, as the
// reference's getCostModelParams returns them.
func TestCostModelStoresSuppliedParams(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		version v1.PlutusVersion
		lang    lang.LanguageVersion
	}{
		"v1": {v1.PlutusVersion_PLUTUS_VERSION_V1, lang.LanguageVersionV1},
		"v2": {v1.PlutusVersion_PLUTUS_VERSION_V2, lang.LanguageVersionV2},
		"v3": {v1.PlutusVersion_PLUTUS_VERSION_V3, lang.LanguageVersionV3},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			params := costParams(tc.lang, -1, 7)
			got, err := costModel(context.Background(), &v1.PlutusCostModelRequest{
				PlutusVersion:   tc.version,
				ProtocolVersion: &v1.ProtocolVersion{Major: 10},
				Params:          params,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil {
				t.Fatalf("rejected: %v", got.GetRejection())
			}
			if !slices.Equal(got.GetParams(), params) {
				t.Fatalf("params = %v, want %v", got.GetParams(), params)
			}
		})
	}
}

func TestCostModelUnspecifiedLanguageIsUnsupported(t *testing.T) {
	t.Parallel()
	_, err := costModel(context.Background(), &v1.PlutusCostModelRequest{})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("err = %v, want UNSUPPORTED", err)
	}
}

// loadCorpusCall reads a corpus case. The transaction cases are mainnet
// transactions with their UTxO, so the tests below share them with the replay.
func loadCorpusCall(t *testing.T, name string) *v1.Call {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "..", "corpus", name+".txtpb"))
	if err != nil {
		t.Fatal(err)
	}
	var c v1.Call
	if err := prototext.Unmarshal(b, &c); err != nil {
		t.Fatal(err)
	}
	return &c
}

func rejectionName(r *v1.Rejection) string {
	if len(r.GetFailures()) != 1 {
		return ""
	}
	return r.GetFailures()[0].GetName()
}

// The one-shot mint is mainnet transaction deab9ef3...; its policy
// eb55e440da235c80974c7883b0920c06b31ddac66c12c238626266e9 is the only script
// purpose, a mint at redeemer index 0.
func TestScriptPurposeOneShotMint(t *testing.T) {
	t.Parallel()
	call := loadCorpusCall(t, "plutus_script_purpose_conway_v3_one_shot_mint")
	got, err := scriptPurpose(context.Background(), call.GetPlutusScriptPurpose())
	if err != nil {
		t.Fatal(err)
	}
	policy, _ := hex.DecodeString("eb55e440da235c80974c7883b0920c06b31ddac66c12c238626266e9")
	if len(got.GetPurposes()) != 1 {
		t.Fatalf("purposes = %v, want one mint", got.GetPurposes())
	}
	p := got.GetPurposes()[0]
	if p.GetTag() != v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT || p.GetIndex() != 0 || !bytes.Equal(p.GetMintPolicyId(), policy) {
		t.Fatalf("purpose = %v", p)
	}
}

// The V3 TxInfo's last field but two is the transaction id, so the PlutusData
// carries the hash of the transaction it describes.
func TestTxInfoOneShotMintHoldsTheTransactionId(t *testing.T) {
	t.Parallel()
	call := loadCorpusCall(t, "plutus_tx_info_conway_v3_one_shot_mint_v3")
	got, err := txInfo(context.Background(), call.GetPlutusTxInfo())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	id, _ := hex.DecodeString("deab9ef3ce7415e492dd6f1eb5ff2920b34bccc0dcee8f4bbc76e97736de8a56")
	if !bytes.Contains(got.GetTxInfo(), id) {
		t.Fatal("TxInfo does not contain the transaction id")
	}
}

// The real transaction's script succeeds, and within the units it declared.
func TestEvalExUnitsOneShotMint(t *testing.T) {
	t.Parallel()
	call := loadCorpusCall(t, "plutus_eval_ex_units_conway_v3_one_shot_mint")
	got, err := evalExUnits(context.Background(), call.GetPlutusEvalExUnits())
	if err != nil {
		t.Fatal(err)
	}
	if len(got.GetResults()) != 1 || !got.GetResults()[0].GetOk() {
		t.Fatalf("results = %v, want one success", got.GetResults())
	}
	if r := got.GetResults()[0]; r.GetExMem() == 0 || r.GetExSteps() == 0 {
		t.Fatalf("units = %d, %d", r.GetExMem(), r.GetExSteps())
	}
}

// A failing script is a failed redeemer, not a rejected call.
func TestEvalExUnitsFailingScript(t *testing.T) {
	t.Parallel()
	call := loadCorpusCall(t, "plutus_eval_ex_units_babbage_v2_failing_script")
	got, err := evalExUnits(context.Background(), call.GetPlutusEvalExUnits())
	if err != nil {
		t.Fatal(err)
	}
	if len(got.GetResults()) != 1 || got.GetResults()[0].GetOk() || got.GetResults()[0].GetFailure() != "ValidationFailure" {
		t.Fatalf("results = %v", got.GetResults())
	}
}

func TestEvalExUnitsMissingCostModel(t *testing.T) {
	t.Parallel()
	call := loadCorpusCall(t, "plutus_eval_ex_units_missing_cost_model")
	got, err := evalExUnits(context.Background(), call.GetPlutusEvalExUnits())
	if err != nil {
		t.Fatal(err)
	}
	if len(got.GetResults()) != 1 || got.GetResults()[0].GetFailure() != "NoCostModelInLedgerState" {
		t.Fatalf("results = %v", got.GetResults())
	}
}

// V1 cannot translate a reference input in Babbage, nor an inline datum or a
// missing input anywhere; a slot past the era history has no time.
func TestTxInfoContextErrors(t *testing.T) {
	t.Parallel()
	for name, want := range map[string]string{
		"plutus_tx_info_babbage_v2_spend_ref_script_v1":     "ReferenceInputsNotSupported",
		"plutus_tx_info_conway_v3_two_spends_ref_script_v1": "InlineDatumsNotSupported",
		"plutus_tx_info_missing_input":                      "TranslationLogicMissingInput",
		"plutus_tx_info_past_horizon":                       "TimeTranslationPastHorizon",
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := txInfo(context.Background(), loadCorpusCall(t, name).GetPlutusTxInfo())
			if err != nil {
				t.Fatal(err)
			}
			if rejectionName(got.GetRejection()) != want {
				t.Fatalf("rejection = %v, want %s", got.GetRejection(), want)
			}
		})
	}
}

func TestTxInfoLanguageNotInEraIsUnsupported(t *testing.T) {
	t.Parallel()
	for _, name := range []string{"plutus_tx_info_language_not_in_era", "plutus_tx_info_v2_in_alonzo"} {
		_, err := txInfo(context.Background(), loadCorpusCall(t, name).GetPlutusTxInfo())
		if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
}

func contextArgs(t *testing.T, name string) [][]byte {
	t.Helper()
	got, err := scriptContext(context.Background(), loadCorpusCall(t, name).GetPlutusScriptContext())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	return got.GetArgs()
}

// A V1 or V2 script is applied to the datum when there is one, the redeemer and
// the context; a V3 script to the context alone, whose first field is the
// TxInfo (Constr 0 of three fields).
func TestScriptContextArguments(t *testing.T) {
	t.Parallel()
	datum, redeemer := []byte{0x18, 0x2a}, []byte{0xd8, 0x79, 0x80}
	args := contextArgs(t, "plutus_script_context_v1_spend")
	if len(args) != 3 || !bytes.Equal(args[0], datum) || !bytes.Equal(args[1], redeemer) {
		t.Fatalf("V1 spend: %d args, want datum, redeemer, context", len(args))
	}
	if args := contextArgs(t, "plutus_script_context_v1_spend_no_datum"); len(args) != 2 || !bytes.Equal(args[0], redeemer) {
		t.Fatalf("V1 spend without datum: %d args, want redeemer, context", len(args))
	}
	args = contextArgs(t, "plutus_script_context_v3_mint")
	if len(args) != 1 || !bytes.HasPrefix(args[0], []byte{0xd8, 0x79, 0x9f}) {
		t.Fatalf("V3 mint: %d args, want the context alone", len(args))
	}
}

func TestScriptContextUnsupported(t *testing.T) {
	t.Parallel()
	req := loadCorpusCall(t, "plutus_script_context_v3_mint").GetPlutusScriptContext()
	req.Context.Era = v1.Era_ERA_BABBAGE
	if _, err := scriptContext(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("V3 in Babbage: err = %v, want UNSUPPORTED", err)
	}
	req = loadCorpusCall(t, "plutus_script_context_v3_vote").GetPlutusScriptContext()
	req.Context.Era = v1.Era_ERA_BABBAGE
	req.PlutusVersion = v1.PlutusVersion_PLUTUS_VERSION_V2
	if _, err := scriptContext(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("voting purpose in Babbage: err = %v, want UNSUPPORTED", err)
	}
}
