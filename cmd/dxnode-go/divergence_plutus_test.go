package main

import (
	"encoding/hex"
	"math/big"
	"os"
	"path/filepath"
	"testing"

	"github.com/blinklabs-io/dingo/ledger/hardfork"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"github.com/blinklabs-io/plutigo/cek"
	"github.com/blinklabs-io/plutigo/data"
	"github.com/blinklabs-io/plutigo/lang"
	"google.golang.org/protobuf/encoding/prototext"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// plutus.cost-model, zero CEK step cost: plutigo's cek.NewEvalContext (what
// dingo and gouroboros build every evaluation context with) refuses a model
// whose CEK step costs are below 1. The reference's mkCostModel accepts the
// same list (Test.Plutus "divergence: mkCostModel accepts zero CEK costs").
// Calls the library directly, bypassing the adapter.
// corpus/divergences/plutus_cost_model_v1_zero_costs.txtpb replays it.
func TestDivergenceZeroCekCostRejected(t *testing.T) {
	t.Parallel()
	_, err := cek.NewEvalContext(lang.LanguageVersionV1, cek.ProtoVersion{Major: 9}, costParams(lang.LanguageVersionV1, 166, 0))
	if err == nil {
		t.Fatal("plutigo accepted zero CEK step costs; the divergence no longer exists")
	}
	t.Logf("plutigo: %v", err)
}

// A list longer than the language's own parameter names: the reference drops
// the extras, plutigo's V1 table holds the later language's names too and
// applies them, so zeros past entry 166 reach the CEK step costs.
func TestDivergenceExtraParamsAreApplied(t *testing.T) {
	t.Parallel()
	params := append(costParams(lang.LanguageVersionV1, 166, 1), make([]int64, 100)...)
	if _, err := cek.NewEvalContext(lang.LanguageVersionV1, cek.ProtoVersion{Major: 9}, params); err == nil {
		t.Fatal("plutigo ignored V1 parameters past the 166th; the divergence no longer exists")
	}
}

// plutus.eval-ex-units, a redeemer whose script is not in the transaction or
// its reference inputs: the reference's evalTxExUnits reports MissingScript for
// its pointer; gouroboros' evaluator skips the redeemer ("Missing script should
// be caught by MissingScriptWitnesses"), so it returns no units and no error
// for it. dingo's phase-1 rules reject such a transaction before evaluation, so
// a validated chain never reaches it. Calls gouroboros directly, bypassing the
// adapter. corpus/divergences/plutus_eval_ex_units_conway_v3_spends_and_withdrawal.txtpb
// replays it.
func TestDivergenceMissingScriptIsSkipped(t *testing.T) {
	t.Parallel()
	b, err := os.ReadFile(filepath.Join("..", "..", "corpus", "divergences", "plutus_eval_ex_units_conway_v3_spends_and_withdrawal.txtpb"))
	if err != nil {
		t.Fatal(err)
	}
	var call v1.Call
	if err := prototext.Unmarshal(b, &call); err != nil {
		t.Fatal(err)
	}
	req := call.GetPlutusEvalExUnits()
	tx, err := ledger.NewConwayTransactionFromCbor(req.GetTx())
	if err != nil {
		t.Fatal(err)
	}
	var utxos common.ResolvedUtxos
	for _, e := range req.GetUtxo() {
		in, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(e.GetTxin().GetTxId()), int(e.GetTxin().GetIndex()))
		if err != nil {
			t.Fatal(err)
		}
		out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			t.Fatal(err)
		}
		utxos = append(utxos, common.Utxo{Id: in, Output: out})
	}
	models := map[uint][]int64{
		0: req.GetPparams().GetCostModelPlutusV1(), 1: req.GetPparams().GetCostModelPlutusV2(), 2: req.GetPparams().GetCostModelPlutusV3(),
	}
	budget := &common.ExUnits{Memory: int64(req.GetPparams().GetMaxTxExMem()), Steps: int64(req.GetPparams().GetMaxTxExSteps())}
	ls := pureLedger{utxos: utxos, summary: mustSummary(t, req.GetEraHistory())}
	used, err := script.EvaluatePlutusScriptsPure(tx, ls, 9, models, 3, "Conway", budget)
	if err != nil {
		t.Fatalf("evaluation failed: %v", err)
	}
	withdrawal := common.RedeemerKey{Tag: common.RedeemerTagReward, Index: 0}
	declared := false
	for k := range tx.Witnesses().Redeemers().Iter() {
		declared = declared || k == withdrawal
	}
	if !declared {
		t.Fatal("the transaction declares no withdrawal redeemer")
	}
	if _, ok := used[withdrawal]; ok {
		t.Fatal("the evaluator reported the withdrawal redeemer; the divergence no longer exists")
	}
}

func mustSummary(t *testing.T, h *v1.EraHistory) *hardfork.Summary {
	t.Helper()
	s, err := historySummary(h)
	if err != nil {
		t.Fatal(err)
	}
	return s
}

// plutus.script-context, a voting purpose in a V1 or V2 context: the
// reference's toPlutusScriptPurpose answers PlutusPurposeNotSupported;
// gouroboros' WithWrappedTransactionId, which renders the purpose of a V1/V2
// ScriptContext, panics. The TxInfo constructors reject the Conway features a
// voting transaction carries first, so evaluation never reaches the panic.
// Calls gouroboros directly, bypassing the adapter.
// corpus/divergences/plutus_script_context_v2_vote.txtpb replays it.
func TestDivergenceVotingPurposeInV2Panics(t *testing.T) {
	t.Parallel()
	defer func() {
		if recover() == nil {
			t.Fatal("no panic; the divergence no longer exists")
		}
	}()
	purpose := script.ScriptPurposeVoting{Voter: common.Voter{Type: common.VoterTypeDRepScriptHash}}
	script.NewScriptContextV1V2(script.TxInfoData{Data: data.NewInteger(big.NewInt(0))}, purpose).ToPlutusData()
}
