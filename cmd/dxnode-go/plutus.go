package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/blinklabs-io/dingo/ledger/hardfork"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/plutigo/data"
	"slices"

	"github.com/blinklabs-io/plutigo/cek"
	"github.com/blinklabs-io/plutigo/lang"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func registerPlutus(r *dispatch.Registry) {
	dispatch.Register(r, costModel)
	dispatch.Register(r, txInfo)
	dispatch.Register(r, scriptPurpose)
	dispatch.Register(r, scriptContext)
	dispatch.Register(r, evalExUnits)
}

func plutusLanguage(v v1.PlutusVersion) (lang.LanguageVersion, error) {
	switch v {
	case v1.PlutusVersion_PLUTUS_VERSION_V1:
		return lang.LanguageVersionV1, nil
	case v1.PlutusVersion_PLUTUS_VERSION_V2:
		return lang.LanguageVersionV2, nil
	case v1.PlutusVersion_PLUTUS_VERSION_V3:
		return lang.LanguageVersionV3, nil
	case v1.PlutusVersion_PLUTUS_VERSION_V4:
		return lang.LanguageVersionV4, nil
	}
	return lang.LanguageVersionV1, dispatch.Unsupported(fmt.Errorf("no Plutus language %v", v))
}

// costModel maps to plutigo cek.NewEvalContext, which dingo's evaluation
// path builds from the protocol-parameter cost model. The parameters are
// stored as supplied.
func costModel(_ context.Context, req *v1.PlutusCostModelRequest) (*v1.PlutusCostModelResponse, error) {
	language, err := plutusLanguage(req.GetPlutusVersion())
	if err != nil {
		return nil, err
	}
	pv := cek.ProtoVersion{
		Major: uint(req.GetProtocolVersion().GetMajor()),
		Minor: uint(req.GetProtocolVersion().GetMinor()),
	}
	if _, err := cek.NewEvalContext(language, pv, req.GetParams()); err != nil {
		return &v1.PlutusCostModelResponse{Rejection: dingoRejection("CostModels", err)}, nil
	}
	return &v1.PlutusCostModelResponse{Params: req.GetParams()}, nil
}

// txInfo maps to script.NewTxInfoV1/V2/V3FromTransaction over the resolved
// inputs and reference inputs, serialised as PlutusData.
func txInfo(_ context.Context, req *v1.PlutusTxInfoRequest) (*v1.PlutusTxInfoResponse, error) {
	if req.GetPlutusVersion() == v1.PlutusVersion_PLUTUS_VERSION_UNSPECIFIED {
		return nil, dispatch.Unsupported(errors.New("no Plutus language"))
	}
	era, err := plutusEraOf(req.GetContext().GetEra())
	if err != nil {
		return nil, err
	}
	if !era.supportsLanguage(req.GetPlutusVersion()) {
		return nil, dispatch.Unsupported(fmt.Errorf("no %v in %s", req.GetPlutusVersion(), era.name))
	}
	tx, err := decodePlutusTx(era, req.GetTx())
	if err != nil {
		return nil, err
	}
	utxos, err := decodeUtxos(req.GetUtxo())
	if err != nil {
		return nil, err
	}
	summary, err := historySummary(req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	ls := pureLedger{utxos: utxos, summary: summary}
	inputs, refs, err := script.ResolveTxInputs(tx, ls)
	if err != nil {
		return &v1.PlutusTxInfoResponse{Rejection: contextRejection(err)}, nil
	}
	resolved := script.ConcatResolvedInputs(inputs, refs)
	if req.GetPlutusVersion() == v1.PlutusVersion_PLUTUS_VERSION_V1 && era.txType != ledger.TxTypeAlonzo {
		if err := validatePlutusV1Translation(tx, inputs, refs); err != nil {
			return &v1.PlutusTxInfoResponse{Rejection: contextRejection(err)}, nil
		}
	}
	major := uint(req.GetContext().GetProtocolVersion().GetMajor())
	var info script.TxInfo
	switch req.GetPlutusVersion() {
	case v1.PlutusVersion_PLUTUS_VERSION_V1:
		info, err = infoOrErr(script.NewTxInfoV1FromTransaction(ls, tx, resolved, era.strictUpper, major))
	case v1.PlutusVersion_PLUTUS_VERSION_V2:
		info, err = infoOrErr(script.NewTxInfoV2FromTransaction(ls, tx, resolved, era.strictUpper, major))
	default:
		info, err = infoOrErr(script.NewTxInfoV3FromTransaction(ls, tx, resolved, major))
	}
	if err != nil {
		return &v1.PlutusTxInfoResponse{Rejection: contextRejection(err)}, nil
	}
	b, err := cbor.Encode(info.ToPlutusData())
	if err != nil {
		return nil, err
	}
	return &v1.PlutusTxInfoResponse{TxInfo: b}, nil
}

func infoOrErr[T script.TxInfo](info T, err error) (script.TxInfo, error) {
	return info, err
}

// scriptPurpose maps to script.ScriptPurposes over the transaction body and
// its resolved inputs.
func scriptPurpose(_ context.Context, req *v1.PlutusScriptPurposeRequest) (*v1.PlutusScriptPurposeResponse, error) {
	era, err := plutusEraOf(req.GetContext().GetEra())
	if err != nil {
		return nil, err
	}
	tx, err := decodePlutusTx(era, req.GetTx())
	if err != nil {
		return nil, err
	}
	utxos, err := decodeUtxos(req.GetUtxo())
	if err != nil {
		return nil, err
	}
	var resolved []common.Utxo
	for _, in := range tx.Inputs() {
		if u, err := utxos.UtxoById(in); err == nil {
			resolved = append(resolved, u)
		}
	}
	var out []*v1.ScriptPurpose
	for _, p := range script.ScriptPurposes(tx, resolved) {
		out = append(out, purposeProto(p.Key, p.Purpose))
	}
	sortPurposes(out)
	return &v1.PlutusScriptPurposeResponse{Purposes: out}, nil
}

// validatePlutusV1Translation applies the PlutusV1 translation restrictions
// (reference inputs and reference scripts in Babbage, inline datums later) as
// if the transaction required a V1 script. dingo applies them as the
// UtxoValidateInlineDatumsWithPlutusV1 rule, outside the TxInfo constructor,
// and only for a transaction whose needed scripts include a V1 script; the
// reference's V1 TxInfo translation fails on them itself.
func validatePlutusV1Translation(tx common.Transaction, inputs, refs []common.Utxo) error {
	view := script.TxScriptView{
		ResolvedInputs:          inputs,
		ResolvedReferenceInputs: refs,
		Needed:                  map[common.ScriptHash]common.Script{{}: common.PlutusV1Script{}},
	}
	return babbage.ValidateInlineDatumsWithPlutusV1(tx, view)
}

// contextRejection reports a TxInfo construction error under the name of the
// reference's ContextError constructor it corresponds to. An error with no
// known counterpart is DingoError, which the comparison shows as a mismatch.
func contextRejection(err error) *v1.Rejection {
	name := "DingoError"
	var (
		refInputs  babbage.PlutusV1ReferenceInputsNotSupportedError
		refScripts babbage.PlutusV1ReferenceScriptsNotSupportedError
		inline     common.InlineDatumsNotSupportedError
		inputErr   common.InputResolutionError
		refInErr   common.ReferenceInputResolutionError
	)
	switch {
	case errors.As(err, &refInputs):
		name = "ReferenceInputsNotSupported"
	case errors.As(err, &refScripts):
		name = "ReferenceScriptsNotSupported"
	case errors.As(err, &inline):
		name = "InlineDatumsNotSupported"
	case errors.As(err, &inputErr), errors.As(err, &refInErr):
		name = "TranslationLogicMissingInput"
	case errors.Is(err, hardfork.ErrPastHorizon):
		name = "TimeTranslationPastHorizon"
	}
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "ContextError", Name: name, Detail: err.Error()}}}
}

// evalExUnits maps to script.EvaluatePlutusScriptsPure, which runs every
// redeemer through common.EvaluatePlutusScriptPure. The budget each script
// may spend is the transaction maximum, as the reference's evalTxExUnits
// uses, not the budget the transaction declares.
func evalExUnits(_ context.Context, req *v1.PlutusEvalExUnitsRequest) (*v1.PlutusEvalExUnitsResponse, error) {
	era, err := plutusEraOf(req.GetContext().GetEra())
	if err != nil {
		return nil, err
	}
	tx, err := decodePlutusTx(era, req.GetTx())
	if err != nil {
		return nil, err
	}
	utxos, err := decodeUtxos(req.GetUtxo())
	if err != nil {
		return nil, err
	}
	summary, err := historySummary(req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	models := map[uint][]int64{0: pp.GetCostModelPlutusV1(), 1: pp.GetCostModelPlutusV2(), 2: pp.GetCostModelPlutusV3()}
	budget := &common.ExUnits{Memory: int64(pp.GetMaxTxExMem()), Steps: int64(pp.GetMaxTxExSteps())}
	major := uint(req.GetContext().GetProtocolVersion().GetMajor())
	ls := pureLedger{utxos: utxos, summary: summary}
	var results []*v1.ScriptEvaluation
	used := map[common.RedeemerKey]common.ExUnits{}
	// dingo runs these rules before it evaluates: a script whose language has
	// no cost model, or a V1 script whose translation is restricted, never
	// reaches the evaluator. The reference reports the first per redeemer.
	slot := req.GetContext().GetSlot()
	err = costModelsPresent(era, tx, slot, ls, models)
	if err != nil {
		results = allRedeemers(tx, "NoCostModelInLedgerState")
	} else if era.txType != ledger.TxTypeAlonzo {
		err = babbage.UtxoValidateInlineDatumsWithPlutusV1(tx, slot, ls, nil)
		if err != nil {
			results = allRedeemers(tx, "ContextError")
		}
	}
	if err == nil {
		used, err = script.EvaluatePlutusScriptsPure(tx, ls, major, models, era.maxLanguage, era.name, budget)
		if err != nil {
			results = failedEvaluations(tx, err)
		}
	}
	for key, units := range used {
		results = append(results, &v1.ScriptEvaluation{
			Purpose: &v1.ScriptPurpose{Tag: redeemerTag(key.Tag), Index: key.Index},
			Ok:      true, ExMem: uint64(units.Memory), ExSteps: uint64(units.Steps),
		})
	}
	slices.SortStableFunc(results, func(a, b *v1.ScriptEvaluation) int {
		if a.GetPurpose().GetTag() != b.GetPurpose().GetTag() {
			return int(a.GetPurpose().GetTag()) - int(b.GetPurpose().GetTag())
		}
		return int(a.GetPurpose().GetIndex()) - int(b.GetPurpose().GetIndex())
	})
	return &v1.PlutusEvalExUnitsResponse{Results: results}, nil
}

func redeemerTag(t common.RedeemerTag) v1.ScriptPurposeTag {
	switch t {
	case common.RedeemerTagSpend:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_SPEND
	case common.RedeemerTagMint:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT
	case common.RedeemerTagCert:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_CERT
	case common.RedeemerTagReward:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_REWARD
	case common.RedeemerTagVoting:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE
	case common.RedeemerTagProposing:
		return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_PROPOSE
	}
	return v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_UNSPECIFIED
}

// failedEvaluations reports a failed evaluation. The pure evaluator stops at
// the first failure, so the failing redeemer is the only one it names;
// failures that precede any script run (inputs, context) apply to every
// redeemer, as the reference reports them per redeemer.
func failedEvaluations(tx common.Transaction, err error) []*v1.ScriptEvaluation {
	failure := func(key common.RedeemerKey, name string) *v1.ScriptEvaluation {
		return &v1.ScriptEvaluation{Purpose: &v1.ScriptPurpose{Tag: redeemerTag(key.Tag), Index: key.Index}, Failure: name}
	}
	var failed common.PlutusScriptFailedError
	if errors.As(err, &failed) {
		return []*v1.ScriptEvaluation{failure(common.RedeemerKey{Tag: failed.Tag, Index: failed.Index}, "ValidationFailure")}
	}
	var extra common.ExtraRedeemerError
	if errors.As(err, &extra) {
		return []*v1.ScriptEvaluation{failure(extra.RedeemerKey, "RedeemerPointsToUnknownScriptHash")}
	}
	name := "DingoError"
	var missing common.MissingDatumForSpendingScriptError
	var ctxErr common.ScriptContextConstructionError
	switch {
	case errors.As(err, &missing):
		name = "MissingDatum"
	case errors.As(err, &ctxErr):
		name = "ContextError"
	}
	return allRedeemers(tx, name)
}

// allRedeemers reports every redeemer of the transaction as failed with name.
func allRedeemers(tx common.Transaction, name string) []*v1.ScriptEvaluation {
	var out []*v1.ScriptEvaluation
	if w := tx.Witnesses(); w != nil && w.Redeemers() != nil {
		for key := range w.Redeemers().Iter() {
			out = append(out, &v1.ScriptEvaluation{Purpose: &v1.ScriptPurpose{Tag: redeemerTag(key.Tag), Index: key.Index}, Failure: name})
		}
	}
	return out
}

// costModelsPresent runs the era's UtxoValidateCostModelsPresent rule against
// parameters that carry only the cost models.
func costModelsPresent(era plutusEra, tx common.Transaction, slot uint64, ls common.LedgerState, models map[uint][]int64) error {
	switch era.txType {
	case ledger.TxTypeAlonzo:
		return alonzo.UtxoValidateCostModelsPresent(tx, slot, ls, &alonzo.AlonzoProtocolParameters{CostModels: models})
	case ledger.TxTypeBabbage:
		return babbage.UtxoValidateCostModelsPresent(tx, slot, ls, &babbage.BabbageProtocolParameters{CostModels: models})
	}
	return conway.UtxoValidateCostModelsPresent(tx, slot, ls, &conway.ConwayProtocolParameters{CostModels: models})
}

// scriptContext maps to script.NewScriptContextV1V2 and NewScriptContextV3
// over the TxInfo as given. The arguments are what a script is applied to:
// the datum when there is one, the redeemer and the context for V1 and V2, the
// context alone for V3.
func scriptContext(_ context.Context, req *v1.PlutusScriptContextRequest) (resp *v1.PlutusScriptContextResponse, err error) {
	// WithWrappedTransactionId panics on a purpose the V1 and V2 contexts have
	// no form for (voting, proposing). The panic is a dingo answer: report it
	// as one, so the comparison shows a mismatch and the node survives.
	defer func() {
		if r := recover(); r != nil {
			resp, err = &v1.PlutusScriptContextResponse{Rejection: dingoRejection("ContextError", fmt.Errorf("panic: %v", r))}, nil
		}
	}()
	if req.GetPlutusVersion() == v1.PlutusVersion_PLUTUS_VERSION_UNSPECIFIED {
		return nil, dispatch.Unsupported(errors.New("no Plutus language"))
	}
	era, err := plutusEraOf(req.GetContext().GetEra())
	if err != nil {
		return nil, err
	}
	if !era.supportsLanguage(req.GetPlutusVersion()) {
		return nil, dispatch.Unsupported(fmt.Errorf("no %v in %s", req.GetPlutusVersion(), era.name))
	}
	decode := func(name string, b []byte) (data.PlutusData, error) {
		d, err := data.Decode(b)
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
		}
		return d, nil
	}
	info, err := decode("tx_info", req.GetTxInfo())
	if err != nil {
		return nil, err
	}
	redeemer, err := decode("redeemer", req.GetRedeemer())
	if err != nil {
		return nil, err
	}
	var datum data.PlutusData
	if len(req.GetDatum()) != 0 {
		if datum, err = decode("datum", req.GetDatum()); err != nil {
			return nil, err
		}
	}
	major := uint(req.GetContext().GetProtocolVersion().GetMajor())
	purpose, err := purposeFromProto(era, major, req.GetPurpose(), datum)
	if err != nil {
		return nil, err
	}
	txInfo := script.TxInfoData{Data: info}
	var args []data.PlutusData
	if req.GetPlutusVersion() == v1.PlutusVersion_PLUTUS_VERSION_V3 {
		r := script.Redeemer{Tag: common.RedeemerTag(req.GetPurpose().GetTag() - 1), Index: req.GetPurpose().GetIndex(), Data: redeemer}
		args = append(args, script.NewScriptContextV3(txInfo, r, purpose).ToPlutusData())
	} else {
		if datum != nil {
			args = append(args, datum)
		}
		args = append(args, redeemer, script.NewScriptContextV1V2(txInfo, purpose).ToPlutusData())
	}
	out := &v1.PlutusScriptContextResponse{}
	for _, a := range args {
		b, err := cbor.Encode(a)
		if err != nil {
			return nil, err
		}
		out.Args = append(out.Args, b)
	}
	return out, nil
}
