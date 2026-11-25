package main

import (
	"context"
	"errors"
	"fmt"
	"sort"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func registerRule(r *dispatch.Registry) {
	dispatch.Register(r, ruleUtxo)
	dispatch.Register(r, ruleUtxow)
}

// ruleUtxo maps to the UTXO rule and the UTXOS rule it embeds, run through
// gouroboros <era>.UtxoValidationFailures with those STS families selected.
func ruleUtxo(_ context.Context, req *v1.RuleUtxoRequest) (*v1.RuleUtxoResponse, error) {
	rej, err := runUtxoFamilies(req.GetContext(), req.GetGlobals(), req.GetPparams(), req.GetState(), req.GetTx(),
		common.UtxoFamilyUtxo, common.UtxoFamilyUtxos)
	if err != nil {
		return nil, err
	}
	return &v1.RuleUtxoResponse{Rejection: rej}, nil
}

// ruleUtxow maps to the UTXOW rule, which embeds UTXO and UTXOS.
func ruleUtxow(_ context.Context, req *v1.RuleUtxowRequest) (*v1.RuleUtxowResponse, error) {
	rej, err := runUtxoFamilies(req.GetContext(), req.GetGlobals(), req.GetPparams(), req.GetState(), req.GetTx(),
		common.UtxoFamilyUtxow, common.UtxoFamilyUtxo, common.UtxoFamilyUtxos)
	if err != nil {
		return nil, err
	}
	return &v1.RuleUtxowResponse{Rejection: rej}, nil
}

func txTypeOf(era v1.Era) (uint, error) {
	switch era {
	case v1.Era_ERA_ALONZO:
		return ledger.TxTypeAlonzo, nil
	case v1.Era_ERA_BABBAGE:
		return ledger.TxTypeBabbage, nil
	case v1.Era_ERA_CONWAY:
		return ledger.TxTypeConway, nil
	}
	return 0, dispatch.Unsupported(fmt.Errorf("transaction rules are modelled for Alonzo, Babbage and Conway, not %v", era))
}

func runUtxoFamilies(
	lc *v1.LedgerContext, g *v1.Globals, pp *v1.ProtocolParameters, st *v1.LedgerState, txCbor []byte,
	families ...common.UtxoValidationFamily,
) (*v1.Rejection, error) {
	txType, err := txTypeOf(lc.GetEra())
	if err != nil {
		return nil, err
	}
	params, err := ruleProtocolParameters(lc.GetEra(), pp)
	if err != nil {
		return nil, err
	}
	tx, err := ledger.NewTransactionFromCbor(txType, txCbor)
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	rs, err := newRuleState(lc, g, st)
	if err != nil {
		return nil, err
	}
	var failures []common.UtxoValidationFailure
	switch lc.GetEra() {
	case v1.Era_ERA_ALONZO:
		failures = alonzo.UtxoValidationFailures(tx, lc.GetSlot(), rs, params, families...)
	case v1.Era_ERA_BABBAGE:
		failures = babbage.UtxoValidationFailures(tx, lc.GetSlot(), rs, params, families...)
	case v1.Era_ERA_CONWAY:
		failures = conway.UtxoValidationFailures(tx, lc.GetSlot(), rs, params, families...)
	}
	return rejectionOf(lc.GetEra(), failures), nil
}

type ruleName struct{ rule, name string }

// rejectionOf names each failure by the Haskell constructor and the STS rule
// that owns it; the rule is not the gouroboros family, which groups rules by
// where the Go code runs them.
func rejectionOf(era v1.Era, failures []common.UtxoValidationFailure) *v1.Rejection {
	seen := map[ruleName]string{}
	for _, f := range failures {
		n := haskellFailure(era, f)
		if _, ok := seen[n]; !ok {
			seen[n] = f.Err.Error()
		}
	}
	if len(seen) == 0 {
		return nil
	}
	var out []*v1.Failure
	for n, d := range seen {
		out = append(out, &v1.Failure{Rule: n.rule, Name: n.name, Detail: d})
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Rule != out[j].Rule {
			return out[i].Rule < out[j].Rule
		}
		return out[i].Name < out[j].Name
	})
	return &v1.Rejection{Failures: out}
}

// utxoRuleNames maps a gouroboros rule id to the constructor the reference
// reports for it. Rules that report different constructors depending on the
// era are handled in haskellFailure.
var utxoRuleNames = map[common.UtxoValidationRuleId]ruleName{
	common.UtxoValidationRuleRequiredVKeyWitnesses:        {"UTXOW", "MissingVKeyWitnessesUTXOW"},
	common.UtxoValidationRuleCollateralVKeyWitnesses:      {"UTXOW", "MissingVKeyWitnessesUTXOW"},
	common.UtxoValidationRuleSignatures:                   {"UTXOW", "InvalidWitnessesUTXOW"},
	common.UtxoValidationRuleNativeScripts:                {"UTXOW", "ScriptWitnessNotValidatingUTXOW"},
	common.UtxoValidationRuleExtraneousRedeemers:          {"UTXOW", "ExtraRedeemers"},
	common.UtxoValidationRuleRequiredRedeemers:            {"UTXOW", "MissingRedeemers"},
	common.UtxoValidationRuleSupplementalDatums:           {"UTXOW", "NotAllowedSupplementalDatums"},
	common.UtxoValidationRuleMalformedReferenceScripts:    {"UTXOW", "MalformedReferenceScripts"},
	common.UtxoValidationRuleTimeToLive:                   {"UTXO", "OutsideValidityIntervalUTxO"},
	common.UtxoValidationRuleOutsideValidityInterval:      {"UTXO", "OutsideValidityIntervalUTxO"},
	common.UtxoValidationRuleInputSetEmpty:                {"UTXO", "InputSetEmptyUTxO"},
	common.UtxoValidationRuleFeeTooSmall:                  {"UTXO", "FeeTooSmallUTxO"},
	common.UtxoValidationRuleBadInputs:                    {"UTXO", "BadInputsUTxO"},
	common.UtxoValidationRuleWrongNetwork:                 {"UTXO", "WrongNetwork"},
	common.UtxoValidationRuleWrongNetworkWithdrawal:       {"UTXO", "WrongNetworkWithdrawal"},
	common.UtxoValidationRuleValueNotConserved:            {"UTXO", "ValueNotConservedUTxO"},
	common.UtxoValidationRuleOutputBootAddrAttrsTooBig:    {"UTXO", "OutputBootAddrAttrsTooBig"},
	common.UtxoValidationRuleMaxTxSize:                    {"UTXO", "MaxTxSizeUTxO"},
	common.UtxoValidationRuleOutsideForecast:              {"UTXO", "OutsideForecast"},
	common.UtxoValidationRuleOutputTooBig:                 {"UTXO", "OutputTooBigUTxO"},
	common.UtxoValidationRuleCollateralKeyLocked:          {"UTXO", "ScriptsNotPaidUTxO"},
	common.UtxoValidationRuleInsufficientCollateral:       {"UTXO", "InsufficientCollateral"},
	common.UtxoValidationRuleCollateralContainsNonAda:     {"UTXO", "CollateralContainsNonADA"},
	common.UtxoValidationRuleNoCollateralInputs:           {"UTXO", "NoCollateralInputs"},
	common.UtxoValidationRuleExUnitsTooBig:                {"UTXO", "ExUnitsTooBigUTxO"},
	common.UtxoValidationRuleDisjointRefInputs:            {"UTXO", "BabbageNonDisjointRefInputs"},
	common.UtxoValidationRuleCollateralEqBalance:          {"UTXO", "IncorrectTotalCollateralField"},
	common.UtxoValidationRuleTooManyCollateralInputs:      {"UTXO", "TooManyCollateralInputs"},
	common.UtxoValidationRuleTransactionNetworkId:         {"UTXO", "WrongNetworkInTxBody"},
	common.UtxoValidationRuleIsValidFlag:                  {"UTXOS", "ValidationTagMismatch"},
	common.UtxoValidationRuleCostModelsPresent:            {"UTXOS", "CollectErrors"},
	common.UtxoValidationRulePlutusScripts:                {"UTXOS", "ValidationTagMismatch"},
	common.UtxoValidationRuleInlineDatumsWithPlutusV1:     {"UTXOS", "CollectErrors"},
	common.UtxoValidationRuleConwayFeaturesWithPlutusV1V2: {"UTXOS", "CollectErrors"},
}

func haskellFailure(era v1.Era, f common.UtxoValidationFailure) ruleName {
	// gouroboros checks vkey signatures and the presence of a witness for
	// every key-locked input in one rule; the reference reports them as
	// InvalidWitnessesUTXOW and MissingVKeyWitnessesUTXOW.
	if f.RuleId == common.UtxoValidationRuleSignatures {
		var ve *common.ValidationError
		if errors.As(f.Err, &ve) && (ve.Message == "missing vkey witness for input" || ve.Message == "missing bootstrap witness for Byron input") {
			return ruleName{"UTXOW", "MissingVKeyWitnessesUTXOW"}
		}
	}
	if f.RuleId == common.UtxoValidationRuleOutputTooSmall {
		if era == v1.Era_ERA_ALONZO {
			return ruleName{"UTXO", "OutputTooSmallUTxO"}
		}
		return ruleName{"UTXO", "BabbageOutputTooSmallUTxO"}
	}
	if n, ok := utxoRuleNames[f.RuleId]; ok {
		return n
	}
	return ruleName{string(f.Family), "GoRule:" + string(f.RuleId)}
}
