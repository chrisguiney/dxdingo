package main

import (
	"errors"
	"fmt"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// legacyTxType maps the pre-Alonzo eras to their gouroboros transaction type.
func legacyTxType(era v1.Era) (uint, error) {
	switch era {
	case v1.Era_ERA_SHELLEY:
		return ledger.TxTypeShelley, nil
	case v1.Era_ERA_ALLEGRA:
		return ledger.TxTypeAllegra, nil
	case v1.Era_ERA_MARY:
		return ledger.TxTypeMary, nil
	}
	return 0, dispatch.Unsupported(fmt.Errorf("transaction rules are modelled for Shelley through Conway, not %v", era))
}

// legacyUtxoFailures runs the Shelley, Allegra or Mary rule list.
func legacyUtxoFailures(
	era v1.Era, tx common.Transaction, slot uint64, ls common.LedgerState, pp common.ProtocolParameters,
	families ...common.UtxoValidationFamily,
) []common.UtxoValidationFailure {
	switch era {
	case v1.Era_ERA_SHELLEY:
		return shelley.UtxoValidationFailures(tx, slot, ls, pp, families...)
	case v1.Era_ERA_ALLEGRA:
		return allegra.UtxoValidationFailures(tx, slot, ls, pp, families...)
	}
	return mary.UtxoValidationFailures(tx, slot, ls, pp, families...)
}

// legacyProtocolParameters converts the request tables into the parameter
// struct of Shelley, Allegra (an alias of Shelley's) or Mary.
func legacyProtocolParameters(era v1.Era, p *v1.ProtocolParameters) (common.ProtocolParameters, error) {
	if era != v1.Era_ERA_SHELLEY && era != v1.Era_ERA_ALLEGRA && era != v1.Era_ERA_MARY {
		return nil, dispatch.Unsupported(fmt.Errorf("rule functions are modelled for Shelley through Conway, not %v", era))
	}
	rat := func(name string, r *v1.Rational) (*cbor.Rat, error) {
		x, err := ratArg(name, r)
		if err != nil {
			return nil, err
		}
		return &cbor.Rat{Rat: x}, nil
	}
	a0, err := rat("a0", p.GetA0())
	if err != nil {
		return nil, err
	}
	rho, err := rat("rho", p.GetRho())
	if err != nil {
		return nil, err
	}
	tau, err := rat("tau", p.GetTau())
	if err != nil {
		return nil, err
	}
	d, err := rat("d", p.GetD())
	if err != nil {
		return nil, err
	}
	pv := p.GetProtocolVersion()
	entropy := common.Nonce{Type: common.NonceTypeNeutral}
	if era == v1.Era_ERA_MARY {
		return &mary.MaryProtocolParameters{
			MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
			MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
			MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
			PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
			A0: a0, Rho: rho, Tau: tau, Decentralization: d, ExtraEntropy: entropy,
			ProtocolMajor: uint(pv.GetMajor()), ProtocolMinor: uint(pv.GetMinor()),
			MinUtxoValue: uint(p.GetMinUtxoValue()), MinPoolCost: p.GetMinPoolCost(),
		}, nil
	}
	return &shelley.ShelleyProtocolParameters{
		MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
		MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
		MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
		PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
		A0: a0, Rho: rho, Tau: tau, Decentralization: d, ExtraEntropy: entropy,
		ProtocolMajor: uint(pv.GetMajor()), ProtocolMinor: uint(pv.GetMinor()),
		MinUtxoValue: uint(p.GetMinUtxoValue()), MinPoolCost: p.GetMinPoolCost(),
	}, nil
}

// legacyFailureName names failures whose constructor depends on the era or on
// the kind of error a shared gouroboros rule reports.
func legacyFailureName(era v1.Era, f common.UtxoValidationFailure) (ruleName, bool) {
	switch f.RuleId {
	case common.UtxoValidationRuleTimeToLive:
		if era == v1.Era_ERA_SHELLEY {
			return ruleName{"UTXO", "ExpiredUTxO"}, true
		}
	case common.UtxoValidationRuleOutputTooSmall:
		if era == v1.Era_ERA_SHELLEY || era == v1.Era_ERA_ALLEGRA || era == v1.Era_ERA_MARY {
			return ruleName{"UTXO", "OutputTooSmallUTxO"}, true
		}
	case common.UtxoValidationRuleScriptWitnesses, common.UtxoValidationRuleRedeemerAndScriptWitnesses:
		// One gouroboros rule covers the reference's script-witness and
		// redeemer checks; the error type says which one failed.
		var extra common.ExtraneousScriptWitnessesError
		var missingRedeemer common.MissingRedeemerForScriptError
		var extraRedeemer common.ExtraneousRedeemerError
		switch {
		case errors.As(f.Err, &extra):
			return ruleName{"UTXOW", "ExtraneousScriptWitnessesUTXOW"}, true
		case errors.As(f.Err, &missingRedeemer):
			return ruleName{"UTXOW", "MissingRedeemers"}, true
		case errors.As(f.Err, &extraRedeemer):
			return ruleName{"UTXOW", "ExtraRedeemers"}, true
		}
		return ruleName{"UTXOW", "MissingScriptWitnessesUTXOW"}, true
	case common.UtxoValidationRuleScriptDataHash:
		return ruleName{"UTXOW", "PPViewHashesDontMatch"}, true
	case common.UtxoValidationRulePlutusScripts:
		// The script run reports failures the reference attributes to other
		// rules: a redeemer or datum the transaction does not need or supply
		// is UTXOW's, and anything that stops the scripts being collected is
		// UTXOS's CollectErrors. Only a script outcome is a tag mismatch.
		var scriptFailed common.PlutusScriptFailedError
		var unexpected common.UnexpectedPlutusScriptSuccessError
		var extraRedeemer common.ExtraRedeemerError
		var missingDatum common.MissingDatumForSpendingScriptError
		switch {
		case errors.As(f.Err, &scriptFailed), errors.As(f.Err, &unexpected):
			return ruleName{"UTXOS", "ValidationTagMismatch"}, true
		case errors.As(f.Err, &extraRedeemer):
			return ruleName{"UTXOW", "ExtraRedeemers"}, true
		case errors.As(f.Err, &missingDatum):
			return ruleName{"UTXOW", "MissingRequiredDatums"}, true
		}
		return ruleName{"UTXOS", "CollectErrors"}, true
	case common.UtxoValidationRuleMetadata:
		var missing common.MissingTransactionMetadataError
		var noHash common.MissingTransactionAuxiliaryDataHashError
		var conflict common.ConflictingMetadataHashError
		switch {
		case errors.As(f.Err, &missing):
			return ruleName{"UTXOW", "MissingTxMetadata"}, true
		case errors.As(f.Err, &noHash):
			return ruleName{"UTXOW", "MissingTxBodyMetadataHash"}, true
		case errors.As(f.Err, &conflict):
			return ruleName{"UTXOW", "ConflictingMetadataHash"}, true
		}
		return ruleName{"UTXOW", "InvalidMetadata"}, true
	case common.UtxoValidationRuleMIRGenesisQuorum:
		return ruleName{"UTXOW", "MIRInsufficientGenesisSigsUTXOW"}, true
	}
	return ruleName{}, false
}

// withoutRule drops the failures owned by rule. The UTXO rule does not embed
// UTXOW, but a gouroboros rule it does run can fail with an error the
// reference reports from UTXOW.
func withoutRule(rej *v1.Rejection, rule string) *v1.Rejection {
	if rej == nil {
		return nil
	}
	var kept []*v1.Failure
	for _, f := range rej.GetFailures() {
		if f.GetRule() != rule {
			kept = append(kept, f)
		}
	}
	if len(kept) == 0 {
		return nil
	}
	return &v1.Rejection{Failures: kept}
}
