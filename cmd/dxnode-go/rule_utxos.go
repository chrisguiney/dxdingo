package main

import (
	"context"
	"errors"
	"fmt"
	"math/big"
	"time"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// ruleUtxos maps to the UTXOS rule: every Plutus script of the transaction is
// run, through gouroboros script.EvaluatePlutusScriptsPure with the request's
// cost models, and the outcome is compared with the transaction's is_valid
// flag. A transaction flagged invalid whose scripts do fail is accepted and
// forfeits its collateral.
func ruleUtxos(_ context.Context, req *v1.RuleUtxosRequest) (*v1.RuleUtxosResponse, error) {
	lc := req.GetContext()
	var maxPlutus uint
	var eraName string
	switch lc.GetEra() {
	case v1.Era_ERA_ALONZO:
		maxPlutus, eraName = 1, alonzo.EraNameAlonzo
	case v1.Era_ERA_BABBAGE:
		maxPlutus, eraName = 2, babbage.EraNameBabbage
	case v1.Era_ERA_CONWAY:
		maxPlutus, eraName = 3, conway.EraNameConway
	default:
		return nil, dispatch.Unsupported(fmt.Errorf("the UTXOS rule exists from Alonzo on, not in %v", lc.GetEra()))
	}
	txType, err := txTypeOf(lc.GetEra())
	if err != nil {
		return nil, err
	}
	params, err := ruleProtocolParameters(lc.GetEra(), req.GetPparams())
	if err != nil {
		return nil, err
	}
	tx, err := ledger.NewTransactionFromCbor(txType, req.GetTx())
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	rs, err := newRuleState(lc, req.GetGlobals(), req.GetState())
	if err != nil {
		return nil, err
	}
	history, err := newHistoryState(rs, req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	models := map[uint][]int64{0: pp.GetCostModelPlutusV1(), 1: pp.GetCostModelPlutusV2(), 2: pp.GetCostModelPlutusV3()}
	// The reference collects every script's inputs before running any; the
	// production UTXOS rules that can fail at that stage run first.
	evalErr := collectFailure(lc.GetEra(), tx, lc.GetSlot(), history, params)
	if evalErr == nil {
		_, evalErr = script.EvaluatePlutusScriptsPure(tx, history, uint(pp.GetProtocolVersion().GetMajor()), models, maxPlutus, eraName, nil)
	}

	resp := &v1.RuleUtxosResponse{}
	var failed common.PlutusScriptFailedError
	switch {
	case evalErr == nil:
		resp.ScriptsValid = true
		if !tx.IsValid() {
			resp.Rejection = utxosRejection("ValidationTagMismatch", "scripts passed but the transaction is flagged invalid")
		}
	case errors.As(evalErr, &failed):
		if tx.IsValid() {
			resp.Rejection = utxosRejection("ValidationTagMismatch", evalErr.Error())
		} else {
			resp.CollateralConsumed = collateralConsumed(tx, history)
		}
	default:
		resp.Rejection = utxosRejection("CollectErrors", evalErr.Error())
	}
	return resp, nil
}

// collectFailure runs the era's UTXOS-family rules other than the script run
// itself and the is_valid check, which ruleUtxos performs on the evaluation
// result. It returns the first failure.
func collectFailure(
	era v1.Era, tx common.Transaction, slot uint64, ls common.LedgerState, params common.ProtocolParameters,
) error {
	var descriptors []common.UtxoValidationRuleDescriptor
	switch era {
	case v1.Era_ERA_ALONZO:
		descriptors = alonzo.UtxoValidationRuleDescriptors()
	case v1.Era_ERA_BABBAGE:
		descriptors = babbage.UtxoValidationRuleDescriptors()
	default:
		descriptors = conway.UtxoValidationRuleDescriptors()
	}
	for _, d := range descriptors {
		if family, _ := common.UtxoValidationRuleFamily(d.Id); family != common.UtxoFamilyUtxos ||
			d.Id == common.UtxoValidationRuleIsValidFlag || d.Id == common.UtxoValidationRulePlutusScripts {
			continue
		}
		if err := d.Validator(tx, slot, ls, params); err != nil {
			return err
		}
	}
	return nil
}

func utxosRejection(name, detail string) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "UTXOS", Name: name, Detail: detail}}}
}

// collateralConsumed is what a phase-2 failure forfeits: the collateral
// inputs less the collateral return.
func collateralConsumed(tx common.Transaction, ls common.LedgerState) uint64 {
	total := new(big.Int)
	for _, in := range tx.Collateral() {
		if u, err := ls.UtxoById(in); err == nil {
			total.Add(total, u.Output.Amount())
		}
	}
	if ret := tx.CollateralReturn(); ret != nil {
		total.Sub(total, ret.Amount())
	}
	return total.Uint64()
}

// historyState answers slot-time queries from an era history instead of the
// single-era globals.
type historyState struct {
	*ruleState
	h *v1.EraHistory
}

func newHistoryState(rs *ruleState, h *v1.EraHistory) (*historyState, error) {
	if len(h.GetEras()) == 0 {
		return nil, dispatch.BadRequest(errors.New("era_history is empty"))
	}
	return &historyState{ruleState: rs, h: h}, nil
}

func (s *historyState) SlotToTime(slot uint64) (time.Time, error) {
	eras := s.h.GetEras()
	var era *v1.EraSummary
	for _, e := range eras {
		if slot >= e.GetStartSlot() && (e.EndSlot == nil || slot < e.GetEndSlot()) {
			era = e
			break
		}
	}
	if era == nil {
		return time.Time{}, errors.New("slot is past the era history horizon")
	}
	start, err := convert.Rat(era.GetStartTime())
	if err != nil {
		return time.Time{}, err
	}
	length, err := convert.Rat(era.GetSlotLength())
	if err != nil {
		return time.Time{}, err
	}
	secs := new(big.Rat).Add(start, new(big.Rat).Mul(new(big.Rat).SetUint64(slot-era.GetStartSlot()), length))
	secs.Add(secs, new(big.Rat).SetInt64(s.h.GetSystemStartUnix()))
	ns := new(big.Rat).Mul(secs, big.NewRat(1_000_000_000, 1))
	n := new(big.Int).Quo(ns.Num(), ns.Denom())
	return time.Unix(0, n.Int64()).UTC(), nil
}
