package main

import (
	"context"
	"errors"
	"slices"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// collectScripts maps to the three steps dingo and gouroboros take apart where
// the reference's collectPlutusScriptsWithContext takes them together: a
// redeemer for every Plutus script a purpose needs
// (script.ValidateRequiredRedeemers, the Conway UtxoValidateRequiredRedeemers
// rule), a cost model for every language used (the era's
// UtxoValidateCostModelsPresent) and the scripts with their arguments
// (script.CollectPlutusScripts). Each failing step is a CollectError
// failure of the reference's name, so a transaction that fails several shows
// every kind. The reference's NoWitness is not produced by the pinned ledger.
func collectScripts(_ context.Context, req *v1.PlutusCollectScriptsRequest) (*v1.PlutusCollectScriptsResponse, error) {
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
	major := uint(req.GetContext().GetProtocolVersion().GetMajor())
	slot := req.GetContext().GetSlot()
	ls := pureLedger{utxos: utxos, summary: summary}

	var names []string
	add := func(name string) {
		if !slices.Contains(names, name) {
			names = append(names, name)
		}
	}
	var detail error
	note := func(name string, err error) {
		add(name)
		detail = errors.Join(detail, err)
	}
	if err := script.ValidateRequiredRedeemers(tx, ls); err != nil {
		note(collectErrorName(err), err)
	}
	if err := costModelsPresent(era, tx, slot, ls, models); err != nil {
		note(collectErrorName(err), err)
	}
	if era.txType != ledger.TxTypeAlonzo {
		if err := babbage.UtxoValidateInlineDatumsWithPlutusV1(tx, slot, ls, nil); err != nil {
			note("BadTranslation", err)
		}
	}
	results, err := script.CollectPlutusScripts(tx, ls, major, era.maxLanguage, era.name)
	if err != nil {
		note(collectErrorName(err), err)
	}
	var scripts []*v1.CollectedScript
	for _, r := range results {
		switch {
		case r.Err != nil:
			note(collectErrorName(r.Err), r.Err)
		case r.Script != nil:
			c, err := collectedProto(*r.Script)
			if err != nil {
				return nil, err
			}
			scripts = append(scripts, c)
		}
	}
	if len(names) != 0 {
		slices.Sort(names)
		rej := &v1.Rejection{}
		for _, n := range names {
			rej.Failures = append(rej.Failures, &v1.Failure{Rule: "CollectError", Name: n, Detail: detail.Error()})
		}
		return &v1.PlutusCollectScriptsResponse{Rejection: rej}, nil
	}
	sortCollected(scripts)
	return &v1.PlutusCollectScriptsResponse{Scripts: scripts}, nil
}

// collectErrorName names an error as the reference's CollectError constructor
// it corresponds to. An error with no known counterpart is DingoError, which
// the comparison shows as a mismatch.
func collectErrorName(err error) string {
	var (
		noRedeemer  common.MissingRedeemerForScriptError
		noCostModel common.MissingCostModelError
		ctxErr      common.ScriptContextConstructionError
		inputErr    common.InputResolutionError
		refInErr    common.ReferenceInputResolutionError
	)
	switch {
	case errors.As(err, &noRedeemer):
		return "NoRedeemer"
	case errors.As(err, &noCostModel):
		return "NoCostModel"
	case errors.As(err, &ctxErr), errors.As(err, &inputErr), errors.As(err, &refInErr):
		return "BadTranslation"
	}
	return "DingoError"
}

func collectedProto(c script.CollectedScript) (*v1.CollectedScript, error) {
	out := &v1.CollectedScript{
		Purpose:    purposeProto(c.Key, c.Purpose),
		ScriptHash: c.ScriptHash.Bytes(),
		ExMem:      uint64(c.ExUnits.Memory),
		ExSteps:    uint64(c.ExUnits.Steps),
	}
	switch c.Language() {
	case 3:
		out.PlutusVersion = v1.PlutusVersion_PLUTUS_VERSION_V3
	case 2:
		out.PlutusVersion = v1.PlutusVersion_PLUTUS_VERSION_V2
	default:
		out.PlutusVersion = v1.PlutusVersion_PLUTUS_VERSION_V1
	}
	var err error
	if c.Datum != nil {
		if out.Datum, err = cbor.Encode(c.Datum); err != nil {
			return nil, err
		}
	}
	if c.Redeemer != nil {
		if out.Redeemer, err = cbor.Encode(c.Redeemer); err != nil {
			return nil, err
		}
	}
	if out.Context, err = cbor.Encode(c.Context); err != nil {
		return nil, err
	}
	return out, nil
}

func sortCollected(scripts []*v1.CollectedScript) {
	slices.SortStableFunc(scripts, func(a, b *v1.CollectedScript) int {
		if a.GetPurpose().GetTag() != b.GetPurpose().GetTag() {
			return int(a.GetPurpose().GetTag()) - int(b.GetPurpose().GetTag())
		}
		return int(a.GetPurpose().GetIndex()) - int(b.GetPurpose().GetIndex())
	})
}
