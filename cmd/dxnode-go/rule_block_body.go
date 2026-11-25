package main

import (
	"context"
	"errors"
	"fmt"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/gouroboros/ledger"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// blockFailureNames maps Dingo's block-level checks to the constructors of
// the reference's BBODY rule.
var blockFailureNames = map[dxoracle.BlockFailureKind]string{
	dxoracle.BlockBodySize:              "WrongBlockBodySizeBBODY",
	dxoracle.BlockBodyHash:              "InvalidBodyHashBBODY",
	dxoracle.BlockExUnits:               "TooManyExUnits",
	dxoracle.BlockRefScripts:            "BodyRefScriptsSizeTooBig",
	dxoracle.BlockHeaderProtocolVersion: "HeaderProtVerTooHigh",
}

// blockTypeOf maps an era to its gouroboros block type.
func blockTypeOf(era v1.Era) (uint, error) {
	switch era {
	case v1.Era_ERA_SHELLEY:
		return ledger.BlockTypeShelley, nil
	case v1.Era_ERA_ALLEGRA:
		return ledger.BlockTypeAllegra, nil
	case v1.Era_ERA_MARY:
		return ledger.BlockTypeMary, nil
	case v1.Era_ERA_ALONZO:
		return ledger.BlockTypeAlonzo, nil
	case v1.Era_ERA_BABBAGE:
		return ledger.BlockTypeBabbage, nil
	case v1.Era_ERA_CONWAY:
		return ledger.BlockTypeConway, nil
	}
	return 0, dispatch.Unsupported(fmt.Errorf("the BBODY rule is modelled for Shelley through Conway, not %v", era))
}

// ruleBlockBody maps to BBODY: Dingo's block checks (body size and hash, the
// block execution-unit and reference-script budgets, the header's protocol
// version) and then each transaction, validated and applied in order against
// one scratch ledger database. The reference's LEDGERS rule reports the
// failures of every transaction; Dingo stops at the first one that fails.
// Shelley to Babbage run the era's own rule list.
func ruleBlockBody(ctx context.Context, req *v1.RuleBlockBodyRequest) (*v1.RuleBlockBodyResponse, error) {
	lc := req.GetContext()
	blockType, err := blockTypeOf(lc.GetEra())
	if err != nil {
		return nil, err
	}
	legacyID, legacy := legacyEraID[lc.GetEra()]
	params, err := ruleProtocolParameters(lc.GetEra(), req.GetPparams())
	if err != nil {
		return nil, err
	}
	tables, err := ledgerTablesOf(lc, req.GetState())
	if err != nil {
		return nil, err
	}
	block, err := ledger.NewBlockFromCbor(blockType, req.GetBlock(), lcommon.VerifyConfig{SkipBodyHashValidation: true})
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("block: %w", err))
	}
	slot := block.SlotNumber()
	epoch, err := slotEpoch(req.GetGlobals(), req.GetEraHistory(), slot)
	if err != nil {
		return nil, err
	}
	pv := uint(req.GetPparams().GetProtocolVersion().GetMajor())
	in := dxoracle.BlockRunInput{
		Slot: slot, Epoch: epoch, NetworkID: uint(lc.GetNetworkId()), Params: params, State: *tables,
		BlockCbor: req.GetBlock(), BlockType: blockType,
		Families: []lcommon.UtxoValidationFamily{
			lcommon.UtxoFamilyUtxo, lcommon.UtxoFamilyUtxow, lcommon.UtxoFamilyUtxos, lcommon.UtxoFamilyGov, lcommon.UtxoFamilyLedger,
		},
		CertFamilies: []lcommon.UtxoValidationFamily{
			lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyGovCert, lcommon.UtxoFamilyCerts,
		},
		Withdrawals: true,
	}
	if legacy {
		in.LegacyEraID = legacyID
		in.Families = []lcommon.UtxoValidationFamily{lcommon.UtxoFamilyUtxo, lcommon.UtxoFamilyUtxow, lcommon.UtxoFamilyUtxos}
		in.CertFamilies = []lcommon.UtxoValidationFamily{lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyCerts}
	}
	in.Ignore = func(f lcommon.UtxoValidationFailure) bool {
		_, ok := ledgerFailureName(lc.GetEra(), f, pv)
		return !ok
	}
	o, err := theCertOracle()
	if err != nil {
		return nil, err
	}
	res, err := o.RunBlock(ctx, in)
	if err != nil {
		return nil, err
	}
	resp := &v1.RuleBlockBodyResponse{
		ExMemTotal:     uint64(res.ExUnits.Memory),
		ExStepsTotal:   uint64(res.ExUnits.Steps),
		RefScriptBytes: res.RefScriptBytes,
	}
	seen := map[ruleName]string{}
	for _, f := range res.BlockFailures {
		seen[ruleName{"BBODY", blockFailureNames[f.Kind]}] = f.Err.Error()
	}
	for _, f := range res.Failures {
		for _, n := range ledgerFailureNames(lc.GetEra(), f, pv, res.FailedTransaction) {
			if _, dup := seen[n]; !dup {
				seen[n] = f.Err.Error()
			}
		}
	}
	if len(seen) > 0 {
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
		resp.Rejection = &v1.Rejection{Failures: out}
		return resp, nil
	}
	var txs []lcommon.Transaction
	txs = append(txs, block.Transactions()...)
	resp.Delta = ledgerDelta(lc, req.GetState(), txs, &res.LedgerRunResult)
	return resp, nil
}

// slotEpoch is the epoch that holds slot: under the era history when the
// request has one, otherwise under the globals' fixed epoch length.
func slotEpoch(g *v1.Globals, h *v1.EraHistory, slot uint64) (uint64, error) {
	if len(h.GetEras()) == 0 {
		if g.GetEpochLength() == 0 {
			return 0, dispatch.Unsupported(errors.New("epoch_length must be nonzero"))
		}
		return slot / g.GetEpochLength(), nil
	}
	eras, err := parseHistory(h, true)
	if err != nil {
		return 0, err
	}
	for _, e := range eras {
		if slot >= e.start.slot && (e.end == nil || slot < e.end.slot) {
			return e.start.epoch + (slot-e.start.slot)/e.epochLen, nil
		}
	}
	return 0, dispatch.Unsupported(fmt.Errorf("slot %d is beyond the era history", slot))
}
