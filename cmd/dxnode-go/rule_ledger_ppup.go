package main

import (
	"context"
	"errors"
	"fmt"
	"sort"

	dledger "github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// ppupState answers the two capabilities the classic PPUP rule asks of the
// ledger: the genesis delegation table and the voting window. The delegation
// table is the request's; the window comes from dingo's
// ClassicPParamUpdateWindow over the globals' epoch geometry.
type ppupState struct {
	*ruleState
	stabilityWindow uint64
}

var (
	_ common.GenesisDelegationState                    = (*ppupState)(nil)
	_ common.ClassicProtocolParameterUpdateWindowState = (*ppupState)(nil)
)

func (s *ppupState) GenesisDelegateForGenesisKey(key common.Blake2b224, _ uint64) (common.Blake2b224, bool, error) {
	for _, d := range s.state.GetGenesisDelegations() {
		if string(d.GetGenesisKeyHash()) == string(key[:]) {
			var out common.Blake2b224
			copy(out[:], d.GetDelegateKeyHash())
			return out, true, nil
		}
	}
	return common.Blake2b224{}, false, nil
}

func (s *ppupState) GenesisDelegateKeyHashes(uint64) ([]common.Blake2b224, error) {
	var out []common.Blake2b224
	for _, d := range s.state.GetGenesisDelegations() {
		out = append(out, common.NewBlake2b224(d.GetDelegateKeyHash()))
	}
	return out, nil
}

// GenesisUpdateQuorum is the globals' quorum.
func (s *ppupState) GenesisUpdateQuorum() (uint, error) { return uint(s.globals.GetQuorum()), nil }

func (s *ppupState) ProtocolParameterUpdateWindow(slot uint64) (uint64, uint64, error) {
	length := s.globals.GetEpochLength()
	epoch := slot / length
	id, noReturn := dledger.ClassicPParamUpdateWindow(epoch, epoch*length, length, s.stabilityWindow)
	return id, noReturn, nil
}

// ppupGlobals checks the globals the voting window reads and returns the
// stability window, ceiling(3k/f) as Shelley genesis defines it.
func ppupGlobals(g *v1.Globals) (uint64, error) {
	if g.GetEpochLength() == 0 {
		return 0, dispatch.Unsupported(errors.New("epoch_length must be nonzero"))
	}
	if g.GetSecurityParam() == 0 {
		return 0, dispatch.Unsupported(errors.New("security_param must be nonzero"))
	}
	f, err := ratArg("active_slot_coeff", g.GetActiveSlotCoeff())
	if err != nil {
		return 0, err
	}
	if err := positiveUnitInterval("active_slot_coeff", f); err != nil {
		return 0, err
	}
	w := dledger.ComputeNonceStabilityWindow(shelley.EraIdShelley, uint64(g.GetSecurityParam()), f)
	if w == 0 {
		return 0, dispatch.Unsupported(errors.New("stability window does not fit"))
	}
	return w, nil
}

// eraFailures runs an era's rule list for the given families.
func eraFailures(
	era v1.Era, tx common.Transaction, slot uint64, ls common.LedgerState, pp common.ProtocolParameters,
	families ...common.UtxoValidationFamily,
) []common.UtxoValidationFailure {
	switch era {
	case v1.Era_ERA_ALONZO:
		return alonzo.UtxoValidationFailures(tx, slot, ls, pp, families...)
	case v1.Era_ERA_BABBAGE:
		return babbage.UtxoValidationFailures(tx, slot, ls, pp, families...)
	}
	return legacyUtxoFailures(era, tx, slot, ls, pp, families...)
}

// rulePpup maps to PPUP: the Shelley-family update proposals of a transaction.
func rulePpup(_ context.Context, req *v1.RulePpupRequest) (*v1.RulePpupResponse, error) {
	era := req.GetContext().GetEra()
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY, v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE:
	default:
		return nil, dispatch.Unsupported(fmt.Errorf("the PPUP rule exists from Shelley to Babbage, not in %v", era))
	}
	window, err := ppupGlobals(req.GetGlobals())
	if err != nil {
		return nil, err
	}
	txType, err := txTypeOf(era)
	if err != nil {
		return nil, err
	}
	params, err := ruleProtocolParameters(era, req.GetPparams())
	if err != nil {
		return nil, err
	}
	tx, err := ledger.NewTransactionFromCbor(txType, req.GetTx())
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	rs, err := newRuleState(req.GetContext(), req.GetGlobals(), req.GetState())
	if err != nil {
		return nil, err
	}
	ls := &ppupState{ruleState: rs, stabilityWindow: window}
	var kept []common.UtxoValidationFailure
	for _, f := range eraFailures(era, tx, req.GetContext().GetSlot(), ls, params, common.UtxoFamilyPpup) {
		// The delegate's signature is checked by UTXOW, not PPUP.
		var witness common.ProtocolParameterUpdateWitnessError
		if errors.As(f.Err, &witness) {
			continue
		}
		kept = append(kept, f)
	}
	if rej := ppupRejection(kept); rej != nil {
		return &v1.RulePpupResponse{Rejection: rej}, nil
	}
	return &v1.RulePpupResponse{Delta: ppupDelta(req.GetState(), tx)}, nil
}

func ppupRejection(failures []common.UtxoValidationFailure) *v1.Rejection {
	seen := map[ruleName]string{}
	for _, f := range failures {
		var (
			delegate common.ProtocolParameterUpdateDelegateError
			epoch    common.ProtocolParameterUpdateEpochError
			version  common.ProtocolParameterUpdateVersionError
		)
		n := ruleName{"PPUP", "GoRule:" + string(f.RuleId) + fmt.Sprintf(":%T", f.Err)}
		switch {
		case errors.As(f.Err, &delegate):
			n.name = "NonGenesisUpdatePPUP"
		case errors.As(f.Err, &epoch):
			n.name = "PPUpdateWrongEpoch"
		case errors.As(f.Err, &version):
			n.name = "PVCannotFollowPPUP"
		}
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
	sort.Slice(out, func(i, j int) bool { return out[i].Name < out[j].Name })
	return &v1.Rejection{Failures: out}
}

// ppupDelta lists the proposals the transaction adds or replaces, keyed by
// genesis key and target epoch.
func ppupDelta(before *v1.LedgerState, tx common.Transaction) *v1.LedgerDelta {
	target, updates := tx.ProtocolParameterUpdates()
	d := &v1.LedgerDelta{}
	for key, upd := range updates {
		row := &v1.PParamProposal{GenesisKeyHash: key.Bytes(), TargetEpoch: target, Update: upd.Cbor()}
		same := false
		for _, b := range before.GetPparamProposals() {
			if string(b.GetGenesisKeyHash()) == string(row.GenesisKeyHash) && b.GetTargetEpoch() == target {
				same = proto.Equal(&v1.PParamProposal{Update: b.GetUpdate()}, &v1.PParamProposal{Update: row.Update})
			}
		}
		if !same {
			d.PparamProposalsChanged = append(d.PparamProposalsChanged, row)
		}
	}
	sort.Slice(d.PparamProposalsChanged, func(i, j int) bool {
		a, b := d.PparamProposalsChanged[i], d.PparamProposalsChanged[j]
		if string(a.GenesisKeyHash) != string(b.GenesisKeyHash) {
			return string(a.GenesisKeyHash) < string(b.GenesisKeyHash)
		}
		return a.TargetEpoch < b.TargetEpoch
	})
	return d
}
