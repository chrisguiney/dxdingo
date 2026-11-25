package main

import (
	"bytes"
	"context"
	"errors"
	"math"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// enactEpoch is the epoch of the boundary the action is enacted at. Enactment
// reads no epoch.
const enactEpoch = 5

// govEnact enacts one ratified action through dingo's boundary ENACT.
//
// The action is the only proposal of the scratch ledger. It must follow the
// enact state's root of its purpose, the invariant RATIFY establishes before
// an action reaches ENACT; a request that breaks it is UNSUPPORTED, as is a
// treasury withdrawal above the treasury (RATIFY refuses it).
//
// Dingo pays withdrawals and refunds the deposit when it enacts, where the
// reference pays at the EPOCH rule that applies the enacted state; both are
// compared as the registered accounts' balances afterwards.
func govEnact(ctx context.Context, req *v1.GovEnactRequest) (*v1.GovEnactResponse, error) {
	action, err := decodeGovAction(req.GetAction())
	if err != nil {
		return nil, err
	}
	if len(req.GetActionId().GetTxId()) != 32 {
		return nil, dispatch.BadRequest(errors.New("action_id: want 32-byte tx id"))
	}
	pp, err := govFullPParams(req.GetPparams())
	if err != nil {
		return nil, err
	}
	st := req.GetState()
	if err := checkWithdrawalTotal(action, st.GetPots().GetTreasury()); err != nil {
		return nil, err
	}
	target := enactTarget(req)
	ledger := &v1.LedgerState{
		Proposals: []*v1.Proposal{target}, Accounts: st.GetAccounts(), Committee: st.GetCommittee(),
		Pots: st.GetPots(), EnactState: st.GetEnactState(),
	}
	rows, _, err := govRows(pp, ledger, enactEpoch, map[string]bool{idKey(target.GetId()): true})
	if err != nil {
		return nil, err
	}
	if c := st.GetEnactState().GetConstitution(); c != nil {
		rows.Constitution = &models.Constitution{AnchorURL: c.GetAnchorUrl(), AnchorHash: c.GetAnchorHash(), PolicyHash: c.GetGuardrailScriptHash()}
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	got, err := oracle.Enact(ctx, rows, enactEpoch)
	if err != nil {
		return &v1.GovEnactResponse{Rejection: dingoRejection("ENACT", err)}, nil
	}
	return enactResponse(req, action, rows, got)
}

// enactTarget is the proposal being enacted: the request's own when its state
// holds one, else a proposal with no deposit.
func enactTarget(req *v1.GovEnactRequest) *v1.Proposal {
	target := &v1.Proposal{
		Id: req.GetActionId(), Action: req.GetAction(), ExpiresEpoch: 100,
		ReturnAccount: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: bytes.Repeat([]byte{0}, 28)},
	}
	for _, p := range req.GetState().GetProposals() {
		if sameActionID(p.GetId(), req.GetActionId()) {
			target.ProposedEpoch, target.Deposit = p.GetProposedEpoch(), p.GetDeposit()
			if p.GetReturnAccount() != nil {
				target.ReturnAccount = p.GetReturnAccount()
			}
		}
	}
	return target
}

// checkWithdrawalTotal refuses a treasury withdrawal above the treasury.
func checkWithdrawalTotal(a decodedAction, treasury uint64) error {
	w, ok := a.Action.(*lcommon.TreasuryWithdrawalGovAction)
	if !ok {
		return nil
	}
	var total uint64
	for _, amount := range w.Withdrawals {
		if total > math.MaxUint64-amount {
			return dispatch.Unsupported(errors.New("withdrawal total overflows"))
		}
		total += amount
	}
	if total > treasury {
		return dispatch.Unsupported(errors.New("withdrawal exceeds the treasury"))
	}
	return nil
}

func enactResponse(req *v1.GovEnactRequest, action decodedAction, rows dxoracle.GovState, got *dxoracle.EnactOutput) (*v1.GovEnactResponse, error) {
	resp := &v1.GovEnactResponse{Delta: &v1.LedgerDelta{}}
	var err error
	if resp.Withdrawals, err = credits(got.Accounts, rows.Accounts); err != nil {
		return nil, err
	}
	byKey := map[string]*v1.Account{}
	for _, a := range req.GetState().GetAccounts() {
		byKey[string([]byte{credTag(a.GetCredential())})+string(a.GetCredential().GetHash())] = a
	}
	for _, a := range got.Accounts {
		row := proto.Clone(byKey[string([]byte{a.Credential.Tag})+string(a.Credential.Key)]).(*v1.Account)
		row.Balance = a.Reward
		resp.Delta.AccountsChanged = append(resp.Delta.AccountsChanged, row)
	}
	sortAccounts(resp.Delta.AccountsChanged)
	resp.Delta.TreasuryDelta = int64(got.Treasury) - int64(rows.Treasury) //nolint:gosec // test-sized pots

	switch action.Type {
	case lcommon.GovActionTypeNoConfidence:
		resp.Delta.CommitteeAfter = &v1.Committee{}
	case lcommon.GovActionTypeUpdateCommittee:
		c := &v1.Committee{}
		for _, m := range got.Committee {
			c.Members = append(c.Members, &v1.CommitteeMember{Cold: credProtoGov(m.ColdCredentialTag, m.ColdCredHash), Expiry: m.ExpiresEpoch})
		}
		sort.Slice(c.Members, func(i, j int) bool { return credLess(c.Members[i].GetCold(), c.Members[j].GetCold()) })
		c.Threshold = ratOrNil(got.Quorum)
		resp.Delta.CommitteeAfter = c
	}

	es := &v1.EnactState{
		PrevPparamUpdate: rootProto(got.Roots, lcommon.GovActionTypeParameterChange),
		PrevHardFork:     rootProto(got.Roots, lcommon.GovActionTypeHardForkInitiation),
		PrevCommittee:    rootProto(got.Roots, lcommon.GovActionTypeNoConfidence),
		PrevConstitution: rootProto(got.Roots, lcommon.GovActionTypeNewConstitution),
	}
	if c := got.Constitution; c != nil {
		es.Constitution = &v1.Constitution{AnchorUrl: c.AnchorURL, AnchorHash: c.AnchorHash, GuardrailScriptHash: c.PolicyHash}
	}
	resp.Delta.EnactStateAfter = es

	if action.Type == lcommon.GovActionTypeParameterChange || action.Type == lcommon.GovActionTypeHardForkInitiation {
		after, ok := got.PParams.(*conway.ConwayProtocolParameters)
		if !ok {
			return nil, errors.New("enacted parameters are not Conway's")
		}
		resp.Delta.PparamsAfter = conwayPParamsProto(after)
	}
	return resp, nil
}

func rootProto(roots map[uint8]dxoracle.GovAction, t lcommon.GovActionType) *v1.GovActionId {
	r, ok := roots[uint8(t)]
	if !ok {
		return nil
	}
	return govActionIDProto(r)
}

func credTag(c *v1.Credential) uint8 {
	if c.GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
		return 1
	}
	return 0
}

func credProtoGov(tag uint8, hash []byte) *v1.Credential {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if tag == 1 {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.Credential{Kind: kind, Hash: hash}
}

func credLess(a, b *v1.Credential) bool {
	if a.GetKind() != b.GetKind() {
		return a.GetKind() < b.GetKind()
	}
	return bytes.Compare(a.GetHash(), b.GetHash()) < 0
}

func sortAccounts(as []*v1.Account) {
	sort.Slice(as, func(i, j int) bool { return credLess(as[i].GetCredential(), as[j].GetCredential()) })
}
