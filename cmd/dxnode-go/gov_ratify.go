package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
	"github.com/blinklabs-io/gouroboros/cbor"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// govBoundary describes a request as the rows dingo's boundary reads. It is
// translation only: every decision is dingo's, made by the production
// governance.ProcessEpoch over a scratch database holding these rows.

// purposeRoot names the last enacted action of each purpose.
type purposeRoot struct {
	typ lcommon.GovActionType
	id  *v1.GovActionId
}

// enactRoots lists the purpose roots of an enact state as enacted proposals.
// The committee purpose holds NoConfidence or UpdateCommittee, which the enact
// state does not say: a root with no committee left is the NoConfidence that
// removed it.
func enactRoots(es *v1.EnactState, committee bool) []purposeRoot {
	committeeType := lcommon.GovActionTypeNoConfidence
	if committee {
		committeeType = lcommon.GovActionTypeUpdateCommittee
	}
	var out []purposeRoot
	for _, r := range []purposeRoot{
		{lcommon.GovActionTypeParameterChange, es.GetPrevPparamUpdate()},
		{lcommon.GovActionTypeHardForkInitiation, es.GetPrevHardFork()},
		{committeeType, es.GetPrevCommittee()},
		{lcommon.GovActionTypeNewConstitution, es.GetPrevConstitution()},
	} {
		if r.id != nil {
			out = append(out, r)
		}
	}
	return out
}

func govActionOf(id *v1.GovActionId) dxoracle.GovAction {
	return dxoracle.GovAction{TxHash: id.GetTxId(), Index: id.GetIndex()}
}

func govActionIDProto(a dxoracle.GovAction) *v1.GovActionId {
	return &v1.GovActionId{TxId: a.TxHash, Index: a.Index}
}

// govRows builds the scratch rows for the ledger tables of a request. The
// proposals come in forest order from proposalForest; ratified names the ones
// an earlier boundary accepted.
func govRows(
	pp *conway.ConwayProtocolParameters,
	st *v1.LedgerState,
	epoch uint64,
	ratified map[string]bool,
) (dxoracle.GovState, []*treeNode, error) {
	out := dxoracle.GovState{PParams: pp, Treasury: st.GetPots().GetTreasury()}
	order, _, err := proposalForest(st.GetProposals(), st.GetEnactState())
	if err != nil {
		return out, nil, err
	}
	committee, err := decodeCommittee(st.GetCommittee(), epoch)
	if err != nil {
		return out, nil, err
	}
	if committee != nil {
		out.Committee, out.Quorum = committee.members, committee.quorum
	}
	for _, d := range st.GetDreps() {
		tag, hash, err := govCredential("dreps credential", d.GetCredential())
		if err != nil {
			return out, nil, err
		}
		out.Dreps = append(out.Dreps, dxoracle.DRep{Tag: tag, Key: hash, ExpiryEpoch: d.GetExpiry(), Active: true})
	}
	for _, a := range st.GetAccounts() {
		tag, hash, err := govCredential("accounts credential", a.GetCredential())
		if err != nil {
			return out, nil, err
		}
		out.Accounts = append(out.Accounts, dxoracle.Account{Tag: tag, Key: hash, Reward: a.GetBalance(), Active: true})
	}
	for _, r := range enactRoots(st.GetEnactState(), committee != nil) {
		out.Roots = append(out.Roots, dxoracle.GovProposal{
			ID: govActionOf(r.id), ActionType: uint8(r.typ),
		})
	}
	index := map[string]int{}
	for i, n := range order {
		p := st.GetProposals()[proposalIndex(st.GetProposals(), n.id)]
		tag, hash, err := govCredential("proposals return_account", p.GetReturnAccount())
		if err != nil {
			return out, nil, err
		}
		row := dxoracle.GovProposal{
			ID: govActionOf(n.id), ActionType: n.proposal.ActionType, Cbor: p.GetAction(),
			ProposedEpoch: p.GetProposedEpoch(), ExpiresEpoch: p.GetExpiresEpoch(), Deposit: p.GetDeposit(),
			Return:   dxoracle.Credential{Tag: tag, Key: hash},
			Ratified: ratified[idKey(n.id)],
		}
		if n.proposal.ParentTxHash != nil {
			row.Parent = &dxoracle.GovAction{TxHash: n.proposal.ParentTxHash, Index: *n.proposal.ParentActionIdx}
		}
		out.Proposals = append(out.Proposals, row)
		index[idKey(n.id)] = i
	}
	for _, v := range st.GetVotes() {
		i, ok := index[idKey(v.GetActionId())]
		if !ok {
			continue
		}
		vote, err := voteValue(v.GetVote())
		if err != nil {
			return out, nil, err
		}
		gv := dxoracle.GovVote{Proposal: i, Vote: vote}
		switch v.GetVoter().GetRole() {
		case v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE:
			gv.Role = 0
		case v1.VoterRole_VOTER_ROLE_DREP:
			gv.Role = 1
		case v1.VoterRole_VOTER_ROLE_STAKE_POOL:
			gv.Role = 2
		default:
			return out, nil, dispatch.BadRequest(fmt.Errorf("vote voter role %v", v.GetVoter().GetRole()))
		}
		if gv.Role == 2 {
			if gv.Key, err = hash28("vote pool_id", v.GetVoter().GetPoolId()); err != nil {
				return out, nil, err
			}
		} else if gv.Tag, gv.Key, err = govCredential("vote credential", v.GetVoter().GetCredential()); err != nil {
			return out, nil, err
		}
		out.Votes = append(out.Votes, gv)
	}
	return out, order, nil
}

func proposalIndex(ps []*v1.Proposal, id *v1.GovActionId) int {
	for i, p := range ps {
		if sameActionID(p.GetId(), id) {
			return i
		}
	}
	return -1
}

// govFullPParams decodes a request's protocol parameters, which must be
// Conway's. The thresholds are checked as unit intervals first.
func govFullPParams(pp *v1.ProtocolParameters) (*conway.ConwayProtocolParameters, error) {
	if _, err := govPParams(pp, pp.GetProtocolVersion()); err != nil {
		return nil, err
	}
	out, err := ruleProtocolParameters(v1.Era_ERA_CONWAY, pp)
	if err != nil {
		return nil, dispatch.BadRequest(err)
	}
	return out.(*conway.ConwayProtocolParameters), nil
}

// govRatify runs dingo's boundary RATIFY over the request's tables.
//
// The reference decides over the pulser's snapshot of proposals in
// submission order (RATIFY, Rules/Ratify.hs); dingo orders its proposals by
// proposed epoch, so a request whose proposals are out of that order is
// UNSUPPORTED. A proposal that expired before the previous epoch left the
// proposal set earlier on both nodes, so it is UNSUPPORTED too.
func govRatify(ctx context.Context, req *v1.GovRatifyRequest) (*v1.GovRatifyResponse, error) {
	epoch := req.GetContext().GetEpoch()
	if epoch == 0 {
		return nil, dispatch.Unsupported(errors.New("no ratification before epoch 1"))
	}
	st := req.GetState()
	pp, err := govPParams(req.GetPparams(), req.GetPparams().GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	if err := checkRatifyProposals(st.GetProposals(), epoch); err != nil {
		return nil, err
	}
	rows, _, err := govRows(pp, st, epoch, nil)
	if err != nil {
		return nil, err
	}
	snaps, err := poolSnapshots(ctx, req.GetPoolDistribution(), st.GetPools(), st.GetAccounts())
	if err != nil {
		return nil, err
	}
	var total uint64
	for _, s := range snaps {
		total += uint64(s.TotalStake)
	}
	if total == 0 {
		return nil, dispatch.Unsupported(errors.New("pool distribution has no stake"))
	}
	rows.SPO = &governance.SPOVotingState{Dist: snaps, TotalStake: total}
	for _, e := range req.GetDrepDistribution() {
		del, err := delegation(e.GetDrep())
		if err != nil || del.Type == dxoracle.DelegateNone {
			return nil, dispatch.BadRequest(errors.New("drep_distribution: drep target unset"))
		}
		rows.DRepStake = append(rows.DRepStake, dxoracle.GovDRepStake{Delegation: del, Stake: e.GetStake()})
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	got, err := oracle.Ratify(ctx, rows, epoch)
	if err != nil {
		return &v1.GovRatifyResponse{Rejection: dingoRejection("RATIFY", err)}, nil
	}
	resp := &v1.GovRatifyResponse{Delayed: got.Delayed}
	for _, a := range got.Ratified {
		resp.Enacted = append(resp.Enacted, govActionIDProto(a))
	}
	for _, a := range got.Expired {
		resp.Expired = append(resp.Expired, govActionIDProto(a))
	}
	sortActionIDs(resp.Expired)
	return resp, nil
}

// checkRatifyProposals enforces the request shapes RATIFY is defined on.
func checkRatifyProposals(ps []*v1.Proposal, epoch uint64) error {
	for i, p := range ps {
		if i > 0 && p.GetProposedEpoch() < ps[i-1].GetProposedEpoch() {
			return dispatch.Unsupported(errors.New("proposals are not in submission order"))
		}
		if p.GetExpiresEpoch()+1 < epoch {
			return dispatch.Unsupported(fmt.Errorf("proposal %s expired before epoch %d", idKey(p.GetId()), epoch-1))
		}
	}
	return nil
}

// govProposalExpiry runs dingo's boundaries that expire proposals and then
// return their deposits. Every voting threshold is 1 over an empty ledger, so
// nothing is accepted and the only way out of the proposal set is expiry.
func govProposalExpiry(ctx context.Context, req *v1.GovProposalExpiryRequest) (*v1.GovProposalExpiryResponse, error) {
	epoch := req.GetEpoch()
	if epoch == 0 {
		return nil, dispatch.Unsupported(errors.New("no expiry before epoch 1"))
	}
	st := &v1.LedgerState{Proposals: req.GetProposals(), Accounts: req.GetAccounts()}
	rows, _, err := govRows(expiryPParams(), st, epoch, nil)
	if err != nil {
		return nil, err
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	got, err := oracle.ProposalExpiry(ctx, rows, epoch)
	if err != nil {
		return &v1.GovProposalExpiryResponse{Rejection: dingoRejection("EPOCH", err)}, nil
	}
	resp := &v1.GovProposalExpiryResponse{UnclaimedToTreasury: got.Unclaimed}
	for _, a := range got.Expired {
		resp.Expired = append(resp.Expired, govActionIDProto(a))
	}
	resp.Refunds, err = credits(got.Accounts, rows.Accounts)
	return resp, err
}

// expiryPParams are the parameters the expiry function runs under: protocol
// version 10 and every voting threshold 1.
func expiryPParams() *conway.ConwayProtocolParameters {
	one := cbor.Rat{Rat: big.NewRat(1, 1)}
	pp := &conway.ConwayProtocolParameters{}
	pp.ProtocolVersion.Major = 10
	pt, dt := &pp.PoolVotingThresholds, &pp.DRepVotingThresholds
	pt.MotionNoConfidence, pt.CommitteeNormal, pt.CommitteeNoConfidence = one, one, one
	pt.HardForkInitiation, pt.PpSecurityGroup = one, one
	dt.MotionNoConfidence, dt.CommitteeNormal, dt.CommitteeNoConfidence = one, one, one
	dt.UpdateToConstitution, dt.HardForkInitiation, dt.PpNetworkGroup = one, one, one
	dt.PpEconomicGroup, dt.PpTechnicalGroup, dt.PpGovGroup, dt.TreasuryWithdrawal = one, one, one, one
	return pp
}

// credits lists the balance increase of each account that changed.
func credits(after []dxoracle.AccountReward, before []dxoracle.Account) ([]*v1.Credit, error) {
	old := map[string]uint64{}
	for _, a := range before {
		old[string([]byte{a.Tag})+string(a.Key)] = a.Reward
	}
	var out []*v1.Credit
	for _, a := range after {
		was, ok := old[string([]byte{a.Credential.Tag})+string(a.Credential.Key)]
		if !ok || a.Reward < was {
			return nil, fmt.Errorf("account %x changed from %d to %d", a.Credential.Key, was, a.Reward)
		}
		kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
		if a.Credential.Tag == 1 {
			kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
		}
		out = append(out, &v1.Credit{
			Credential: &v1.Credential{Kind: kind, Hash: a.Credential.Key},
			Amount:     a.Reward - was,
		})
	}
	sort.Slice(out, func(i, j int) bool {
		a, b := out[i].GetCredential(), out[j].GetCredential()
		if a.GetKind() != b.GetKind() {
			return a.GetKind() < b.GetKind()
		}
		return bytes.Compare(a.GetHash(), b.GetHash()) < 0
	})
	return out, nil
}
