package main

import (
	"context"
	"errors"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// newEpochHasGovernance reports whether req carries governance state: the
// DRep pulser's snapshot taken at the previous boundary.
func newEpochHasGovernance(req *v1.EpochNewEpochRequest) bool {
	st := req.GetState()
	return len(st.GetDreps()) > 0 || len(st.GetProposals()) > 0 || len(st.GetVotes()) > 0 ||
		st.GetCommittee() != nil || st.GetEnactState() != nil ||
		len(req.GetPoolDistribution()) > 0 || len(req.GetDrepDistribution()) > 0
}

// newEpochGovState describes the governance part of req as the rows dingo's
// RATIFY reads at the boundary into the epoch before req's. The domain is
// gov_ratify's, taken at that epoch, with one more exclusion: a proposal for
// a hard fork is UNSUPPORTED, since enacting it changes the protocol version
// and runs the HARDFORK rule, which the response does not model.
func newEpochGovState(ctx context.Context, req *v1.EpochNewEpochRequest, pp *conway.ConwayProtocolParameters) (*dxoracle.GovState, error) {
	st := req.GetState()
	prior := req.GetContext().GetEpoch() - 1
	if err := checkRatifyProposals(st.GetProposals(), prior); err != nil {
		return nil, err
	}
	for _, p := range st.GetProposals() {
		a, err := decodeGovAction(p.GetAction())
		if err != nil {
			return nil, err
		}
		if a.Type == lcommon.GovActionTypeHardForkInitiation {
			return nil, dispatch.Unsupported(errors.New("a hard fork proposal changes the protocol version"))
		}
	}
	rows, _, err := govRows(pp, st, prior, nil)
	if err != nil {
		return nil, err
	}
	if c := st.GetEnactState().GetConstitution(); c != nil {
		rows.Constitution = &models.Constitution{AnchorURL: c.GetAnchorUrl(), AnchorHash: c.GetAnchorHash(), PolicyHash: c.GetGuardrailScriptHash()}
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
	return &rows, nil
}

// applyNewEpochGov reports what the governance side of the boundary did:
// the actions enacted and expired, the proposals and DReps that changed, and
// the enacted committee and roots.
func applyNewEpochGov(req *v1.EpochNewEpochRequest, rows *dxoracle.GovState, got *dxoracle.NewEpochGovOutput, resp *v1.EpochNewEpochResponse) {
	for _, a := range got.Enacted {
		resp.Enacted = append(resp.Enacted, govActionIDProto(a))
	}
	for _, a := range got.Expired {
		resp.Expired = append(resp.Expired, govActionIDProto(a))
	}
	sortActionIDs(resp.Expired)
	delta := resp.Delta
	for _, a := range got.Removed {
		delta.ProposalsRemoved = append(delta.ProposalsRemoved, govActionIDProto(a))
	}
	sortActionIDs(delta.ProposalsRemoved)

	records := map[string]*v1.DRepRecord{}
	for _, d := range req.GetState().GetDreps() {
		records[string([]byte{credTag(d.GetCredential())})+string(d.GetCredential().GetHash())] = d
	}
	for _, d := range got.Dreps {
		before := records[string([]byte{d.Tag})+string(d.Key)]
		if before.GetExpiry() == d.ExpiryEpoch {
			continue
		}
		row := proto.Clone(before).(*v1.DRepRecord)
		row.Expiry = d.ExpiryEpoch
		delta.DrepsChanged = append(delta.DrepsChanged, row)
	}
	sort.Slice(delta.DrepsChanged, func(i, j int) bool {
		return credLess(delta.DrepsChanged[i].GetCredential(), delta.DrepsChanged[j].GetCredential())
	})

	types := map[string]uint8{}
	for _, p := range rows.Proposals {
		types[proposalIDKey(p.ID)] = p.ActionType
	}
	for _, a := range got.Enacted {
		switch lcommon.GovActionType(types[proposalIDKey(a)]) {
		case lcommon.GovActionTypeNoConfidence:
			delta.CommitteeAfter = &v1.Committee{}
		case lcommon.GovActionTypeUpdateCommittee:
			c := &v1.Committee{}
			for _, m := range got.Committee {
				c.Members = append(c.Members, &v1.CommitteeMember{Cold: credProtoGov(m.ColdCredentialTag, m.ColdCredHash), Expiry: m.ExpiresEpoch})
			}
			sort.Slice(c.Members, func(i, j int) bool { return credLess(c.Members[i].GetCold(), c.Members[j].GetCold()) })
			c.Threshold = ratOrNil(got.Quorum)
			delta.CommitteeAfter = c
		}
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
	delta.EnactStateAfter = es
}

func proposalIDKey(a dxoracle.GovAction) string {
	return idKey(govActionIDProto(a))
}
