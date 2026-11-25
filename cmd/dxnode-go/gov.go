package main

import (
	"context"
	"errors"
	"math/big"

	"github.com/blinklabs-io/dingo/database"
	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/database/types"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func registerGov(r *dispatch.Registry) {
	dispatch.Register(r, govThresholds)
	dispatch.Register(r, govSpoRatio)
	dispatch.Register(r, govDRepRatio)
	dispatch.Register(r, govCommitteeRatio)
	dispatch.Register(r, govCommitteeQuorum)
	dispatch.Register(r, govProposalTree)
	dispatch.Register(r, govDRepExpiry)
}

func ratOrNil(r *big.Rat) *v1.Rational {
	if r == nil {
		return nil
	}
	return convert.RatProto(r)
}

// govThresholds maps to dingo's threshold entry points. The committee entry
// is unset when the committee does not vote on the action or cannot approve
// it.
func govThresholds(_ context.Context, req *v1.GovThresholdsRequest) (*v1.GovThresholdsResponse, error) {
	action, err := decodeGovAction(req.GetAction())
	if err != nil {
		return nil, err
	}
	pp, err := govPParams(req.GetPparams(), req.GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	committee, err := decodeCommittee(req.GetCommittee(), req.GetEpoch())
	if err != nil {
		return nil, err
	}
	actionType := action.Type
	pc, _ := action.Action.(lcommon.ParameterChangeGovAction)
	major := pp.ProtocolVersion.Major
	present := committee != nil

	resp := &v1.GovThresholdsResponse{
		Spo:  ratOrNil(governance.SPOThreshold(actionType, pp, pc, present)),
		Drep: ratOrNil(governance.DRepThresholdInForce(major, actionType, pp, pc, present)),
	}
	var quorum *big.Rat
	if committee != nil {
		quorum = committee.quorum
	}
	active := 0
	if committee != nil {
		active = len(committee.hot)
	}
	cc := governance.CommitteeRequirement(
		actionType, major, !present, false, active, pp.MinCommitteeSize, quorum,
	)
	if cc.Voting && cc.Reason == "" {
		resp.Committee = convert.RatProto(cc.Quorum)
	}
	return resp, nil
}

// poolSnapshots builds the pool stake snapshot rows with each pool's
// reward-account auto-vote resolved by dingo's production resolver over the
// request's pools and accounts.
func poolSnapshots(ctx context.Context, dist []*v1.PoolStake, pools []*v1.Pool, accounts []*v1.Account) ([]*models.PoolStakeSnapshot, error) {
	snaps := make([]*models.PoolStakeSnapshot, 0, len(dist))
	for _, p := range dist {
		id, err := hash28("pool_distribution pool_id", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		snaps = append(snaps, &models.PoolStakeSnapshot{PoolKeyHash: id, TotalStake: types.Uint64(p.GetStake())})
	}
	poolRows := map[string]models.Pool{}
	for _, p := range pools {
		id, err := hash28("pools pool_id", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		row := models.Pool{PoolKeyHash: id}
		if p.GetRewardAccount() != nil {
			tag, hash, err := govCredential("pools reward_account", p.GetRewardAccount())
			if err != nil {
				return nil, err
			}
			row.RewardAccount, row.RewardAccountCredentialTag = hash, tag
		}
		poolRows[string(id)] = row
	}
	accountRows := map[string]*models.Account{}
	for _, a := range accounts {
		tag, hash, err := govCredential("accounts credential", a.GetCredential())
		if err != nil {
			return nil, err
		}
		del, err := delegation(a.GetDrep())
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		row := &models.Account{StakingKey: hash, CredentialTag: tag, Active: true}
		switch del.Type {
		case dxoracle.DelegateAlwaysAbstain:
			row.DrepType = models.DrepTypeAlwaysAbstain
		case dxoracle.DelegateAlwaysNoConfidence:
			row.DrepType = models.DrepTypeAlwaysNoConfidence
		}
		accountRows[models.StakeCredentialRef{Tag: tag, Key: hash}.MapKey()] = row
	}
	err := database.ResolveRewardAccountAutoVotes(ctx, snaps,
		func(pkhs []lcommon.PoolKeyHash) ([]models.Pool, error) {
			var out []models.Pool
			for _, k := range pkhs {
				if row, ok := poolRows[string(k[:])]; ok {
					out = append(out, row)
				}
			}
			return out, nil
		},
		func(refs []models.StakeCredentialRef) (map[string]*models.Account, error) {
			out := map[string]*models.Account{}
			for _, ref := range refs {
				if row, ok := accountRows[ref.MapKey()]; ok {
					out[ref.MapKey()] = row
				}
			}
			return out, nil
		})
	return snaps, err
}

// votesFor selects the votes of one role on one action as dingo vote rows.
func votesFor(votes []*v1.VoteRecord, id *v1.GovActionId, role v1.VoterRole) ([]*models.GovernanceVote, error) {
	var out []*models.GovernanceVote
	for _, v := range votes {
		if v.GetVoter().GetRole() != role || !sameActionID(v.GetActionId(), id) {
			continue
		}
		val, err := voteValue(v.GetVote())
		if err != nil {
			return nil, err
		}
		row := &models.GovernanceVote{Vote: val}
		if role == v1.VoterRole_VOTER_ROLE_STAKE_POOL {
			pid, err := hash28("vote pool_id", v.GetVoter().GetPoolId())
			if err != nil {
				return nil, err
			}
			row.VoterCredential = pid
		} else {
			tag, hash, err := govCredential("vote credential", v.GetVoter().GetCredential())
			if err != nil {
				return nil, err
			}
			row.VoterCredentialTag, row.VoterCredential = tag, hash
		}
		out = append(out, row)
	}
	return out, nil
}

// govSpoRatio maps to governance.TallySPOVotes and SPOYesRatio. The reward
// account auto-votes come from dingo's resolver; the stake total is the sum
// of the distribution.
func govSpoRatio(ctx context.Context, req *v1.GovSpoRatioRequest) (*v1.GovSpoRatioResponse, error) {
	action, err := decodeGovAction(req.GetAction())
	if err != nil {
		return nil, err
	}
	snaps, err := poolSnapshots(ctx, req.GetPoolDistribution(), req.GetPools(), req.GetAccounts())
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
	votes, err := votesFor(req.GetVotes(), req.GetActionId(), v1.VoterRole_VOTER_ROLE_STAKE_POOL)
	if err != nil {
		return nil, err
	}
	tally := &governance.ProposalTally{ActionType: uint8(action.Type)}
	state := &governance.SPOVotingState{Dist: snaps, TotalStake: total}
	if err := governance.TallySPOVotes(state, uint(req.GetProtocolVersion().GetMajor()), votes, tally); err != nil {
		return &v1.GovSpoRatioResponse{Rejection: dingoRejection("RATIFY", err)}, nil
	}
	return &v1.GovSpoRatioResponse{Ratio: convert.RatProto(tally.SPOYesRatio())}, nil
}

// govDRepRatio maps to governance.TallyDRepVotes and DRepYesRatio. The voting
// state holds the DReps dingo's loader keeps: registered and active at the
// epoch (governance.DRepActiveAtEpoch).
func govDRepRatio(_ context.Context, req *v1.GovDRepRatioRequest) (*v1.GovDRepRatioResponse, error) {
	action, err := decodeGovAction(req.GetAction())
	if err != nil {
		return nil, err
	}
	state := &governance.DRepVotingState{Powers: map[string]uint64{}}
	registered := map[string]*models.Drep{}
	for _, d := range req.GetDreps() {
		tag, hash, err := govCredential("dreps credential", d.GetCredential())
		if err != nil {
			return nil, err
		}
		drep := &models.Drep{CredentialTag: tag, Credential: hash, ExpiryEpoch: d.GetExpiry(), Active: true}
		registered[models.StakeCredentialRef{Tag: tag, Key: hash}.MapKey()] = drep
	}
	for _, e := range req.GetDrepDistribution() {
		switch t := e.GetDrep().GetTarget().(type) {
		case *v1.DRep_Credential:
			tag, hash, err := govCredential("drep_distribution credential", t.Credential)
			if err != nil {
				return nil, err
			}
			key := models.StakeCredentialRef{Tag: tag, Key: hash}.MapKey()
			drep, ok := registered[key]
			if !ok || !governance.DRepActiveAtEpoch(drep, req.GetEpoch()) {
				continue
			}
			if _, dup := state.Powers[key]; !dup {
				state.Dreps = append(state.Dreps, drep)
			}
			state.Powers[key] += e.GetStake()
		case *v1.DRep_AlwaysAbstain:
			state.AbstainPower += e.GetStake()
		case *v1.DRep_AlwaysNoConfidence:
			state.NoConfidencePower += e.GetStake()
		default:
			return nil, dispatch.BadRequest(errors.New("drep_distribution: drep target unset"))
		}
	}
	votes, err := votesFor(req.GetVotes(), req.GetActionId(), v1.VoterRole_VOTER_ROLE_DREP)
	if err != nil {
		return nil, err
	}
	tally := &governance.ProposalTally{ActionType: uint8(action.Type)}
	if err := governance.TallyDRepVotes(state, votes, tally); err != nil {
		return &v1.GovDRepRatioResponse{Rejection: dingoRejection("RATIFY", err)}, nil
	}
	return &v1.GovDRepRatioResponse{Ratio: convert.RatProto(tally.DRepYesRatio())}, nil
}

// votingState is dingo's committee voting view of c.
func (c *govCommittee) votingState() *governance.CommitteeVotingState {
	state := &governance.CommitteeVotingState{
		CommitteePresent:      true,
		ActiveMemberCount:     len(c.hot),
		HotCredentialPresence: map[string]struct{}{},
	}
	for _, h := range c.hot {
		key := models.CommitteeCredential{CredentialTag: h.tag, Credential: h.hash}.Key()
		state.MemberHotCredentials = append(state.MemberHotCredentials, key)
		state.HotCredentialPresence[key] = struct{}{}
	}
	return state
}

// govCommitteeRatio maps to governance.TallyCCVotes and CCYesRatio.
func govCommitteeRatio(_ context.Context, req *v1.GovCommitteeRatioRequest) (*v1.GovCommitteeRatioResponse, error) {
	committee, err := decodeCommittee(req.GetCommittee(), req.GetEpoch())
	if err != nil {
		return nil, err
	}
	votes, err := votesFor(req.GetVotes(), req.GetActionId(), v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE)
	if err != nil {
		return nil, err
	}
	tally := &governance.ProposalTally{}
	state := &governance.CommitteeVotingState{}
	if committee != nil {
		state = committee.votingState()
	}
	if err := governance.TallyCCVotes(state, votes, tally); err != nil {
		return &v1.GovCommitteeRatioResponse{Rejection: dingoRejection("RATIFY", err)}, nil
	}
	return &v1.GovCommitteeRatioResponse{Ratio: convert.RatProto(tally.CCYesRatio())}, nil
}

// govCommitteeQuorum maps to governance.CommitteeRequirement for an action
// that needs committee approval.
func govCommitteeQuorum(_ context.Context, req *v1.GovCommitteeQuorumRequest) (*v1.GovCommitteeQuorumResponse, error) {
	pp, err := govPParams(req.GetPparams(), req.GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	committee, err := decodeCommittee(req.GetCommittee(), req.GetEpoch())
	if err != nil {
		return nil, err
	}
	var quorum *big.Rat
	active := 0
	if committee != nil {
		quorum, active = committee.quorum, len(committee.hot)
	}
	cc := governance.CommitteeRequirement(
		lcommon.GovActionTypeTreasuryWithdrawal, pp.ProtocolVersion.Major, committee == nil, false, active, pp.MinCommitteeSize, quorum,
	)
	if cc.Reason != "" {
		return &v1.GovCommitteeQuorumResponse{}, nil
	}
	return &v1.GovCommitteeQuorumResponse{Quorum: ratOrNil(cc.Quorum)}, nil
}
