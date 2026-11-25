package main

import (
	"context"
	"errors"
	"fmt"
	"math/big"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/governance"
	"github.com/blinklabs-io/gouroboros/cbor"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Argument decoding shared by the gov functions. Everything here translates
// request fields into the arguments of dingo's governance entry points.

// decodeGovAction decodes GovAction CBOR. An undecodable action is a bad
// request on both nodes.
func decodeGovAction(b []byte) (decodedAction, error) {
	if len(b) == 0 {
		return decodedAction{}, dispatch.BadRequest(errors.New("action: empty"))
	}
	var wrapper conway.ConwayGovAction
	if err := wrapper.UnmarshalCBOR(b); err != nil {
		return decodedAction{}, dispatch.BadRequest(fmt.Errorf("action: %w", err))
	}
	return decodedAction{Action: wrapper.Action, Type: lcommon.GovActionType(wrapper.Type)}, nil
}

type decodedAction struct {
	Action lcommon.GovAction
	Type   lcommon.GovActionType
}

// hash28 requires a Blake2b-224 hash.
func hash28(name string, b []byte) ([]byte, error) {
	if len(b) != 28 {
		return nil, dispatch.BadRequest(fmt.Errorf("%s: want 28 bytes, got %d", name, len(b)))
	}
	return b, nil
}

// govCredential returns the credential tag and its checked hash.
func govCredential(name string, c *v1.Credential) (uint8, []byte, error) {
	tag, err := credentialTag(c)
	if err != nil {
		return 0, nil, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
	}
	h, err := hash28(name, c.GetHash())
	return tag, h, err
}

// voteValue maps a Vote onto dingo's vote codes.
func voteValue(v v1.Vote) (uint8, error) {
	switch v {
	case v1.Vote_VOTE_NO:
		return 0, nil
	case v1.Vote_VOTE_YES:
		return 1, nil
	case v1.Vote_VOTE_ABSTAIN:
		return 2, nil
	}
	return 0, dispatch.BadRequest(fmt.Errorf("vote %v", v))
}

func sameActionID(a, b *v1.GovActionId) bool {
	return string(a.GetTxId()) == string(b.GetTxId()) && a.GetIndex() == b.GetIndex()
}

// unitRat decodes a threshold. Thresholds are UnitIntervals in the reference.
func unitRat(name string, r *v1.Rational) (cbor.Rat, error) {
	x, err := ratArg(name, r)
	if err != nil {
		return cbor.Rat{}, err
	}
	if err := unitInterval(name, x); err != nil {
		return cbor.Rat{}, err
	}
	return cbor.Rat{Rat: x}, nil
}

// govPParams decodes the parameters the gov functions read: the voting
// thresholds, the committee size and term limit, and the protocol version
// (taken from pv, which a request carries beside its pparams).
func govPParams(pp *v1.ProtocolParameters, pv *v1.ProtocolVersion) (*conway.ConwayProtocolParameters, error) {
	out := &conway.ConwayProtocolParameters{}
	// An unset group is zero, as in the reference's empty parameters; dingo
	// dereferences every threshold.
	zero := cbor.Rat{Rat: new(big.Rat)}
	out.PoolVotingThresholds = conway.PoolVotingThresholds{
		MotionNoConfidence: zero, CommitteeNormal: zero, CommitteeNoConfidence: zero, HardForkInitiation: zero, PpSecurityGroup: zero,
	}
	out.DRepVotingThresholds = conway.DRepVotingThresholds{
		MotionNoConfidence: zero, CommitteeNormal: zero, CommitteeNoConfidence: zero, UpdateToConstitution: zero,
		HardForkInitiation: zero, PpNetworkGroup: zero, PpEconomicGroup: zero, PpTechnicalGroup: zero,
		PpGovGroup: zero, TreasuryWithdrawal: zero,
	}
	out.ProtocolVersion.Major = uint(pv.GetMajor())
	out.ProtocolVersion.Minor = uint(pv.GetMinor())
	out.MinCommitteeSize = uint(pp.GetCommitteeMinSize())
	out.CommitteeTermLimit = pp.GetCommitteeMaxTermLength()
	if p := pp.GetPoolVotingThresholds(); p != nil {
		t := &out.PoolVotingThresholds
		for _, f := range []struct {
			name string
			in   *v1.Rational
			out  *cbor.Rat
		}{
			{"pool motion_no_confidence", p.GetMotionNoConfidence(), &t.MotionNoConfidence},
			{"pool committee_normal", p.GetCommitteeNormal(), &t.CommitteeNormal},
			{"pool committee_no_confidence", p.GetCommitteeNoConfidence(), &t.CommitteeNoConfidence},
			{"pool hard_fork_initiation", p.GetHardForkInitiation(), &t.HardForkInitiation},
			{"pool pp_security_group", p.GetPpSecurityGroup(), &t.PpSecurityGroup},
		} {
			r, err := unitRat(f.name, f.in)
			if err != nil {
				return nil, err
			}
			*f.out = r
		}
	}
	if d := pp.GetDrepVotingThresholds(); d != nil {
		t := &out.DRepVotingThresholds
		for _, f := range []struct {
			name string
			in   *v1.Rational
			out  *cbor.Rat
		}{
			{"drep motion_no_confidence", d.GetMotionNoConfidence(), &t.MotionNoConfidence},
			{"drep committee_normal", d.GetCommitteeNormal(), &t.CommitteeNormal},
			{"drep committee_no_confidence", d.GetCommitteeNoConfidence(), &t.CommitteeNoConfidence},
			{"drep update_to_constitution", d.GetUpdateToConstitution(), &t.UpdateToConstitution},
			{"drep hard_fork_initiation", d.GetHardForkInitiation(), &t.HardForkInitiation},
			{"drep pp_network_group", d.GetPpNetworkGroup(), &t.PpNetworkGroup},
			{"drep pp_economic_group", d.GetPpEconomicGroup(), &t.PpEconomicGroup},
			{"drep pp_technical_group", d.GetPpTechnicalGroup(), &t.PpTechnicalGroup},
			{"drep pp_gov_group", d.GetPpGovGroup(), &t.PpGovGroup},
			{"drep treasury_withdrawal", d.GetTreasuryWithdrawal(), &t.TreasuryWithdrawal},
		} {
			r, err := unitRat(f.name, f.in)
			if err != nil {
				return nil, err
			}
			*f.out = r
		}
	}
	return out, nil
}

// govCommittee holds a decoded committee: the quorum and the seated members.
// The voting view of the members is dingo's own loader's, run by votingState.
type govCommittee struct {
	quorum  *big.Rat
	epoch   uint64
	members []dxoracle.CommitteeMember
}

func decodeCommittee(c *v1.Committee, epoch uint64) (*govCommittee, error) {
	if c == nil {
		return nil, nil
	}
	q, err := ratArg("committee.threshold", c.GetThreshold())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("committee.threshold", q); err != nil {
		return nil, err
	}
	out := &govCommittee{quorum: q, epoch: epoch}
	for _, m := range c.GetMembers() {
		tag, hash, err := govCredential("committee cold", m.GetCold())
		if err != nil {
			return nil, err
		}
		member := dxoracle.CommitteeMember{Tag: tag, Key: hash, Expiry: m.GetExpiry(), Resigned: m.GetResigned()}
		if m.GetHot() != nil {
			htag, hhash, err := govCredential("committee hot", m.GetHot())
			if err != nil {
				return nil, err
			}
			member.Hot = &dxoracle.Credential{Tag: htag, Key: hhash}
		}
		out.members = append(out.members, member)
	}
	return out, nil
}

// votingState is the committee's voting view as dingo's production loader
// reads it from a scratch database seeded with the members.
func (c *govCommittee) votingState(ctx context.Context) (*governance.CommitteeVotingState, error) {
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	return oracle.CommitteeVotingState(ctx, dxoracle.CommitteeVotingInput{Members: c.members, Epoch: c.epoch})
}

// actionParent is the previous action id embedded in a GovAction, nil when
// the action has none or has no chain.
func actionParent(a lcommon.GovAction) *lcommon.GovActionId {
	switch x := a.(type) {
	case *conway.ConwayParameterChangeGovAction:
		return x.ActionId
	case *lcommon.HardForkInitiationGovAction:
		return x.ActionId
	case *lcommon.NoConfidenceGovAction:
		return x.ActionId
	case *lcommon.UpdateCommitteeGovAction:
		return x.ActionId
	case *lcommon.NewConstitutionGovAction:
		return x.ActionId
	}
	return nil
}
