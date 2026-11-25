package corpusgen

import (
	"bytes"
	"encoding/hex"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateGovCorpus(t *testing.T) {
	writeCorpus(t, "", govCases())
	writeCorpus(t, "divergences", govDivergenceCases())
}

// govDivergenceCases holds confirmed mismatches; cmd/dxnode-go/divergence_gov_test.go
// and the divergence checks in Test/Gov.hs call the libraries directly.
func govDivergenceCases() []entry {
	return []entry{
		{"gov_thresholds_pc_empty_update", "an update touching no group: the reference threshold is zero, dingo falls back to the governance group; the ledger rejects such an update as malformed",
			&v1.Call{Request: &v1.Call_GovThresholds{GovThresholds: &v1.GovThresholdsRequest{
				ProtocolVersion: gpv(10), Pparams: govParams(10, 3), Committee: govCommittee(false), Action: govHex("8400f6a0f6"), Epoch: 50,
			}}}},
	}
}

func govHex(s string) []byte {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return b
}

// GovAction CBOR. Tags: 0 parameter change, 1 hard fork, 2 treasury
// withdrawals, 3 no confidence, 4 update committee, 5 new constitution, 6 info.
var (
	gaNoConfidence = govHex("8203f6")
	gaInfo         = govHex("8106")
	gaHardFork     = govHex("8301f6820a00")
	gaTreasury     = govHex("8302a0f6")
	gaUpdateCmte   = govHex("8504f6d9010280a0d81e820102")
	gaConstitution = govHex("8305f68282617858" + "20" + hex.EncodeToString(make([]byte, 32)) + "f6")
	// maxBBSize (network and security groups), keyDeposit (economic), nOpt
	// (technical), committeeMaxTermLength (governance).
	gaPCNetworkSecurity = govHex("8400f6a1021a00016000f6")
	gaPCEconomic        = govHex("8400f6a1051a001e8480f6")
	gaPCTechnical       = govHex("8400f6a1081901f4f6")
	gaPCGov             = govHex("8400f6a1181c0af6")
)

func gid(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

func gcred(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: gid(b)}
}

func gpv(major uint32) *v1.ProtocolVersion { return &v1.ProtocolVersion{Major: major} }

func govParams(major uint32, minSize uint32) *v1.ProtocolParameters {
	return &v1.ProtocolParameters{
		ProtocolVersion: gpv(major),
		PoolVotingThresholds: &v1.PoolVotingThresholds{
			MotionNoConfidence: rat("51", "100"), CommitteeNormal: rat("13", "25"),
			CommitteeNoConfidence: rat("53", "100"), HardForkInitiation: rat("27", "50"),
			PpSecurityGroup: rat("11", "20"),
		},
		DrepVotingThresholds: &v1.DRepVotingThresholds{
			MotionNoConfidence: rat("61", "100"), CommitteeNormal: rat("31", "50"),
			CommitteeNoConfidence: rat("63", "100"), UpdateToConstitution: rat("16", "25"),
			HardForkInitiation: rat("13", "20"), PpNetworkGroup: rat("33", "50"),
			PpEconomicGroup: rat("67", "100"), PpTechnicalGroup: rat("17", "25"),
			PpGovGroup: rat("69", "100"), TreasuryWithdrawal: rat("7", "10"),
		},
		CommitteeMinSize: &minSize,
	}
}

// govCommittee has three active members (quorum 2/3) at epoch 50; with extra
// it also holds a resigned, an expired and an unauthorized member.
func govCommittee(extra bool) *v1.Committee {
	c := &v1.Committee{Threshold: rat("2", "3")}
	for i := byte(1); i <= 3; i++ {
		c.Members = append(c.Members, &v1.CommitteeMember{Cold: gcred(0x10 + i), Hot: gcred(0x20 + i), Expiry: 100})
	}
	if extra {
		c.Members = append(c.Members,
			&v1.CommitteeMember{Cold: gcred(0x14), Hot: gcred(0x24), Expiry: 100, Resigned: true},
			&v1.CommitteeMember{Cold: gcred(0x15), Hot: gcred(0x25), Expiry: 10},
			&v1.CommitteeMember{Cold: gcred(0x16), Expiry: 100})
	}
	return c
}

func gaid(b byte) *v1.GovActionId { return &v1.GovActionId{TxId: bytes.Repeat([]byte{b}, 32)} }

func poolStake(b byte, stake uint64) *v1.PoolStake {
	return &v1.PoolStake{PoolId: gid(b), Stake: stake}
}

func govVote(role v1.VoterRole, who byte, v v1.Vote, id *v1.GovActionId) *v1.VoteRecord {
	voter := &v1.Voter{Role: role}
	if role == v1.VoterRole_VOTER_ROLE_STAKE_POOL {
		voter.PoolId = gid(who)
	} else {
		voter.Credential = gcred(who)
	}
	return &v1.VoteRecord{ActionId: id, Voter: voter, Vote: v}
}

func govCases() []entry {
	thr := func(name, why string, action []byte, major uint32, c *v1.Committee) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovThresholds{GovThresholds: &v1.GovThresholdsRequest{
			ProtocolVersion: gpv(major), Pparams: govParams(major, 3), Committee: c, Action: action, Epoch: 50,
		}}}}
	}
	two := govCommittee(false)
	two.Members = two.Members[:2]
	cases := []entry{
		thr("gov_thresholds_no_confidence", "SPO and DRep vote on a motion; the committee does not", gaNoConfidence, 10, govCommittee(false)),
		thr("gov_thresholds_info", "an info action has no threshold for any role", gaInfo, 10, govCommittee(false)),
		thr("gov_thresholds_hard_fork", "all three roles vote on a hard fork", gaHardFork, 10, govCommittee(false)),
		thr("gov_thresholds_treasury", "stake pools do not vote on treasury withdrawals", gaTreasury, 10, govCommittee(false)),
		thr("gov_thresholds_new_constitution", "stake pools do not vote on a new constitution", gaConstitution, 10, govCommittee(false)),
		thr("gov_thresholds_update_committee_present", "committee-normal thresholds when a committee exists", gaUpdateCmte, 10, govCommittee(false)),
		thr("gov_thresholds_update_committee_absent", "committee-no-confidence thresholds without a committee", gaUpdateCmte, 10, nil),
		thr("gov_thresholds_pc_network_security", "maxBBSize is network and security: SPOs vote, DRep uses the network group", gaPCNetworkSecurity, 10, govCommittee(false)),
		thr("gov_thresholds_pc_economic", "economic group only: no SPO vote", gaPCEconomic, 10, govCommittee(false)),
		thr("gov_thresholds_pc_technical", "technical group only", gaPCTechnical, 10, govCommittee(false)),
		thr("gov_thresholds_pc_governance", "governance group only", gaPCGov, 10, govCommittee(false)),
		thr("gov_thresholds_no_committee", "without a committee the committee cannot approve", gaHardFork, 10, nil),
		thr("gov_thresholds_committee_too_small", "two active members under a minimum of three", gaHardFork, 10, two),
		thr("gov_thresholds_inactive_members", "resigned, expired and unauthorized members do not count toward the minimum", gaHardFork, 10, govCommittee(true)),
		thr("gov_thresholds_bootstrap_hard_fork", "protocol 9: DRep threshold is zero", gaHardFork, 9, govCommittee(false)),
		thr("gov_thresholds_bootstrap_small_committee", "protocol 9 waives the minimum committee size", gaHardFork, 9, two),
	}

	spo := func(name, why string, major uint32, action []byte, p4 *v1.DRep) entry {
		id := gaid(7)
		req := &v1.GovSpoRatioRequest{
			ProtocolVersion: gpv(major), ActionId: id, Action: action,
			PoolDistribution: []*v1.PoolStake{poolStake(1, 10), poolStake(2, 20), poolStake(3, 30), poolStake(4, 40)},
			Votes: []*v1.VoteRecord{
				govVote(v1.VoterRole_VOTER_ROLE_STAKE_POOL, 1, v1.Vote_VOTE_YES, id),
				govVote(v1.VoterRole_VOTER_ROLE_STAKE_POOL, 2, v1.Vote_VOTE_NO, id),
				govVote(v1.VoterRole_VOTER_ROLE_STAKE_POOL, 3, v1.Vote_VOTE_ABSTAIN, id),
				govVote(v1.VoterRole_VOTER_ROLE_STAKE_POOL, 4, v1.Vote_VOTE_YES, gaid(8)),
			},
		}
		if p4 != nil {
			req.Pools = []*v1.Pool{{PoolId: gid(4), RewardAccount: gcred(0x44)}}
			req.Accounts = []*v1.Account{{Credential: gcred(0x44), Drep: p4}}
		}
		return entry{name, why, &v1.Call{Request: &v1.Call_GovSpoRatio{GovSpoRatio: req}}}
	}
	abstain := &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	noConf := &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
	cases = append(cases,
		spo("gov_spo_ratio_silent_pool_is_no", "protocol 10: the silent pool counts against", 10, gaPCNetworkSecurity, nil),
		spo("gov_spo_ratio_bootstrap_silent_abstains", "protocol 9: the silent pool abstains", 9, gaPCNetworkSecurity, nil),
		spo("gov_spo_ratio_hard_fork_silent_is_no", "hard fork: a silent pool is no even when its reward account abstains", 10, gaHardFork, abstain),
		spo("gov_spo_ratio_bootstrap_hard_fork", "protocol 9 hard fork: a silent pool stays no", 9, gaHardFork, nil),
		spo("gov_spo_ratio_no_confidence_default_yes", "reward account delegated to always-no-confidence votes yes on a motion", 10, gaNoConfidence, noConf),
		spo("gov_spo_ratio_no_confidence_default_no", "always-no-confidence is a no vote on other actions", 10, gaPCNetworkSecurity, noConf),
		spo("gov_spo_ratio_always_abstain_default", "reward account delegated to always-abstain abstains", 10, gaPCNetworkSecurity, abstain),
		spo("gov_spo_ratio_bootstrap_ignores_default", "protocol 9: delegation defaults are not consulted", 9, gaNoConfidence, noConf),
		entry{"gov_spo_ratio_empty_distribution", "no pool stake is outside the reference types", &v1.Call{Request: &v1.Call_GovSpoRatio{GovSpoRatio: &v1.GovSpoRatioRequest{
			ProtocolVersion: gpv(10), ActionId: gaid(7), Action: gaPCNetworkSecurity,
		}}}},
	)

	drep := func(name, why string, epoch uint64, action []byte) entry {
		id := gaid(7)
		kd := func(b byte) *v1.DRep { return &v1.DRep{Target: &v1.DRep_Credential{Credential: gcred(b)}} }
		ds := func(d *v1.DRep, s uint64) *v1.DRepStake { return &v1.DRepStake{Drep: d, Stake: s} }
		req := &v1.GovDRepRatioRequest{
			Epoch: epoch, ActionId: id, Action: action,
			DrepDistribution: []*v1.DRepStake{
				ds(kd(1), 10), ds(kd(2), 20), ds(kd(3), 30), ds(kd(4), 40), ds(kd(5), 50), ds(kd(6), 60), ds(abstain, 7), ds(noConf, 9),
			},
			Dreps: []*v1.DRepRecord{
				{Credential: gcred(1), Expiry: 100}, {Credential: gcred(2), Expiry: 100}, {Credential: gcred(3), Expiry: 100},
				{Credential: gcred(4), Expiry: 100}, {Credential: gcred(5), Expiry: 40},
			},
			Votes: []*v1.VoteRecord{
				govVote(v1.VoterRole_VOTER_ROLE_DREP, 1, v1.Vote_VOTE_YES, id),
				govVote(v1.VoterRole_VOTER_ROLE_DREP, 2, v1.Vote_VOTE_NO, id),
				govVote(v1.VoterRole_VOTER_ROLE_DREP, 3, v1.Vote_VOTE_ABSTAIN, id),
				govVote(v1.VoterRole_VOTER_ROLE_DREP, 4, v1.Vote_VOTE_YES, gaid(8)),
			},
		}
		return entry{name, why, &v1.Call{Request: &v1.Call_GovDrepRatio{GovDrepRatio: req}}}
	}
	cases = append(cases,
		drep("gov_drep_ratio_parameter_change", "silent DRep counts no; abstainers, expired and unregistered DReps drop out; always-no-confidence counts no", 50, gaPCNetworkSecurity),
		drep("gov_drep_ratio_no_confidence", "always-no-confidence counts yes on a motion of no confidence", 50, gaNoConfidence),
		drep("gov_drep_ratio_expiry_boundary", "a DRep is active through its expiry epoch", 40, gaPCNetworkSecurity),
		drep("gov_drep_ratio_after_expiry", "DRep 5 expires at epoch 40", 41, gaPCNetworkSecurity),
	)

	cc := func(name, why string, c *v1.Committee, votes ...*v1.VoteRecord) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovCommitteeRatio{GovCommitteeRatio: &v1.GovCommitteeRatioRequest{
			Epoch: 50, ProtocolVersion: gpv(10), Pparams: govParams(10, 3), Committee: c, ActionId: gaid(7), Votes: votes,
		}}}}
	}
	ccv := func(who byte, v v1.Vote, id *v1.GovActionId) *v1.VoteRecord {
		return govVote(v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, who, v, id)
	}
	cases = append(cases,
		cc("gov_committee_ratio_yes_no_silent", "a silent member counts no", govCommittee(true),
			ccv(0x21, v1.Vote_VOTE_YES, gaid(7)), ccv(0x22, v1.Vote_VOTE_NO, gaid(7))),
		cc("gov_committee_ratio_abstain", "an abstaining member leaves the denominator", govCommittee(true),
			ccv(0x21, v1.Vote_VOTE_YES, gaid(7)), ccv(0x22, v1.Vote_VOTE_ABSTAIN, gaid(7))),
		cc("gov_committee_ratio_inactive_hot_keys", "resigned, expired and unauthorized members cannot vote", govCommittee(true),
			ccv(0x24, v1.Vote_VOTE_YES, gaid(7)), ccv(0x25, v1.Vote_VOTE_YES, gaid(7)), ccv(0x21, v1.Vote_VOTE_YES, gaid(7))),
		cc("gov_committee_ratio_no_committee", "no committee: ratio zero", nil),
	)

	q := func(name, why string, major uint32, c *v1.Committee) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovCommitteeQuorum{GovCommitteeQuorum: &v1.GovCommitteeQuorumRequest{
			Epoch: 50, ProtocolVersion: gpv(major), Pparams: govParams(major, 3), Committee: c,
		}}}}
	}
	cases = append(cases,
		q("gov_committee_quorum_in_force", "three active members meet the minimum", 10, govCommittee(true)),
		q("gov_committee_quorum_no_committee", "no committee: cannot approve", 10, nil),
		q("gov_committee_quorum_too_small", "two active members under a minimum of three", 10, two),
		q("gov_committee_quorum_bootstrap", "protocol 9 waives the minimum", 9, two),
		q("gov_committee_quorum_zero_threshold", "a zero quorum in bootstrap with no members", 9, &v1.Committee{Threshold: rat("0", "1")}),
	)
	return append(cases, govTreeCases()...)
}

func gtx(b byte) []byte { return bytes.Repeat([]byte{b}, 32) }

// gaPC and gaHF are a parameter change and a hard fork whose previous action
// id is (parent, 0), or none for parent 0.
func gaPC(parent byte) []byte {
	prefix := "8400f6"
	if parent != 0 {
		prefix = "840082" + "5820" + hex.EncodeToString(gtx(parent)) + "00"
	}
	return govHex(prefix + "a1021a00016000f6")
}

func gaHF(parent byte) []byte {
	prefix := "8301f6"
	if parent != 0 {
		prefix = "830182" + "5820" + hex.EncodeToString(gtx(parent)) + "00"
	}
	return govHex(prefix + "820a00")
}

func gprop(id byte, action []byte) *v1.Proposal {
	return &v1.Proposal{
		Id: &v1.GovActionId{TxId: gtx(id)}, Action: action,
		ProposedEpoch: 1, ExpiresEpoch: 10, Deposit: 1000, ReturnAccount: gcred(0x50),
	}
}

func gids(ids ...byte) []*v1.GovActionId {
	var out []*v1.GovActionId
	for _, b := range ids {
		out = append(out, &v1.GovActionId{TxId: gtx(b)})
	}
	return out
}

// govTreeCases: 1 and 2 compete for the parameter root; 3 follows 1 and 4
// follows 2; 5 is a hard fork; 6 is a treasury withdrawal; 7 is an info action.
func govTreeCases() []entry {
	props := func() []*v1.Proposal {
		return []*v1.Proposal{
			gprop(1, gaPC(0)), gprop(2, gaPC(0)), gprop(3, gaPC(1)), gprop(4, gaPC(2)),
			gprop(5, gaHF(0)), gprop(6, govHex("8302a0f6")), gprop(7, govHex("8106")),
		}
	}
	tree := func(name, why string, enacted, expired []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovProposalTree{GovProposalTree: &v1.GovProposalTreeRequest{
			Proposals: props(), Enacted: gids(enacted...), Expired: gids(expired...),
		}}}}
	}
	return []entry{
		tree("gov_proposal_tree_enacted", "enacting 1 removes its competitor 2 and 2's child 4", []byte{1}, nil),
		tree("gov_proposal_tree_expired", "expiring 1 removes it and its child 3", nil, []byte{1}),
		tree("gov_proposal_tree_chain", "1 then its child 3 enacted in one round", []byte{1, 3}, nil),
		tree("gov_proposal_tree_enacted_and_expired", "an unrelated hard fork expires while 1 is enacted", []byte{1}, []byte{5}),
		tree("gov_proposal_tree_treasury", "a treasury withdrawal has no purpose chain", []byte{6}, nil),
		tree("gov_proposal_tree_idle", "nothing enacted or expired", nil, nil),
		{"gov_proposal_tree_enacted_root", "proposals following an enacted root: enacting 2 removes competitor 1 and its child 3", &v1.Call{Request: &v1.Call_GovProposalTree{GovProposalTree: &v1.GovProposalTreeRequest{
			Proposals:  []*v1.Proposal{gprop(1, gaPC(9)), gprop(2, gaPC(9)), gprop(3, gaPC(1))},
			EnactState: &v1.EnactState{PrevPparamUpdate: &v1.GovActionId{TxId: gtx(9)}},
			Enacted:    gids(2),
		}}}},
		{"gov_proposal_tree_orphan_parent", "a parent that is neither the root nor a proposal cannot form a forest", &v1.Call{Request: &v1.Call_GovProposalTree{GovProposalTree: &v1.GovProposalTreeRequest{
			Proposals: []*v1.Proposal{gprop(1, gaPC(9))},
		}}}},
	}
}
