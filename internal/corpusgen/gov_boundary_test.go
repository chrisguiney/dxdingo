package corpusgen

import (
	"bytes"
	"encoding/hex"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// gaWithdraw is a TreasuryWithdrawals action paying each (key, amount) to the
// key-hash reward account made of key repeated.
func gaWithdraw(pairs ...[2]uint64) []byte {
	out := []byte{0x83, 0x02, 0xa0 + byte(len(pairs))}
	for _, p := range pairs {
		out = append(out, 0x58, 0x1d, 0xe0)
		out = append(out, bytes.Repeat([]byte{byte(p[0])}, 28)...)
		out = append(out, 0x1b)
		for s := 56; s >= 0; s -= 8 {
			out = append(out, byte(p[1]>>s))
		}
	}
	return append(out, 0xf6)
}

// gaUpdateCommittee removes member 0x11, adds member 0x77 expiring at expiry
// (below 256) and sets the quorum to 3/4.
func gaUpdateCommittee(expiry byte) []byte {
	out := govHex("8504f6d90102818200581c" + hex.EncodeToString(gid(0x11)))
	out = append(out, govHex("a18200581c"+hex.EncodeToString(gid(0x77)))...)
	if expiry >= 24 {
		out = append(out, 0x18)
	}
	return append(out, expiry, 0xd8, 0x1e, 0x82, 0x03, 0x04)
}

func gdrepKey(b byte) *v1.DRep { return &v1.DRep{Target: &v1.DRep_Credential{Credential: gcred(b)}} }

func gvote(id byte, role v1.VoterRole, who byte, v v1.Vote) *v1.VoteRecord {
	return govVote(role, who, v, gaid(id))
}

// gall is a vote of every role on action id: DRep d1, pool b1 and the three
// committee members.
func gall(id byte, v v1.Vote) []*v1.VoteRecord {
	out := []*v1.VoteRecord{
		gvote(id, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, v),
		gvote(id, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, v),
	}
	for i := byte(1); i <= 3; i++ {
		out = append(out, gvote(id, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x20+i, v))
	}
	return out
}

func gratifyProp(id byte, action []byte, expires uint64) *v1.Proposal {
	p := gprop(id, action)
	p.ExpiresEpoch = expires
	return p
}

// gratifyReq is a RATIFY request at epoch with one DRep (d1, expiry 100, all
// the DRep stake), one pool (b1, all the pool stake) and the three-member
// committee.
func gratifyReq(epoch, treasury uint64, major uint32, props []*v1.Proposal, votes ...[]*v1.VoteRecord) *v1.GovRatifyRequest {
	var all []*v1.VoteRecord
	for _, v := range votes {
		all = append(all, v...)
	}
	pp := govParams(major, 3)
	term := uint64(10)
	pp.CommitteeMaxTermLength = &term
	return &v1.GovRatifyRequest{
		Context: &v1.LedgerContext{Epoch: epoch, ProtocolVersion: gpv(major)},
		Pparams: pp,
		State: &v1.LedgerState{
			Dreps:     []*v1.DRepRecord{{Credential: gcred(0xd1), Expiry: 100}},
			Committee: govCommittee(false),
			Proposals: props,
			Votes:     all,
			Pots:      &v1.Pots{Treasury: treasury},
		},
		PoolDistribution: []*v1.PoolStake{poolStake(0xb1, 100)},
		DrepDistribution: []*v1.DRepStake{{Drep: gdrepKey(0xd1), Stake: 100}},
	}
}

func gratifyCase(name, why string, req *v1.GovRatifyRequest) entry {
	return entry{name, why, &v1.Call{Request: &v1.Call_GovRatify{GovRatify: req}}}
}

func govRatifyCases() []entry {
	yes, no := v1.Vote_VOTE_YES, v1.Vote_VOTE_NO
	drepYes := gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, yes)
	poolYes := gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes)

	// Two DReps and two pools split the stake 61/39 and 51/49.
	split := func(req *v1.GovRatifyRequest, drepYes, drepNo, poolYes, poolNo uint64) *v1.GovRatifyRequest {
		req.State.Dreps = []*v1.DRepRecord{{Credential: gcred(0xd1), Expiry: 100}, {Credential: gcred(0xd2), Expiry: 100}}
		req.DrepDistribution = []*v1.DRepStake{{Drep: gdrepKey(0xd1), Stake: drepYes}, {Drep: gdrepKey(0xd2), Stake: drepNo}}
		req.PoolDistribution = []*v1.PoolStake{poolStake(0xb1, poolYes), poolStake(0xb2, poolNo)}
		return req
	}
	splitVotes := []*v1.VoteRecord{
		drepYes, gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd2, no),
		poolYes, gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb2, no),
	}

	expiredDRep := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, []*v1.VoteRecord{
		gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, no), gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd2, yes), poolYes,
	})
	expiredDRep.State.Dreps = []*v1.DRepRecord{{Credential: gcred(0xd1), Expiry: 100}, {Credential: gcred(0xd2), Expiry: 4}}
	expiredDRep.DrepDistribution = []*v1.DRepStake{{Drep: gdrepKey(0xd1), Stake: 40}, {Drep: gdrepKey(0xd2), Stake: 60}}

	defaultAbstain := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, []*v1.VoteRecord{drepYes, poolYes})
	defaultAbstain.PoolDistribution = []*v1.PoolStake{poolStake(0xb1, 40), poolStake(0xb2, 60)}
	defaultAbstain.State.Pools = []*v1.Pool{{PoolId: gid(0xb2), RewardAccount: gcred(0x44)}}
	defaultAbstain.State.Accounts = []*v1.Account{{Credential: gcred(0x44), Drep: &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}}}

	rooted := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaPC(9), 9)}, gall(1, yes))
	rooted.State.EnactState = &v1.EnactState{PrevPparamUpdate: &v1.GovActionId{TxId: gtx(9)}}

	bootstrap := gratifyReq(5, 0, 9, []*v1.Proposal{gratifyProp(1, gaHF(0), 9)}, []*v1.VoteRecord{
		poolYes, gvote(1, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x21, yes),
		gvote(1, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x22, yes), gvote(1, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x23, yes),
	})

	noCommittee := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaHF(0), 9)}, []*v1.VoteRecord{drepYes, poolYes})
	noCommittee.State.Committee = nil

	afterNoConfidence := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, govHex("850482"+"5820"+hex.EncodeToString(gtx(8))+"00"+"d9010280"+"a0"+"d81e820304"), 9)}, []*v1.VoteRecord{drepYes, poolYes})
	afterNoConfidence.State.Committee = nil
	afterNoConfidence.State.EnactState = &v1.EnactState{PrevCommittee: gaid(8)}

	scriptDRep := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, []*v1.VoteRecord{{
		ActionId: gaid(1), Vote: v1.Vote_VOTE_YES,
		Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_DREP, Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: gid(0xd1)}},
	}, poolYes})
	scriptCred := &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: gid(0xd1)}
	scriptDRep.State.Dreps = []*v1.DRepRecord{{Credential: scriptCred, Expiry: 100}, {Credential: gcred(0xd1), Expiry: 100}}
	scriptDRep.DrepDistribution = []*v1.DRepStake{{Drep: &v1.DRep{Target: &v1.DRep_Credential{Credential: scriptCred}}, Stake: 70}, {Drep: gdrepKey(0xd1), Stake: 30}}

	ratify := func(name, why string, req *v1.GovRatifyRequest) entry { return gratifyCase(name, why, req) }
	return []entry{
		ratify("gov_ratify_script_drep", "a script DRep and a key DRep with one hash are different DReps: only the script one voted", scriptDRep),
		ratify("gov_ratify_update_committee_after_no_confidence", "no committee after a no-confidence root: update committee uses the no-confidence thresholds", afterNoConfidence),
		ratify("gov_ratify_no_confidence_accepted", "no confidence needs only DReps and pools; both vote yes with all the stake",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, []*v1.VoteRecord{drepYes, poolYes})),
		ratify("gov_ratify_silent_pool", "no pool vote leaves the pool ratio at zero",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, []*v1.VoteRecord{drepYes})),
		ratify("gov_ratify_delaying_blocks", "accepting a hard fork delays the parameter change behind it",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaHF(0), 9), gratifyProp(2, gaPC(0), 9)}, gall(1, yes), gall(2, yes))),
		ratify("gov_ratify_committee_below_quorum", "one yes of three members is 1/3, below the 2/3 quorum",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaHF(0), 9)}, []*v1.VoteRecord{drepYes, poolYes,
				gvote(1, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x21, yes)})),
		ratify("gov_ratify_expired", "an unaccepted action past its expiry expires; one expiring at the epoch does not",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaInfo, 4), gratifyProp(2, gaInfo, 5)})),
		ratify("gov_ratify_last_chance", "an action past its expiry that is accepted at the same boundary is enacted, not expired",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaHF(0), 4)}, gall(1, yes))),
		ratify("gov_ratify_parameter_chain", "non-delaying parameter changes chain within one pass",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaPC(0), 9), gratifyProp(2, gaPC(1), 9)}, gall(1, yes), gall(2, yes))),
		ratify("gov_ratify_treasury_budget", "1500 in the treasury pays one of two 1000 withdrawals",
			gratifyReq(5, 1500, 10, []*v1.Proposal{
				gratifyProp(1, gaWithdraw([2]uint64{0x61, 1000}), 9), gratifyProp(2, gaWithdraw([2]uint64{0x62, 1000}), 9),
			}, gall(1, yes), gall(2, yes))),
		ratify("gov_ratify_treasury_within_budget", "a withdrawal that fits the treasury exactly is accepted",
			gratifyReq(5, 1000, 10, []*v1.Proposal{gratifyProp(1, gaWithdraw([2]uint64{0x61, 1000}), 9)}, gall(1, yes))),
		ratify("gov_ratify_expired_drep", "a DRep past its expiry does not count: its yes stake is ignored", expiredDRep),
		ratify("gov_ratify_drep_threshold_exact", "61/100 yes is exactly the no-confidence threshold: accepted", split(
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, splitVotes), 61, 39, 100, 0)),
		ratify("gov_ratify_drep_threshold_below", "60/100 yes is below the threshold", split(
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, splitVotes), 60, 40, 100, 0)),
		ratify("gov_ratify_pool_threshold_exact", "51/100 of pool stake is exactly the threshold: accepted", split(
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, splitVotes), 100, 0, 51, 49)),
		ratify("gov_ratify_pool_threshold_below", "50/100 of pool stake is below the threshold", split(
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 9)}, splitVotes), 100, 0, 50, 50)),
		ratify("gov_ratify_pool_default_abstain", "a silent pool whose reward account is always-abstain leaves the denominator", defaultAbstain),
		ratify("gov_ratify_bootstrap_hard_fork", "protocol 9: the DRep threshold is zero and a silent DRep does not block", bootstrap),
		ratify("gov_ratify_no_committee", "without a committee a hard fork cannot be accepted", noCommittee),
		ratify("gov_ratify_enacted_root", "a parameter change following the enacted root is accepted", rooted),
		ratify("gov_ratify_committee_term_within", "a new member expiring within the term limit (epoch 5 + 10)",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaUpdateCommittee(15), 9)}, []*v1.VoteRecord{drepYes, poolYes})),
		ratify("gov_ratify_committee_term_beyond", "a new member expiring beyond the term limit blocks the update",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaUpdateCommittee(16), 9)}, []*v1.VoteRecord{drepYes, poolYes})),
		ratify("gov_ratify_orphan_parent", "a parent that is neither the root nor a proposal cannot form a forest",
			gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaPC(9), 9)})),
	}
}

// genactReq enacts action 1 with a ledger holding the state.
func genactReq(action []byte, st *v1.LedgerState) *v1.GovEnactRequest {
	pp := govParams(10, 3)
	u64 := func(x uint64) *uint64 { return &x }
	u32 := func(x uint32) *uint32 { return &x }
	pp.MinFeeA, pp.MinFeeB = u64(44), u64(155381)
	pp.MaxBlockBodySize, pp.MaxTxSize, pp.MaxBlockHeaderSize = u32(65536), u32(16384), u32(1100)
	pp.KeyDeposit, pp.PoolDeposit, pp.MaxEpoch, pp.NOpt = u64(2000000), u64(500000000), u64(18), u32(500)
	pp.A0, pp.Rho, pp.Tau = rat("3", "10"), rat("3", "1000"), rat("1", "5")
	pp.MinPoolCost, pp.CoinsPerUtxoByte = u64(170000000), u64(4310)
	pp.PriceMem, pp.PriceSteps = rat("577", "10000"), rat("721", "10000000")
	pp.MaxTxExMem, pp.MaxTxExSteps, pp.MaxBlockExMem, pp.MaxBlockExSteps = u64(14000000), u64(10000000000), u64(62000000), u64(20000000000)
	pp.MaxValueSize, pp.CollateralPercentage, pp.MaxCollateralInputs = u32(5000), u32(150), u32(3)
	pp.CommitteeMaxTermLength, pp.GovActionLifetime, pp.GovActionDeposit = u64(146), u64(6), u64(100000000000)
	pp.DrepDeposit, pp.DrepActivity = u64(500000000), u64(20)
	pp.MinFeeRefScriptCostPerByte = rat("15", "1")
	if st == nil {
		st = &v1.LedgerState{}
	}
	return &v1.GovEnactRequest{Pparams: pp, State: st, ActionId: gaid(1), Action: action}
}

func govEnactCases() []entry {
	enact := func(name, why string, action []byte, st *v1.LedgerState) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovEnact{GovEnact: genactReq(action, st)}}}
	}
	withDeposit := func(registered bool) *v1.LedgerState {
		p := gprop(1, gaInfo)
		p.Deposit, p.ReturnAccount = 100, gcred(0x61)
		st := &v1.LedgerState{Proposals: []*v1.Proposal{p}}
		if registered {
			st.Accounts = []*v1.Account{{Credential: gcred(0x61), Balance: 7}}
		}
		return st
	}
	wdState := &v1.LedgerState{
		Pots:     &v1.Pots{Treasury: 2000},
		Accounts: []*v1.Account{{Credential: gcred(0x61), Balance: 7}, {Credential: gcred(0x63), Balance: 0}},
	}
	roots := &v1.LedgerState{
		Committee: govCommittee(false),
		EnactState: &v1.EnactState{
			PrevHardFork: gaid(9), PrevCommittee: gaid(8),
			Constitution: &v1.Constitution{AnchorUrl: "https://c", AnchorHash: bytes.Repeat([]byte{3}, 32)},
		},
	}
	script := &v1.LedgerState{EnactState: &v1.EnactState{
		Constitution: &v1.Constitution{AnchorUrl: "https://c", AnchorHash: bytes.Repeat([]byte{3}, 32), GuardrailScriptHash: gid(0x5c)},
	}}
	return []entry{
		enact("gov_enact_parameter_change", "the update sets the maximum block body size and nothing else", gaPC(0), nil),
		enact("gov_enact_hard_fork", "the protocol version becomes 10.0", gaHF(0), nil),
		enact("gov_enact_treasury_withdrawal", "500 reaches the registered account, 300 to an unregistered one stays in the treasury",
			gaWithdraw([2]uint64{0x61, 500}, [2]uint64{0x62, 300}), wdState),
		enact("gov_enact_treasury_two_accounts", "two registered accounts are paid", gaWithdraw([2]uint64{0x61, 500}, [2]uint64{0x63, 300}), wdState),
		enact("gov_enact_treasury_unregistered", "no registered destination: the treasury is unchanged", gaWithdraw([2]uint64{0x62, 300}), wdState),
		enact("gov_enact_treasury_all", "withdrawing the whole treasury", gaWithdraw([2]uint64{0x61, 2000}), wdState),
		enact("gov_enact_deposit_registered", "the proposal deposit returns to its registered account", gaInfo, withDeposit(true)),
		enact("gov_enact_deposit_unregistered", "an unregistered return account leaves the deposit in the treasury", gaInfo, withDeposit(false)),
		enact("gov_enact_no_confidence", "no confidence leaves no committee",
			gaNoConfidence, &v1.LedgerState{Committee: govCommittee(false)}),
		enact("gov_enact_update_committee", "member 11 leaves, 77 joins, quorum 3/4", gaUpdateCommittee(100),
			&v1.LedgerState{Committee: govCommittee(false)}),
		enact("gov_enact_new_constitution", "the constitution becomes the action's", gaConstitution, nil),
		enact("gov_enact_keeps_roots", "enacting a parameter change keeps the other purposes' roots and the constitution", gaPC(0), roots),
		enact("gov_enact_constitution_script", "a constitution with a guardrail script is replaced by one without", gaConstitution, script),
		enact("gov_enact_chained_parameter_change", "a parameter change following the enacted root",
			gaPC(9), &v1.LedgerState{EnactState: &v1.EnactState{PrevPparamUpdate: &v1.GovActionId{TxId: gtx(9)}}}),
		enact("gov_enact_info", "an info action changes nothing", gaInfo, nil),
		enact("gov_enact_withdrawal_and_refund_same_account", "the withdrawal and the deposit refund both reach one registered account", gaWithdraw([2]uint64{0x61, 500}),
			func() *v1.LedgerState {
				st := withDeposit(true)
				st.Pots = &v1.Pots{Treasury: 2000}
				return st
			}()),
		enact("gov_enact_orphan_parent", "a parent that is not the root of its purpose", gaPC(9), nil),
		enact("gov_enact_withdrawal_above_treasury", "RATIFY never lets a withdrawal above the treasury through",
			gaWithdraw([2]uint64{0x61, 3000}), wdState),
	}
}

func govProposalExpiryCases() []entry {
	mk := func(id byte, action []byte, expires, deposit uint64, ret byte) *v1.Proposal {
		p := gprop(id, action)
		p.ExpiresEpoch, p.Deposit, p.ReturnAccount = expires, deposit, gcred(ret)
		return p
	}
	acct := func(b byte, balance uint64) *v1.Account { return &v1.Account{Credential: gcred(b), Balance: balance} }
	exp := func(name, why string, epoch uint64, props []*v1.Proposal, accts ...*v1.Account) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_GovProposalExpiry{GovProposalExpiry: &v1.GovProposalExpiryRequest{
			Epoch: epoch, Proposals: props, Accounts: accts,
		}}}}
	}
	return []entry{
		exp("gov_proposal_expiry_mixed", "1 expires and takes its child 2 along; 3 expires alone; 4 expires at the epoch and stays", 5, []*v1.Proposal{
			mk(1, gaPC(0), 3, 1000, 0xa1), mk(2, gaPC(1), 20, 500, 0xa2), mk(3, gaInfo, 4, 300, 0xa3), mk(4, gaInfo, 5, 200, 0xa1),
		}, acct(0xa1, 10)),
		exp("gov_proposal_expiry_boundary", "an expiry equal to the epoch is alive; one below it is expired", 5, []*v1.Proposal{
			mk(1, gaInfo, 5, 100, 0xa1), mk(2, gaInfo, 4, 100, 0xa1),
		}, acct(0xa1, 0)),
		exp("gov_proposal_expiry_none", "nothing has expired", 5, []*v1.Proposal{mk(1, gaInfo, 9, 100, 0xa1)}, acct(0xa1, 0)),
		exp("gov_proposal_expiry_shared_account", "two deposits to one registered account add up", 5, []*v1.Proposal{
			mk(1, gaInfo, 1, 100, 0xa1), mk(2, gaInfo, 2, 250, 0xa1),
		}, acct(0xa1, 5)),
		exp("gov_proposal_expiry_unregistered", "no registered return account: every deposit goes to the treasury", 5, []*v1.Proposal{
			mk(1, gaInfo, 1, 100, 0xa1), mk(2, gaHF(0), 2, 250, 0xa2),
		}),
		exp("gov_proposal_expiry_subtree", "a live grandchild of an expired action leaves with it", 5, []*v1.Proposal{
			mk(1, gaPC(0), 3, 100, 0xa1), mk(2, gaPC(1), 20, 200, 0xa1), mk(3, gaPC(2), 30, 400, 0xa1),
		}, acct(0xa1, 0)),
		exp("gov_proposal_expiry_zero_deposit", "a zero deposit refunds nothing", 5, []*v1.Proposal{mk(1, gaInfo, 1, 0, 0xa1)}, acct(0xa1, 3)),
		exp("gov_proposal_expiry_orphan_parent", "a parent that is not a root cannot form a forest", 5, []*v1.Proposal{mk(1, gaPC(9), 3, 100, 0xa1)}),
	}
}

// govDivergenceDormantBoundary is a final-chance ratification at a boundary
// that leaves no live proposal, with the only DRep expired one epoch earlier.
func govDivergenceDormantBoundary() []entry {
	yes := v1.Vote_VOTE_YES
	req := gratifyReq(5, 0, 10, []*v1.Proposal{gratifyProp(1, gaNoConfidence, 4)}, []*v1.VoteRecord{
		gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, yes), gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes),
	})
	req.State.Dreps = []*v1.DRepRecord{{Credential: gcred(0xd1), Expiry: 4}}
	return []entry{gratifyCase("gov_ratify_dormant_boundary",
		"every proposal expires at this boundary and the only DRep expired one epoch earlier: the reference leaves its expiry and counts nothing, dingo extends it before RATIFY and accepts the action",
		req)}
}

// govDivergenceZeroExpiry is a DRep whose expiry is zero.
func govDivergenceZeroExpiry() []entry {
	id := gaid(7)
	return []entry{{"gov_drep_ratio_zero_expiry", "a DRep with expiry 0 at epoch 5: the reference counts it expired, dingo reads zero as unbounded",
		&v1.Call{Request: &v1.Call_GovDrepRatio{GovDrepRatio: &v1.GovDRepRatioRequest{
			Epoch: 5, ActionId: id, Action: gaPCNetworkSecurity,
			DrepDistribution: []*v1.DRepStake{{Drep: gdrepKey(1), Stake: 100}},
			Dreps:            []*v1.DRepRecord{{Credential: gcred(1), Expiry: 0}},
			Votes:            []*v1.VoteRecord{govVote(v1.VoterRole_VOTER_ROLE_DREP, 1, v1.Vote_VOTE_YES, id)},
		}}}}}
}
