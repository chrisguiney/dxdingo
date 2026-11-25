package corpusgen

import (
	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"testing"
)

func TestGenerateEpochNewEpochGovCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochGovCases())
}

func TestGenerateEpochNewEpochGovDivergences(t *testing.T) {
	writeCorpus(t, "divergences", epochNewEpochGovDivergences())
}

// ngBoundary is the boundary into epoch 11 over the pulser snapshot taken at
// the boundary into epoch 10: DRep d1 (all the DRep stake, expiry 100), pool b1
// (all the pool stake), a three member committee with quorum 2/3, treasury
// 5000 and the registered accounts 0x61 (balance 7) and 0x62 (balance 11).
func ngBoundary(name, why string, props []*v1.Proposal, votes ...[]*v1.VoteRecord) entry {
	var all []*v1.VoteRecord
	for _, v := range votes {
		all = append(all, v...)
	}
	return entry{name, why, &v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: ngRequest(props, all)}}}
}

func ngRequest(props []*v1.Proposal, votes []*v1.VoteRecord) *v1.EpochNewEpochRequest {
	return &v1.EpochNewEpochRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
		Pparams: epochConwayPParams(),
		State: &v1.LedgerState{
			Accounts:  []*v1.Account{{Credential: gcred(0x61), Balance: 7}, {Credential: gcred(0x62), Balance: 11}},
			Pots:      &v1.Pots{Reserves: 10_000_000_000, Treasury: 5000},
			Dreps:     []*v1.DRepRecord{{Credential: gcred(0xd1), Deposit: 500, Expiry: 100}},
			Committee: govCommittee(false),
			Proposals: props,
			Votes:     votes,
		},
		PoolDistribution: []*v1.PoolStake{poolStake(0xb1, 100)},
		DrepDistribution: []*v1.DRepStake{{Drep: gdrepKey(0xd1), Stake: 100}},
	}
}

// ngProp is a proposal with an expiry, a deposit and a return account.
func ngProp(id byte, action []byte, expires, deposit uint64, ret byte) *v1.Proposal {
	p := gprop(id, action)
	p.ExpiresEpoch, p.Deposit, p.ReturnAccount = expires, deposit, gcred(ret)
	return p
}

func epochNewEpochGovCases() []entry {
	yes := v1.Vote_VOTE_YES
	drepPool := []*v1.VoteRecord{
		gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, yes), gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes),
	}
	stay := ngProp(9, gaInfo, 15, 0, 0x50)

	rejected := ngBoundary("epoch_new_epoch_gov_rejected",
		"the committee is one vote short of 2/3, so the withdrawal is neither enacted nor expired and the boundary changes nothing",
		[]*v1.Proposal{ngProp(1, gaWithdraw([2]uint64{0x61, 1000}), 15, 400, 0x50)}, gall(1, yes)[:3])

	atLastChance := ngBoundary("epoch_new_epoch_gov_accepted_at_expiry",
		"a withdrawal that expired at epoch 10 but was accepted at the same RATIFY is enacted, not expired",
		[]*v1.Proposal{ngProp(1, gaWithdraw([2]uint64{0x61, 1000}), 9, 400, 0x62), stay}, gall(1, yes))

	overBudget := ngBoundary("epoch_new_epoch_gov_withdrawals_share_budget",
		"two withdrawals of 3000 against a treasury of 5000: the first is accepted and uses the budget, the second stays",
		[]*v1.Proposal{
			ngProp(1, gaWithdraw([2]uint64{0x61, 3000}), 15, 400, 0x62),
			ngProp(2, gaWithdraw([2]uint64{0x61, 3000}), 15, 400, 0x62), stay,
		}, gall(1, yes), gall(2, yes))

	return []entry{
		ngBoundary("epoch_new_epoch_gov_treasury_withdrawal",
			"a withdrawal accepted by every voter is paid to its registered account; the expired info action refunds its registered return account and the enacted withdrawal's deposit, return account unregistered, goes to the treasury; an active proposal keeps the DReps awake",
			[]*v1.Proposal{
				ngProp(1, gaWithdraw([2]uint64{0x61, 1000}), 15, 400, 0x50), ngProp(2, gaInfo, 9, 300, 0x62), stay,
			}, gall(1, yes)),
		ngBoundary("epoch_new_epoch_gov_parameter_change",
			"an accepted parameter change is enacted and becomes the root of its purpose; its deposit refunds its registered return account",
			[]*v1.Proposal{ngProp(1, gaPC(0), 15, 400, 0x62), stay}, gall(1, yes)),
		ngBoundary("epoch_new_epoch_gov_parameter_change_chain",
			"a parameter change and its child are accepted in one pass and enacted in order; the child is the root after",
			[]*v1.Proposal{ngProp(1, gaPC(0), 15, 400, 0x62), ngProp(2, gaPC(1), 15, 300, 0x61), stay}, gall(1, yes), gall(2, yes)),
		ngBoundary("epoch_new_epoch_gov_info_expired",
			"an unaccepted info action past its expiry is expired and its deposit returns to its registered account; one whose expiry is the previous epoch's last is not",
			[]*v1.Proposal{ngProp(1, gaInfo, 9, 300, 0x62), ngProp(2, gaInfo, 10, 200, 0x61), stay}),
		ngBoundary("epoch_new_epoch_gov_no_confidence",
			"no confidence needs no committee vote and removes the committee, which becomes the committee root; the delaying action stops nothing before it",
			[]*v1.Proposal{ngProp(1, gaNoConfidence, 15, 400, 0x62), stay}, drepPool),
		ngBoundary("epoch_new_epoch_gov_update_committee",
			"an accepted committee update replaces the committee members and quorum, and becomes the committee root",
			[]*v1.Proposal{ngProp(1, gaUpdateCommittee(100), 15, 400, 0x62), stay}, drepPool),
		ngBoundary("epoch_new_epoch_gov_expired_subtree",
			"an expired parameter change leaves with its child, and each deposit goes to its registered return account or, unclaimed, to the treasury",
			[]*v1.Proposal{ngProp(1, gaPC(0), 9, 1000, 0x61), ngProp(2, gaPC(1), 15, 500, 0x50), ngProp(3, gaInfo, 9, 300, 0x51), stay}),
		ngBoundary("epoch_new_epoch_gov_delaying_action",
			"an accepted no confidence delays the withdrawal behind it: only the delaying action is enacted and the withdrawal waits",
			[]*v1.Proposal{
				ngProp(1, gaNoConfidence, 15, 400, 0x62), ngProp(2, gaWithdraw([2]uint64{0x61, 1000}), 15, 300, 0x61), stay,
			}, drepPool, gall(2, yes)),
		withRoot(ngBoundary("epoch_new_epoch_gov_enacted_root_chain",
			"a parameter change that follows the enacted root is accepted and enacted",
			[]*v1.Proposal{ngProp(1, gaPC(8), 15, 400, 0x62), stay}, gall(1, yes))),
		atLastChance, overBudget, rejected,
		{"epoch_new_epoch_gov_hard_fork_unsupported", "a hard fork proposal changes the protocol version, which the boundary here does not model: outside the answered domain",
			&v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: ngRequest([]*v1.Proposal{ngProp(1, gaHF(0), 15, 0, 0x50), stay}, nil)}}},
	}
}

// withRoot gives the request the enact state whose parameter root is action 8.
func withRoot(e entry) entry {
	e.call.GetEpochNewEpoch().State.EnactState = &v1.EnactState{PrevPparamUpdate: &v1.GovActionId{TxId: gtx(8)}}
	return e
}

// epochNewEpochGovDivergences are the boundaries where dingo and the reference
// part. Both are the dormancy rule: the reference counts a dormant epoch and
// applies the count to every DRep expiry when a proposal next arrives
// (updateDormantDRepExpiry), so EPOCH leaves each DRep's expiry alone; dingo
// extends every active DRep's expiry by one at each boundary that leaves no
// active proposal, and does so before RATIFY.
func epochNewEpochGovDivergences() []entry {
	yes := v1.Vote_VOTE_YES
	expiredDRep := ngRequest([]*v1.Proposal{ngProp(1, gaNoConfidence, 9, 400, 0x62)}, []*v1.VoteRecord{
		gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, yes), gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes),
	})
	expiredDRep.State.Dreps = []*v1.DRepRecord{{Credential: gcred(0xd1), Deposit: 500, Expiry: 9}}
	return []entry{
		ngBoundary("epoch_new_epoch_gov_dormant_no_proposals",
			"a boundary with no proposals: the reference leaves the DRep's expiry at 100 and counts a dormant epoch, dingo extends the expiry to 101", nil),
		ngBoundary("epoch_new_epoch_gov_dormant_after_expiry",
			"the only proposal expires and leaves, so no active proposal remains: the reference leaves the DRep's expiry, dingo extends it",
			[]*v1.Proposal{ngProp(1, gaInfo, 9, 300, 0x62)}),
		{"epoch_new_epoch_gov_expired_drep_votes",
			"the only proposal expires at the previous boundary and the only DRep expired one epoch before it: the reference counts nothing and expires the action, dingo extends the DRep's expiry before RATIFY, counts its vote and enacts the action",
			&v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: expiredDRep}}},
	}
}
