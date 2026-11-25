package corpusgen

import (
	"encoding/hex"
	"fmt"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochPParamsCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochPParamsCases())
}

func TestGenerateEpochNewEpochPParamsDivergences(t *testing.T) {
	writeCorpus(t, "divergences", epochNewEpochPParamsDivergences())
}

// gaPCUpdate is a parameter change carrying the CBOR update map update.
func gaPCUpdate(parent byte, update string) []byte {
	prefix := "8400f6"
	if parent != 0 {
		prefix = "840082" + "5820" + hex.EncodeToString(gtx(parent)) + "00"
	}
	return govHex(prefix + update + "f6")
}

// gaHFTo is a hard fork to protocol major.minor, both below 24.
func gaHFTo(parent byte, major, minor int) []byte {
	prefix := "8301f6"
	if parent != 0 {
		prefix = "830182" + "5820" + hex.EncodeToString(gtx(parent)) + "00"
	}
	return govHex(prefix + fmt.Sprintf("82%02x%02x", major, minor))
}

// ngAtProtocol is the ngBoundary request at protocol major.minor with three
// DRep delegations: 0x71 to the registered DRep d1, 0x72 to a DRep nobody
// registered and 0x73 to always abstain.
func ngAtProtocol(name, why string, major, minor uint32, props []*v1.Proposal, votes ...[]*v1.VoteRecord) entry {
	e := ngBoundary(name, why, props, votes...)
	r := e.call.GetEpochNewEpoch()
	r.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: major, Minor: minor}
	abstain := &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	r.State.Accounts = append(r.State.Accounts,
		&v1.Account{Credential: gcred(0x71), Drep: gdrepKey(0xd1)},
		&v1.Account{Credential: gcred(0x72), Drep: gdrepKey(0xdd)},
		&v1.Account{Credential: gcred(0x73), Drep: abstain})
	return e
}

// epochNewEpochPParamsCases are boundaries that enact a parameter change or a
// hard fork. The response's pparams_after is the full parameter set after, and
// is unset when nothing changed.
func epochNewEpochPParamsCases() []entry {
	yes := v1.Vote_VOTE_YES
	stay := ngProp(9, gaInfo, 15, 0, 0x50)
	drepPool := []*v1.VoteRecord{
		gvote(1, v1.VoterRole_VOTER_ROLE_DREP, 0xd1, yes), gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes),
	}
	committee := func(id byte) []*v1.VoteRecord {
		var out []*v1.VoteRecord
		for i := byte(1); i <= 3; i++ {
			out = append(out, gvote(id, v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, 0x20+i, yes))
		}
		return out
	}
	return []entry{
		ngBoundary("epoch_new_epoch_pparams_two_keys",
			"one change sets min_fee_a (key 0) to 50 and max_block_body_size (key 2) to 90112; both are in pparams_after and no other parameter moves",
			[]*v1.Proposal{ngProp(1, gaPCUpdate(0, "a200183202"+"1a00016000"), 15, 400, 0x62), stay}, gall(1, yes)),
		ngBoundary("epoch_new_epoch_pparams_chain_later_wins",
			"a change and its child both set max_block_body_size and are enacted in order: the child's value is the one after",
			[]*v1.Proposal{
				ngProp(1, gaPCUpdate(0, "a1021a00016000"), 15, 400, 0x62),
				ngProp(2, gaPCUpdate(1, "a1021a00018000"), 15, 300, 0x61), stay,
			}, gall(1, yes), gall(2, yes)),
		ngBoundary("epoch_new_epoch_pparams_governance_group",
			"gov_action_lifetime (key 29) is a governance-group parameter and needs the higher DRep threshold, which all-yes voting meets",
			[]*v1.Proposal{ngProp(1, gaPCUpdate(0, "a1181d08"), 15, 400, 0x62), stay}, gall(1, yes)),
		ngBoundary("epoch_new_epoch_pparams_rational",
			"a0 (key 9) is the rational 1/3: pparams_after carries the exact numerator and denominator",
			[]*v1.Proposal{ngProp(1, gaPCUpdate(0, "a109d81e820103"), 15, 400, 0x62), stay}, gall(1, yes)),
		ngBoundary("epoch_new_epoch_pparams_not_ratified",
			"the committee is one vote short, so the change is not enacted and pparams_after is unset",
			[]*v1.Proposal{ngProp(1, gaPCUpdate(0, "a1021a00016000"), 15, 400, 0x62), stay}, gall(1, yes)[:4]),
		ngBoundary("epoch_new_epoch_pparams_no_governance",
			"a change that is enacted while a withdrawal is paid leaves the parameters of the withdrawal's pass alone: only the change appears in pparams_after",
			[]*v1.Proposal{
				ngProp(1, gaPCUpdate(0, "a1021a00016000"), 15, 400, 0x62),
				ngProp(2, gaWithdraw([2]uint64{0x61, 1000}), 15, 300, 0x61), stay,
			}, gall(1, yes), gall(2, yes)),
		ngAtProtocol("epoch_new_epoch_hard_fork_clears_dangling_drep",
			"a hard fork to 10 enacted at protocol 9 runs HARDFORK: pparams_after is protocol 10.0 and the delegation to the unregistered DRep is cleared, the registered and predefined ones kept",
			9, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 10, 0), 15, 400, 0x62), stay}, gall(1, yes)),
		ngAtProtocol("epoch_new_epoch_hard_fork_bootstrap_without_drep_votes",
			"protocol 9 is the bootstrap phase, where DRep votes do not count: a hard fork with only the committee and pool votes is enacted",
			9, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 10, 0), 15, 400, 0x62), stay},
			[]*v1.VoteRecord{gvote(1, v1.VoterRole_VOTER_ROLE_STAKE_POOL, 0xb1, yes)}, committee(1)),
		ngAtProtocol("epoch_new_epoch_hard_fork_to_11",
			"a hard fork from 10 to 11 enacts the new version and leaves every delegation alone",
			10, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 11, 0), 15, 400, 0x62), stay}, gall(1, yes)),
		ngAtProtocol("epoch_new_epoch_hard_fork_not_ratified",
			"the DReps and pools vote yes but the committee does not, so the hard fork is not enacted and nothing changes",
			9, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 10, 0), 15, 400, 0x62), stay}, drepPool),
		ngAtProtocol("epoch_new_epoch_hard_fork_minor_at_9",
			"a minor bump 9.0 to 9.1 changes the version and runs HARDFORK with no major-version step",
			9, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 9, 1), 15, 400, 0x62), stay}, gall(1, yes)),
		ngAtProtocol("epoch_new_epoch_hard_fork_delays_parameter_change",
			"an accepted hard fork delays the parameter change after it: only the fork is enacted this boundary",
			9, 0, []*v1.Proposal{
				ngProp(1, gaHFTo(0, 10, 0), 15, 400, 0x62),
				ngProp(2, gaPCUpdate(0, "a1021a00016000"), 15, 300, 0x61), stay,
			}, gall(1, yes), gall(2, yes)),
	}
}

// epochNewEpochPParamsDivergences are boundaries where dingo and the reference
// part over a hard fork.
func epochNewEpochPParamsDivergences() []entry {
	yes := v1.Vote_VOTE_YES
	stay := ngProp(9, gaInfo, 15, 0, 0x50)
	return []entry{
		ngAtProtocol("epoch_new_epoch_hard_fork_minor_at_10",
			"a minor bump 10.0 to 10.1: the reference's HARDFORK matches on the new major version 10 and clears the delegation to the unregistered DRep again, dingo runs its protocol 10 step only when the major version changes and keeps it",
			10, 0, []*v1.Proposal{ngProp(1, gaHFTo(0, 10, 1), 15, 400, 0x62), stay}, gall(1, yes)),
	}
}
