package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGeneratePraosCorpus(t *testing.T) {
	writeCorpus(t, "", praosCases())
	writeCorpus(t, "divergences", praosDivergenceCases())
}

func praosCases() []entry {
	leader := func(name, why string, era v1.Era, v []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PraosLeaderCheck{PraosLeaderCheck: &v1.PraosLeaderCheckRequest{
			Era: era, LeaderValue: v, PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
		}}}}
	}
	nonce := func(name, why string, c, l, e []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PraosNonceEpoch{PraosNonceEpoch: &v1.PraosNonceEpochRequest{
			Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{Hash: c}, PreviousLab: &v1.Nonce{Hash: l}, ExtraEntropy: &v1.Nonce{Hash: e},
		}}}}
	}
	var keys []*v1.GenesisDelegation
	for i := byte(1); i <= 7; i++ {
		keys = append(keys, &v1.GenesisDelegation{GenesisKeyHash: rep(i, 28)})
	}
	overlay := func(name, why string, slot uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TpraosOverlaySchedule{TpraosOverlaySchedule: &v1.TpraosOverlayScheduleRequest{
			Slot: slot, EpochFirstSlot: 1000, D: rat("1", "2"), ActiveSlotCoeff: rat("1", "20"), GenesisDelegations: keys,
		}}}}
	}
	a, b, c := rep(0xaa, 32), rep(0xbb, 32), rep(0xcc, 32)
	// Leader threshold for sigma 1/2, f 1/20: 1 - 0.95^0.5 = 0.02532.
	return []entry{
		leader("praos_leader_check_praos_min", "Praos: value 0 always leads", v1.Era_ERA_BABBAGE, make([]byte, 32)),
		leader("praos_leader_check_praos_max", "Praos: the largest value never leads", v1.Era_ERA_BABBAGE, rep(0xff, 32)),
		leader("praos_leader_check_praos_below_threshold", "Praos: p = 0.0250 < 0.02532", v1.Era_ERA_CONWAY, fraction(250, 10000, 32)),
		leader("praos_leader_check_praos_above_threshold", "Praos: p = 0.0255 > 0.02532", v1.Era_ERA_CONWAY, fraction(255, 10000, 32)),
		leader("praos_leader_check_tpraos_below_threshold", "TPraos: 64-byte value, p = 0.0250", v1.Era_ERA_SHELLEY, fraction(250, 10000, 64)),
		leader("praos_leader_check_tpraos_above_threshold", "TPraos: 64-byte value, p = 0.0255", v1.Era_ERA_ALONZO, fraction(255, 10000, 64)),
		nonce("praos_nonce_epoch_candidate_lab", "candidate ⭒ lab", a, b, nil),
		nonce("praos_nonce_epoch_candidate_only", "neutral lab and extra entropy: the candidate", a, nil, nil),
		nonce("praos_nonce_epoch_extra_entropy", "candidate ⭒ extra entropy", a, nil, c),
		nonce("praos_nonce_epoch_all", "(candidate ⭒ lab) ⭒ extra entropy", a, b, c),
		overlay("tpraos_overlay_schedule_active_first", "relative slot 0: active, genesis key 0", 1000),
		overlay("tpraos_overlay_schedule_praos", "relative slot 1: not an overlay slot", 1001),
		overlay("tpraos_overlay_schedule_non_active", "relative slot 2: overlay slot nobody produces", 1002),
		overlay("tpraos_overlay_schedule_active_second_key", "relative slot 40: active, genesis key 1", 1040),
	}
}

// praosDivergenceCases are known mismatches, kept out of corpus/ so the default
// replay stays green; each has a direct confirmation test in dxnode-go.
func praosDivergenceCases() []entry {
	return []entry{{
		"praos_nonce_epoch_neutral_candidate",
		"reference: NeutralNonce ⭒ lab = lab; dingo hashes its 32-zero neutral candidate (TestDivergenceNeutralCandidateIsHashed)",
		&v1.Call{Request: &v1.Call_PraosNonceEpoch{PraosNonceEpoch: &v1.PraosNonceEpochRequest{
			Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{}, PreviousLab: &v1.Nonce{Hash: rep(0xbb, 32)}, ExtraEntropy: &v1.Nonce{},
		}}},
	}}
}
