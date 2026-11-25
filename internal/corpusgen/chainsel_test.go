package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateChainselCorpus(t *testing.T) {
	writeCorpus(t, "", chainselCases())
	writeCorpus(t, "divergences", chainselDivergenceCases())
}

func ptr(n uint64) *uint64 { return &n }

func chainTip(blockNo, slot uint64, issuer byte, issueNo uint64, vrf []byte) *v1.ChainTip {
	return &v1.ChainTip{BlockNo: blockNo, Slot: slot, IssuerVkey: rep(issuer, 32), IssueNo: issueNo, TiebreakVrf: vrf}
}

// vrfAt is a 64-byte VRF output whose only nonzero byte is v at index i, so
// two outputs differ at a chosen position.
func vrfAt(i int, v byte) []byte {
	b := make([]byte, 64)
	b[i] = v
	return b
}

func chainselCases() []entry {
	compare := func(name, why string, era v1.Era, window *uint64, a, b *v1.ChainTip) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ChainselCompare{ChainselCompare: &v1.ChainselCompareRequest{
			Era: era, ProtocolVersion: &v1.ProtocolVersion{Major: 10}, RestrictedVrfWindow: window, A: a, B: b,
		}}}}
	}
	tiebreak := func(name, why string, window *uint64, a, b *v1.ChainTip) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ChainselVrfTiebreak{ChainselVrfTiebreak: &v1.ChainselVrfTiebreakRequest{
			RestrictedVrfWindow: window, A: a, B: b,
		}}}}
	}
	lowVRF, highVRF := vrfAt(0, 1), vrfAt(0, 2)
	return []entry{
		compare("chainsel_compare_longer_chain", "higher block number wins whatever the tiebreakers say", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, highVRF), chainTip(99, 5000, 2, 0, lowVRF)),
		compare("chainsel_compare_longer_candidate", "the candidate with the higher block number wins", v1.Era_ERA_BABBAGE, nil,
			chainTip(99, 5000, 1, 0, lowVRF), chainTip(100, 5000, 2, 0, highVRF)),
		compare("chainsel_compare_max_block_numbers", "block numbers at the top of the Word64 range", v1.Era_ERA_CONWAY, ptr(5),
			chainTip(1<<64-1, 5000, 1, 0, lowVRF), chainTip(1<<64-2, 5000, 2, 0, lowVRF)),
		compare("chainsel_compare_opcert_issue_number", "same issuer and slot: the higher opcert issue number wins", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 4, highVRF), chainTip(100, 5000, 1, 5, lowVRF)),
		compare("chainsel_compare_opcert_issue_number_lower", "same issuer and slot: a lower issue number loses even with the better VRF", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 5, highVRF), chainTip(100, 5000, 1, 4, lowVRF)),
		compare("chainsel_compare_opcert_equal_falls_to_vrf", "same issuer, slot and issue number: the VRF decides", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 4, highVRF), chainTip(100, 5000, 1, 4, lowVRF)),
		compare("chainsel_compare_opcert_other_issuer", "different issuers: issue numbers are unrelated, so the VRF decides", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 9, lowVRF), chainTip(100, 5000, 2, 0, highVRF)),
		compare("chainsel_compare_opcert_other_slot", "same issuer, different slots: issue numbers are not compared", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 9, highVRF), chainTip(100, 5001, 1, 0, lowVRF)),
		compare("chainsel_compare_vrf_unrestricted_far", "unrestricted tiebreaker: slots a million apart still compare VRFs", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, highVRF), chainTip(100, 1005000, 2, 0, lowVRF)),
		compare("chainsel_compare_vrf_first_byte", "VRF outputs differing in the first byte", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, vrfAt(0, 0x80)), chainTip(100, 5001, 2, 0, vrfAt(0, 0x7f))),
		compare("chainsel_compare_vrf_last_byte", "VRF outputs differing only in the last byte", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, vrfAt(63, 2)), chainTip(100, 5001, 2, 0, vrfAt(63, 1))),
		compare("chainsel_compare_vrf_equal", "equal VRFs from different issuers tie", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, lowVRF), chainTip(100, 5001, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_at_window", "restricted window 5: slots exactly 5 apart arm the VRF", v1.Era_ERA_CONWAY, ptr(5),
			chainTip(100, 5000, 1, 0, highVRF), chainTip(100, 5005, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_past_window", "restricted window 5: slots 6 apart disarm the VRF, so the tips tie", v1.Era_ERA_CONWAY, ptr(5),
			chainTip(100, 5000, 1, 0, highVRF), chainTip(100, 5006, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_past_window_reversed", "restricted window 5: the distance is symmetric", v1.Era_ERA_CONWAY, ptr(5),
			chainTip(100, 5006, 1, 0, highVRF), chainTip(100, 5000, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_window_zero", "restricted window 0: only the same slot arms the VRF", v1.Era_ERA_CONWAY, ptr(0),
			chainTip(100, 5000, 1, 0, highVRF), chainTip(100, 5001, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_same_slot", "restricted window 0: the same slot arms the VRF", v1.Era_ERA_CONWAY, ptr(0),
			chainTip(100, 5000, 1, 0, highVRF), chainTip(100, 5000, 2, 0, lowVRF)),
		compare("chainsel_compare_restricted_issue_number_outside", "restricted window 0: the issue-number rule needs no window", v1.Era_ERA_CONWAY, ptr(0),
			chainTip(100, 5000, 1, 3, highVRF), chainTip(100, 5000, 1, 4, lowVRF)),
		compare("chainsel_compare_identical", "identical tips tie", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 4, lowVRF), chainTip(100, 5000, 1, 4, lowVRF)),
		compare("chainsel_compare_byron", "Byron has no Praos tiebreaker: unsupported on both nodes", v1.Era_ERA_BYRON, nil,
			chainTip(100, 5000, 1, 0, lowVRF), chainTip(100, 5001, 2, 0, highVRF)),
		compare("chainsel_compare_short_vrf", "a 63-byte VRF output cannot be an OutputVRF: unsupported on both nodes", v1.Era_ERA_BABBAGE, nil,
			chainTip(100, 5000, 1, 0, lowVRF[:63]), chainTip(100, 5001, 2, 0, highVRF)),
		tiebreak("chainsel_vrf_tiebreak_candidate_wins", "lower candidate VRF, unrestricted", nil,
			chainTip(1, 5000, 1, 0, highVRF), chainTip(1, 5001, 2, 0, lowVRF)),
		tiebreak("chainsel_vrf_tiebreak_ours_wins", "lower VRF on our side, unrestricted", nil,
			chainTip(1, 5000, 1, 0, lowVRF), chainTip(1, 5001, 2, 0, highVRF)),
		tiebreak("chainsel_vrf_tiebreak_ignores_block_numbers", "block numbers play no part in the tiebreaker", nil,
			chainTip(7, 5000, 1, 0, lowVRF), chainTip(900, 5001, 2, 0, highVRF)),
		tiebreak("chainsel_vrf_tiebreak_restricted_inside", "window 5, distance 5", ptr(5),
			chainTip(1, 5000, 1, 0, highVRF), chainTip(1, 5005, 2, 0, lowVRF)),
		tiebreak("chainsel_vrf_tiebreak_restricted_outside", "window 5, distance 6", ptr(5),
			chainTip(1, 5000, 1, 0, highVRF), chainTip(1, 5006, 2, 0, lowVRF)),
		tiebreak("chainsel_vrf_tiebreak_issue_number", "same issuer and slot: the issue number outranks the VRF", nil,
			chainTip(1, 5000, 1, 2, highVRF), chainTip(1, 5000, 1, 1, lowVRF)),
		tiebreak("chainsel_vrf_tiebreak_issue_number_cand", "same issuer and slot: a higher candidate issue number wins", ptr(0),
			chainTip(1, 5000, 1, 1, lowVRF), chainTip(1, 5000, 1, 2, highVRF)),
		tiebreak("chainsel_vrf_tiebreak_equal", "identical VRFs tie", nil,
			chainTip(1, 5000, 1, 0, lowVRF), chainTip(1, 5001, 2, 0, lowVRF)),
		tiebreak("chainsel_vrf_tiebreak_short_issuer", "a 31-byte issuer key is not a VKey: unsupported on both nodes", nil,
			&v1.ChainTip{Slot: 1, IssuerVkey: rep(1, 31), TiebreakVrf: lowVRF}, chainTip(1, 2, 2, 0, highVRF)),
		// Genesis density. Sizes follow mainnet: window 3k/f = 129600, k 2160.
		density("chainsel_genesis_density_dense_beats_sparse", "a peer with 6480 headers in the window disconnects one with 1296", 129600, 2160, 4000000,
			peer("honest", false, every(4000000, 20, 6480), 4129600), peer("sparse", false, every(4000000, 100, 1296), 4129600)),
		density("chainsel_genesis_density_small_window", "window 10, k 2: b can still reach 6 headers, a has 10", 10, 2, 100,
			peer("a", false, every(100, 1, 10), 110), peer("b", false, []uint64{105}, 105)),
		density("chainsel_genesis_density_idling", "an idling peer is held to what it delivered", 10, 2, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", true, []uint64{105}, 105)),
		density("chainsel_genesis_density_rival_at_most_k", "the rival needs more than k headers unless the window is complete", 10, 20, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", true, []uint64{105}, 105)),
		density("chainsel_genesis_density_rival_exactly_k", "k = 4: a rival with exactly 4 headers does not exceed k", 10, 4, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", true, []uint64{105}, 105)),
		density("chainsel_genesis_density_rival_k_plus_one", "k = 3: a rival with 4 headers exceeds k", 10, 3, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", true, []uint64{105}, 105)),
		density("chainsel_genesis_density_exact_bound", "a reaches at most 3 + 7 = 10 headers and b has 10: meeting the bound exactly is enough", 10, 2, 100,
			peer("a", false, []uint64{101, 102, 103}, 103), peer("b", false, every(100, 1, 10), 110)),
		density("chainsel_genesis_density_one_below_bound", "a reaches at most 4 + 6 = 10 and b has 9: the bound is not met", 10, 2, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", false, every(100, 1, 9), 109)),
		density("chainsel_genesis_density_complete_window", "a's window is complete: 3 headers, head at the window end, versus 5", 10, 2, 100,
			peer("a", false, []uint64{101, 105, 110}, 110), peer("b", false, []uint64{101, 103, 105, 107, 109}, 109)),
		density("chainsel_genesis_density_empty_peer", "a peer with no headers and not idling is not compared", 10, 2, 100,
			peer("a", false, every(100, 1, 10), 110), peer("b", false, nil, 100)),
		density("chainsel_genesis_density_lone_peer", "one candidate is never disconnected", 10, 2, 100,
			peer("a", false, []uint64{105}, 105)),
		density("chainsel_genesis_density_three_peers", "two sparse peers lose to the dense one", 10, 2, 100,
			peer("dense", false, every(100, 1, 10), 110), peer("s1", false, []uint64{104}, 104), peer("s2", false, []uint64{101, 109}, 109)),
		density("chainsel_genesis_density_headers_past_window", "headers beyond the window do not count toward its density", 10, 2, 100,
			peer("a", false, []uint64{101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112}, 112), peer("b", false, []uint64{105, 106, 107}, 107)),
		density("chainsel_genesis_density_no_candidates", "no candidates: nobody to disconnect", 10, 2, 100),
		density("chainsel_genesis_density_zero_window", "a zero Genesis window is unsupported on both nodes", 0, 2, 100,
			peer("a", false, []uint64{105}, 105)),
	}
}

func every(from, step uint64, n int) []uint64 {
	out := make([]uint64, n)
	for i := range out {
		out[i] = from + step*uint64(i+1)
	}
	return out
}

func peer(name string, idling bool, slots []uint64, tip uint64) *v1.DensityCandidate {
	return &v1.DensityCandidate{Peer: name, Idling: idling, Slots: slots, TipSlot: tip}
}

func density(name, why string, window, k, loe uint64, peers ...*v1.DensityCandidate) entry {
	return entry{name, why, &v1.Call{Request: &v1.Call_ChainselGenesisDensity{ChainselGenesisDensity: &v1.ChainselGenesisDensityRequest{
		GenesisWindow: window, SecurityParam: k, LoeSlot: loe, Candidates: peers,
	}}}}
}

// chainselDivergenceCases are confirmed mismatches, each with a direct test in
// dxnode-go (divergence_chainsel_test.go) and a "divergence:" check in
// Test/Chainsel.hs.
func chainselDivergenceCases() []entry {
	return []entry{
		density("chainsel_genesis_density_equal_density_both_lose",
			"reference: densityDisconnect reports every peer with an equal-or-better rival, so both are disconnected; dingo reports a, then b has no rival (TestDivergenceDensityEqualDensityReportsOnePeer)",
			10, 2, 100,
			peer("a", false, []uint64{101, 105, 110}, 110), peer("b", false, []uint64{102, 108, 110}, 110)),
		density("chainsel_genesis_density_idle_peer_without_headers",
			"reference: an idling peer with no header after the intersection is disconnected when a rival has more than k; dingo skips pairs where either peer has no header after the intersection (TestDivergenceDensityIdlePeerWithoutHeaders)",
			10, 2, 100,
			peer("a", false, every(100, 1, 10), 110), peer("b", true, nil, 100)),
		density("chainsel_genesis_density_complete_window_ties_all",
			"reference: four peers with one header each in a one-slot window all lose to each other, so all four are disconnected; dingo stops once a reported peer is no longer a rival and keeps the last (TestDivergenceDensityEqualDensityReportsOnePeer)",
			1, 2, 100,
			peer("p0", false, []uint64{101}, 101), peer("p1", false, []uint64{101, 102}, 102), peer("p2", false, []uint64{101}, 101), peer("p3", true, []uint64{101}, 101)),
		density("chainsel_genesis_density_tip_slot_beyond_window",
			"reference: a latest-received slot past the window closes the peer's upper bound (csLatestSlot); dingo has no such input and leaves b room to reach 6 headers (TestDivergenceDensityLatestSlotIgnored)",
			10, 2, 100,
			peer("a", false, []uint64{101, 102, 103, 104}, 104), peer("b", false, []uint64{105}, 120)),
	}
}
