package corpusgen

import (
	"bytes"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/praosfixtures"
)

func TestGeneratePraosCorpus(t *testing.T) {
	writeCorpus(t, "", append(praosCases(), praosHeaderCases(t)...))
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
// replay stays green; each has a direct confirmation test in dxnode-go and in
// the Haskell tests.
func praosDivergenceCases() []entry {
	out := rep(0x11, 64)
	return []entry{{
		"praos_nonce_epoch_neutral_candidate",
		"reference: NeutralNonce ⭒ lab = lab; dingo hashes its 32-zero neutral candidate (TestDivergenceNeutralCandidateIsHashed)",
		&v1.Call{Request: &v1.Call_PraosNonceEpoch{PraosNonceEpoch: &v1.PraosNonceEpochRequest{
			Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{}, PreviousLab: &v1.Nonce{Hash: rep(0xbb, 32)}, ExtraEntropy: &v1.Nonce{},
		}}},
	}, {
		"praos_nonce_rolling_neutral_evolving",
		"reference: NeutralNonce ⭒ c = c; gouroboros CalculateRollingNonce hashes the contribution (TestDivergenceNeutralEvolvingIsHashed)",
		&v1.Call{Request: &v1.Call_PraosNonceRolling{PraosNonceRolling: &v1.PraosNonceRollingRequest{
			Era: v1.Era_ERA_CONWAY, Evolving: &v1.Nonce{}, VrfOutput: out,
		}}},
	}, {
		"praos_nonce_candidate_neutral_evolving",
		"the same neutral evolving nonce through the candidate fold (TestDivergenceNeutralEvolvingIsHashed)",
		&v1.Call{Request: &v1.Call_PraosNonceCandidate{PraosNonceCandidate: &v1.PraosNonceCandidateRequest{
			Era: v1.Era_ERA_CONWAY, Evolving: &v1.Nonce{}, Candidate: &v1.Nonce{Hash: rep(0xcc, 32)},
			EpochFirstSlot: 1000, NextEpochFirstSlot: 1100, SecurityParam: 10, ActiveSlotCoeff: rat("1", "2"),
			Blocks: []*v1.BlockNonceInput{{Slot: 1010, VrfOutput: out}},
		}}},
	}}
}

func blake256(parts ...[]byte) []byte {
	h, _ := blake2b.New256(nil)
	for _, p := range parts {
		h.Write(p)
	}
	return h.Sum(nil)
}

func blake224(b []byte) []byte {
	h, _ := blake2b.New(28, nil)
	h.Write(b)
	return h.Sum(nil)
}

// headerFacts are the fields of a real header the cases build requests from.
type headerFacts struct {
	praosfixtures.Header
	issuer, vrfKey, vrfProof, vrfOutput, coldSig []byte
	nonceVrfOutput                               []byte
	bodySize                                     uint64
}

func factsOf(t *testing.T, h praosfixtures.Header) headerFacts {
	t.Helper()
	f := headerFacts{Header: h}
	var bt uint
	switch h.Era {
	case v1.Era_ERA_BABBAGE:
		bt = ledger.BlockTypeBabbage
	case v1.Era_ERA_CONWAY:
		bt = ledger.BlockTypeConway
	case v1.Era_ERA_SHELLEY:
		bt = ledger.BlockTypeShelley
	default:
		t.Fatalf("no facts for %v", h.Era)
	}
	hdr, err := ledger.NewBlockHeaderFromCbor(bt, h.CBOR)
	if err != nil {
		t.Fatal(err)
	}
	vk := hdr.IssuerVkey()
	f.issuer, f.bodySize = vk[:], hdr.BlockBodySize()
	oc, err := ledger.ExtractOpCertFromHeader(hdr)
	if err != nil {
		t.Fatal(err)
	}
	f.coldSig = oc.ColdSignature
	switch x := hdr.(type) {
	case *babbage.BabbageBlockHeader:
		f.vrfKey, f.vrfProof, f.vrfOutput = x.Body.VrfKey, x.Body.VrfResult.Proof, x.Body.VrfResult.Output
	case *conway.ConwayBlockHeader:
		f.vrfKey, f.vrfProof, f.vrfOutput = x.Body.VrfKey, x.Body.VrfResult.Proof, x.Body.VrfResult.Output
	case *shelley.ShelleyBlockHeader:
		f.nonceVrfOutput = x.Body.NonceVrf.Output
	}
	return f
}

func flipAt(b []byte, i int) []byte { return flip(b, i) }

func indexOf(t *testing.T, b, sub []byte) int {
	t.Helper()
	i := bytes.Index(b, sub)
	if i < 0 {
		t.Fatal("subsequence not found")
	}
	return i
}

func u32p(n uint32) *uint32 { return &n }

func u64p(n uint64) *uint64 { return &n }

func pparamsLimits(header, body uint32) *v1.ProtocolParameters {
	return &v1.ProtocolParameters{MaxBlockHeaderSize: u32p(header), MaxBlockBodySize: u32p(body)}
}

// praosHeaderCases are the header and nonce cases. Headers are real mainnet
// headers; only those whose epoch nonce gouroboros records can show a valid
// VRF proof, but the KES, operational certificate and envelope checks need no
// nonce and run on all of them.
func praosHeaderCases(t *testing.T) []entry {
	var out []entry
	add := func(name, why string, c *v1.Call) { out = append(out, entry{name, why, c}) }

	praosHeaders := []string{"babbage", "babbage_m8", "conway", "conway_a", "conway_b"}
	byName := map[string]headerFacts{}
	for _, n := range praosHeaders {
		byName[n] = factsOf(t, praosfixtures.ByName(n))
	}

	// praos_header_envelope: the encoded header against the header bound, then
	// the declared body size against the body bound, each accepted at equality.
	env := func(name, why string, h headerFacts, header []byte, maxH, maxB uint32) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosHeaderEnvelope{PraosHeaderEnvelope: &v1.PraosHeaderEnvelopeRequest{
			Era: h.Era, Header: header, Pparams: pparamsLimits(maxH, maxB),
		}}})
	}
	ca, cb, bm8 := byName["conway_a"], byName["conway_b"], byName["babbage_m8"]
	env("praos_header_envelope_conway_mainnet_limits", "Conway mainnet header under the mainnet bounds", ca, ca.CBOR, 1100, 90112)
	env("praos_header_envelope_babbage_mainnet_limits", "Babbage mainnet header under the mainnet bounds", bm8, bm8.CBOR, 1100, 90112)
	env("praos_header_envelope_at_bounds", "header 859 bytes and declared body 50666: both equal their bounds", ca, ca.CBOR, 859, uint32(ca.bodySize))
	env("praos_header_envelope_header_one_over", "header bound one below the 859-byte header", ca, ca.CBOR, 858, 90112)
	env("praos_header_envelope_body_one_over", "body bound one below the declared 50666", ca, ca.CBOR, 1100, uint32(ca.bodySize)-1)
	env("praos_header_envelope_both_over", "both over: the header is reported, the body check is not reached", ca, ca.CBOR, 858, uint32(ca.bodySize)-1)
	env("praos_header_envelope_large_body", "declared body 88553 of a header 861 bytes long", cb, cb.CBOR, 861, 88553)
	env("praos_header_envelope_zero_bounds", "zero bounds reject on the header first", ca, ca.CBOR, 0, 0)
	env("praos_header_envelope_truncated", "a header cut short does not decode: DeserialiseFailure", ca, ca.CBOR[:100], 1100, 90112)
	env("praos_header_envelope_header_bound_word16", "a header bound above Word16 is not representable in the reference", ca, ca.CBOR, 65536, 90112)

	// praos_vrf_check.
	vrfCase := func(name, why string, h headerFacts, header []byte, nonce, keyHash []byte) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosVrfCheck{PraosVrfCheck: &v1.PraosVrfCheckRequest{
			Era: h.Era, Header: header, EpochNonce: &v1.Nonce{Hash: nonce}, PoolVrfKeyHash: keyHash,
		}}})
	}
	for _, n := range []string{"conway_a", "conway_b", "babbage_m8"} {
		h := byName[n]
		vrfCase("praos_vrf_check_valid_"+n, "real header, its epoch nonce and registered key hash: leader and nonce values", h, h.CBOR, h.EpochNonce, blake256(h.vrfKey))
	}
	vrfCase("praos_vrf_check_wrong_nonce", "a different epoch nonce: the VRF input changes and the proof fails", ca, ca.CBOR, rep(1, 32), blake256(ca.vrfKey))
	vrfCase("praos_vrf_check_unknown_nonce", "a real header whose epoch nonce is not known: the proof fails", byName["conway"], byName["conway"].CBOR, rep(2, 32), blake256(byName["conway"].vrfKey))
	vrfCase("praos_vrf_check_wrong_key_hash", "registered key hash differs from the header's key", ca, ca.CBOR, ca.EpochNonce, rep(3, 32))
	vrfCase("praos_vrf_check_key_hash_first", "both wrong: the key hash is reported, as it is checked first", ca, ca.CBOR, rep(1, 32), rep(3, 32))
	proofAt := indexOf(t, ca.CBOR, ca.vrfProof)
	vrfCase("praos_vrf_check_tampered_proof", "one bit of the VRF proof flipped", ca, flipAt(ca.CBOR, proofAt+5), ca.EpochNonce, blake256(ca.vrfKey))
	vrfCase("praos_vrf_check_tampered_output", "one bit of the VRF output flipped: the proof no longer hashes to it", ca,
		flipAt(ca.CBOR, indexOf(t, ca.CBOR, ca.vrfOutput)+3), ca.EpochNonce, blake256(ca.vrfKey))
	vrfCase("praos_vrf_check_truncated", "a header cut short does not decode", ca, ca.CBOR[:200], ca.EpochNonce, blake256(ca.vrfKey))

	// praos_kes_check: KES period window, cold signature, KES signature.
	kesCase := func(name, why string, h headerFacts, header []byte, spkp, maxEvo uint64) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosKesCheck{PraosKesCheck: &v1.PraosKesCheckRequest{
			Era: h.Era, Header: header, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
		}}})
	}
	const spkp, maxEvo = praosfixtures.SlotsPerKESPeriod, praosfixtures.MaxKESEvolutions
	// Evolutions at mainnet parameters: babbage 46, babbage_m8 21, conway 9, conway_a 1, conway_b 36.
	for _, n := range praosHeaders {
		h := byName[n]
		kesCase("praos_kes_check_valid_"+n, "real header at the mainnet KES parameters", h, h.CBOR, spkp, maxEvo)
	}
	kesCase("praos_kes_check_evolution_equals_max", "babbage_m8 is at evolution 21: a bound of 21 excludes it", bm8, bm8.CBOR, spkp, 21)
	kesCase("praos_kes_check_evolution_below_max", "a bound of 22 admits evolution 21", bm8, bm8.CBOR, spkp, 22)
	kesCase("praos_kes_check_first_evolution", "conway_a at evolution 1", ca, ca.CBOR, spkp, 2)
	kesCase("praos_kes_check_period_before_start", "135000 slots per period puts the header before its certificate's start period", bm8, bm8.CBOR, 135000, 62)
	kesCase("praos_kes_check_wrong_period_length", "129000 slots per period: another evolution, the KES signature fails", bm8, bm8.CBOR, 129000, 62)
	kesCase("praos_kes_check_tampered_cold_signature", "one bit of the certificate's cold signature flipped", bm8, flipAt(bm8.CBOR, indexOf(t, bm8.CBOR, bm8.coldSig)), spkp, maxEvo)
	kesCase("praos_kes_check_tampered_kes_signature", "last bit of the KES signature flipped", bm8, flipAt(bm8.CBOR, len(bm8.CBOR)-1), spkp, maxEvo)
	kesCase("praos_kes_check_tampered_body", "slot byte of the header body flipped: the KES signature no longer covers it", ca, flipAt(ca.CBOR, 10), spkp, maxEvo)
	kesCase("praos_kes_check_window_before_signature", "tampered cold signature with the window exceeded: the window is reported", bm8, flipAt(bm8.CBOR, indexOf(t, bm8.CBOR, bm8.coldSig)), spkp, 21)
	kesCase("praos_kes_check_cold_before_kes", "both signatures tampered: the cold signature is reported", bm8,
		flipAt(flipAt(bm8.CBOR, indexOf(t, bm8.CBOR, bm8.coldSig)), len(bm8.CBOR)-1), spkp, maxEvo)
	kesCase("praos_kes_check_truncated", "a header cut short does not decode", bm8, bm8.CBOR[:300], spkp, maxEvo)

	// praos_opcert_check: counter m <= n <= m + 1, then the signatures before it.
	opcert := func(name, why string, h headerFacts, header []byte, major uint32, last *uint64, registered bool) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosOpcertCheck{PraosOpcertCheck: &v1.PraosOpCertCheckRequest{
			ProtocolVersion: &v1.ProtocolVersion{Major: major}, Header: header, LastCounter: last, PoolRegistered: registered,
			SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
		}}})
	}
	opcert("praos_opcert_check_same_counter", "conway_a carries counter 10; last counter 10", ca, ca.CBOR, 10, u64p(10), true)
	opcert("praos_opcert_check_next_counter", "last counter 9: n = m + 1", ca, ca.CBOR, 10, u64p(9), true)
	opcert("praos_opcert_check_skips_one", "last counter 8: n = m + 2", ca, ca.CBOR, 10, u64p(8), true)
	opcert("praos_opcert_check_lower_than_last", "last counter 11: n < m", ca, ca.CBOR, 10, u64p(11), true)
	opcert("praos_opcert_check_babbage_same", "babbage_m8 carries counter 18, protocol 8", bm8, bm8.CBOR, 8, u64p(18), true)
	opcert("praos_opcert_check_babbage_gap", "babbage_m8: last counter 5 skips ahead under protocol 8", bm8, bm8.CBOR, 8, u64p(5), true)
	opcert("praos_opcert_check_conway_b_next", "conway_b carries counter 17; last counter 16", cb, cb.CBOR, 9, u64p(16), false)
	opcert("praos_opcert_check_unseen_registered", "no counter recorded, pool registered: counted from zero, counter 10 skips", ca, ca.CBOR, 10, nil, true)
	opcert("praos_opcert_check_unseen_unregistered", "no counter recorded, pool not registered", ca, ca.CBOR, 10, nil, false)
	opcert("praos_opcert_check_recorded_unregistered", "a recorded counter decides, whatever the registration", ca, ca.CBOR, 10, u64p(10), false)
	opcert("praos_opcert_check_tampered_kes", "tampered KES signature with a counter that would also fail: the signature is reported", ca, flipAt(ca.CBOR, len(ca.CBOR)-1), 10, u64p(8), true)
	opcert("praos_opcert_check_truncated", "a header cut short does not decode", ca, ca.CBOR[:300], 10, u64p(10), true)

	// praos_vrf_key_registered: the pool lookup, then the registered key hash.
	keyReg := func(name, why string, h headerFacts, header []byte, dist ...*v1.PoolStake) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosVrfKeyRegistered{PraosVrfKeyRegistered: &v1.PraosVrfKeyRegisteredRequest{
			Header: header, PoolDistribution: dist,
		}}})
	}
	stake := func(pool, vrfHash []byte) *v1.PoolStake {
		return &v1.PoolStake{PoolId: pool, Stake: 1_000_000, Share: rat("1", "100"), VrfKeyHash: vrfHash}
	}
	for _, n := range []string{"conway_a", "babbage_m8"} {
		h := byName[n]
		keyReg("praos_vrf_key_registered_"+n, "the issuer's pool is registered under the header's VRF key", h, h.CBOR,
			stake(rep(9, 28), rep(1, 32)), stake(blake224(h.issuer), blake256(h.vrfKey)))
	}
	keyReg("praos_vrf_key_registered_unknown_pool", "the issuer is not in the distribution", ca, ca.CBOR, stake(rep(9, 28), blake256(ca.vrfKey)))
	keyReg("praos_vrf_key_registered_empty", "an empty distribution", ca, ca.CBOR)
	keyReg("praos_vrf_key_registered_wrong_key", "the issuer is registered under another VRF key", ca, ca.CBOR, stake(blake224(ca.issuer), rep(1, 32)))
	keyReg("praos_vrf_key_registered_other_pools_key", "another pool holds the header's key: the issuer's own entry decides", ca, ca.CBOR,
		stake(rep(9, 28), blake256(ca.vrfKey)), stake(blake224(ca.issuer), rep(1, 32)))
	keyReg("praos_vrf_key_registered_truncated", "a header cut short does not decode", ca, ca.CBOR[:300], stake(blake224(ca.issuer), blake256(ca.vrfKey)))

	// praos_nonce_rolling: evolving ⭒ the era's nonce of the VRF output.
	sh := factsOf(t, praosfixtures.ByName("shelley"))
	evolving := rep(0xaa, 32)
	rolling := func(name, why string, era v1.Era, ev, vrfOut []byte) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosNonceRolling{PraosNonceRolling: &v1.PraosNonceRollingRequest{
			Era: era, Evolving: &v1.Nonce{Hash: ev}, VrfOutput: vrfOut,
		}}})
	}
	for _, e := range []struct {
		name string
		era  v1.Era
	}{{"shelley", v1.Era_ERA_SHELLEY}, {"allegra", v1.Era_ERA_ALLEGRA}, {"mary", v1.Era_ERA_MARY}, {"alonzo", v1.Era_ERA_ALONZO}} {
		rolling("praos_nonce_rolling_tpraos_"+e.name, "TPraos hashes the real nonce VRF output once before mixing", e.era, evolving, sh.nonceVrfOutput)
	}
	rolling("praos_nonce_rolling_babbage", "Praos range-extends a real VRF output: blake2b_256 of blake2b_256 (N || output)", v1.Era_ERA_BABBAGE, evolving, bm8.vrfOutput)
	rolling("praos_nonce_rolling_conway", "Conway uses the Praos contribution", v1.Era_ERA_CONWAY, evolving, ca.vrfOutput)
	rolling("praos_nonce_rolling_chained", "an evolving nonce that is itself a hash output", v1.Era_ERA_CONWAY, blake256(evolving), cb.vrfOutput)
	rolling("praos_nonce_rolling_zero_output", "an all-zero VRF output is an ordinary input", v1.Era_ERA_CONWAY, evolving, make([]byte, 64))

	// praos_nonce_candidate: a mainnet epoch (k = 2160, f = 1/20, 432000 slots,
	// epoch 300), whose stability window is 3k/f = 129600 slots before Conway
	// and 4k/f = 172800 from Conway.
	const first, length = 129_600_000, 432_000
	next := uint64(first + length)
	cutoff3, cutoff4 := next-129_600, next-172_800
	outputs := [][]byte{ca.vrfOutput, cb.vrfOutput, bm8.vrfOutput, byName["conway"].vrfOutput, byName["babbage"].vrfOutput}
	blocksAt := func(slots ...uint64) []*v1.BlockNonceInput {
		var bs []*v1.BlockNonceInput
		for i, s := range slots {
			bs = append(bs, &v1.BlockNonceInput{Slot: s, VrfOutput: outputs[i%len(outputs)]})
		}
		return bs
	}
	cand := func(name, why string, era v1.Era, epochFirst, epochNext, k uint64, f *v1.Rational, blocks []*v1.BlockNonceInput) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosNonceCandidate{PraosNonceCandidate: &v1.PraosNonceCandidateRequest{
			Era: era, Evolving: &v1.Nonce{Hash: rep(0xaa, 32)}, Candidate: &v1.Nonce{Hash: rep(0xcc, 32)},
			EpochFirstSlot: epochFirst, NextEpochFirstSlot: epochNext, SecurityParam: k, ActiveSlotCoeff: f, Blocks: blocks,
		}}})
	}
	mainnet := func(name, why string, era v1.Era, slots ...uint64) {
		cand(name, why, era, first, next, 2160, rat("1", "20"), blocksAt(slots...))
	}
	mainnet("praos_nonce_candidate_no_blocks", "an empty fold leaves both nonces", v1.Era_ERA_SHELLEY)
	early := []uint64{first + 1_000, first + 200_000, cutoff4 - 1}
	mainnet("praos_nonce_candidate_early_blocks_conway", "all blocks before both cutoffs: the candidate is the evolving nonce", v1.Era_ERA_CONWAY, early...)
	mainnet("praos_nonce_candidate_early_blocks_shelley", "TPraos, all blocks before the cutoff", v1.Era_ERA_SHELLEY, early...)
	between := []uint64{first + 1_000, cutoff4 - 1, cutoff4, cutoff3 - 1}
	mainnet("praos_nonce_candidate_babbage_between_cutoffs", "Babbage keeps 3k/f: blocks at 4k/f and later still move the candidate up to 3k/f", v1.Era_ERA_BABBAGE, between...)
	mainnet("praos_nonce_candidate_conway_between_cutoffs", "Conway uses 4k/f: the block at cutoff 4k/f freezes the candidate", v1.Era_ERA_CONWAY, between...)
	mainnet("praos_nonce_candidate_alonzo_between_cutoffs", "TPraos keeps 3k/f", v1.Era_ERA_ALONZO, between...)
	mainnet("praos_nonce_candidate_babbage_at_cutoff", "a block exactly at the 3k/f cutoff does not move the candidate", v1.Era_ERA_BABBAGE, cutoff3-1, cutoff3)
	mainnet("praos_nonce_candidate_after_cutoff", "blocks after the cutoff: only the evolving nonce moves", v1.Era_ERA_CONWAY, cutoff4+1, cutoff4+5_000, next-1)
	mainnet("praos_nonce_candidate_mary", "Mary, a block either side of the cutoff", v1.Era_ERA_MARY, cutoff3-1, cutoff3+1)
	cand("praos_nonce_candidate_window_covers_epoch", "a window longer than the epoch: no block moves the candidate", v1.Era_ERA_CONWAY, 1000, 1100, 10, rat("1", "2"), blocksAt(1000, 1050))
	cand("praos_nonce_candidate_small_epoch_alonzo", "k = 10, f = 1/2: window 60, epoch [1000, 1100), cutoff 1040", v1.Era_ERA_ALONZO, 1000, 1100, 10, rat("1", "2"), blocksAt(1010, 1039, 1040, 1099))
	cand("praos_nonce_candidate_small_epoch_conway", "k = 10, f = 1/2: window 80, cutoff 1020", v1.Era_ERA_CONWAY, 1000, 1100, 10, rat("1", "2"), blocksAt(1010, 1039, 1040, 1099))
	cand("praos_nonce_candidate_fractional_window", "k = 7, f = 1/3: 3k/f = 63 exactly, 4k/f = 84", v1.Era_ERA_BABBAGE, 1000, 1100, 7, rat("1", "3"), blocksAt(1036, 1037))
	cand("praos_nonce_candidate_window_rounds_up", "k = 10, f = 3/10: 3k/f = 100, 4k/f = ceiling(133.3) = 134", v1.Era_ERA_CONWAY, 1000, 1200, 10, rat("3", "10"), blocksAt(1050, 1066, 1067))
	cand("praos_nonce_candidate_slot_outside_epoch", "a block past the epoch is not representable in the reference's epoch info", v1.Era_ERA_CONWAY, 1000, 1100, 10, rat("1", "2"), blocksAt(1100))
	return out
}
