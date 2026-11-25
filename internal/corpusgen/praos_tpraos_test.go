package corpusgen

import (
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/praosfixtures"
)

// tpFacts are the fields of a real TPraos header the cases build requests from.
type tpFacts struct {
	praosfixtures.Header
	major                                   uint32
	issuer, vrfKey, nonceProof, leaderProof []byte
	coldSig, prevHash                       []byte
	blockNo, slot                           uint64
	headerLen                               uint32
	bodySize                                uint32
}

func tpFactsOf(t *testing.T, name string, major uint32) tpFacts {
	t.Helper()
	h := praosfixtures.ByName(name)
	bt := map[v1.Era]uint{
		v1.Era_ERA_SHELLEY: ledger.BlockTypeShelley, v1.Era_ERA_ALLEGRA: ledger.BlockTypeAllegra,
		v1.Era_ERA_MARY: ledger.BlockTypeMary, v1.Era_ERA_ALONZO: ledger.BlockTypeAlonzo,
	}[h.Era]
	hdr, err := ledger.NewBlockHeaderFromCbor(bt, h.CBOR)
	if err != nil {
		t.Fatal(err)
	}
	var body *shelley.ShelleyBlockHeaderBody
	switch x := hdr.(type) {
	case *shelley.ShelleyBlockHeader:
		body = &x.Body
	case *allegra.AllegraBlockHeader:
		body = &x.Body
	case *mary.MaryBlockHeader:
		body = &x.Body
	case *alonzo.AlonzoBlockHeader:
		body = &x.Body
	}
	oc, err := ledger.ExtractOpCertFromHeader(hdr)
	if err != nil {
		t.Fatal(err)
	}
	vk := hdr.IssuerVkey()
	prev := hdr.PrevHash()
	return tpFacts{
		Header: h, major: major, issuer: vk[:], vrfKey: body.VrfKey,
		nonceProof: body.NonceVrf.Proof, leaderProof: body.LeaderVrf.Proof,
		coldSig: oc.ColdSignature, prevHash: prev.Bytes(),
		blockNo: hdr.BlockNumber(), slot: hdr.SlotNumber(),
		headerLen: uint32(len(h.CBOR)), bodySize: uint32(hdr.BlockBodySize()),
	}
}

// tpraosCases are the Shelley to Alonzo header cases and the position checks
// of the envelope. Headers are the real mainnet headers of the gouroboros test
// data; each has the epoch nonce under which its proofs verify.
func tpraosCases(t *testing.T) []entry {
	var out []entry
	add := func(name, why string, c *v1.Call) { out = append(out, entry{name, why, c}) }
	const spkp, maxEvo = praosfixtures.SlotsPerKESPeriod, praosfixtures.MaxKESEvolutions
	eras := []struct {
		name  string
		major uint32
	}{{"shelley", 2}, {"allegra", 3}, {"mary", 4}, {"alonzo", 5}}
	facts := map[string]tpFacts{}
	for _, e := range eras {
		facts[e.name] = tpFactsOf(t, e.name, e.major)
	}
	// KES evolution at 129600 slots per period: shelley 27, allegra 3, mary 40, alonzo 10.
	evolution := map[string]uint64{"shelley": 27, "allegra": 3, "mary": 40, "alonzo": 10}

	env := func(name, why string, h tpFacts, header []byte, maxH, maxB uint32, parent *v1.EnvelopeTip, origin bool) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PraosHeaderEnvelope{PraosHeaderEnvelope: &v1.PraosHeaderEnvelopeRequest{
			Era: h.Era, Header: header, Pparams: pparamsLimits(maxH, maxB), Parent: parent, Origin: origin,
		}}}}
	}
	vrfCase := func(name, why string, h tpFacts, header, nonce, keyHash []byte, overlay *v1.TpraosOverlayEnv) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosVrfCheck{PraosVrfCheck: &v1.PraosVrfCheckRequest{
			Era: h.Era, Header: header, EpochNonce: &v1.Nonce{Hash: nonce}, PoolVrfKeyHash: keyHash, Overlay: overlay,
		}}})
	}
	kesCase := func(name, why string, h tpFacts, header []byte, spkp, maxEvo uint64) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosKesCheck{PraosKesCheck: &v1.PraosKesCheckRequest{
			Era: h.Era, Header: header, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
		}}})
	}
	opcert := func(name, why string, h tpFacts, header []byte, last *uint64, registered, genesis bool) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosOpcertCheck{PraosOpcertCheck: &v1.PraosOpCertCheckRequest{
			ProtocolVersion: &v1.ProtocolVersion{Major: h.major}, Header: header, LastCounter: last, PoolRegistered: registered,
			IssuerIsGenesisDelegate: genesis, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
		}}})
	}
	stake := func(pool, vrfHash []byte) *v1.PoolStake {
		return &v1.PoolStake{PoolId: pool, Stake: 1_000_000, Share: rat("1", "100"), VrfKeyHash: vrfHash}
	}
	keyReg := func(name, why string, h tpFacts, dist ...*v1.PoolStake) {
		add(name, why, &v1.Call{Request: &v1.Call_PraosVrfKeyRegistered{PraosVrfKeyRegistered: &v1.PraosVrfKeyRegisteredRequest{
			Header: h.CBOR, Era: h.Era, PoolDistribution: dist,
		}}})
	}

	for _, e := range eras {
		h := facts[e.name]
		n := e.name
		out = append(out,
			env("praos_header_envelope_tpraos_"+n+"_mainnet_limits", "TPraos header under the mainnet bounds", h, h.CBOR, 1100, 90112, nil, false),
			env("praos_header_envelope_tpraos_"+n+"_at_bounds", "header and declared body both equal their bounds", h, h.CBOR, h.headerLen, h.bodySize, nil, false),
			env("praos_header_envelope_tpraos_"+n+"_header_one_over", "header bound one below the encoded header", h, h.CBOR, h.headerLen-1, 90112, nil, false),
			env("praos_header_envelope_tpraos_"+n+"_body_one_over", "body bound one below the declared body", h, h.CBOR, 1100, h.bodySize-1, nil, false),
		)
		vrfCase("praos_vrf_check_tpraos_valid_"+n, "real TPraos header, its epoch nonce and registered key hash: leader and nonce values", h, h.CBOR, h.EpochNonce, blake256(h.vrfKey), nil)
		kesCase("praos_kes_check_tpraos_valid_"+n, "real TPraos header at the mainnet KES parameters", h, h.CBOR, spkp, maxEvo)
		kesCase("praos_kes_check_tpraos_evolution_equals_max_"+n, "evolution equals the bound: KESAfterEndOCERT", h, h.CBOR, spkp, evolution[n])
		kesCase("praos_kes_check_tpraos_evolution_below_max_"+n, "a bound one above the evolution admits it", h, h.CBOR, spkp, evolution[n]+1)
		kesCase("praos_kes_check_tpraos_tampered_kes_signature_"+n, "last bit of the KES signature flipped", h, flipAt(h.CBOR, len(h.CBOR)-1), spkp, maxEvo)
		keyReg("praos_vrf_key_registered_tpraos_match_"+n, "the issuer's pool is registered under the header's VRF key", h,
			stake(rep(9, 28), rep(1, 32)), stake(blake224(h.issuer), blake256(h.vrfKey)))
		keyReg("praos_vrf_key_registered_tpraos_unknown_pool_"+n, "the issuer is not in the distribution", h, stake(rep(9, 28), blake256(h.vrfKey)))
		keyReg("praos_vrf_key_registered_tpraos_wrong_key_"+n, "the issuer is registered under another VRF key", h, stake(blake224(h.issuer), rep(1, 32)))
		counter := uint64(0)
		switch n {
		case "allegra", "alonzo":
			counter = 1
		case "mary":
			counter = 5
		}
		opcert("praos_opcert_check_tpraos_same_counter_"+n, "recorded counter equals the header's", h, h.CBOR, u64p(counter), false, false)
		opcert("praos_opcert_check_tpraos_unseen_registered_"+n, "no counter recorded, pool registered: counted from zero, no upper bound", h, h.CBOR, nil, true, false)
		opcert("praos_opcert_check_tpraos_unseen_unregistered_"+n, "no counter recorded, issuer unknown: NoCounterForKeyHashOCERT", h, h.CBOR, nil, false, false)
		opcert("praos_opcert_check_tpraos_lower_than_last_"+n, "recorded counter one above the header's: CounterTooSmallOCERT", h, h.CBOR, u64p(counter+1), true, false)
	}

	sh, mar, alo := facts["shelley"], facts["mary"], facts["alonzo"]
	coldAt := indexOf(t, sh.CBOR, sh.coldSig)
	out = append(out,
		env("praos_header_envelope_tpraos_truncated", "a TPraos header cut short does not decode", sh, sh.CBOR[:100], 1100, 90112, nil, false),
		env("praos_header_envelope_tpraos_zero_bounds", "zero bounds reject on the header first", sh, sh.CBOR, 0, 0, nil, false),
	)
	vrfCase("praos_vrf_check_tpraos_wrong_nonce", "a different epoch nonce: the nonce proof fails first", sh, sh.CBOR, rep(1, 32), blake256(sh.vrfKey), nil)
	vrfCase("praos_vrf_check_tpraos_wrong_key_hash", "registered key hash differs from the header's key", sh, sh.CBOR, sh.EpochNonce, rep(3, 32), nil)
	vrfCase("praos_vrf_check_tpraos_key_hash_first", "both wrong: the key hash is reported, as it is checked first", sh, sh.CBOR, rep(1, 32), rep(3, 32), nil)
	vrfCase("praos_vrf_check_tpraos_tampered_nonce_proof", "one bit of the nonce VRF proof flipped", mar, flipAt(mar.CBOR, indexOf(t, mar.CBOR, mar.nonceProof)+5), mar.EpochNonce, blake256(mar.vrfKey), nil)
	vrfCase("praos_vrf_check_tpraos_tampered_leader_proof", "one bit of the leader VRF proof flipped: the nonce proof still verifies", alo, flipAt(alo.CBOR, indexOf(t, alo.CBOR, alo.leaderProof)+5), alo.EpochNonce, blake256(alo.vrfKey), nil)
	vrfCase("praos_vrf_check_tpraos_both_proofs_tampered", "both proofs tampered: the nonce proof is reported", alo,
		flipAt(flipAt(alo.CBOR, indexOf(t, alo.CBOR, alo.leaderProof)+5), indexOf(t, alo.CBOR, alo.nonceProof)+5), alo.EpochNonce, blake256(alo.vrfKey), nil)
	vrfCase("praos_vrf_check_tpraos_truncated", "a TPraos header cut short does not decode", sh, sh.CBOR[:200], sh.EpochNonce, blake256(sh.vrfKey), nil)

	// Overlay: the shelley header is at slot 16156972, 172972 into a 432000-slot
	// epoch. d = 1 and f = 1 make every slot an active overlay slot whose key
	// is position mod 3 = 1 of the three sorted genesis keys.
	genesis := func(delegate, vrfHash []byte) []*v1.GenesisDelegation {
		ks := []*v1.GenesisDelegation{
			{GenesisKeyHash: rep(1, 28), DelegateKeyHash: rep(0xd1, 28), VrfKeyHash: rep(0xe1, 32)},
			{GenesisKeyHash: rep(2, 28), DelegateKeyHash: delegate, VrfKeyHash: vrfHash},
			{GenesisKeyHash: rep(3, 28), DelegateKeyHash: rep(0xd3, 28), VrfKeyHash: rep(0xe3, 32)},
		}
		return ks
	}
	ov := func(dn, dd, fn, fd string, length uint64, ks []*v1.GenesisDelegation) *v1.TpraosOverlayEnv {
		return &v1.TpraosOverlayEnv{D: rat(dn, dd), ActiveSlotCoeff: rat(fn, fd), EpochLength: length, GenesisDelegations: ks}
	}
	delegate, vrfHash := blake224(sh.issuer), blake256(sh.vrfKey)
	pool := rep(9, 32)
	vrfCase("praos_vrf_check_tpraos_overlay_scheduled_delegate", "an overlay slot whose genesis delegation names the header's issuer and VRF key", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "1", 432000, genesis(delegate, vrfHash)))
	vrfCase("praos_vrf_check_tpraos_overlay_wrong_delegate", "the slot's genesis delegate is another key: WrongGenesisColdKeyOVERLAY", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "1", 432000, genesis(rep(0xaa, 28), vrfHash)))
	vrfCase("praos_vrf_check_tpraos_overlay_wrong_vrf_hash", "the slot's genesis VRF key hash is another key's: WrongGenesisVRFKeyOVERLAY", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "1", 432000, genesis(delegate, rep(0xaa, 32))))
	vrfCase("praos_vrf_check_tpraos_overlay_both_wrong", "delegate and VRF key hash both wrong: both are reported", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "1", 432000, genesis(rep(0xaa, 28), rep(0xbb, 32))))
	vrfCase("praos_vrf_check_tpraos_overlay_other_key_slot", "two genesis keys put the slot with key 0, which holds another delegation", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "1", 432000, genesis(delegate, vrfHash)[:2]))
	vrfCase("praos_vrf_check_tpraos_overlay_non_active", "f = 1/20 leaves position 172972 (mod 20 = 12) an overlay slot nobody produces", sh, sh.CBOR, sh.EpochNonce, pool, ov("1", "1", "1", "20", 432000, genesis(delegate, vrfHash)))
	vrfCase("praos_vrf_check_tpraos_overlay_not_overlay_slot", "d = 1/2 in a 432001-slot epoch: relative slot 172935 is odd and not an overlay slot, so the pool registration decides", sh, sh.CBOR, sh.EpochNonce, vrfHash, ov("1", "2", "1", "1", 432001, genesis(delegate, vrfHash)))
	vrfCase("praos_vrf_check_tpraos_overlay_leader_threshold_unsupported", "outside the overlay schedule f < 1 would add the leader threshold, which the request cannot express", sh, sh.CBOR, sh.EpochNonce, vrfHash, ov("1", "2", "1", "20", 432001, genesis(delegate, vrfHash)))

	kesCase("praos_kes_check_tpraos_tampered_cold_signature", "the certificate is part of the KES-signed body: both signature checks fail", sh, flipAt(sh.CBOR, coldAt), spkp, maxEvo)
	both := flipAt(flipAt(sh.CBOR, coldAt), len(sh.CBOR)-1)
	kesCase("praos_kes_check_tpraos_window_and_signatures", "OCERT records every failure: the expired window does not stop the signature checks", sh, both, spkp, evolution["shelley"])
	kesCase("praos_kes_check_tpraos_before_start", "one million slots per period puts the header before its certificate's start period", sh, sh.CBOR, 1_000_000, maxEvo)
	kesCase("praos_kes_check_tpraos_wrong_period_length", "129000 slots per period: another evolution, the KES signature fails", mar, mar.CBOR, 129000, maxEvo)
	kesCase("praos_kes_check_tpraos_truncated", "a TPraos header cut short does not decode", mar, mar.CBOR[:300], spkp, maxEvo)

	opcert("praos_opcert_check_tpraos_skipped_counter", "mary carries counter 5: with 0 recorded, TPraos has no upper bound", mar, mar.CBOR, u64p(0), false, false)
	opcert("praos_opcert_check_tpraos_genesis_delegate_unseen", "no counter recorded, issuer a genesis delegate: counted from zero", mar, mar.CBOR, nil, false, true)
	opcert("praos_opcert_check_tpraos_tampered_kes_and_counter", "the counter check runs whatever the KES check found: both reported", alo, flipAt(alo.CBOR, len(alo.CBOR)-1), u64p(2), true, false)
	opcert("praos_opcert_check_tpraos_truncated", "a TPraos header cut short does not decode", mar, mar.CBOR[:300], u64p(5), true, false)

	// Envelope positions, parent = the block the header names as previous.
	tip := func(h tpFacts, dNo, dSlot int64, hash []byte) *v1.EnvelopeTip {
		return &v1.EnvelopeTip{BlockNo: uint64(int64(h.blockNo) - 1 + dNo), Slot: uint64(int64(h.slot) - 1 + dSlot), Hash: hash}
	}
	for _, h := range []tpFacts{sh, alo} {
		n := h.Name
		out = append(out,
			env("praos_header_envelope_position_"+n+"_consecutive", "parent is the previous block: number + 1, a later slot, matching hash", h, h.CBOR, 1100, 90112, tip(h, 0, 0, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_same_number", "block number equals the parent's: UnexpectedBlockNo", h, h.CBOR, 1100, 90112, tip(h, 1, 0, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_number_gap", "block number two above the parent's", h, h.CBOR, 1100, 90112, tip(h, -1, 0, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_same_slot", "slot equals the parent's: UnexpectedSlotNo", h, h.CBOR, 1100, 90112, tip(h, 0, 1, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_slot_before_parent", "parent slot after the header's", h, h.CBOR, 1100, 90112, tip(h, 0, 5, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_other_hash", "the parent's hash is not the header's previous hash: UnexpectedPrevHash", h, h.CBOR, 1100, 90112, tip(h, 0, 0, flipAt(h.prevHash, 31)), false),
			env("praos_header_envelope_position_"+n+"_number_before_slot", "number and slot both wrong: the number is reported", h, h.CBOR, 1100, 90112, tip(h, 1, 1, h.prevHash), false),
			env("praos_header_envelope_position_"+n+"_slot_before_hash", "slot and hash both wrong: the slot is reported", h, h.CBOR, 1100, 90112, tip(h, 0, 1, flipAt(h.prevHash, 0)), false),
			env("praos_header_envelope_position_"+n+"_before_sizes", "position failure and size failure: the position is reported", h, h.CBOR, 0, 0, tip(h, 1, 0, h.prevHash), false),
		)
	}
	// Praos headers extend a parent the same way.
	for _, name := range []string{"babbage_m8", "conway_a"} {
		hd := praosfixtures.ByName(name)
		prev, no, slot := praosHeaderPosition(t, hd)
		h := tpFacts{Header: hd, blockNo: no, slot: slot, prevHash: prev}
		out = append(out,
			env("praos_header_envelope_position_"+name+"_consecutive", "Praos header on its parent", h, hd.CBOR, 1100, 90112, tip(h, 0, 0, prev), false),
			env("praos_header_envelope_position_"+name+"_same_number", "block number equals the parent's: UnexpectedBlockNo", h, hd.CBOR, 1100, 90112, tip(h, 1, 0, prev), false),
			env("praos_header_envelope_position_"+name+"_same_slot", "slot equals the parent's: UnexpectedSlotNo", h, hd.CBOR, 1100, 90112, tip(h, 0, 1, prev), false),
			env("praos_header_envelope_position_"+name+"_other_hash", "the parent's hash is not the previous hash: UnexpectedPrevHash", h, hd.CBOR, 1100, 90112, tip(h, 0, 0, flipAt(prev, 31)), false),
		)
	}
	ca := praosfixtures.ByName("conway_a")
	cprev, cno, cslot := praosHeaderPosition(t, ca)
	cf := tpFacts{Header: ca, blockNo: cno, slot: cslot, prevHash: cprev}
	out = append(out,
		env("praos_header_envelope_position_parent_max_slot", "a parent slot of 2^64-1 has no successor", cf, ca.CBOR, 1100, 90112,
			&v1.EnvelopeTip{BlockNo: cno - 1, Slot: ^uint64(0), Hash: cprev}, false),
	)
	return out
}

// praosHeaderPosition reads a Praos header's previous hash, block number and slot.
func praosHeaderPosition(t *testing.T, h praosfixtures.Header) (prev []byte, no, slot uint64) {
	t.Helper()
	bt := ledger.BlockTypeConway
	if h.Era == v1.Era_ERA_BABBAGE {
		bt = ledger.BlockTypeBabbage
	}
	hdr, err := ledger.NewBlockHeaderFromCbor(uint(bt), h.CBOR)
	if err != nil {
		t.Fatal(err)
	}
	p := hdr.PrevHash()
	return p.Bytes(), hdr.BlockNumber(), hdr.SlotNumber()
}

// tpraosDivergenceCases are known mismatches: the envelope's position check
// against an empty chain.
func tpraosDivergenceCases(t *testing.T) []entry {
	sh := tpFactsOf(t, "shelley", 2)
	ca := praosfixtures.ByName("conway_a")
	call := func(era v1.Era, header []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_PraosHeaderEnvelope{PraosHeaderEnvelope: &v1.PraosHeaderEnvelopeRequest{
			Era: era, Header: header, Pparams: pparamsLimits(1100, 90112), Origin: true,
		}}}
	}
	return []entry{
		{"praos_header_envelope_origin_shelley", "reference: a header on the empty chain needs block number 0 and the genesis hash; dingo checks nothing without a parent (TestDivergenceOriginHasNoPositionCheck)", call(sh.Era, sh.CBOR)},
		{"praos_header_envelope_origin_conway_a", "the same for a Conway header (TestDivergenceOriginHasNoPositionCheck)", call(ca.Era, ca.CBOR)},
	}
}
