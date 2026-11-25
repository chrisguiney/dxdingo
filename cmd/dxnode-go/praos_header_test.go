package main

import (
	"bytes"
	"context"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/praosfixtures"
)

func rejName(r *v1.Rejection) string {
	if len(r.GetFailures()) != 1 {
		return ""
	}
	return r.GetFailures()[0].GetName()
}

func b2b256(parts ...[]byte) []byte {
	h, _ := blake2b.New256(nil)
	for _, p := range parts {
		h.Write(p)
	}
	return h.Sum(nil)
}

func b2b224(b []byte) []byte {
	h, _ := blake2b.New(28, nil)
	h.Write(b)
	return h.Sum(nil)
}

func u32Ptr(n uint32) *uint32 { return &n }

// Fixture facts, read from the headers' CBOR:
//
//	conway_a   header 859 bytes, declared body 50666, slot 135747596, counter 10, KES start 1046
//	babbage_m8 header 859 bytes, slot 129124527, counter 18, KES start 975
//
// With 129600 slots per KES period, babbage_m8 is in period 996, so its KES
// evolution is 996 - 975 = 21.

// Shelley.Protocol.EnvelopeChecks.envelopeCheck: header size, then body size,
// each accepted when equal to its bound.
func TestHeaderEnvelope(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("conway_a")
	for name, tc := range map[string]struct {
		maxHeader, maxBody uint32
		want               string
	}{
		"both at the bound":         {859, 50666, ""},
		"header one over":           {858, 90112, "HeaderSizeTooLarge"},
		"body one over":             {1100, 50665, "BlockSizeTooLarge"},
		"both over, header first":   {858, 50665, "HeaderSizeTooLarge"},
		"mainnet limits":            {1100, 90112, ""},
		"zero limits, header first": {0, 0, "HeaderSizeTooLarge"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{
				Era: h.Era, Header: h.CBOR,
				Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(tc.maxHeader), MaxBlockBodySize: u32Ptr(tc.maxBody)},
			})
			if err != nil {
				t.Fatal(err)
			}
			if rejName(got.GetRejection()) != tc.want {
				t.Fatalf("rejection = %v, want %q", got.GetRejection(), tc.want)
			}
		})
	}
	for name, req := range map[string]*v1.PraosHeaderEnvelopeRequest{
		"truncated header": {Era: h.Era, Header: h.CBOR[:100], Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(1100), MaxBlockBodySize: u32Ptr(90112)}},
	} {
		got, err := headerEnvelope(context.Background(), req)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		if rejName(got.GetRejection()) != "DeserialiseFailure" {
			t.Fatalf("%s: rejection = %v, want DeserialiseFailure", name, got.GetRejection())
		}
	}
	_, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{Era: h.Era, Header: h.CBOR})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("missing limits: err = %v, want BAD_REQUEST", err)
	}
	// The reference holds the header bound as a Word16.
	_, err = headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{
		Era: h.Era, Header: h.CBOR, Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(65536), MaxBlockBodySize: u32Ptr(1)},
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("header bound above Word16: err = %v, want UNSUPPORTED", err)
	}
	_, err = headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{
		Era: v1.Era_ERA_BYRON, Header: h.CBOR, Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(1), MaxBlockBodySize: u32Ptr(1)},
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("byron: err = %v, want UNSUPPORTED", err)
	}
}

// validateVRFSignature: key hash against the registered one, then the VRF
// proof over blake2b_256(slot_be64 || epoch nonce). The leader value is
// blake2b_256("L" || output) and the nonce value blake2b_256(blake2b_256("N"
// || output)).
func TestVrfCheck(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("conway_a")
	vrfKeyHash := vrfKeyHashOf(t, h)
	run := func(nonce, keyHash []byte) *v1.PraosVrfCheckResponse {
		got, err := vrfCheck(context.Background(), &v1.PraosVrfCheckRequest{
			Era: h.Era, Header: h.CBOR, EpochNonce: &v1.Nonce{Hash: nonce}, PoolVrfKeyHash: keyHash,
		})
		if err != nil {
			t.Fatal(err)
		}
		return got
	}
	ok := run(h.EpochNonce, vrfKeyHash)
	if ok.GetRejection() != nil {
		t.Fatalf("valid header rejected: %v", ok.GetRejection())
	}
	out := vrfOutputOf(t, h)
	if want := b2b256([]byte("L"), out); !bytes.Equal(ok.GetLeaderValue(), want) {
		t.Fatalf("leader value = %x, want %x", ok.GetLeaderValue(), want)
	}
	if want := b2b256(b2b256([]byte("N"), out)); !bytes.Equal(ok.GetNonceValue(), want) {
		t.Fatalf("nonce value = %x, want %x", ok.GetNonceValue(), want)
	}
	if got := rejName(run(bytes.Repeat([]byte{1}, 32), vrfKeyHash).GetRejection()); got != "VRFKeyBadProof" {
		t.Fatalf("wrong nonce: %q, want VRFKeyBadProof", got)
	}
	if got := rejName(run(h.EpochNonce, bytes.Repeat([]byte{2}, 32)).GetRejection()); got != "VRFKeyWrongVRFKey" {
		t.Fatalf("wrong key hash: %q, want VRFKeyWrongVRFKey", got)
	}
	// The key hash is checked first, so both wrong reports only the key.
	if got := rejName(run(bytes.Repeat([]byte{1}, 32), bytes.Repeat([]byte{2}, 32)).GetRejection()); got != "VRFKeyWrongVRFKey" {
		t.Fatalf("both wrong: %q, want VRFKeyWrongVRFKey", got)
	}
	_, err := vrfCheck(context.Background(), &v1.PraosVrfCheckRequest{
		Era: h.Era, Header: h.CBOR, EpochNonce: &v1.Nonce{}, PoolVrfKeyHash: vrfKeyHash,
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("neutral epoch nonce: err = %v, want UNSUPPORTED", err)
	}
}

// doValidateKESSignature: the KES period window [c0, c0 + maxEvo), the
// operational certificate's cold signature, then the KES signature at
// evolution kp - c0.
func TestKesCheck(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("babbage_m8")
	run := func(header []byte, spkp, maxEvo uint64) *v1.PraosKesCheckResponse {
		got, err := kesCheck(context.Background(), &v1.PraosKesCheckRequest{
			Era: h.Era, Header: header, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
		})
		if err != nil {
			t.Fatal(err)
		}
		return got
	}
	ok := run(h.CBOR, praosfixtures.SlotsPerKESPeriod, praosfixtures.MaxKESEvolutions)
	if ok.GetRejection() != nil || ok.GetKesPeriod() != 21 {
		t.Fatalf("valid header: rejection %v, evolution %d, want none and 21", ok.GetRejection(), ok.GetKesPeriod())
	}
	if got := run(h.CBOR, praosfixtures.SlotsPerKESPeriod, 22); got.GetRejection() != nil {
		t.Fatalf("maxEvo 22 (996 < 975 + 22): %v", got.GetRejection())
	}
	for name, tc := range map[string]struct {
		header       []byte
		spkp, maxEvo uint64
		want         string
	}{
		"evolution equals max":                   {h.CBOR, praosfixtures.SlotsPerKESPeriod, 21, "KESAfterEndOCERT"},
		"period before start":                    {h.CBOR, 135000, 62, "KESBeforeStartOCERT"},
		"tampered cold signature":                {flipCold(t, h.CBOR), praosfixtures.SlotsPerKESPeriod, 62, "InvalidSignatureOCERT"},
		"tampered KES signature":                 {flipByte(h.CBOR, len(h.CBOR)-1), praosfixtures.SlotsPerKESPeriod, 62, "InvalidKesSignatureOCERT"},
		"wrong evolution (period 1000, not 996)": {h.CBOR, 129000, 62, "InvalidKesSignatureOCERT"},
		"window beats cold sig":                  {flipCold(t, h.CBOR), praosfixtures.SlotsPerKESPeriod, 21, "KESAfterEndOCERT"},
		"cold sig beats KES sig":                 {flipByte(flipCold(t, h.CBOR), len(h.CBOR)-1), praosfixtures.SlotsPerKESPeriod, 62, "InvalidSignatureOCERT"},
	} {
		if got := rejName(run(tc.header, tc.spkp, tc.maxEvo).GetRejection()); got != tc.want {
			t.Errorf("%s: rejection %q, want %q", name, got, tc.want)
		}
	}
	_, err := kesCheck(context.Background(), &v1.PraosKesCheckRequest{Era: h.Era, Header: h.CBOR, MaxKesEvolutions: 62})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("zero slots per KES period: err = %v, want UNSUPPORTED", err)
	}
}

// The operational certificate counter, conway_a carries counter 10: m <= n
// and n <= m + 1 against the last counter m, with an unseen registered pool at
// m = 0 and an unseen unregistered pool refused.
func TestOpcertCheck(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("conway_a")
	one := func(n uint64) *uint64 { return &n }
	run := func(header []byte, last *uint64, registered bool, protocol uint32) (*v1.PraosOpCertCheckResponse, error) {
		return opcertCheck(context.Background(), &v1.PraosOpCertCheckRequest{
			ProtocolVersion: &v1.ProtocolVersion{Major: protocol}, Header: header, LastCounter: last, PoolRegistered: registered,
			SlotsPerKesPeriod: praosfixtures.SlotsPerKESPeriod, MaxKesEvolutions: praosfixtures.MaxKESEvolutions,
		})
	}
	for name, tc := range map[string]struct {
		last       *uint64
		registered bool
		want       string
		newCounter uint64
	}{
		"same counter":            {one(10), true, "", 10},
		"next counter":            {one(9), false, "", 10},
		"skips one":               {one(8), true, "CounterOverIncrementedOCERT", 0},
		"lower than last":         {one(11), true, "CounterTooSmallOCERT", 0},
		"unseen pool, registered": {nil, true, "CounterOverIncrementedOCERT", 0},
		"unseen pool, unknown":    {nil, false, "NoCounterForKeyHashOCERT", 0},
		"registered flag ignored when a counter exists": {one(10), false, "", 10},
	} {
		got, err := run(h.CBOR, tc.last, tc.registered, 10)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		if rejName(got.GetRejection()) != tc.want || got.GetNewCounter() != tc.newCounter {
			t.Errorf("%s: rejection %v new counter %d, want %q and %d", name, got.GetRejection(), got.GetNewCounter(), tc.want, tc.newCounter)
		}
	}
	// The signatures are checked before the counter.
	got, err := run(flipByte(h.CBOR, len(h.CBOR)-1), one(8), true, 10)
	if err != nil || rejName(got.GetRejection()) != "InvalidKesSignatureOCERT" {
		t.Fatalf("tampered KES signature: %v %v", got.GetRejection(), err)
	}
	// Versions 2 to 6 select the TPraos header formats and are covered by the
	// TPraos tests; version 1 has no header format.
	if _, err := run(h.CBOR, one(10), true, 1); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("protocol version before Shelley: err = %v, want UNSUPPORTED", err)
	}
}

// doValidateVRFSignature looks the issuer up in the pool distribution by the
// hash of its cold key, then compares the registered VRF key hash.
func TestVrfKeyRegistered(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("conway_a")
	pool := b2b224(issuerOf(t, h))
	vrfHash := vrfKeyHashOf(t, h)
	other := bytes.Repeat([]byte{9}, 28)
	run := func(dist ...*v1.PoolStake) *v1.PraosVrfKeyRegisteredResponse {
		got, err := vrfKeyRegistered(context.Background(), &v1.PraosVrfKeyRegisteredRequest{Header: h.CBOR, PoolDistribution: dist})
		if err != nil {
			t.Fatal(err)
		}
		return got
	}
	ok := run(&v1.PoolStake{PoolId: other, VrfKeyHash: vrfHash}, &v1.PoolStake{PoolId: pool, VrfKeyHash: vrfHash})
	if ok.GetRejection() != nil || !bytes.Equal(ok.GetPoolId(), pool) {
		t.Fatalf("registered: rejection %v pool %x, want none and %x", ok.GetRejection(), ok.GetPoolId(), pool)
	}
	if got := rejName(run(&v1.PoolStake{PoolId: other, VrfKeyHash: vrfHash}).GetRejection()); got != "VRFKeyUnknown" {
		t.Fatalf("unknown pool: %q", got)
	}
	if got := rejName(run().GetRejection()); got != "VRFKeyUnknown" {
		t.Fatalf("empty distribution: %q", got)
	}
	if got := rejName(run(&v1.PoolStake{PoolId: pool, VrfKeyHash: bytes.Repeat([]byte{1}, 32)}).GetRejection()); got != "VRFKeyWrongVRFKey" {
		t.Fatalf("wrong key hash: %q", got)
	}
}

// Evolving nonce: eta_v ⭒ nonce, where Nonce a ⭒ Nonce b = Nonce
// (blake2b_256 (a <> b)). Praos range-extends the VRF output (vrfNonceValue =
// blake2b_256 (blake2b_256 ("N" <> out))); TPraos hashes it once
// (mkNonceFromOutputVRF = blake2b_256 out).
func TestNonceRolling(t *testing.T) {
	t.Parallel()
	ev := bytes.Repeat([]byte{0xaa}, 32)
	out := bytes.Repeat([]byte{0x11}, 64)
	praos := b2b256(ev, b2b256(b2b256([]byte("N"), out)))
	tpraos := b2b256(ev, b2b256(out))
	for era, want := range map[v1.Era][]byte{
		v1.Era_ERA_SHELLEY: tpraos, v1.Era_ERA_ALLEGRA: tpraos, v1.Era_ERA_MARY: tpraos, v1.Era_ERA_ALONZO: tpraos,
		v1.Era_ERA_BABBAGE: praos, v1.Era_ERA_CONWAY: praos,
	} {
		got, err := nonceRolling(context.Background(), &v1.PraosNonceRollingRequest{Era: era, Evolving: &v1.Nonce{Hash: ev}, VrfOutput: out})
		if err != nil {
			t.Fatalf("%v: %v", era, err)
		}
		if !bytes.Equal(got.GetEvolving().GetHash(), want) {
			t.Errorf("%v: evolving %x, want %x", era, got.GetEvolving().GetHash(), want)
		}
	}
	for name, tc := range map[string]*v1.PraosNonceRollingRequest{
		"byron":            {Era: v1.Era_ERA_BYRON, Evolving: &v1.Nonce{Hash: ev}, VrfOutput: out},
		"short vrf output": {Era: v1.Era_ERA_CONWAY, Evolving: &v1.Nonce{Hash: ev}, VrfOutput: out[:32]},
		"short evolving":   {Era: v1.Era_ERA_CONWAY, Evolving: &v1.Nonce{Hash: ev[:7]}, VrfOutput: out},
	} {
		_, err := nonceRolling(context.Background(), tc)
		if c := dispatch.Code(err); c != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED && c != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
			t.Errorf("%s: err = %v, want UNSUPPORTED or BAD_REQUEST", name, err)
		}
	}
}

// Candidate nonce: the candidate follows the evolving nonce for a block whose
// slot + window < first slot of the next epoch and stays put otherwise. With
// k = 10 and f = 1/2 the window is 3k/f = 60 before Conway and 4k/f = 80 from
// Conway; for the epoch [1000, 1100) the cutoffs are 1040 and 1020.
func TestNonceCandidate(t *testing.T) {
	t.Parallel()
	ev0 := bytes.Repeat([]byte{0xaa}, 32)
	cand0 := bytes.Repeat([]byte{0xcc}, 32)
	outs := [][]byte{bytes.Repeat([]byte{1}, 64), bytes.Repeat([]byte{2}, 64), bytes.Repeat([]byte{3}, 64), bytes.Repeat([]byte{4}, 64)}
	slots := []uint64{1010, 1039, 1040, 1099}
	run := func(era v1.Era, n int) *v1.PraosNonceCandidateResponse {
		var blocks []*v1.BlockNonceInput
		for i := range n {
			blocks = append(blocks, &v1.BlockNonceInput{Slot: slots[i], VrfOutput: outs[i]})
		}
		got, err := nonceCandidate(context.Background(), &v1.PraosNonceCandidateRequest{
			Era: era, Evolving: &v1.Nonce{Hash: ev0}, Candidate: &v1.Nonce{Hash: cand0},
			EpochFirstSlot: 1000, NextEpochFirstSlot: 1100, SecurityParam: 10, ActiveSlotCoeff: rat("1", "2"), Blocks: blocks,
		})
		if err != nil {
			t.Fatalf("%v: %v", era, err)
		}
		return got
	}
	// TPraos contribution: blake2b_256 out; Praos: blake2b_256 (blake2b_256 ("N" <> out)).
	tp := func(o []byte) []byte { return b2b256(o) }
	pr := func(o []byte) []byte { return b2b256(b2b256([]byte("N"), o)) }
	evolve := func(contrib func([]byte) []byte) [][]byte {
		prev := ev0
		var all [][]byte
		for _, o := range outs {
			prev = b2b256(prev, contrib(o))
			all = append(all, prev)
		}
		return all
	}
	etp, epr := evolve(tp), evolve(pr)
	for name, tc := range map[string]struct {
		era  v1.Era
		n    int
		evo  []byte
		cand []byte
	}{
		"no blocks": {v1.Era_ERA_SHELLEY, 0, ev0, cand0},
		"tpraos, window 60: block 1039 moves it, 1040 freezes": {v1.Era_ERA_SHELLEY, 4, etp[3], etp[1]},
		"tpraos, one block":             {v1.Era_ERA_ALONZO, 1, etp[0], etp[0]},
		"babbage keeps 3k/f":            {v1.Era_ERA_BABBAGE, 4, epr[3], epr[1]},
		"conway uses 4k/f, cutoff 1020": {v1.Era_ERA_CONWAY, 4, epr[3], epr[0]},
		"conway before the cutoff":      {v1.Era_ERA_CONWAY, 1, epr[0], epr[0]},
	} {
		got := run(tc.era, tc.n)
		if !bytes.Equal(got.GetEvolving().GetHash(), tc.evo) || !bytes.Equal(got.GetCandidate().GetHash(), tc.cand) {
			t.Errorf("%s: evolving %x candidate %x, want %x and %x", name, got.GetEvolving().GetHash(), got.GetCandidate().GetHash(), tc.evo, tc.cand)
		}
	}
	_, err := nonceCandidate(context.Background(), &v1.PraosNonceCandidateRequest{
		Era: v1.Era_ERA_CONWAY, Evolving: &v1.Nonce{Hash: ev0}, Candidate: &v1.Nonce{Hash: cand0},
		EpochFirstSlot: 1000, NextEpochFirstSlot: 1100, SecurityParam: 10, ActiveSlotCoeff: rat("1", "2"),
		Blocks: []*v1.BlockNonceInput{{Slot: 1100, VrfOutput: outs[0]}},
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("slot outside the epoch: err = %v, want UNSUPPORTED", err)
	}
}

func decode(t *testing.T, h praosfixtures.Header) *praosHeader {
	t.Helper()
	ph, err := decodePraosHeader(h.Era, h.CBOR)
	if err != nil {
		t.Fatal(err)
	}
	return ph
}

func vrfKeyHashOf(t *testing.T, h praosfixtures.Header) []byte {
	return b2b256(decode(t, h).vrfKey)
}

func vrfOutputOf(t *testing.T, h praosfixtures.Header) []byte { return decode(t, h).vrfOutput }

func issuerOf(t *testing.T, h praosfixtures.Header) []byte { return decode(t, h).issuer }

func flipByte(b []byte, i int) []byte {
	c := bytes.Clone(b)
	c[i] ^= 1
	return c
}

// flipCold flips a bit of the operational certificate's cold signature, which
// leaves the KES-signed header body's other fields intact.
func flipCold(t *testing.T, header []byte) []byte {
	t.Helper()
	hdr, err := ledger.NewBlockHeaderFromCbor(ledger.BlockTypeBabbage, header)
	if err != nil {
		t.Fatal(err)
	}
	oc, err := ledger.ExtractOpCertFromHeader(hdr)
	if err != nil {
		t.Fatal(err)
	}
	i := bytes.Index(header, oc.ColdSignature)
	if i < 0 {
		t.Fatal("cold signature not found in header")
	}
	return flipByte(header, i)
}
