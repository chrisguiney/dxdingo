package main

import (
	"bytes"
	"context"
	"slices"
	"sort"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/praosfixtures"
)

// TPraos headers (Shelley to Alonzo) are mainnet headers. Facts read from
// their CBOR, with 129600 slots per KES period (evolution = slot / 129600 -
// start period):
//
//	shelley  block 5065327 slot 16156972 start 97  evolution 27 counter 0 body 2201 header 1005
//	allegra  block 5406740 slot 23068573 start 174 evolution 3  counter 1 body 5350 header 1005
//	mary     block 6236051 slot 39916670 start 267 evolution 40 counter 5 body 4114 header 1006
//	alonzo   block 7791695 slot 72316767 start 547 evolution 10 counter 1 body 7252 header 1006
type tpraosFact struct {
	name                 string
	major                uint32
	blockNo, slot        uint64
	evolution, counter   uint64
	bodySize, headerSize uint32
}

var tpraosFacts = []tpraosFact{
	{"shelley", 2, 5065327, 16156972, 27, 0, 2201, 1005},
	{"allegra", 3, 5406740, 23068573, 3, 1, 5350, 1005},
	{"mary", 4, 6236051, 39916670, 40, 5, 4114, 1006},
	{"alonzo", 5, 7791695, 72316767, 10, 1, 7252, 1006},
}

// ruleNames lists a rejection as sorted "Rule/Name" strings.
func ruleNames(r *v1.Rejection) []string {
	var out []string
	for _, f := range r.GetFailures() {
		out = append(out, f.GetRule()+"/"+f.GetName())
	}
	sort.Strings(out)
	return out
}

func wantRules(t *testing.T, label string, r *v1.Rejection, want ...string) {
	t.Helper()
	sort.Strings(want)
	if got := ruleNames(r); !slices.Equal(got, want) {
		t.Errorf("%s: rejection = %v, want %v", label, got, want)
	}
}

func flipColdOf(t *testing.T, h praosfixtures.Header) []byte {
	t.Helper()
	oc, err := ledger.ExtractOpCertFromHeader(decode(t, h).hdr)
	if err != nil {
		t.Fatal(err)
	}
	i := bytes.Index(h.CBOR, oc.ColdSignature)
	if i < 0 {
		t.Fatal("cold signature not found in header")
	}
	return flipByte(h.CBOR, i)
}

func mustOK[T any](t *testing.T, label string, got T, err error) T {
	t.Helper()
	if err != nil {
		t.Fatalf("%s: %v", label, err)
	}
	return got
}

// The header facts above, as the decoder reads them.
func TestTPraosDecode(t *testing.T) {
	t.Parallel()
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		d := decode(t, h)
		if !d.tpraos || d.blockNo != f.blockNo || d.slot != f.slot || len(h.CBOR) != int(f.headerSize) {
			t.Errorf("%s: tpraos=%v block=%d slot=%d len=%d", f.name, d.tpraos, d.blockNo, d.slot, len(h.CBOR))
		}
		if len(d.nonceProof) != 80 || len(d.nonceOutput) != 64 || len(d.vrfProof) != 80 || len(d.vrfOutput) != 64 {
			t.Errorf("%s: VRF certificate sizes %d/%d %d/%d", f.name, len(d.nonceProof), len(d.nonceOutput), len(d.vrfProof), len(d.vrfOutput))
		}
	}
}

// envelopeCheck on a TPraos header: header size against its bound, then the
// declared body size against its bound, each accepted at equality.
func TestTPraosHeaderEnvelope(t *testing.T) {
	t.Parallel()
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		run := func(maxH, maxB uint32) *v1.Rejection {
			got, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{
				Era: h.Era, Header: h.CBOR, Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(maxH), MaxBlockBodySize: u32Ptr(maxB)},
			})
			return mustOK(t, f.name, got, err).GetRejection()
		}
		if r := run(f.headerSize, f.bodySize); r != nil {
			t.Errorf("%s at both bounds: %v", f.name, r)
		}
		wantRules(t, f.name+" header one over", run(f.headerSize-1, f.bodySize), "Envelope/HeaderSizeTooLarge")
		wantRules(t, f.name+" body one over", run(f.headerSize, f.bodySize-1), "Envelope/BlockSizeTooLarge")
		wantRules(t, f.name+" both over", run(0, 0), "Envelope/HeaderSizeTooLarge")
	}
}

// validateEnvelope: block number, slot, previous hash, in that order. The
// parent is the block the header's previous hash names.
func TestHeaderEnvelopePosition(t *testing.T) {
	t.Parallel()
	for _, name := range []string{"shelley", "alonzo", "babbage_m8", "conway_a"} {
		h := praosfixtures.ByName(name)
		d := decode(t, h)
		limits := &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(1100), MaxBlockBodySize: u32Ptr(90112)}
		run := func(parent *v1.EnvelopeTip) *v1.Rejection {
			got, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{Era: h.Era, Header: h.CBOR, Pparams: limits, Parent: parent})
			return mustOK(t, name, got, err).GetRejection()
		}
		tip := func(dNo, dSlot uint64, hash []byte) *v1.EnvelopeTip {
			return &v1.EnvelopeTip{BlockNo: d.blockNo - 1 + dNo, Slot: d.slot - 1 + dSlot, Hash: hash}
		}
		if r := run(tip(0, 0, d.prevHash)); r != nil {
			t.Errorf("%s: consecutive parent rejected: %v", name, r)
		}
		wantRules(t, name+" same block number", run(tip(1, 0, d.prevHash)), "Envelope/UnexpectedBlockNo")
		wantRules(t, name+" gap in block numbers", run(tip(^uint64(0), 0, d.prevHash)), "Envelope/UnexpectedBlockNo")
		wantRules(t, name+" same slot", run(tip(0, 1, d.prevHash)), "Envelope/UnexpectedSlotNo")
		wantRules(t, name+" parent after the header", run(tip(0, 5, d.prevHash)), "Envelope/UnexpectedSlotNo")
		wantRules(t, name+" other parent hash", run(tip(0, 0, flipByte(d.prevHash, 31))), "Envelope/UnexpectedPrevHash")
		wantRules(t, name+" number before slot", run(tip(1, 1, flipByte(d.prevHash, 0))), "Envelope/UnexpectedBlockNo")
		wantRules(t, name+" slot before hash", run(tip(0, 1, flipByte(d.prevHash, 0))), "Envelope/UnexpectedSlotNo")
		// Position is checked before the sizes.
		got, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{
			Era: h.Era, Header: h.CBOR, Parent: tip(1, 0, d.prevHash),
			Pparams: &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(0), MaxBlockBodySize: u32Ptr(0)},
		})
		wantRules(t, name+" position before size", mustOK(t, name, got, err).GetRejection(), "Envelope/UnexpectedBlockNo")
	}
}

// kes: OCERT accumulates its failures: the period window, the certificate's
// cold signature and the KES signature (at evolution 0 when the header is
// before the certificate's start period) are each checked.
func TestTPraosKes(t *testing.T) {
	t.Parallel()
	const spkp, maxEvo = praosfixtures.SlotsPerKESPeriod, praosfixtures.MaxKESEvolutions
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		run := func(header []byte, spkp, maxEvo uint64) *v1.PraosKesCheckResponse {
			got, err := kesCheck(context.Background(), &v1.PraosKesCheckRequest{Era: h.Era, Header: header, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo})
			return mustOK(t, f.name, got, err)
		}
		ok := run(h.CBOR, spkp, maxEvo)
		if ok.GetRejection() != nil || ok.GetKesPeriod() != f.evolution {
			t.Errorf("%s: rejection %v evolution %d, want evolution %d", f.name, ok.GetRejection(), ok.GetKesPeriod(), f.evolution)
		}
		if r := run(h.CBOR, spkp, f.evolution+1).GetRejection(); r != nil {
			t.Errorf("%s: a bound one above the evolution rejected: %v", f.name, r)
		}
		wantRules(t, f.name+" evolution equals the bound", run(h.CBOR, spkp, f.evolution).GetRejection(), "OCERT/KESAfterEndOCERT")
		// The certificate is part of the KES-signed header body, so a tampered
		// cold signature breaks the KES signature too.
		wantRules(t, f.name+" tampered cold signature", run(flipColdOf(t, h), spkp, maxEvo).GetRejection(), "OCERT/InvalidSignatureOCERT", "OCERT/InvalidKesSignatureOCERT")
		wantRules(t, f.name+" tampered KES signature", run(flipByte(h.CBOR, len(h.CBOR)-1), spkp, maxEvo).GetRejection(), "OCERT/InvalidKesSignatureOCERT")
		both := flipByte(flipColdOf(t, h), len(h.CBOR)-1)
		wantRules(t, f.name+" both signatures tampered", run(both, spkp, maxEvo).GetRejection(), "OCERT/InvalidSignatureOCERT", "OCERT/InvalidKesSignatureOCERT")
		// An expired window does not stop the signature checks.
		wantRules(t, f.name+" window and signatures", run(both, spkp, f.evolution).GetRejection(),
			"OCERT/KESAfterEndOCERT", "OCERT/InvalidSignatureOCERT", "OCERT/InvalidKesSignatureOCERT")
		// 1000000 slots per period puts every header in period 16..72, before
		// the certificate's start period.
		wantRules(t, f.name+" before the start period", run(h.CBOR, 1_000_000, maxEvo).GetRejection(), "OCERT/KESBeforeStartOCERT", "OCERT/InvalidKesSignatureOCERT")
	}
	_, err := kesCheck(context.Background(), &v1.PraosKesCheckRequest{Era: v1.Era_ERA_MARY, Header: praosfixtures.ByName("mary").CBOR, MaxKesEvolutions: 62})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Errorf("zero slots per period: %v, want UNSUPPORTED", err)
	}
}

// OCERT counter: m <= n, with no upper bound; with no counter recorded a
// registered pool or genesis delegate counts from zero and anything else has
// no counter.
func TestTPraosOpcert(t *testing.T) {
	t.Parallel()
	const spkp, maxEvo = praosfixtures.SlotsPerKESPeriod, praosfixtures.MaxKESEvolutions
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		run := func(header []byte, last *uint64, registered, genesis bool) *v1.PraosOpCertCheckResponse {
			got, err := opcertCheck(context.Background(), &v1.PraosOpCertCheckRequest{
				ProtocolVersion: &v1.ProtocolVersion{Major: f.major}, Header: header, LastCounter: last,
				PoolRegistered: registered, IssuerIsGenesisDelegate: genesis, SlotsPerKesPeriod: spkp, MaxKesEvolutions: maxEvo,
			})
			return mustOK(t, f.name, got, err)
		}
		n := f.counter
		if got := run(h.CBOR, &n, false, false); got.GetRejection() != nil || got.GetNewCounter() != n {
			t.Errorf("%s: same counter: %v new %d", f.name, got.GetRejection(), got.GetNewCounter())
		}
		if got := run(h.CBOR, nil, true, false); got.GetRejection() != nil || got.GetNewCounter() != n {
			t.Errorf("%s: unseen registered pool counts from zero: %v", f.name, got.GetRejection())
		}
		if n > 0 {
			below := n - 1
			if r := run(h.CBOR, &below, false, false).GetRejection(); r != nil {
				t.Errorf("%s: next counter: %v", f.name, r)
			}
			// Unlike Praos there is no upper bound: counter n over a baseline of 0.
			zero := uint64(0)
			if r := run(h.CBOR, &zero, false, false).GetRejection(); r != nil {
				t.Errorf("%s: a skipped counter is allowed under TPraos: %v", f.name, r)
			}
		}
		above := n + 1
		wantRules(t, f.name+" lower than the last", run(h.CBOR, &above, true, false).GetRejection(), "OCERT/CounterTooSmallOCERT")
		wantRules(t, f.name+" unseen, unregistered", run(h.CBOR, nil, false, false).GetRejection(), "OCERT/NoCounterForKeyHashOCERT")
		if r := run(h.CBOR, nil, false, true); r.GetRejection() != nil {
			t.Errorf("%s: unseen genesis delegate counts from zero: %v", f.name, r.GetRejection())
		}
		// The counter check runs whatever the KES check found.
		wantRules(t, f.name+" tampered KES and a counter too small",
			run(flipByte(h.CBOR, len(h.CBOR)-1), &above, true, false).GetRejection(), "OCERT/InvalidKesSignatureOCERT", "OCERT/CounterTooSmallOCERT")
	}
	_, err := opcertCheck(context.Background(), &v1.PraosOpCertCheckRequest{ProtocolVersion: &v1.ProtocolVersion{Major: 1}, Header: praosfixtures.ByName("mary").CBOR, SlotsPerKesPeriod: 1, MaxKesEvolutions: 1})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Errorf("protocol version 1: %v, want UNSUPPORTED", err)
	}
}

func genesisKeys(n int) []*v1.GenesisDelegation {
	var out []*v1.GenesisDelegation
	for i := 1; i <= n; i++ {
		out = append(out, &v1.GenesisDelegation{GenesisKeyHash: bytes.Repeat([]byte{byte(i)}, 28), DelegateKeyHash: bytes.Repeat([]byte{0xd0 + byte(i)}, 28), VrfKeyHash: bytes.Repeat([]byte{0xe0 + byte(i)}, 32)})
	}
	return out
}

// OVERLAY's VRF checks on TPraos headers: the registered key hash, the nonce
// VRF proof, then the leader VRF proof; the nonce value is the Blake2b-256 of
// the nonce VRF output and the leader value the leader VRF output.
func TestTPraosVrfCheck(t *testing.T) {
	t.Parallel()
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		d := decode(t, h)
		keyHash := b2b256(d.vrfKey)
		run := func(header, nonce, key []byte, overlay *v1.TpraosOverlayEnv) *v1.PraosVrfCheckResponse {
			got, err := vrfCheck(context.Background(), &v1.PraosVrfCheckRequest{Era: h.Era, Header: header, EpochNonce: &v1.Nonce{Hash: nonce}, PoolVrfKeyHash: key, Overlay: overlay})
			return mustOK(t, f.name, got, err)
		}
		ok := run(h.CBOR, h.EpochNonce, keyHash, nil)
		if ok.GetRejection() != nil {
			t.Fatalf("%s: valid header rejected: %v", f.name, ok.GetRejection())
		}
		if !bytes.Equal(ok.GetLeaderValue(), d.vrfOutput) || !bytes.Equal(ok.GetNonceValue(), b2b256(d.nonceOutput)) {
			t.Errorf("%s: leader %x nonce %x", f.name, ok.GetLeaderValue(), ok.GetNonceValue())
		}
		wantRules(t, f.name+" wrong key hash", run(h.CBOR, h.EpochNonce, bytes.Repeat([]byte{3}, 32), nil).GetRejection(), "OVERLAY/VRFKeyWrongVRFKey")
		wantRules(t, f.name+" wrong epoch nonce", run(h.CBOR, bytes.Repeat([]byte{1}, 32), keyHash, nil).GetRejection(), "OVERLAY/VRFKeyBadNonce")
		wantRules(t, f.name+" key hash first", run(h.CBOR, bytes.Repeat([]byte{1}, 32), bytes.Repeat([]byte{3}, 32), nil).GetRejection(), "OVERLAY/VRFKeyWrongVRFKey")
		nonceAt := bytes.Index(h.CBOR, d.nonceProof)
		wantRules(t, f.name+" tampered nonce proof", run(flipByte(h.CBOR, nonceAt+5), h.EpochNonce, keyHash, nil).GetRejection(), "OVERLAY/VRFKeyBadNonce")
		leaderAt := bytes.Index(h.CBOR, d.vrfProof)
		wantRules(t, f.name+" tampered leader proof", run(flipByte(h.CBOR, leaderAt+5), h.EpochNonce, keyHash, nil).GetRejection(), "OVERLAY/VRFKeyBadLeaderValue")
		wantRules(t, f.name+" nonce proof before leader proof", run(flipByte(flipByte(h.CBOR, leaderAt+5), nonceAt+5), h.EpochNonce, keyHash, nil).GetRejection(), "OVERLAY/VRFKeyBadNonce")
	}
}

// The overlay schedule: a header in an overlay slot is checked against the
// genesis delegation of its slot instead of the pool registration. The shelley
// header is at slot 16156972, 172972 into a 432000-slot epoch. With d = 1 and
// f = 1 every slot is an active overlay slot and the producing key is
// position mod count of the genesis keys sorted by hash: 172972 mod 3 = 1, so
// the second key of three.
func TestTPraosVrfCheckOverlay(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("shelley")
	d := decode(t, h)
	keyHash, delegate := b2b256(d.vrfKey), b2b224(d.issuer)
	env := func(dNum, dDen, fNum, fDen string, epochLength uint64, keys []*v1.GenesisDelegation) *v1.TpraosOverlayEnv {
		return &v1.TpraosOverlayEnv{D: rat(dNum, dDen), ActiveSlotCoeff: rat(fNum, fDen), EpochLength: epochLength, GenesisDelegations: keys}
	}
	run := func(o *v1.TpraosOverlayEnv) (*v1.PraosVrfCheckResponse, error) {
		return vrfCheck(context.Background(), &v1.PraosVrfCheckRequest{Era: h.Era, Header: h.CBOR, EpochNonce: &v1.Nonce{Hash: h.EpochNonce}, PoolVrfKeyHash: bytes.Repeat([]byte{9}, 32), Overlay: o})
	}
	keys := genesisKeys(3)
	keys[1].DelegateKeyHash, keys[1].VrfKeyHash = delegate, keyHash
	got, err := run(env("1", "1", "1", "1", 432000, keys))
	if r := mustOK(t, "overlay slot", got, err).GetRejection(); r != nil {
		t.Errorf("the scheduled delegate's header rejected: %v", r)
	}
	for name, mut := range map[string]func(*v1.GenesisDelegation){
		"delegate":     func(g *v1.GenesisDelegation) { g.DelegateKeyHash = bytes.Repeat([]byte{0xaa}, 28) },
		"vrf key hash": func(g *v1.GenesisDelegation) { g.VrfKeyHash = bytes.Repeat([]byte{0xaa}, 32) },
	} {
		ks := genesisKeys(3)
		ks[1].DelegateKeyHash, ks[1].VrfKeyHash = delegate, keyHash
		mut(ks[1])
		got, err := run(env("1", "1", "1", "1", 432000, ks))
		want := map[string][]string{"delegate": {"OVERLAY/WrongGenesisColdKeyOVERLAY"}, "vrf key hash": {"OVERLAY/WrongGenesisVRFKeyOVERLAY"}}[name]
		wantRules(t, "wrong "+name, mustOK(t, name, got, err).GetRejection(), want...)
	}
	ks := genesisKeys(3)
	ks[1].DelegateKeyHash, ks[1].VrfKeyHash = bytes.Repeat([]byte{0xaa}, 28), bytes.Repeat([]byte{0xbb}, 32)
	got, err = run(env("1", "1", "1", "1", 432000, ks))
	wantRules(t, "both wrong: the cold key does not stop the VRF key check", mustOK(t, "both", got, err).GetRejection(),
		"OVERLAY/WrongGenesisColdKeyOVERLAY", "OVERLAY/WrongGenesisVRFKeyOVERLAY")
	// Two keys: 172972 mod 2 = 0, the first key, which holds no delegation for the issuer.
	two := genesisKeys(2)
	two[1].DelegateKeyHash, two[1].VrfKeyHash = delegate, keyHash
	got, err = run(env("1", "1", "1", "1", 432000, two))
	wantRules(t, "another key's slot", mustOK(t, "two", got, err).GetRejection(), "OVERLAY/WrongGenesisColdKeyOVERLAY", "OVERLAY/WrongGenesisVRFKeyOVERLAY")
	// f = 1/20: position 172972 mod 20 = 12, an overlay slot nobody produces.
	got, err = run(env("1", "1", "1", "20", 432000, keys))
	wantRules(t, "non-active overlay slot", mustOK(t, "non-active", got, err).GetRejection(), "OVERLAY/NotActiveSlotOVERLAY")
	// d = 1/2 and a 432001-slot epoch: relative slot 172935 is odd, not an
	// overlay slot, so the pool registration decides. With f < 1 the leader
	// threshold would also apply, which the request cannot express.
	_, err = run(env("1", "2", "1", "20", 432001, keys))
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Errorf("non-overlay slot with f < 1: %v, want UNSUPPORTED", err)
	}
	got, err = vrfCheck(context.Background(), &v1.PraosVrfCheckRequest{Era: h.Era, Header: h.CBOR, EpochNonce: &v1.Nonce{Hash: h.EpochNonce}, PoolVrfKeyHash: keyHash,
		Overlay: env("1", "2", "1", "1", 432001, keys)})
	if r := mustOK(t, "non-overlay", got, err).GetRejection(); r != nil {
		t.Errorf("non-overlay slot under the pool registration: %v", r)
	}
}

// praosVrfChecks' registration lookups on a TPraos header: the pool in the
// distribution, then its VRF key hash.
func TestTPraosVrfKeyRegistered(t *testing.T) {
	t.Parallel()
	for _, f := range tpraosFacts {
		h := praosfixtures.ByName(f.name)
		d := decode(t, h)
		pool, key := b2b224(d.issuer), b2b256(d.vrfKey)
		run := func(dist ...*v1.PoolStake) *v1.PraosVrfKeyRegisteredResponse {
			got, err := vrfKeyRegistered(context.Background(), &v1.PraosVrfKeyRegisteredRequest{Header: h.CBOR, Era: h.Era, PoolDistribution: dist})
			return mustOK(t, f.name, got, err)
		}
		ok := run(&v1.PoolStake{PoolId: pool, VrfKeyHash: key})
		if ok.GetRejection() != nil || !bytes.Equal(ok.GetPoolId(), pool) {
			t.Errorf("%s: registered: %v pool %x", f.name, ok.GetRejection(), ok.GetPoolId())
		}
		wantRules(t, f.name+" unknown pool", run(&v1.PoolStake{PoolId: bytes.Repeat([]byte{9}, 28), VrfKeyHash: key}).GetRejection(), "OVERLAY/VRFKeyUnknown")
		wantRules(t, f.name+" empty distribution", run().GetRejection(), "OVERLAY/VRFKeyUnknown")
		wantRules(t, f.name+" wrong key", run(&v1.PoolStake{PoolId: pool, VrfKeyHash: bytes.Repeat([]byte{1}, 32)}).GetRejection(), "OVERLAY/VRFKeyWrongVRFKey")
	}
}

// The parent tip must be representable in the reference: a 32-byte hash, and
// a block number and slot whose successor exists.
func TestHeaderEnvelopeParentDomain(t *testing.T) {
	t.Parallel()
	h := praosfixtures.ByName("conway_a")
	d := decode(t, h)
	limits := &v1.ProtocolParameters{MaxBlockHeaderSize: u32Ptr(1100), MaxBlockBodySize: u32Ptr(90112)}
	for name, tc := range map[string]struct {
		parent *v1.EnvelopeTip
		want   v1.HarnessErrorCode
	}{
		"short hash":       {&v1.EnvelopeTip{BlockNo: d.blockNo - 1, Slot: d.slot - 1, Hash: d.prevHash[:31]}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"no hash":          {&v1.EnvelopeTip{BlockNo: d.blockNo - 1, Slot: d.slot - 1}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"largest block no": {&v1.EnvelopeTip{BlockNo: ^uint64(0), Slot: d.slot - 1, Hash: d.prevHash}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"largest slot":     {&v1.EnvelopeTip{BlockNo: d.blockNo - 1, Slot: ^uint64(0), Hash: d.prevHash}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
	} {
		_, err := headerEnvelope(context.Background(), &v1.PraosHeaderEnvelopeRequest{Era: h.Era, Header: h.CBOR, Pparams: limits, Parent: tc.parent})
		if dispatch.Code(err) != tc.want {
			t.Errorf("%s: err = %v, want code %v", name, err, tc.want)
		}
	}
}
