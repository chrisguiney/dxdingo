package main

import (
	"context"
	"errors"
	"fmt"
	"math"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/consensus"
	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/vrf"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// praosHeader is a decoded Babbage or Conway block header: the fields the
// consensus checks read, next to the decoded header they come from.
type praosHeader struct {
	hdr       lcommon.BlockHeader
	slot      uint64
	issuer    []byte
	vrfKey    []byte
	vrfProof  []byte
	vrfOutput []byte
}

// praosBlockType is the gouroboros block type for a Praos era. Eras before
// Babbage run TPraos and Dijkstra is not modelled.
func praosBlockType(era v1.Era) (uint, error) {
	switch era {
	case v1.Era_ERA_BABBAGE:
		return gledger.BlockTypeBabbage, nil
	case v1.Era_ERA_CONWAY:
		return gledger.BlockTypeConway, nil
	}
	return 0, dispatch.Unsupported(fmt.Errorf("no Praos header check in %v", era))
}

// decodePraosHeader decodes a block header, as it appears in a block, with
// gouroboros. A decode failure is returned as the error and reported by the
// callers as the rejection DeserialiseFailure.
func decodePraosHeader(era v1.Era, raw []byte) (*praosHeader, error) {
	bt, err := praosBlockType(era)
	if err != nil {
		return nil, err
	}
	hdr, err := gledger.NewBlockHeaderFromCbor(bt, raw)
	if err != nil {
		return nil, &decodeError{err}
	}
	issuer := hdr.IssuerVkey()
	out := &praosHeader{hdr: hdr, slot: hdr.SlotNumber(), issuer: issuer[:]}
	switch h := hdr.(type) {
	case *babbage.BabbageBlockHeader:
		out.vrfKey, out.vrfProof, out.vrfOutput = h.Body.VrfKey, h.Body.VrfResult.Proof, h.Body.VrfResult.Output
	case *conway.ConwayBlockHeader:
		out.vrfKey, out.vrfProof, out.vrfOutput = h.Body.VrfKey, h.Body.VrfResult.Proof, h.Body.VrfResult.Output
	default:
		return nil, dispatch.Unsupported(fmt.Errorf("unexpected header type %T", hdr))
	}
	return out, nil
}

// decodeError marks a header that does not decode.
type decodeError struct{ err error }

func (e *decodeError) Error() string { return e.err.Error() }
func (e *decodeError) Unwrap() error { return e.err }

// deserialiseFailure is the rejection for a header that does not decode.
func deserialiseFailure(err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "header", Name: "DeserialiseFailure", Detail: err.Error()}}}
}

// praosRejection is a single-failure rejection under the rule Praos, the
// reference's PraosValidationErr.
func praosRejection(name string, detail error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "Praos", Name: name, Detail: detail.Error()}}}
}

// decodeOrReject decodes the header; a header that does not decode is the
// rejection DeserialiseFailure, anything else a harness error.
func decodeOrReject(era v1.Era, raw []byte) (*praosHeader, *v1.Rejection, error) {
	h, err := decodePraosHeader(era, raw)
	var de *decodeError
	if errors.As(err, &de) {
		return nil, deserialiseFailure(err), nil
	}
	if err != nil {
		return nil, nil, err
	}
	if h.slot > math.MaxInt64 {
		return nil, nil, dispatch.Unsupported(errors.New("slot above the int64 range"))
	}
	return h, nil, nil
}

// headerEnvelope maps to dingo ledger.CheckHeaderSizeLimit and
// CheckBodySizeLimit, the two checks CheckBlockSizeLimits applies in this
// order: the encoded header against maxBlockHeaderSize, then the body size the
// header declares against maxBlockBodySize.
func headerEnvelope(_ context.Context, req *v1.PraosHeaderEnvelopeRequest) (*v1.PraosHeaderEnvelopeResponse, error) {
	if _, err := praosBlockType(req.GetEra()); err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	if pp == nil || pp.MaxBlockHeaderSize == nil || pp.MaxBlockBodySize == nil {
		return nil, dispatch.BadRequest(errors.New("pparams needs max_block_header_size and max_block_body_size"))
	}
	if *pp.MaxBlockHeaderSize > math.MaxUint16 {
		return nil, dispatch.Unsupported(errors.New("max_block_header_size does not fit a Word16"))
	}
	h, rej, err := decodeOrReject(req.GetEra(), req.GetHeader())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosHeaderEnvelopeResponse{Rejection: rej}, nil
	}
	if err := ledger.CheckHeaderSizeLimit(uint64(len(h.hdr.Cbor())), uint64(*pp.MaxBlockHeaderSize)); err != nil {
		return &v1.PraosHeaderEnvelopeResponse{Rejection: envelopeRejection("HeaderSizeTooLarge", err)}, nil
	}
	if err := ledger.CheckBodySizeLimit(h.hdr.BlockBodySize(), uint64(*pp.MaxBlockBodySize)); err != nil {
		return &v1.PraosHeaderEnvelopeResponse{Rejection: envelopeRejection("BlockSizeTooLarge", err)}, nil
	}
	return &v1.PraosHeaderEnvelopeResponse{}, nil
}

func envelopeRejection(name string, err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "Envelope", Name: name, Detail: err.Error()}}}
}

func sizedHash(name string, b []byte, size int) error {
	if len(b) != size {
		return dispatch.BadRequest(fmt.Errorf("%s must be %d bytes", name, size))
	}
	return nil
}

// vrfCheck maps to dingo ledger.CheckRegisteredVrfKey for the key hash and
// gouroboros vrf.MkInputVrf and vrf.Verify for the proof over the Praos VRF
// input blake2b_256(slot || epoch nonce). The leader value is
// consensus.VrfLeaderValue and the nonce value eras.PraosVRFNonceValue. The
// key hash is checked first, as the reference does.
func vrfCheck(_ context.Context, req *v1.PraosVrfCheckRequest) (*v1.PraosVrfCheckResponse, error) {
	eta0 := req.GetEpochNonce().GetHash()
	if len(eta0) == 0 {
		return nil, dispatch.Unsupported(errors.New("neutral epoch nonce"))
	}
	if err := sizedHash("epoch_nonce", eta0, 32); err != nil {
		return nil, err
	}
	if err := sizedHash("pool_vrf_key_hash", req.GetPoolVrfKeyHash(), 32); err != nil {
		return nil, err
	}
	h, rej, err := decodeOrReject(req.GetEra(), req.GetHeader())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosVrfCheckResponse{Rejection: rej}, nil
	}
	if err := ledger.CheckRegisteredVrfKey(h.slot, h.poolID(), h.vrfKey, req.GetPoolVrfKeyHash()); err != nil {
		return &v1.PraosVrfCheckResponse{Rejection: praosRejection("VRFKeyWrongVRFKey", err)}, nil
	}
	input, err := vrf.MkInputVrf(int64(h.slot), eta0) //nolint:gosec // bounded by decodeOrReject
	if err != nil {
		return &v1.PraosVrfCheckResponse{Rejection: praosRejection("VRFKeyBadProof", err)}, nil
	}
	if ok, err := vrf.Verify(h.vrfKey, h.vrfProof, h.vrfOutput, input); err != nil || !ok {
		return &v1.PraosVrfCheckResponse{Rejection: praosRejection("VRFKeyBadProof", fmt.Errorf("verify: ok=%v err=%v", ok, err))}, nil
	}
	return &v1.PraosVrfCheckResponse{
		LeaderValue: consensus.VrfLeaderValue(h.vrfOutput),
		NonceValue:  eras.PraosVRFNonceValue(h.vrfOutput),
	}, nil
}

// poolID is the issuing pool's id: the Blake2b-224 hash of its cold key.
func (h *praosHeader) poolID() []byte {
	var vk lcommon.IssuerVkey
	copy(vk[:], h.issuer)
	return vk.Hash().Bytes()
}

// praosKes runs the checks of the reference's doValidateKESSignature that
// precede the counter, in its order: the KES period window, the operational
// certificate's cold signature, then the KES signature. It returns the KES
// evolution the header was signed at, or a rejection.
func praosKes(h *praosHeader, slotsPerKesPeriod, maxKesEvolutions uint64) (evolution, counter uint64, rej *v1.Rejection, err error) {
	if slotsPerKesPeriod == 0 || maxKesEvolutions == 0 {
		return 0, 0, nil, dispatch.Unsupported(errors.New("slots_per_kes_period and max_kes_evolutions must be nonzero"))
	}
	oc, err := gledger.ExtractOpCertFromHeader(h.hdr)
	if err != nil {
		return 0, 0, nil, err
	}
	evolution, err = gledger.ValidateKesPeriod(oc.KesPeriod, h.slot, slotsPerKesPeriod, maxKesEvolutions)
	if err != nil {
		name := "KESAfterEndOCERT"
		if h.slot/slotsPerKesPeriod < oc.KesPeriod {
			name = "KESBeforeStartOCERT"
		}
		return 0, 0, praosRejection(name, err), nil
	}
	if err := gledger.VerifyOpCertSignature(oc, h.issuer); err != nil {
		return 0, 0, praosRejection("InvalidSignatureOCERT", err), nil
	}
	if ok, err := gledger.VerifyKes(h.hdr, slotsPerKesPeriod); err != nil || !ok {
		return 0, 0, praosRejection("InvalidKesSignatureOCERT", fmt.Errorf("verify: ok=%v err=%v", ok, err)), nil
	}
	return evolution, oc.IssueNumber, nil, nil
}

// kesCheck runs praosKes. The reported kes_period is the evolution.
func kesCheck(_ context.Context, req *v1.PraosKesCheckRequest) (*v1.PraosKesCheckResponse, error) {
	h, rej, err := decodeOrReject(req.GetEra(), req.GetHeader())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosKesCheckResponse{Rejection: rej}, nil
	}
	evolution, _, rej, err := praosKes(h, req.GetSlotsPerKesPeriod(), req.GetMaxKesEvolutions())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosKesCheckResponse{Rejection: rej}, nil
	}
	return &v1.PraosKesCheckResponse{KesPeriod: evolution}, nil
}

// praosEraOf is the era whose header format a protocol major version uses:
// Babbage at 7 and 8, Conway from 9. Earlier versions run TPraos.
func praosEraOf(major uint32) (v1.Era, error) {
	switch {
	case major >= 9:
		return v1.Era_ERA_CONWAY, nil
	case major >= 7:
		return v1.Era_ERA_BABBAGE, nil
	}
	return 0, dispatch.Unsupported(fmt.Errorf("protocol version %d runs TPraos", major))
}

// opcertCheck maps to dingo ledger.CheckOpCertCounter, after the checks that
// precede the counter in the reference's doValidateKESSignature. Without a
// recorded counter the reference counts a registered pool from zero and
// refuses an unregistered one.
func opcertCheck(_ context.Context, req *v1.PraosOpCertCheckRequest) (*v1.PraosOpCertCheckResponse, error) {
	era, err := praosEraOf(req.GetProtocolVersion().GetMajor())
	if err != nil {
		return nil, err
	}
	h, rej, err := decodeOrReject(era, req.GetHeader())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosOpCertCheckResponse{Rejection: rej}, nil
	}
	_, counter, rej, err := praosKes(h, req.GetSlotsPerKesPeriod(), req.GetMaxKesEvolutions())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosOpCertCheckResponse{Rejection: rej}, nil
	}
	found := req.LastCounter != nil
	if !found && !req.GetPoolRegistered() {
		return &v1.PraosOpCertCheckResponse{Rejection: praosRejection("NoCounterForKeyHashOCERT", errors.New("no counter for the pool"))}, nil
	}
	eraID := uint8(babbage.EraIdBabbage)
	if era == v1.Era_ERA_CONWAY {
		eraID = uint8(conway.EraIdConway)
	}
	res := ledger.CheckOpCertCounter(req.GetLastCounter(), found, counter, eraID)
	if !res.Ok {
		name := "CounterOverIncrementedOCERT"
		if counter < res.NewCounter {
			name = "CounterTooSmallOCERT"
		}
		return &v1.PraosOpCertCheckResponse{Rejection: praosRejection(name, res.Err)}, nil
	}
	return &v1.PraosOpCertCheckResponse{NewCounter: res.NewCounter}, nil
}

// vrfKeyRegistered maps to dingo ledger.CheckRegisteredVrfKey, after the
// lookup of the issuing pool in the distribution. The request names no era;
// Babbage and Conway headers share one format, so it is decoded as Conway.
func vrfKeyRegistered(_ context.Context, req *v1.PraosVrfKeyRegisteredRequest) (*v1.PraosVrfKeyRegisteredResponse, error) {
	dist := make(map[string][]byte, len(req.GetPoolDistribution()))
	for _, p := range req.GetPoolDistribution() {
		if err := sizedHash("pool_id", p.GetPoolId(), 28); err != nil {
			return nil, err
		}
		if err := sizedHash("vrf_key_hash", p.GetVrfKeyHash(), 32); err != nil {
			return nil, err
		}
		if _, dup := dist[string(p.GetPoolId())]; dup {
			return nil, dispatch.Unsupported(errors.New("duplicate pool in the distribution"))
		}
		dist[string(p.GetPoolId())] = p.GetVrfKeyHash()
	}
	h, rej, err := decodeOrReject(v1.Era_ERA_CONWAY, req.GetHeader())
	if err != nil {
		return nil, err
	}
	if rej != nil {
		return &v1.PraosVrfKeyRegisteredResponse{Rejection: rej}, nil
	}
	pool := h.poolID()
	registered, ok := dist[string(pool)]
	if !ok {
		return &v1.PraosVrfKeyRegisteredResponse{Rejection: praosRejection("VRFKeyUnknown", fmt.Errorf("pool %x is not in the distribution", pool))}, nil
	}
	if err := ledger.CheckRegisteredVrfKey(h.slot, pool, h.vrfKey, registered); err != nil {
		return &v1.PraosVrfKeyRegisteredResponse{Rejection: praosRejection("VRFKeyWrongVRFKey", err)}, nil
	}
	return &v1.PraosVrfKeyRegisteredResponse{PoolId: pool}, nil
}
