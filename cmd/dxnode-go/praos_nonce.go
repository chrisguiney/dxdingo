package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/eras"
	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// etaV is dingo's per-era evolving-nonce step for a block: the previous
// nonce folded with the block's VRF contribution.
type etaV func(prev []byte) ([]byte, error)

// etaVFor builds a block carrying only the VRF output and returns the era's
// CalculateEtaV on it, with the gouroboros era id the stability window is
// keyed by. TPraos eras fold the nonce VRF output of the header body and Praos
// eras the leader VRF output.
func etaVFor(era v1.Era, vrfOutput []byte) (etaV, uint, error) {
	vrfResult := lcommon.VrfResult{Output: vrfOutput}
	tpraosBody := shelley.ShelleyBlockHeaderBody{NonceVrf: vrfResult}
	call := func(f func(prev []byte, b gledger.Block) ([]byte, error), b gledger.Block) etaV {
		return func(prev []byte) ([]byte, error) { return f(prev, b) }
	}
	switch era {
	case v1.Era_ERA_SHELLEY:
		b := &shelley.ShelleyBlock{BlockHeader: &shelley.ShelleyBlockHeader{Body: tpraosBody}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVShelley(nil, p, b) }, b), shelley.EraIdShelley, nil
	case v1.Era_ERA_ALLEGRA:
		b := &allegra.AllegraBlock{BlockHeader: &allegra.AllegraBlockHeader{ShelleyBlockHeader: shelley.ShelleyBlockHeader{Body: tpraosBody}}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVAllegra(nil, p, b) }, b), allegra.EraIdAllegra, nil
	case v1.Era_ERA_MARY:
		b := &mary.MaryBlock{BlockHeader: &mary.MaryBlockHeader{ShelleyBlockHeader: shelley.ShelleyBlockHeader{Body: tpraosBody}}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVMary(nil, p, b) }, b), mary.EraIdMary, nil
	case v1.Era_ERA_ALONZO:
		b := &alonzo.AlonzoBlock{BlockHeader: &alonzo.AlonzoBlockHeader{ShelleyBlockHeader: shelley.ShelleyBlockHeader{Body: tpraosBody}}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVAlonzo(nil, p, b) }, b), alonzo.EraIdAlonzo, nil
	case v1.Era_ERA_BABBAGE:
		b := &babbage.BabbageBlock{BlockHeader: &babbage.BabbageBlockHeader{Body: babbage.BabbageBlockHeaderBody{VrfResult: vrfResult}}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVBabbage(nil, p, b) }, b), babbage.EraIdBabbage, nil
	case v1.Era_ERA_CONWAY:
		b := &conway.ConwayBlock{BlockHeader: &conway.ConwayBlockHeader{Body: babbage.BabbageBlockHeaderBody{VrfResult: vrfResult}}}
		return call(func(p []byte, b gledger.Block) ([]byte, error) { return eras.CalculateEtaVConway(nil, p, b) }, b), conway.EraIdConway, nil
	}
	return nil, 0, dispatch.Unsupported(fmt.Errorf("no nonce evolution in %v", era))
}

// nonceArg reads a nonce as dingo holds it: 32 bytes, with the neutral nonce
// (empty) as 32 zero bytes.
func nonceArg(name string, n *v1.Nonce) ([]byte, error) {
	switch len(n.GetHash()) {
	case 0:
		return make([]byte, 32), nil
	case 32:
		return bytes.Clone(n.GetHash()), nil
	}
	return nil, dispatch.BadRequest(fmt.Errorf("%s must be empty (neutral) or 32 bytes", name))
}

// nonceReply is the inverse of nonceArg: 32 zero bytes read as neutral.
func nonceReply(b []byte) *v1.Nonce {
	if bytes.Equal(b, make([]byte, 32)) {
		return &v1.Nonce{}
	}
	return &v1.Nonce{Hash: b}
}

func vrfOutputArg(b []byte) error { return sizedHash("vrf_output", b, 64) }

// nonceRolling maps to dingo eras.CalculateEtaV for the era, which folds the
// block's VRF contribution into the evolving nonce with gouroboros
// CalculateRollingNonce.
func nonceRolling(_ context.Context, req *v1.PraosNonceRollingRequest) (*v1.PraosNonceRollingResponse, error) {
	step, _, err := etaVFor(req.GetEra(), req.GetVrfOutput())
	if err != nil {
		return nil, err
	}
	if err := vrfOutputArg(req.GetVrfOutput()); err != nil {
		return nil, err
	}
	evolving, err := nonceArg("evolving", req.GetEvolving())
	if err != nil {
		return nil, err
	}
	next, err := step(evolving)
	if err != nil {
		return &v1.PraosNonceRollingResponse{Rejection: dingoRejection("praos", err)}, nil
	}
	return &v1.PraosNonceRollingResponse{Evolving: nonceReply(next)}, nil
}

// nonceCandidate follows dingo's computeCandidateNonceSlow loop: per block,
// eras.CalculateEtaV for the evolving nonce, then ledger.AdvanceCandidateNonce
// with the cutoff from ledger.CandidateFreezeCutoff over the window from
// ledger.ComputeNonceStabilityWindow.
func nonceCandidate(_ context.Context, req *v1.PraosNonceCandidateRequest) (*v1.PraosNonceCandidateResponse, error) {
	_, eraID, err := etaVFor(req.GetEra(), nil)
	if err != nil {
		return nil, err
	}
	f, err := ratArg("active_slot_coeff", req.GetActiveSlotCoeff())
	if err != nil {
		return nil, err
	}
	if err := positiveUnitInterval("active_slot_coeff", f); err != nil {
		return nil, err
	}
	if err := nonzero("security_param", req.GetSecurityParam()); err != nil {
		return nil, err
	}
	first, next := req.GetEpochFirstSlot(), req.GetNextEpochFirstSlot()
	if next <= first {
		return nil, dispatch.Unsupported(errors.New("next_epoch_first_slot must follow epoch_first_slot"))
	}
	// The reference reads the epoch boundary from fixed-size epochs, so the
	// epoch must start at a multiple of its length.
	if first%(next-first) != 0 {
		return nil, dispatch.Unsupported(errors.New("epoch_first_slot is not a multiple of the epoch length"))
	}
	evolving, err := nonceArg("evolving", req.GetEvolving())
	if err != nil {
		return nil, err
	}
	candidate, err := nonceArg("candidate", req.GetCandidate())
	if err != nil {
		return nil, err
	}
	cutoff := ledger.CandidateFreezeCutoff(first, next, ledger.ComputeNonceStabilityWindow(eraID, req.GetSecurityParam(), f))
	for i, b := range req.GetBlocks() {
		if b.GetSlot() < first || b.GetSlot() >= next {
			return nil, dispatch.Unsupported(fmt.Errorf("block %d: slot %d outside [%d, %d)", i, b.GetSlot(), first, next))
		}
		if err := vrfOutputArg(b.GetVrfOutput()); err != nil {
			return nil, err
		}
		step, _, err := etaVFor(req.GetEra(), b.GetVrfOutput())
		if err != nil {
			return nil, err
		}
		if evolving, err = step(evolving); err != nil {
			return &v1.PraosNonceCandidateResponse{Rejection: dingoRejection("praos", err)}, nil
		}
		candidate = ledger.AdvanceCandidateNonce(evolving, candidate, b.GetSlot(), cutoff)
	}
	return &v1.PraosNonceCandidateResponse{Evolving: nonceReply(evolving), Candidate: nonceReply(candidate)}, nil
}
