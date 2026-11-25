package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"github.com/blinklabs-io/gouroboros/consensus"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// leaderCheck maps to gouroboros LeaderEligibility.CheckCertifiedNatural.
// leader_value is the certified natural, big-endian: 64 bytes under TPraos
// (Shelley to Alonzo, bound 2^512) and 32 bytes under Praos (Babbage on,
// bound 2^256), as checkLeaderNatValue bounds it.
func leaderCheck(_ context.Context, req *v1.PraosLeaderCheckRequest) (*v1.PraosLeaderCheckResponse, error) {
	f, err := ratArg("active_slot_coeff", req.GetActiveSlotCoeff())
	if err != nil {
		return nil, err
	}
	if err := positiveUnitInterval("active_slot_coeff", f); err != nil {
		return nil, err
	}
	if err := nonzero("total_stake", req.GetTotalStake()); err != nil {
		return nil, err
	}
	if req.GetPoolStake() > req.GetTotalStake() {
		return nil, dispatch.Unsupported(errors.New("pool_stake exceeds total_stake"))
	}
	mode, size, err := consensusMode(req.GetEra())
	if err != nil {
		return nil, err
	}
	if len(req.GetLeaderValue()) != size {
		return nil, dispatch.Unsupported(fmt.Errorf("%v leader value must be %d bytes", req.GetEra(), size))
	}
	elig, err := consensus.NewLeaderEligibility(req.GetPoolStake(), req.GetTotalStake(), f, mode)
	if err != nil {
		return &v1.PraosLeaderCheckResponse{Rejection: dingoRejection("praos", err)}, nil
	}
	ok, err := elig.CheckCertifiedNatural(new(big.Int).SetBytes(req.GetLeaderValue()))
	if err != nil {
		return &v1.PraosLeaderCheckResponse{Rejection: dingoRejection("praos", err)}, nil
	}
	return &v1.PraosLeaderCheckResponse{IsLeader: ok}, nil
}

func consensusMode(era v1.Era) (consensus.ConsensusMode, int, error) {
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY, v1.Era_ERA_ALONZO:
		return consensus.ConsensusModeTPraos, 64, nil
	case v1.Era_ERA_BABBAGE, v1.Era_ERA_CONWAY:
		return consensus.ConsensusModeCPraos, 32, nil
	}
	return 0, 0, dispatch.Unsupported(fmt.Errorf("no Praos leader check in %v", era))
}

// nonceEpoch maps to dingo epochengine.AssembleEpochNonce:
// (candidate ⭒ previous lab) ⭒ extra entropy. Dingo represents the neutral
// candidate as 32 zero bytes, so a zero candidate and a zero result are read
// as neutral.
func nonceEpoch(_ context.Context, req *v1.PraosNonceEpochRequest) (*v1.PraosNonceEpochResponse, error) {
	candidate := req.GetCandidate().GetHash()
	for name, n := range map[string][]byte{
		"candidate": candidate, "previous_lab": req.GetPreviousLab().GetHash(), "extra_entropy": req.GetExtraEntropy().GetHash(),
	} {
		if len(n) != 0 && len(n) != 32 {
			return nil, dispatch.BadRequest(fmt.Errorf("%s must be empty (neutral) or 32 bytes", name))
		}
	}
	if len(candidate) == 0 {
		candidate = make([]byte, 32)
	}
	got, err := epochengine.AssembleEpochNonce(candidate, req.GetPreviousLab().GetHash(), req.GetExtraEntropy().GetHash())
	if err != nil {
		return &v1.PraosNonceEpochResponse{Rejection: dingoRejection("praos", err)}, nil
	}
	if bytes.Equal(got, make([]byte, 32)) {
		got = nil
	}
	return &v1.PraosNonceEpochResponse{EpochNonce: &v1.Nonce{Hash: got}}, nil
}

// overlaySchedule maps to dingo ledger.ClassifyGenesisOverlaySlot. The
// genesis keys are passed in request order; the reference indexes a Set,
// which orders them by hash.
func overlaySchedule(_ context.Context, req *v1.TpraosOverlayScheduleRequest) (*v1.TpraosOverlayScheduleResponse, error) {
	d, err := ratArg("d", req.GetD())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("d", d); err != nil {
		return nil, err
	}
	f, err := ratArg("active_slot_coeff", req.GetActiveSlotCoeff())
	if err != nil {
		return nil, err
	}
	if err := positiveUnitInterval("active_slot_coeff", f); err != nil {
		return nil, err
	}
	keys := req.GetGenesisDelegations()
	if len(keys) == 0 {
		return nil, dispatch.Unsupported(errors.New("no genesis keys"))
	}
	if req.GetSlot() < req.GetEpochFirstSlot() {
		return nil, dispatch.Unsupported(errors.New("slot precedes the epoch's first slot"))
	}
	idx, status := ledger.ClassifyGenesisOverlaySlot(req.GetSlot()-req.GetEpochFirstSlot(), d, f, uint64(len(keys)))
	switch status {
	case ledger.GenesisOverlayActive:
		if idx >= uint64(len(keys)) {
			return &v1.TpraosOverlayScheduleResponse{Rejection: dingoRejection("overlay", fmt.Errorf("key index %d of %d", idx, len(keys)))}, nil
		}
		return &v1.TpraosOverlayScheduleResponse{
			Kind: v1.OverlaySlotKind_OVERLAY_SLOT_KIND_ACTIVE, GenesisKeyHash: keys[idx].GetGenesisKeyHash(),
		}, nil
	case ledger.GenesisOverlayNonActive:
		return &v1.TpraosOverlayScheduleResponse{Kind: v1.OverlaySlotKind_OVERLAY_SLOT_KIND_NON_ACTIVE}, nil
	default:
		return &v1.TpraosOverlayScheduleResponse{Kind: v1.OverlaySlotKind_OVERLAY_SLOT_KIND_PRAOS}, nil
	}
}

func registerPraos(r *dispatch.Registry) {
	dispatch.Register(r, leaderCheck)
	dispatch.Register(r, nonceEpoch)
	dispatch.Register(r, overlaySchedule)
	dispatch.Register(r, headerEnvelope)
	dispatch.Register(r, vrfCheck)
	dispatch.Register(r, kesCheck)
	dispatch.Register(r, opcertCheck)
	dispatch.Register(r, vrfKeyRegistered)
	dispatch.Register(r, nonceRolling)
	dispatch.Register(r, nonceCandidate)
}
