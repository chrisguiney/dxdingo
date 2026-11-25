package main

import (
	"context"
	"errors"

	"github.com/blinklabs-io/dingo/database"
	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// maxProtocolMajor is the reference's MaxVersion; a larger major is not a
// representable ProtVer.
const maxProtocolMajor = 13

// pparamEra is dingo's per-era update machinery for the classic update system.
type pparamEra struct {
	decode func([]byte) (any, error)
	update func(common.ProtocolParameters, any) (common.ProtocolParameters, error)
	from   func(*v1.ProtocolParameters) (common.ProtocolParameters, error)
	to     func(common.ProtocolParameters) (*v1.ProtocolParameters, error)
}

func asShelley(pp common.ProtocolParameters) (*shelley.ShelleyProtocolParameters, error) {
	p, ok := pp.(*shelley.ShelleyProtocolParameters)
	if !ok {
		return nil, errors.New("enacted parameters have an unexpected type")
	}
	return p, nil
}

var pparamEras = map[v1.Era]pparamEra{
	v1.Era_ERA_SHELLEY: {
		eras.DecodePParamsUpdateShelley, eras.PParamsUpdateShelley,
		func(p *v1.ProtocolParameters) (common.ProtocolParameters, error) { return shelleyPParamsFromProto(p) },
		func(pp common.ProtocolParameters) (*v1.ProtocolParameters, error) {
			p, err := asShelley(pp)
			if err != nil {
				return nil, err
			}
			return shelleyPParamsToProto(p), nil
		},
	},
	v1.Era_ERA_ALLEGRA: {
		eras.DecodePParamsUpdateAllegra, eras.PParamsUpdateAllegra,
		func(p *v1.ProtocolParameters) (common.ProtocolParameters, error) { return shelleyPParamsFromProto(p) },
		func(pp common.ProtocolParameters) (*v1.ProtocolParameters, error) {
			p, err := asShelley(pp)
			if err != nil {
				return nil, err
			}
			return shelleyPParamsToProto(p), nil
		},
	},
	v1.Era_ERA_MARY: {
		eras.DecodePParamsUpdateMary, eras.PParamsUpdateMary,
		func(p *v1.ProtocolParameters) (common.ProtocolParameters, error) { return maryPParamsFromProto(p) },
		func(pp common.ProtocolParameters) (*v1.ProtocolParameters, error) {
			p, ok := pp.(*mary.MaryProtocolParameters)
			if !ok {
				return nil, errors.New("enacted parameters have an unexpected type")
			}
			return maryPParamsToProto(p), nil
		},
	},
	v1.Era_ERA_ALONZO: {
		eras.DecodePParamsUpdateAlonzo, eras.PParamsUpdateAlonzo,
		func(p *v1.ProtocolParameters) (common.ProtocolParameters, error) { return alonzoPParamsFromProto(p) },
		func(pp common.ProtocolParameters) (*v1.ProtocolParameters, error) {
			p, ok := pp.(*alonzo.AlonzoProtocolParameters)
			if !ok {
				return nil, errors.New("enacted parameters have an unexpected type")
			}
			return alonzoPParamsToProto(p), nil
		},
	},
	v1.Era_ERA_BABBAGE: {
		eras.DecodePParamsUpdateBabbage, eras.PParamsUpdateBabbage,
		func(p *v1.ProtocolParameters) (common.ProtocolParameters, error) { return babbagePParamsFromProto(p) },
		func(pp common.ProtocolParameters) (*v1.ProtocolParameters, error) {
			p, ok := pp.(*babbage.BabbageProtocolParameters)
			if !ok {
				return nil, errors.New("enacted parameters have an unexpected type")
			}
			return babbagePParamsToProto(p), nil
		},
	},
}

// epochPParamUpdate maps to database.ComputeClassicPParamEnactmentFromRows,
// the one implementation of classic update-system enactment.
//
// Scope and domain, shared with dxnode-hs:
//   - Eras Shelley through Babbage; any other era is UNSUPPORTED.
//   - The proposals for the boundary into context.epoch are those targeting
//     epoch-1. A proposal made before that epoch began (added_slot below its
//     first slot) was submitted for the next epoch and is carried over; it is
//     dropped unless every carried proposal's protocol version can follow the
//     parameters in force.
//   - List order is chain order: added_slot never decreases. A proposal is
//     made in the epoch it targets or the one before it.
//   - An update that does not decode is the rejection PPUP/DeserialiseFailure,
//     as a chain would reject the transaction carrying it.
//   - Every parameter of the era's table must be present.
func epochPParamUpdate(_ context.Context, req *v1.EpochPParamUpdateRequest) (*v1.EpochPParamUpdateResponse, error) {
	ops, ok := pparamEras[req.GetContext().GetEra()]
	if !ok {
		return nil, dispatch.Unsupported(errors.New("era is outside Shelley to Babbage"))
	}
	epochLength := req.GetGlobals().GetEpochLength()
	if epochLength == 0 {
		return nil, dispatch.Unsupported(errors.New("epoch_length is zero"))
	}
	pp, err := ops.from(req.GetPparams())
	if err != nil {
		return nil, err
	}
	var rows []models.PParamUpdate
	var decodeFailure error
	var lastSlot uint64
	for i, p := range req.GetProposals() {
		if len(p.GetGenesisKeyHash()) != 28 {
			return nil, dispatch.BadRequest(errors.New("genesis key hash must be 28 bytes"))
		}
		if p.GetAddedSlot() < lastSlot {
			return nil, dispatch.Unsupported(errors.New("proposals are not in chain order"))
		}
		lastSlot = p.GetAddedSlot()
		made := p.GetAddedSlot() / epochLength
		if made != p.GetTargetEpoch() && made+1 != p.GetTargetEpoch() {
			return nil, dispatch.Unsupported(errors.New("proposal targets neither its own epoch nor the next"))
		}
		if _, err := ops.decode(p.GetUpdate()); err != nil {
			decodeFailure = err
		}
		rows = append(rows, models.PParamUpdate{
			GenesisHash: p.GetGenesisKeyHash(), Cbor: p.GetUpdate(), ID: uint(i + 1),
			AddedSlot: p.GetAddedSlot(), Epoch: p.GetTargetEpoch(),
		})
	}
	if decodeFailure != nil {
		return &v1.EpochPParamUpdateResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{
			{Rule: "PPUP", Name: "DeserialiseFailure", Detail: decodeFailure.Error()},
		}}}, nil
	}
	epoch := req.GetContext().GetEpoch()
	// Quorum bounds the vote count, which cannot exceed the proposals.
	quorum := int(min(req.GetGlobals().GetQuorum(), uint64(1<<31)))
	got, err := database.ComputeClassicPParamEnactmentFromRows(
		rows, epoch, quorum, pp, ops.decode, ops.update, nil,
		func(e uint64) (*models.Epoch, error) { return &models.Epoch{StartSlot: e * epochLength}, nil },
	)
	if err != nil {
		return &v1.EpochPParamUpdateResponse{Rejection: dingoRejection("NEWPP", err)}, nil
	}
	if got == nil {
		return &v1.EpochPParamUpdateResponse{Pparams: req.GetPparams()}, nil
	}
	out, err := ops.to(got.PParams)
	if err != nil {
		return nil, err
	}
	return &v1.EpochPParamUpdateResponse{Adopted: true, Pparams: out}, nil
}
