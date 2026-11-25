package main

import (
	"errors"

	"github.com/blinklabs-io/dingo/dxoracle"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// newEpochUpdates reads the update-system proposals of a boundary before
// Conway as the rows dingo's rollover reads. The domain is epoch_pparam_update's
// with the scratch ledger's epoch length: dingo's epochs are 432000 slots, so
// a proposal's slot only means the same epoch on both nodes under that length.
// An update that does not decode is the rejection PPUP/DeserialiseFailure.
func newEpochUpdates(req *v1.EpochNewEpochRequest, era v1.Era) ([]dxoracle.NewEpochPParamUpdate, int, *v1.Rejection, error) {
	proposals := req.GetState().GetPparamProposals()
	if len(proposals) == 0 {
		return nil, 0, nil, nil
	}
	ops, ok := pparamEras[era]
	if !ok {
		return nil, 0, nil, unsupportedf("parameter proposals do not exist in Conway")
	}
	if req.GetGlobals().GetEpochLength() != newEpochEpochLength {
		return nil, 0, nil, unsupportedf("parameter proposals need the scratch ledger's epoch length")
	}
	var rows []dxoracle.NewEpochPParamUpdate
	var decodeFailure error
	var lastSlot uint64
	for _, p := range proposals {
		if len(p.GetGenesisKeyHash()) != 28 {
			return nil, 0, nil, dispatch.BadRequest(errors.New("genesis key hash must be 28 bytes"))
		}
		if p.GetAddedSlot() < lastSlot {
			return nil, 0, nil, unsupportedf("proposals are not in chain order")
		}
		lastSlot = p.GetAddedSlot()
		made := p.GetAddedSlot() / newEpochEpochLength
		if made != p.GetTargetEpoch() && made+1 != p.GetTargetEpoch() {
			return nil, 0, nil, unsupportedf("proposal targets neither its own epoch nor the next")
		}
		if _, err := ops.decode(p.GetUpdate()); err != nil {
			decodeFailure = err
		}
		rows = append(rows, dxoracle.NewEpochPParamUpdate{
			GenesisHash: p.GetGenesisKeyHash(), Cbor: p.GetUpdate(), AddedSlot: p.GetAddedSlot(), Epoch: p.GetTargetEpoch(),
		})
	}
	if decodeFailure != nil {
		return nil, 0, &v1.Rejection{Failures: []*v1.Failure{{Rule: "PPUP", Name: "DeserialiseFailure", Detail: decodeFailure.Error()}}}, nil
	}
	return rows, int(min(req.GetGlobals().GetQuorum(), uint64(1<<31))), nil, nil
}

// newEpochMIRs reads the MIR certificates of a boundary before Conway as
// epoch_mir reads them, with the same domain.
func newEpochMIRs(req *v1.EpochNewEpochRequest, era v1.Era) ([]dxoracle.NewEpochMIR, error) {
	if len(req.GetMir()) == 0 && req.GetReservesToTreasury() == 0 && req.GetTreasuryToReserves() == 0 {
		return nil, nil
	}
	if era == v1.Era_ERA_CONWAY {
		return nil, unsupportedf("MIR does not exist in Conway")
	}
	pots := req.GetState().GetPots()
	if err := mirDomain(&v1.EpochMirRequest{
		ProtocolVersion: req.GetPparams().GetProtocolVersion(), Reserves: pots.GetReserves(), Treasury: pots.GetTreasury(),
		Mir: req.GetMir(), ReservesToTreasury: req.GetReservesToTreasury(), TreasuryToReserves: req.GetTreasuryToReserves(),
	}); err != nil {
		return nil, err
	}
	var out []dxoracle.NewEpochMIR
	for _, m := range req.GetMir() {
		tag, err := credentialTag(m.GetCredential())
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		out = append(out, dxoracle.NewEpochMIR{
			FromTreasury: m.GetSource() == v1.MirSource_MIR_SOURCE_TREASURY,
			Tag:          tag, Key: m.GetCredential().GetHash(), Amount: m.GetAmount(),
		})
	}
	if n := req.GetReservesToTreasury(); n > 0 {
		out = append(out, dxoracle.NewEpochMIR{OtherPot: n})
	}
	if n := req.GetTreasuryToReserves(); n > 0 {
		out = append(out, dxoracle.NewEpochMIR{FromTreasury: true, OtherPot: n})
	}
	return out, nil
}
