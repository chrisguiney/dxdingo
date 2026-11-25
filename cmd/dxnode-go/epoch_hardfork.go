package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"math/big"

	"github.com/blinklabs-io/dingo/config/cardano"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// epochHardFork maps to eras.HardFork<Era>. The fork is named by the protocol
// majors on either side and the entered era is the one whose first major is
// `to`: 3 Allegra, 4 Mary, 5 Alonzo, 7 Babbage, 9 Conway. `from` must be
// `to - 1`; the parameters are the previous era's table. Alonzo and Conway
// read genesis data, which the request carries and the adapter loads through
// the node config exactly as a genesis file would be. Any other transition is
// UNSUPPORTED.
func epochHardFork(_ context.Context, req *v1.EpochHardForkRequest) (*v1.EpochHardForkResponse, error) {
	from, to := req.GetFrom().GetMajor(), req.GetTo().GetMajor()
	if from+1 != to {
		return nil, dispatch.Unsupported(errors.New("a fork enters the era after `from`"))
	}
	cfg := &cardano.CardanoNodeConfig{}
	var fork func(*cardano.CardanoNodeConfig, common.ProtocolParameters) (common.ProtocolParameters, error)
	var prev common.ProtocolParameters
	var err error
	switch to {
	case 3:
		fork = eras.HardForkAllegra
		prev, err = shelleyPParamsFromProto(req.GetPparams())
	case 4:
		fork = eras.HardForkMary
		prev, err = shelleyPParamsFromProto(req.GetPparams())
	case 5:
		fork = eras.HardForkAlonzo
		if prev, err = maryPParamsFromProto(req.GetPparams()); err != nil {
			return nil, err
		}
		if err = loadAlonzoGenesis(cfg, req.GetAlonzoGenesis()); err != nil {
			return nil, err
		}
	case 7:
		fork = eras.HardForkBabbage
		prev, err = alonzoPParamsFromProto(req.GetPparams())
	case 9:
		fork = eras.HardForkConway
		if prev, err = babbagePParamsFromProto(req.GetPparams()); err != nil {
			return nil, err
		}
		if err = loadConwayGenesis(cfg, req.GetConwayGenesis()); err != nil {
			return nil, err
		}
	default:
		return nil, dispatch.Unsupported(errors.New("no fork enters protocol major " + big.NewInt(int64(to)).String()))
	}
	if err != nil {
		return nil, err
	}
	got, err := fork(cfg, prev)
	if err != nil {
		return &v1.EpochHardForkResponse{Rejection: dingoRejection("HARDFORK", err)}, nil
	}
	var out *v1.ProtocolParameters
	switch p := got.(type) {
	case *shelley.ShelleyProtocolParameters:
		out = shelleyPParamsToProto(p)
	case *mary.MaryProtocolParameters:
		out = maryPParamsToProto(p)
	case *alonzo.AlonzoProtocolParameters:
		out = alonzoPParamsToProto(p)
	case *babbage.BabbageProtocolParameters:
		out = babbagePParamsToProto(p)
	case *conway.ConwayProtocolParameters:
		out = conwayPParamsToProto(p)
	default:
		return nil, errors.New("unexpected fork result type")
	}
	return &v1.EpochHardForkResponse{Delta: &v1.LedgerDelta{PparamsAfter: out}}, nil
}

func ratJSON(r *v1.Rational) map[string]any {
	n, _ := new(big.Int).SetString(r.GetNumerator(), 10)
	d, _ := new(big.Int).SetString(r.GetDenominator(), 10)
	return map[string]any{"numerator": n.Int64(), "denominator": d.Int64()}
}

func loadAlonzoGenesis(cfg *cardano.CardanoNodeConfig, g *v1.AlonzoGenesisInput) error {
	if g == nil {
		return dispatch.BadRequest(errors.New("alonzo_genesis is required for the Alonzo fork"))
	}
	for _, r := range []*v1.Rational{g.GetPriceMem(), g.GetPriceSteps()} {
		if _, err := ratArg("alonzo genesis price", r); err != nil {
			return err
		}
	}
	doc := map[string]any{
		"lovelacePerUTxOWord":  g.GetCoinsPerUtxoWord(),
		"executionPrices":      map[string]any{"prMem": ratJSON(g.GetPriceMem()), "prSteps": ratJSON(g.GetPriceSteps())},
		"maxTxExUnits":         map[string]any{"exUnitsMem": g.GetMaxTxExMem(), "exUnitsSteps": g.GetMaxTxExSteps()},
		"maxBlockExUnits":      map[string]any{"exUnitsMem": g.GetMaxBlockExMem(), "exUnitsSteps": g.GetMaxBlockExSteps()},
		"maxValueSize":         g.GetMaxValueSize(),
		"collateralPercentage": g.GetCollateralPercentage(),
		"maxCollateralInputs":  g.GetMaxCollateralInputs(),
		"costModels":           map[string]any{"PlutusV1": g.GetCostModelPlutusV1()},
	}
	b, err := json.Marshal(doc)
	if err != nil {
		return err
	}
	return cfg.LoadAlonzoGenesisFromReader(bytes.NewReader(b))
}

func loadConwayGenesis(cfg *cardano.CardanoNodeConfig, g *v1.ConwayGenesisInput) error {
	if g == nil {
		return dispatch.BadRequest(errors.New("conway_genesis is required for the Conway fork"))
	}
	pool, drep := g.GetPoolVotingThresholds(), g.GetDrepVotingThresholds()
	doc := map[string]any{
		"poolVotingThresholds": map[string]any{
			"motionNoConfidence": ratJSON(pool.GetMotionNoConfidence()), "committeeNormal": ratJSON(pool.GetCommitteeNormal()),
			"committeeNoConfidence": ratJSON(pool.GetCommitteeNoConfidence()), "hardForkInitiation": ratJSON(pool.GetHardForkInitiation()),
			"ppSecurityGroup": ratJSON(pool.GetPpSecurityGroup()),
		},
		"dRepVotingThresholds": map[string]any{
			"motionNoConfidence": ratJSON(drep.GetMotionNoConfidence()), "committeeNormal": ratJSON(drep.GetCommitteeNormal()),
			"committeeNoConfidence": ratJSON(drep.GetCommitteeNoConfidence()), "updateToConstitution": ratJSON(drep.GetUpdateToConstitution()),
			"hardForkInitiation": ratJSON(drep.GetHardForkInitiation()), "ppNetworkGroup": ratJSON(drep.GetPpNetworkGroup()),
			"ppEconomicGroup": ratJSON(drep.GetPpEconomicGroup()), "ppTechnicalGroup": ratJSON(drep.GetPpTechnicalGroup()),
			"ppGovGroup": ratJSON(drep.GetPpGovGroup()), "treasuryWithdrawal": ratJSON(drep.GetTreasuryWithdrawal()),
		},
		"committeeMinSize":           g.GetCommitteeMinSize(),
		"committeeMaxTermLength":     g.GetCommitteeMaxTermLength(),
		"govActionLifetime":          g.GetGovActionLifetime(),
		"govActionDeposit":           g.GetGovActionDeposit(),
		"dRepDeposit":                g.GetDrepDeposit(),
		"dRepActivity":               g.GetDrepActivity(),
		"minFeeRefScriptCostPerByte": ratJSON(g.GetMinFeeRefScriptCostPerByte()),
		"plutusV3CostModel":          g.GetCostModelPlutusV3(),
		// The loader requires these; the protocol parameters never read them.
		"constitution": map[string]any{"anchor": map[string]any{"dataHash": "ca41a91f399259bcefe57f9858e91f6d00e1a38d6d9c63d4052914ea7bd70cb2", "url": ""}},
		"committee":    map[string]any{"members": map[string]any{}, "threshold": map[string]any{"numerator": 2, "denominator": 3}},
	}
	b, err := json.Marshal(doc)
	if err != nil {
		return err
	}
	return cfg.LoadConwayGenesisFromReader(bytes.NewReader(b))
}

func rp(r cbor.Rat) *v1.Rational { return ratProto(&r) }

func conwayPParamsToProto(p *conway.ConwayProtocolParameters) *v1.ProtocolParameters {
	sp := &shelley.ShelleyProtocolParameters{
		MinFeeA: p.MinFeeA, MinFeeB: p.MinFeeB, MaxBlockBodySize: p.MaxBlockBodySize, MaxTxSize: p.MaxTxSize,
		MaxBlockHeaderSize: p.MaxBlockHeaderSize, KeyDeposit: p.KeyDeposit, PoolDeposit: p.PoolDeposit,
		MaxEpoch: p.MaxEpoch, NOpt: p.NOpt, A0: p.A0, Rho: p.Rho, Tau: p.Tau,
		ProtocolMajor: p.ProtocolVersion.Major, ProtocolMinor: p.ProtocolVersion.Minor, MinPoolCost: p.MinPoolCost,
		Decentralization: &cbor.Rat{Rat: new(big.Rat)},
	}
	out := shelleyPParamsToProto(sp)
	out.MinUtxoValue, out.D, out.ExtraEntropy = nil, nil, nil
	b := p.AdaPerUtxoByte
	out.CoinsPerUtxoByte = &b
	plutusToProto(out, p.CostModels, p.ExecutionCosts, p.MaxTxExUnits, p.MaxBlockExUnits, p.MaxValueSize, p.CollateralPercentage, p.MaxCollateralInputs)
	out.CostModelPlutusV3 = append([]int64(nil), p.CostModels[2]...)
	out.PoolVotingThresholds = &v1.PoolVotingThresholds{
		MotionNoConfidence: rp(p.PoolVotingThresholds.MotionNoConfidence), CommitteeNormal: rp(p.PoolVotingThresholds.CommitteeNormal),
		CommitteeNoConfidence: rp(p.PoolVotingThresholds.CommitteeNoConfidence), HardForkInitiation: rp(p.PoolVotingThresholds.HardForkInitiation),
		PpSecurityGroup: rp(p.PoolVotingThresholds.PpSecurityGroup),
	}
	d := p.DRepVotingThresholds
	out.DrepVotingThresholds = &v1.DRepVotingThresholds{
		MotionNoConfidence: rp(d.MotionNoConfidence), CommitteeNormal: rp(d.CommitteeNormal), CommitteeNoConfidence: rp(d.CommitteeNoConfidence),
		UpdateToConstitution: rp(d.UpdateToConstitution), HardForkInitiation: rp(d.HardForkInitiation), PpNetworkGroup: rp(d.PpNetworkGroup),
		PpEconomicGroup: rp(d.PpEconomicGroup), PpTechnicalGroup: rp(d.PpTechnicalGroup), PpGovGroup: rp(d.PpGovGroup),
		TreasuryWithdrawal: rp(d.TreasuryWithdrawal),
	}
	cms, ctl, gal, gad, drd, dra := uint32(p.MinCommitteeSize), p.CommitteeTermLimit, p.GovActionValidityPeriod, p.GovActionDeposit, p.DRepDeposit, p.DRepInactivityPeriod
	out.CommitteeMinSize, out.CommitteeMaxTermLength, out.GovActionLifetime = &cms, &ctl, &gal
	out.GovActionDeposit, out.DrepDeposit, out.DrepActivity = &gad, &drd, &dra
	out.MinFeeRefScriptCostPerByte = ratProto(p.MinFeeRefScriptCostPerByte)
	return out
}
