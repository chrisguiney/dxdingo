package main

import (
	"math/big"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
)

// conwayPParamsProto lists the parameters both nodes report after an
// enactment: every parameter but the cost models, which are not compared.
func conwayPParamsProto(p *conway.ConwayProtocolParameters) *v1.ProtocolParameters {
	u64 := func(x uint64) *uint64 { return &x }
	u32 := func(x uint) *uint32 { v := uint32(x); return &v } //nolint:gosec // parameter-sized
	rat := func(r *cbor.Rat) *v1.Rational {
		if r == nil || r.Rat == nil {
			return convert.RatProto(new(big.Rat))
		}
		return convert.RatProto(r.Rat)
	}
	val := func(r cbor.Rat) *v1.Rational { return rat(&r) }
	pool, drep := p.PoolVotingThresholds, p.DRepVotingThresholds
	return &v1.ProtocolParameters{
		MinFeeA:                    u64(uint64(p.MinFeeA)),
		MinFeeB:                    u64(uint64(p.MinFeeB)),
		MaxBlockBodySize:           u32(p.MaxBlockBodySize),
		MaxTxSize:                  u32(p.MaxTxSize),
		MaxBlockHeaderSize:         u32(p.MaxBlockHeaderSize),
		KeyDeposit:                 u64(uint64(p.KeyDeposit)),
		PoolDeposit:                u64(uint64(p.PoolDeposit)),
		MaxEpoch:                   u64(uint64(p.MaxEpoch)),
		NOpt:                       u32(p.NOpt),
		A0:                         rat(p.A0),
		Rho:                        rat(p.Rho),
		Tau:                        rat(p.Tau),
		ProtocolVersion:            &v1.ProtocolVersion{Major: uint32(p.ProtocolVersion.Major), Minor: uint32(p.ProtocolVersion.Minor)}, //nolint:gosec // version-sized
		MinPoolCost:                u64(p.MinPoolCost),
		CoinsPerUtxoByte:           u64(p.AdaPerUtxoByte),
		PriceMem:                   rat(p.ExecutionCosts.MemPrice),
		PriceSteps:                 rat(p.ExecutionCosts.StepPrice),
		MaxTxExMem:                 u64(uint64(p.MaxTxExUnits.Memory)),    //nolint:gosec // non-negative
		MaxTxExSteps:               u64(uint64(p.MaxTxExUnits.Steps)),     //nolint:gosec // non-negative
		MaxBlockExMem:              u64(uint64(p.MaxBlockExUnits.Memory)), //nolint:gosec // non-negative
		MaxBlockExSteps:            u64(uint64(p.MaxBlockExUnits.Steps)),  //nolint:gosec // non-negative
		MaxValueSize:               u32(p.MaxValueSize),
		CollateralPercentage:       u32(p.CollateralPercentage),
		MaxCollateralInputs:        u32(p.MaxCollateralInputs),
		CommitteeMinSize:           u32(p.MinCommitteeSize),
		CommitteeMaxTermLength:     u64(p.CommitteeTermLimit),
		GovActionLifetime:          u64(p.GovActionValidityPeriod),
		GovActionDeposit:           u64(p.GovActionDeposit),
		DrepDeposit:                u64(p.DRepDeposit),
		DrepActivity:               u64(p.DRepInactivityPeriod),
		MinFeeRefScriptCostPerByte: rat(p.MinFeeRefScriptCostPerByte),
		PoolVotingThresholds: &v1.PoolVotingThresholds{
			MotionNoConfidence:    val(pool.MotionNoConfidence),
			CommitteeNormal:       val(pool.CommitteeNormal),
			CommitteeNoConfidence: val(pool.CommitteeNoConfidence),
			HardForkInitiation:    val(pool.HardForkInitiation),
			PpSecurityGroup:       val(pool.PpSecurityGroup),
		},
		DrepVotingThresholds: &v1.DRepVotingThresholds{
			MotionNoConfidence:    val(drep.MotionNoConfidence),
			CommitteeNormal:       val(drep.CommitteeNormal),
			CommitteeNoConfidence: val(drep.CommitteeNoConfidence),
			UpdateToConstitution:  val(drep.UpdateToConstitution),
			HardForkInitiation:    val(drep.HardForkInitiation),
			PpNetworkGroup:        val(drep.PpNetworkGroup),
			PpEconomicGroup:       val(drep.PpEconomicGroup),
			PpTechnicalGroup:      val(drep.PpTechnicalGroup),
			PpGovGroup:            val(drep.PpGovGroup),
			TreasuryWithdrawal:    val(drep.TreasuryWithdrawal),
		},
	}
}
