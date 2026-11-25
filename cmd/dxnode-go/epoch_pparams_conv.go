package main

import (
	"errors"
	"math"
	"math/big"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Alonzo and Babbage parameters as the proto table. The Alonzo and Babbage
// tables carry only the cost models of Plutus V1 and V2 (map keys 0 and 1).

func nonNegRat(name string, r *v1.Rational) (*cbor.Rat, error) {
	x, err := ratArg(name, r)
	if err != nil {
		return nil, err
	}
	if err := nonNegativeInterval(name, x); err != nil {
		return nil, err
	}
	return &cbor.Rat{Rat: x}, nil
}

// plutusFields are the parameters Alonzo and Babbage add to the Shelley set.
type plutusFields struct {
	costModels map[uint][]int64
	prices     common.ExUnitPrice
	maxTx      common.ExUnits
	maxBlock   common.ExUnits
	maxValue   uint
	collateral uint
	maxColl    uint
}

func plutusFromProto(p *v1.ProtocolParameters) (plutusFields, error) {
	var f plutusFields
	if p.MaxTxExMem == nil || p.MaxTxExSteps == nil || p.MaxBlockExMem == nil || p.MaxBlockExSteps == nil ||
		p.MaxValueSize == nil || p.CollateralPercentage == nil || p.MaxCollateralInputs == nil {
		return f, dispatch.BadRequest(errors.New("missing Plutus-era parameter"))
	}
	var err error
	if f.prices.MemPrice, err = nonNegRat("price_mem", p.GetPriceMem()); err != nil {
		return f, err
	}
	if f.prices.StepPrice, err = nonNegRat("price_steps", p.GetPriceSteps()); err != nil {
		return f, err
	}
	for _, v := range []uint64{p.GetMaxTxExMem(), p.GetMaxTxExSteps(), p.GetMaxBlockExMem(), p.GetMaxBlockExSteps()} {
		if v > math.MaxInt64 {
			return f, dispatch.Unsupported(errors.New("execution units exceed int64"))
		}
	}
	if p.GetCollateralPercentage() > 0xffff || p.GetMaxCollateralInputs() > 0xffff {
		return f, dispatch.Unsupported(errors.New("collateral parameter out of range"))
	}
	f.maxTx = common.ExUnits{Memory: int64(p.GetMaxTxExMem()), Steps: int64(p.GetMaxTxExSteps())}
	f.maxBlock = common.ExUnits{Memory: int64(p.GetMaxBlockExMem()), Steps: int64(p.GetMaxBlockExSteps())}
	f.maxValue, f.collateral, f.maxColl = uint(p.GetMaxValueSize()), uint(p.GetCollateralPercentage()), uint(p.GetMaxCollateralInputs())
	if len(p.GetCostModelPlutusV1()) > 0 || len(p.GetCostModelPlutusV2()) > 0 {
		f.costModels = map[uint][]int64{}
		if len(p.GetCostModelPlutusV1()) > 0 {
			f.costModels[0] = append([]int64(nil), p.GetCostModelPlutusV1()...)
		}
		if len(p.GetCostModelPlutusV2()) > 0 {
			f.costModels[1] = append([]int64(nil), p.GetCostModelPlutusV2()...)
		}
	}
	return f, nil
}

func plutusToProto(out *v1.ProtocolParameters, cm map[uint][]int64, prices common.ExUnitPrice, maxTx, maxBlock common.ExUnits, maxValue, collateral, maxColl uint) {
	out.CostModelPlutusV1 = append([]int64(nil), cm[0]...)
	out.CostModelPlutusV2 = append([]int64(nil), cm[1]...)
	out.PriceMem, out.PriceSteps = ratProto(prices.MemPrice), ratProto(prices.StepPrice)
	u64Epoch := func(x int64) *uint64 { v := uint64(x); return &v }
	out.MaxTxExMem, out.MaxTxExSteps = u64Epoch(maxTx.Memory), u64Epoch(maxTx.Steps)
	out.MaxBlockExMem, out.MaxBlockExSteps = u64Epoch(maxBlock.Memory), u64Epoch(maxBlock.Steps)
	v, c, m := uint32(maxValue), uint32(collateral), uint32(maxColl)
	out.MaxValueSize, out.CollateralPercentage, out.MaxCollateralInputs = &v, &c, &m
}

func maryPParamsFromProto(p *v1.ProtocolParameters) (*mary.MaryProtocolParameters, error) {
	sp, err := shelleyPParamsFromProto(p)
	if err != nil {
		return nil, err
	}
	mp := mary.MaryProtocolParameters(*sp)
	return &mp, nil
}

// alonzoPParamsFromProto: the Alonzo table has no min_utxo_value; the
// reference has no such parameter, so it is never read.
func alonzoPParamsFromProto(p *v1.ProtocolParameters) (*alonzo.AlonzoProtocolParameters, error) {
	filled := cloneWithMinUtxo(p)
	sp, err := shelleyPParamsFromProto(filled)
	if err != nil {
		return nil, err
	}
	if p.CoinsPerUtxoWord == nil {
		return nil, dispatch.BadRequest(errors.New("missing coins_per_utxo_word"))
	}
	f, err := plutusFromProto(p)
	if err != nil {
		return nil, err
	}
	return &alonzo.AlonzoProtocolParameters{
		MinFeeA: sp.MinFeeA, MinFeeB: sp.MinFeeB, MaxBlockBodySize: sp.MaxBlockBodySize, MaxTxSize: sp.MaxTxSize,
		MaxBlockHeaderSize: sp.MaxBlockHeaderSize, KeyDeposit: sp.KeyDeposit, PoolDeposit: sp.PoolDeposit,
		MaxEpoch: sp.MaxEpoch, NOpt: sp.NOpt, A0: sp.A0, Rho: sp.Rho, Tau: sp.Tau,
		Decentralization: sp.Decentralization, ExtraEntropy: sp.ExtraEntropy,
		ProtocolMajor: sp.ProtocolMajor, ProtocolMinor: sp.ProtocolMinor, MinPoolCost: sp.MinPoolCost,
		AdaPerUtxoByte: p.GetCoinsPerUtxoWord(), CostModels: f.costModels, ExecutionCosts: f.prices,
		MaxTxExUnits: f.maxTx, MaxBlockExUnits: f.maxBlock, MaxValueSize: f.maxValue,
		CollateralPercentage: f.collateral, MaxCollateralInputs: f.maxColl,
	}, nil
}

// cloneWithMinUtxo gives the Shelley decoder the min_utxo_value the Plutus
// eras do not carry, and the d and extra_entropy Babbage lacks.
func cloneWithMinUtxo(p *v1.ProtocolParameters) *v1.ProtocolParameters {
	c := proto.Clone(p).(*v1.ProtocolParameters)
	zero := uint64(0)
	c.MinUtxoValue = &zero
	if c.D == nil {
		c.D = &v1.Rational{Numerator: "0", Denominator: "1"}
	}
	if c.ExtraEntropy == nil {
		c.ExtraEntropy = &v1.Nonce{}
	}
	return c
}

func babbagePParamsFromProto(p *v1.ProtocolParameters) (*babbage.BabbageProtocolParameters, error) {
	sp, err := shelleyPParamsFromProto(cloneWithMinUtxo(p))
	if err != nil {
		return nil, err
	}
	if p.CoinsPerUtxoByte == nil {
		return nil, dispatch.BadRequest(errors.New("missing coins_per_utxo_byte"))
	}
	f, err := plutusFromProto(p)
	if err != nil {
		return nil, err
	}
	return &babbage.BabbageProtocolParameters{
		MinFeeA: sp.MinFeeA, MinFeeB: sp.MinFeeB, MaxBlockBodySize: sp.MaxBlockBodySize, MaxTxSize: sp.MaxTxSize,
		MaxBlockHeaderSize: sp.MaxBlockHeaderSize, KeyDeposit: sp.KeyDeposit, PoolDeposit: sp.PoolDeposit,
		MaxEpoch: sp.MaxEpoch, NOpt: sp.NOpt, A0: sp.A0, Rho: sp.Rho, Tau: sp.Tau,
		ProtocolMajor: sp.ProtocolMajor, ProtocolMinor: sp.ProtocolMinor, MinPoolCost: sp.MinPoolCost,
		AdaPerUtxoByte: p.GetCoinsPerUtxoByte(), CostModels: f.costModels, ExecutionCosts: f.prices,
		MaxTxExUnits: f.maxTx, MaxBlockExUnits: f.maxBlock, MaxValueSize: f.maxValue,
		CollateralPercentage: f.collateral, MaxCollateralInputs: f.maxColl,
	}, nil
}

func alonzoPParamsToProto(p *alonzo.AlonzoProtocolParameters) *v1.ProtocolParameters {
	sp := &shelley.ShelleyProtocolParameters{
		MinFeeA: p.MinFeeA, MinFeeB: p.MinFeeB, MaxBlockBodySize: p.MaxBlockBodySize, MaxTxSize: p.MaxTxSize,
		MaxBlockHeaderSize: p.MaxBlockHeaderSize, KeyDeposit: p.KeyDeposit, PoolDeposit: p.PoolDeposit,
		MaxEpoch: p.MaxEpoch, NOpt: p.NOpt, A0: p.A0, Rho: p.Rho, Tau: p.Tau, Decentralization: p.Decentralization,
		ExtraEntropy: p.ExtraEntropy, ProtocolMajor: p.ProtocolMajor, ProtocolMinor: p.ProtocolMinor, MinPoolCost: p.MinPoolCost,
	}
	out := shelleyPParamsToProto(sp)
	out.MinUtxoValue = nil
	w := p.AdaPerUtxoByte
	out.CoinsPerUtxoWord = &w
	plutusToProto(out, p.CostModels, p.ExecutionCosts, p.MaxTxExUnits, p.MaxBlockExUnits, p.MaxValueSize, p.CollateralPercentage, p.MaxCollateralInputs)
	return out
}

func babbagePParamsToProto(p *babbage.BabbageProtocolParameters) *v1.ProtocolParameters {
	sp := &shelley.ShelleyProtocolParameters{
		MinFeeA: p.MinFeeA, MinFeeB: p.MinFeeB, MaxBlockBodySize: p.MaxBlockBodySize, MaxTxSize: p.MaxTxSize,
		MaxBlockHeaderSize: p.MaxBlockHeaderSize, KeyDeposit: p.KeyDeposit, PoolDeposit: p.PoolDeposit,
		MaxEpoch: p.MaxEpoch, NOpt: p.NOpt, A0: p.A0, Rho: p.Rho, Tau: p.Tau,
		ProtocolMajor: p.ProtocolMajor, ProtocolMinor: p.ProtocolMinor, MinPoolCost: p.MinPoolCost,
		Decentralization: &cbor.Rat{Rat: new(big.Rat)},
	}
	out := shelleyPParamsToProto(sp)
	out.MinUtxoValue, out.D, out.ExtraEntropy = nil, nil, nil
	b := p.AdaPerUtxoByte
	out.CoinsPerUtxoByte = &b
	plutusToProto(out, p.CostModels, p.ExecutionCosts, p.MaxTxExUnits, p.MaxBlockExUnits, p.MaxValueSize, p.CollateralPercentage, p.MaxCollateralInputs)
	return out
}

func maryPParamsToProto(p *mary.MaryProtocolParameters) *v1.ProtocolParameters {
	sp := shelley.ShelleyProtocolParameters(*p)
	return shelleyPParamsToProto(&sp)
}
func shelleyPParamsFromProto(p *v1.ProtocolParameters) (*shelley.ShelleyProtocolParameters, error) {
	if p.MinFeeA == nil || p.MinFeeB == nil || p.MaxBlockBodySize == nil || p.MaxTxSize == nil ||
		p.MaxBlockHeaderSize == nil || p.KeyDeposit == nil || p.PoolDeposit == nil || p.MaxEpoch == nil ||
		p.NOpt == nil || p.MinUtxoValue == nil || p.MinPoolCost == nil || p.ProtocolVersion == nil {
		return nil, dispatch.BadRequest(errors.New("missing Shelley parameter"))
	}
	a0, err := ratArg("a0", p.GetA0())
	if err != nil {
		return nil, err
	}
	if err := nonNegativeInterval("a0", a0); err != nil {
		return nil, err
	}
	var unit [3]*big.Rat
	for i, f := range []struct {
		name string
		r    *v1.Rational
	}{{"rho", p.GetRho()}, {"tau", p.GetTau()}, {"d", p.GetD()}} {
		x, err := ratArg(f.name, f.r)
		if err != nil {
			return nil, err
		}
		if err := unitInterval(f.name, x); err != nil {
			return nil, err
		}
		unit[i] = x
	}
	if p.GetNOpt() > 0xffff || p.GetMaxEpoch() > 0xffffffff || p.GetMaxBlockHeaderSize() > 0xffff {
		return nil, dispatch.Unsupported(errors.New("n_opt, max_epoch or max_block_header_size out of range"))
	}
	if p.GetProtocolVersion().GetMajor() > maxProtocolMajor {
		return nil, dispatch.Unsupported(errors.New("protocol major above the reference maximum"))
	}
	nonce := common.Nonce{}
	switch len(p.GetExtraEntropy().GetHash()) {
	case 0:
	case 32:
		nonce.Type = common.NonceTypeNonce
		copy(nonce.Value[:], p.GetExtraEntropy().GetHash())
	default:
		return nil, dispatch.BadRequest(errors.New("extra entropy must be 32 bytes"))
	}
	return &shelley.ShelleyProtocolParameters{
		MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
		MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
		MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
		PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
		A0: &cbor.Rat{Rat: a0}, Rho: &cbor.Rat{Rat: unit[0]}, Tau: &cbor.Rat{Rat: unit[1]},
		Decentralization: &cbor.Rat{Rat: unit[2]}, ExtraEntropy: nonce,
		ProtocolMajor: uint(p.GetProtocolVersion().GetMajor()), ProtocolMinor: uint(p.GetProtocolVersion().GetMinor()),
		MinUtxoValue: uint(p.GetMinUtxoValue()), MinPoolCost: p.GetMinPoolCost(),
	}, nil
}

func ratProto(r *cbor.Rat) *v1.Rational {
	return &v1.Rational{Numerator: r.Num().String(), Denominator: r.Denom().String()}
}

func shelleyPParamsToProto(p *shelley.ShelleyProtocolParameters) *v1.ProtocolParameters {
	u64Epoch := func(x uint) *uint64 { v := uint64(x); return &v }
	u32Epoch := func(x uint) *uint32 { v := uint32(x); return &v }
	mp := p.MinPoolCost
	extra := &v1.Nonce{}
	if p.ExtraEntropy.Type == common.NonceTypeNonce {
		extra.Hash = append([]byte(nil), p.ExtraEntropy.Value[:]...)
	}
	return &v1.ProtocolParameters{
		MinFeeA: u64Epoch(p.MinFeeA), MinFeeB: u64Epoch(p.MinFeeB), MaxBlockBodySize: u32Epoch(p.MaxBlockBodySize),
		MaxTxSize: u32Epoch(p.MaxTxSize), MaxBlockHeaderSize: u32Epoch(p.MaxBlockHeaderSize),
		KeyDeposit: u64Epoch(p.KeyDeposit), PoolDeposit: u64Epoch(p.PoolDeposit), MaxEpoch: u64Epoch(p.MaxEpoch),
		NOpt: u32Epoch(p.NOpt), A0: ratProto(p.A0), Rho: ratProto(p.Rho), Tau: ratProto(p.Tau),
		D: ratProto(p.Decentralization), ExtraEntropy: extra,
		ProtocolVersion: &v1.ProtocolVersion{Major: uint32(p.ProtocolMajor), Minor: uint32(p.ProtocolMinor)},
		MinUtxoValue:    u64Epoch(p.MinUtxoValue), MinPoolCost: &mp,
	}
}
