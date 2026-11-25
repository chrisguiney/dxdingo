package main

import (
	"context"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/testgenesis"
)

func gr(r *cbor.Rat) *v1.Rational { return ratProto(r) }

func forkReq(from, to, major uint32) *v1.EpochHardForkRequest {
	pp := shelleyPParams()
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: major}
	return &v1.EpochHardForkRequest{From: &v1.ProtocolVersion{Major: from}, To: &v1.ProtocolVersion{Major: to}, Pparams: pp}
}

func alonzoGenesisInput() *v1.AlonzoGenesisInput {
	g := testgenesis.Alonzo()
	return &v1.AlonzoGenesisInput{
		CoinsPerUtxoWord: g.LovelacePerUtxoWord,
		PriceMem:         gr(g.ExecutionPrices.Mem), PriceSteps: gr(g.ExecutionPrices.Steps),
		MaxTxExMem: g.MaxTxExUnits.Mem, MaxTxExSteps: g.MaxTxExUnits.Steps,
		MaxBlockExMem: g.MaxBlockExUnits.Mem, MaxBlockExSteps: g.MaxBlockExUnits.Steps,
		MaxValueSize: uint32(g.MaxValueSize), CollateralPercentage: uint32(g.CollateralPercentage),
		MaxCollateralInputs: uint32(g.MaxCollateralInputs), CostModelPlutusV1: g.CostModels["PlutusV1"],
	}
}

func conwayGenesisInput() *v1.ConwayGenesisInput {
	g := testgenesis.Conway()
	p, d := g.PoolVotingThresholds, g.DRepVotingThresholds
	return &v1.ConwayGenesisInput{
		PoolVotingThresholds: &v1.PoolVotingThresholds{
			MotionNoConfidence: gr(p.MotionNoConfidence), CommitteeNormal: gr(p.CommitteeNormal),
			CommitteeNoConfidence: gr(p.CommitteeNoConfidence), HardForkInitiation: gr(p.HardForkInitiation), PpSecurityGroup: gr(p.PpSecurityGroup),
		},
		DrepVotingThresholds: &v1.DRepVotingThresholds{
			MotionNoConfidence: gr(d.MotionNoConfidence), CommitteeNormal: gr(d.CommitteeNormal), CommitteeNoConfidence: gr(d.CommitteeNoConfidence),
			UpdateToConstitution: gr(d.UpdateToConstitution), HardForkInitiation: gr(d.HardForkInitiation), PpNetworkGroup: gr(d.PpNetworkGroup),
			PpEconomicGroup: gr(d.PpEconomicGroup), PpTechnicalGroup: gr(d.PpTechnicalGroup), PpGovGroup: gr(d.PpGovGroup), TreasuryWithdrawal: gr(d.TreasuryWithdrawal),
		},
		CommitteeMinSize: uint32(g.MinCommitteeSize), CommitteeMaxTermLength: g.CommitteeTermLimit,
		GovActionLifetime: g.GovActionValidityPeriod, GovActionDeposit: g.GovActionDeposit,
		DrepDeposit: g.DRepDeposit, DrepActivity: g.DRepInactivityPeriod,
		MinFeeRefScriptCostPerByte: gr(g.MinFeeRefScriptCostPerByte), CostModelPlutusV3: g.PlutusV3CostModel,
	}
}

func run(t *testing.T, req *v1.EpochHardForkRequest) *v1.ProtocolParameters {
	t.Helper()
	got, err := epochHardFork(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection: %v", got.GetRejection())
	}
	return got.GetDelta().GetPparamsAfter()
}

// The reference translation keeps every parameter, including the protocol
// version, across Shelley to Allegra and Allegra to Mary.
func TestEpochHardForkShelleyFamily(t *testing.T) {
	t.Parallel()
	for name, req := range map[string]*v1.EpochHardForkRequest{
		"allegra with the version already enacted": forkReq(2, 3, 3),
		"mary": forkReq(3, 4, 3),
	} {
		if got := run(t, req); !proto.Equal(got, req.GetPparams()) {
			t.Fatalf("%s: pparams changed:\n got %v\nwant %v", name, got, req.GetPparams())
		}
	}
}

// Mary to Alonzo: the Alonzo genesis supplies the Plutus parameters and the
// Shelley set carries over; Alonzo has no min_utxo_value.
func TestEpochHardForkAlonzo(t *testing.T) {
	t.Parallel()
	req := forkReq(4, 5, 4)
	req.AlonzoGenesis = alonzoGenesisInput()
	g := req.AlonzoGenesis
	got := run(t, req)
	if got.GetCoinsPerUtxoWord() != g.CoinsPerUtxoWord || len(got.GetCostModelPlutusV1()) != len(g.CostModelPlutusV1) ||
		got.GetMaxTxExMem() != g.MaxTxExMem || got.GetMaxValueSize() != g.MaxValueSize || !proto.Equal(got.GetPriceMem(), g.PriceMem) {
		t.Fatalf("genesis values missing: %v", got)
	}
	if got.MinUtxoValue != nil || got.GetMinFeeA() != 44 || got.GetProtocolVersion().GetMajor() != 4 {
		t.Fatalf("shelley set changed: %v", got)
	}
}

// Alonzo to Babbage: coins_per_utxo_byte is coins_per_utxo_word / 8; the
// Plutus V1 model carries over; d, extra entropy and min_utxo_value are gone.
func TestEpochHardForkBabbage(t *testing.T) {
	t.Parallel()
	req := forkReq(6, 7, 6)
	req.Pparams = alonzoPParamsProto()
	req.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 6}
	req.Pparams.CostModelPlutusV1 = testgenesis.Alonzo().CostModels["PlutusV1"]
	got := run(t, req)
	if got.GetCoinsPerUtxoByte() != 34482/8 || got.CoinsPerUtxoWord != nil || got.D != nil || got.ExtraEntropy != nil {
		t.Fatalf("era-specific parameters wrong: %v", got)
	}
	if len(got.GetCostModelPlutusV1()) != len(req.Pparams.CostModelPlutusV1) {
		t.Fatalf("plutus v1 model lost")
	}
}

// Babbage to Conway: the Conway genesis supplies governance parameters and the
// Plutus V3 model; everything else carries over.
func TestEpochHardForkConway(t *testing.T) {
	t.Parallel()
	req := forkReq(8, 9, 8)
	req.Pparams = babbagePParamsProto()
	req.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 8}
	req.ConwayGenesis = conwayGenesisInput()
	got := run(t, req)
	g := req.ConwayGenesis
	if got.GetGovActionDeposit() != g.GovActionDeposit || got.GetDrepDeposit() != g.DrepDeposit || got.GetDrepActivity() != g.DrepActivity ||
		got.GetCommitteeMaxTermLength() != g.CommitteeMaxTermLength || len(got.GetCostModelPlutusV3()) != len(g.CostModelPlutusV3) ||
		!proto.Equal(got.GetMinFeeRefScriptCostPerByte(), g.MinFeeRefScriptCostPerByte) ||
		!proto.Equal(got.GetDrepVotingThresholds().GetPpGovGroup(), g.DrepVotingThresholds.PpGovGroup) {
		t.Fatalf("genesis values missing: %v", got)
	}
	if got.GetCoinsPerUtxoByte() != 4310 || got.GetMinFeeA() != 44 || got.GetProtocolVersion().GetMajor() != 8 {
		t.Fatalf("carried parameters changed: %v", got)
	}
}

func TestEpochHardForkUnsupported(t *testing.T) {
	t.Parallel()
	for name, req := range map[string]*v1.EpochHardForkRequest{
		"not consecutive":          forkReq(4, 6, 4),
		"no era starts at":         forkReq(5, 6, 5),
		"alonzo needs its genesis": forkReq(4, 5, 4),
	} {
		_, err := epochHardFork(context.Background(), req)
		want := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
		if name == "alonzo needs its genesis" {
			want = v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST
		}
		if dispatch.Code(err) != want {
			t.Fatalf("%s: err = %v, want %v", name, err, want)
		}
	}
}
