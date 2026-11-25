package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"google.golang.org/protobuf/proto"

	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func u64Epoch(x uint64) *uint64 { return &x }
func u32Epoch(x uint32) *uint32 { return &x }

func shelleyPParams() *v1.ProtocolParameters {
	return &v1.ProtocolParameters{
		MinFeeA: u64Epoch(44), MinFeeB: u64Epoch(155381), MaxBlockBodySize: u32Epoch(65536), MaxTxSize: u32Epoch(16384),
		MaxBlockHeaderSize: u32Epoch(1100), KeyDeposit: u64Epoch(2_000_000), PoolDeposit: u64Epoch(500_000_000),
		MaxEpoch: u64Epoch(18), NOpt: u32Epoch(150), A0: rat("3", "10"), Rho: rat("3", "1000"), Tau: rat("1", "5"),
		D: rat("1", "2"), ExtraEntropy: &v1.Nonce{}, ProtocolVersion: &v1.ProtocolVersion{Major: 2},
		MinUtxoValue: u64Epoch(1_000_000), MinPoolCost: u64Epoch(340_000_000),
	}
}

func genesisKey(i byte) []byte { return bytes.Repeat([]byte{i}, 28) }

func hexbEpoch(s string) []byte {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return b
}

// prop is a proposal made during the epoch it targets (epoch length 100).
func prop(key byte, target uint64, cbor string) *v1.PParamProposal {
	return propAt(key, target, target*100+10, cbor)
}

func propAt(key byte, target, slot uint64, cbor string) *v1.PParamProposal {
	return &v1.PParamProposal{GenesisKeyHash: genesisKey(key), TargetEpoch: target, AddedSlot: slot, Update: hexbEpoch(cbor)}
}

func plutusPParams(p *v1.ProtocolParameters) *v1.ProtocolParameters {
	p.PriceMem, p.PriceSteps = rat("577", "10000"), rat("721", "10000000")
	p.MaxTxExMem, p.MaxTxExSteps = u64Epoch(14_000_000), u64Epoch(10_000_000_000)
	p.MaxBlockExMem, p.MaxBlockExSteps = u64Epoch(62_000_000), u64Epoch(40_000_000_000)
	p.MaxValueSize, p.CollateralPercentage, p.MaxCollateralInputs = u32Epoch(5000), u32Epoch(150), u32Epoch(3)
	return p
}

func alonzoPParamsProto() *v1.ProtocolParameters {
	p := plutusPParams(shelleyPParams())
	p.MinUtxoValue = nil
	p.CoinsPerUtxoWord = u64Epoch(34482)
	p.ProtocolVersion = &v1.ProtocolVersion{Major: 5}
	return p
}

func babbagePParamsProto() *v1.ProtocolParameters {
	p := plutusPParams(shelleyPParams())
	p.MinUtxoValue, p.D, p.ExtraEntropy = nil, nil, nil
	p.CoinsPerUtxoByte = u64Epoch(4310)
	p.ProtocolVersion = &v1.ProtocolVersion{Major: 7}
	return p
}

func pparamReq(quorum uint64, ps ...*v1.PParamProposal) *v1.EpochPParamUpdateRequest {
	return &v1.EpochPParamUpdateRequest{
		Context:   &v1.LedgerContext{Era: v1.Era_ERA_SHELLEY, Epoch: 11},
		Globals:   &v1.Globals{Quorum: quorum, EpochLength: 100},
		Pparams:   shelleyPParams(),
		Proposals: ps,
	}
}

// Update CBOR is the ShelleyPParamsUpdate map: key 0 minfee a, 14 protocol
// version, 3 max tx size.
const (
	minFeeA100 = "a1001864"
	minFeeA200 = "a10018c8"
	pv40       = "a10e820400"
	pv30       = "a10e820300"
	maxTx70000 = "a1031a00011170"
	pv25       = "a10e820205"
)

// Proposals for the boundary into epoch 11 target epoch 10 and each genesis
// key's latest one is its vote (votedFuturePParams, Rules/Ppup.hs).
func TestEpochPParamUpdate(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		req     *v1.EpochPParamUpdateRequest
		adopted bool
		minFeeA uint64
		version uint32
	}{
		"quorum reached": {pparamReq(2, prop(1, 10, minFeeA100), prop(2, 10, minFeeA100)), true, 100, 2},
		"below quorum":   {pparamReq(3, prop(1, 10, minFeeA100), prop(2, 10, minFeeA100)), false, 44, 2},
		"two values both reach quorum": {pparamReq(2,
			prop(1, 10, minFeeA100), prop(2, 10, minFeeA100), prop(3, 10, minFeeA200), prop(4, 10, minFeeA200)), false, 44, 2},
		"latest proposal per key": {pparamReq(2,
			prop(1, 10, minFeeA100), prop(1, 10, minFeeA200), prop(2, 10, minFeeA200)), true, 200, 2},
		"other target epoch ignored":                {pparamReq(2, prop(1, 10, minFeeA100), prop(2, 11, minFeeA100)), false, 44, 2},
		"protocol version follows":                  {pparamReq(1, prop(1, 10, pv30)), true, 44, 3},
		"latest proposal replaces the earlier vote": {pparamReq(1, prop(1, 10, minFeeA100), prop(1, 10, pv30)), true, 44, 3},
		// Carried: made in epoch 9 (slots 900-999) for epoch 10.
		"carried proposal counts": {pparamReq(1, propAt(1, 10, 950, minFeeA100)), true, 100, 2},
		"carried proposals with an illegal version are all dropped": {pparamReq(1,
			propAt(1, 10, 950, minFeeA100), propAt(2, 10, 960, pv25)), false, 44, 2},
		"a newer proposal overrides the carried one": {pparamReq(1,
			propAt(1, 10, 950, minFeeA100), propAt(1, 10, 1010, minFeeA200)), true, 200, 2},
		"a proposal made this epoch survives dropped carry-over": {pparamReq(1,
			propAt(1, 10, 950, pv25), propAt(2, 10, 1010, minFeeA100)), true, 100, 2},
		"block sizes guard": {pparamReq(1, prop(1, 10, maxTx70000)), false, 44, 2},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := epochPParamUpdate(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetAdopted() != tc.adopted {
				t.Fatalf("adopted = %v, want %v", got.GetAdopted(), tc.adopted)
			}
			pp := got.GetPparams()
			if pp.GetMinFeeA() != tc.minFeeA || pp.GetProtocolVersion().GetMajor() != tc.version {
				t.Fatalf("min_fee_a = %d major = %d, want %d / %d", pp.GetMinFeeA(), pp.GetProtocolVersion().GetMajor(), tc.minFeeA, tc.version)
			}
			if pp.GetMaxTxSize() != 16384 && !tc.adopted {
				t.Fatalf("unadopted pparams changed")
			}
		})
	}
}

func TestEpochPParamUpdateUndecodable(t *testing.T) {
	t.Parallel()
	got, err := epochPParamUpdate(context.Background(), pparamReq(1, prop(1, 10, "ff")))
	if err != nil {
		t.Fatal(err)
	}
	f := got.GetRejection().GetFailures()
	if len(f) != 1 || f[0].GetRule() != "PPUP" || f[0].GetName() != "DeserialiseFailure" {
		t.Fatalf("rejection = %v", got.GetRejection())
	}
}

func TestEpochPParamUpdateDomain(t *testing.T) {
	t.Parallel()
	nonShelley := pparamReq(1)
	nonShelley.Context.Era = v1.Era_ERA_CONWAY
	missing := pparamReq(1)
	missing.Pparams.MinFeeB = nil
	for name, tc := range map[string]struct {
		req  *v1.EpochPParamUpdateRequest
		code v1.HarnessErrorCode
	}{
		"non-shelley era":   {nonShelley, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"missing parameter": {missing, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		if _, err := epochPParamUpdate(context.Background(), tc.req); dispatch.Code(err) != tc.code {
			t.Fatalf("%s: err = %v, want %v", name, err, tc.code)
		}
	}
}

// The same update system runs in every pre-Conway era; the parameters carry
// each era's own table.
func TestEpochPParamUpdateEras(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		era v1.Era
		pp  *v1.ProtocolParameters
	}{
		"allegra": {v1.Era_ERA_ALLEGRA, shelleyPParams()},
		"mary":    {v1.Era_ERA_MARY, shelleyPParams()},
		"alonzo":  {v1.Era_ERA_ALONZO, alonzoPParamsProto()},
		"babbage": {v1.Era_ERA_BABBAGE, babbagePParamsProto()},
	} {
		req := pparamReq(1, prop(1, 10, minFeeA100))
		req.Context.Era, req.Pparams = tc.era, tc.pp
		got, err := epochPParamUpdate(context.Background(), req)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		if !got.GetAdopted() || got.GetPparams().GetMinFeeA() != 100 {
			t.Fatalf("%s: adopted=%v min_fee_a=%d", name, got.GetAdopted(), got.GetPparams().GetMinFeeA())
		}
		want := proto.Clone(tc.pp).(*v1.ProtocolParameters)
		want.MinFeeA = u64Epoch(100)
		if !proto.Equal(got.GetPparams(), want) {
			t.Fatalf("%s: other parameters changed:\n got %v\nwant %v", name, got.GetPparams(), want)
		}
	}
}
