package main

import (
	"bytes"
	"context"
	"fmt"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected values are derived by hand from the Haskell definitions:
//   - sizeTxF: the Shelley transaction bytes; from Alonzo the canonical
//     [body, wits, aux] size, 1 + |body| + |wits| + |aux|.
//   - getMinFeeTx: a * size + b + ceil(prMem*mem + prSteps*steps), plus the
//     tiered reference-script fee in Conway.
//   - tierRefScriptFee: 25600-byte tiers, price times 6/5 per tier, floor once.
//   - getMinCoinTxOut: Shelley the flat parameter, Alonzo
//     coinsPerUTxOWord * entry words, Babbage on (160 + |txout|) * coinsPerUTxOByte.

func feeMustCbor(t testing.TB, v any) []byte {
	t.Helper()
	b, err := cbor.Encode(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func feeCat(parts ...[]byte) []byte { return bytes.Join(parts, nil) }

func feeAddr() []byte { return append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...) }

func feeTxIn(b byte, idx uint) []any { return []any{bytes.Repeat([]byte{b}, 32), idx} }

// feeBody is a transaction body with one input, one pure-ADA output, a fee and
// a ttl (mandatory in Shelley).
func feeBody(t testing.TB, fee uint64) []byte {
	return feeMustCbor(t, map[uint]any{
		0: []any{feeTxIn(0x01, 0)},
		1: []any{[]any{feeAddr(), uint64(2_000_000)}},
		2: fee,
		3: uint64(1000),
	})
}

// feeShelleyTx is [body, wits, null].
func feeShelleyTx(t testing.TB) []byte {
	return feeCat([]byte{0x83}, feeBody(t, 200_000), []byte{0xa0, 0xf6})
}

// feeConwayTx is [body, wits, true, null], the 4-element wire form.
func feeConwayTx(body []byte) []byte {
	return feeCat([]byte{0x84}, body, []byte{0xa0, 0xf5, 0xf6})
}

// feeBabbageOut is a map-form output; a non-nil script becomes a script
// reference holding a Plutus V2 script of that many bytes.
func feeBabbageOut(t testing.TB, coin uint64, scriptLen int) []byte {
	m := map[uint]any{0: feeAddr(), 1: coin}
	out := feeMustCbor(t, m)
	if scriptLen == 0 {
		return out
	}
	// Append key 3: #6.24(bytes .cbor [2, script]).
	inner := feeMustCbor(t, []any{uint(2), make([]byte, scriptLen)})
	ref := feeCat([]byte{0xd8, 0x18}, feeMustCbor(t, inner))
	out[0]++ // a2 -> a3
	return feeCat(out, []byte{0x03}, ref)
}

func feeUtxoEntry(b byte, idx uint32, txout []byte) *v1.UtxoEntry {
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{b}, 32), Index: idx}, Txout: txout}
}

func feeU64(x uint64) *uint64 { return &x }

func feeMustOK(t *testing.T, resp interface{ GetRejection() *v1.Rejection }, err error) {
	t.Helper()
	if err != nil {
		t.Fatal(err)
	}
	if r := resp.GetRejection(); r != nil {
		t.Fatalf("unexpected rejection %v", r)
	}
}

func feeWantCode(t *testing.T, err error, want v1.HarnessErrorCode) {
	t.Helper()
	if got := dispatch.Code(err); got != want {
		t.Fatalf("code = %v (%v), want %v", got, err, want)
	}
}

func TestFeeTxSize(t *testing.T) {
	ctx := context.Background()
	body := feeBody(t, 200_000)
	shelley := feeShelleyTx(t)
	conway := feeConwayTx(body)

	got, err := feeTxSize(ctx, &v1.FeeTxSizeRequest{Era: v1.Era_ERA_SHELLEY, Tx: shelley})
	feeMustOK(t, got, err)
	if got.GetSize() != uint64(len(shelley)) {
		t.Fatalf("shelley size = %d, want %d", got.GetSize(), len(shelley))
	}

	// Conway: 1 (array header) + body + wits (a0) + aux (f6). The IsValid
	// byte of the 4-element wire form is not counted.
	got, err = feeTxSize(ctx, &v1.FeeTxSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: conway})
	feeMustOK(t, got, err)
	if want := uint64(1 + len(body) + 1 + 1); got.GetSize() != want || got.GetSize() != uint64(len(conway))-1 {
		t.Fatalf("conway size = %d, want %d", got.GetSize(), want)
	}

	bad, err := feeTxSize(ctx, &v1.FeeTxSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: conway[:10]})
	if err != nil || bad.GetRejection().GetFailures()[0].GetName() != "DeserialiseFailure" {
		t.Fatalf("truncated tx: %v, %v", bad, err)
	}
	_, err = feeTxSize(ctx, &v1.FeeTxSizeRequest{Era: v1.Era_ERA_BYRON, Tx: conway})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestFeeMinFee(t *testing.T) {
	ctx := context.Background()
	body := feeBody(t, 200_000)
	conway := feeConwayTx(body)
	size := uint64(len(body) + 3)
	pp := &v1.ProtocolParameters{
		MinFeeA: feeU64(44), MinFeeB: feeU64(155381),
		PriceMem: rat("577", "10000"), PriceSteps: rat("721", "10000000"),
		MinFeeRefScriptCostPerByte: rat("15", "1"),
	}

	got, err := feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_CONWAY, Pparams: pp, Tx: conway})
	feeMustOK(t, got, err)
	if want := 44*size + 155381; got.GetMinFee() != want {
		t.Fatalf("conway min fee = %d, want %d", got.GetMinFee(), want)
	}

	// 30 reference-script bytes at 15 lovelace each, all in the first tier.
	got, err = feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_CONWAY, Pparams: pp, Tx: conway, RefScriptSize: 30})
	feeMustOK(t, got, err)
	if want := 44*size + 155381 + 450; got.GetMinFee() != want {
		t.Fatalf("conway min fee with reference scripts = %d, want %d", got.GetMinFee(), want)
	}

	// Pre-Conway eras ignore the reference-script size.
	sh := feeShelleyTx(t)
	got, err = feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_SHELLEY, Pparams: pp, Tx: sh, RefScriptSize: 30})
	feeMustOK(t, got, err)
	if want := 44*uint64(len(sh)) + 155381; got.GetMinFee() != want {
		t.Fatalf("shelley min fee = %d, want %d", got.GetMinFee(), want)
	}

	_, err = feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_CONWAY, Pparams: &v1.ProtocolParameters{MinFeeB: feeU64(1)}, Tx: conway})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST)
	_, err = feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_DIJKSTRA, Pparams: pp, Tx: conway})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
	// The result does not fit a Word64.
	huge := &v1.ProtocolParameters{MinFeeA: feeU64(^uint64(0)), MinFeeB: feeU64(0)}
	_, err = feeMinFee(ctx, &v1.FeeMinFeeRequest{Era: v1.Era_ERA_SHELLEY, Pparams: huge, Tx: sh})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestFeeRefScriptFee(t *testing.T) {
	ctx := context.Background()
	for name, tc := range map[string]struct {
		size uint64
		cost *v1.Rational
		want uint64
	}{
		// 25600 bytes at 15, then 4400 bytes at 15 * 6/5 = 18.
		"second tier": {30_000, rat("15", "1"), 25_600*15 + 4_400*18},
		// floor(25600/3 + 1 * 2/5) once, not per tier.
		"floor once": {25_601, rat("1", "3"), 8_533},
		"empty":      {0, rat("15", "1"), 0},
		"exact tier": {25_600, rat("15", "1"), 384_000},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := feeRefScriptFee(ctx, &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_CONWAY, CostPerByte: tc.cost, Size: tc.size})
			feeMustOK(t, got, err)
			if got.GetFee() != tc.want {
				t.Fatalf("fee = %d, want %d", got.GetFee(), tc.want)
			}
		})
	}
	_, err := feeRefScriptFee(ctx, &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_BABBAGE, CostPerByte: rat("1", "1"), Size: 1})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
	_, err = feeRefScriptFee(ctx, &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_CONWAY, CostPerByte: rat("-1", "1"), Size: 1})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
	// Past the bound the tiered loop is not attempted.
	_, err = feeRefScriptFee(ctx, &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_CONWAY, CostPerByte: rat("1", "1"), Size: 1 << 29})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
	_, err = feeRefScriptFee(ctx, &v1.FeeRefScriptFeeRequest{Era: v1.Era_ERA_CONWAY, CostPerByte: rat("1", "0"), Size: 1})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST)
}

func TestFeeScriptFee(t *testing.T) {
	ctx := context.Background()
	// 14e6 * 577/10000 + 1e10 * 721/10000000 = 807800 + 721000.
	got, err := feeScriptFee(ctx, &v1.FeeScriptFeeRequest{
		PriceMem: rat("577", "10000"), PriceSteps: rat("721", "10000000"), ExMem: 14_000_000, ExSteps: 10_000_000_000,
	})
	feeMustOK(t, got, err)
	if got.GetFee() != 1_528_800 {
		t.Fatalf("fee = %d, want 1528800", got.GetFee())
	}
	// ceiling of 1/3.
	got, err = feeScriptFee(ctx, &v1.FeeScriptFeeRequest{PriceMem: rat("1", "3"), PriceSteps: rat("0", "1"), ExMem: 1})
	feeMustOK(t, got, err)
	if got.GetFee() != 1 {
		t.Fatalf("fee = %d, want 1", got.GetFee())
	}
	// 1/2 + 1/2: one ceiling over the sum.
	got, err = feeScriptFee(ctx, &v1.FeeScriptFeeRequest{PriceMem: rat("1", "2"), PriceSteps: rat("1", "2"), ExMem: 1, ExSteps: 1})
	feeMustOK(t, got, err)
	if got.GetFee() != 1 {
		t.Fatalf("fee = %d, want 1", got.GetFee())
	}
	_, err = feeScriptFee(ctx, &v1.FeeScriptFeeRequest{PriceMem: rat("-1", "2"), PriceSteps: rat("1", "2")})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestFeeMinUtxo(t *testing.T) {
	ctx := context.Background()
	// {0: addr, 1: 1000000}: 1 + 1 + 2 + 29 + 1 + 5 = 39 bytes.
	out := feeBabbageOut(t, 1_000_000, 0)
	if len(out) != 39 {
		t.Fatalf("test output is %d bytes", len(out))
	}
	byteCost := &v1.ProtocolParameters{CoinsPerUtxoByte: feeU64(4310)}
	for _, era := range []v1.Era{v1.Era_ERA_BABBAGE, v1.Era_ERA_CONWAY} {
		got, err := feeMinUtxo(ctx, &v1.FeeMinUtxoRequest{Era: era, Pparams: byteCost, Txout: out})
		feeMustOK(t, got, err)
		if want := uint64((160 + 39) * 4310); got.GetMinCoin() != want {
			t.Fatalf("%v min coin = %d, want %d", era, got.GetMinCoin(), want)
		}
	}

	// Alonzo: [addr, coin] is 27 + 2 = 29 words at coinsPerUTxOWord.
	alonzoOut := feeMustCbor(t, []any{feeAddr(), uint64(1_000_000)})
	got, err := feeMinUtxo(ctx, &v1.FeeMinUtxoRequest{Era: v1.Era_ERA_ALONZO, Pparams: &v1.ProtocolParameters{CoinsPerUtxoWord: feeU64(34482)}, Txout: alonzoOut})
	feeMustOK(t, got, err)
	if got.GetMinCoin() != 999_978 {
		t.Fatalf("alonzo min coin = %d, want 999978", got.GetMinCoin())
	}

	// Shelley and Allegra: the flat parameter.
	for _, era := range []v1.Era{v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA} {
		got, err = feeMinUtxo(ctx, &v1.FeeMinUtxoRequest{Era: era, Pparams: &v1.ProtocolParameters{MinUtxoValue: feeU64(1_000_000)}, Txout: alonzoOut})
		feeMustOK(t, got, err)
		if got.GetMinCoin() != 1_000_000 {
			t.Fatalf("%v min coin = %d, want 1000000", era, got.GetMinCoin())
		}
	}

	_, err = feeMinUtxo(ctx, &v1.FeeMinUtxoRequest{Era: v1.Era_ERA_BABBAGE, Pparams: &v1.ProtocolParameters{}, Txout: out})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST)
	_, err = feeMinUtxo(ctx, &v1.FeeMinUtxoRequest{Era: v1.Era_ERA_BYRON, Pparams: byteCost, Txout: out})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

// The expected hashes are blake2b-256 over redeemers || datums || language
// views, computed independently with hashlib. Plutus V3 has language tag 2;
// Plutus V1 is double-serialised as 0x4100 with an indefinite-length cost
// model inside a byte string.
func TestFeeScriptIntegrityHash(t *testing.T) {
	ctx := context.Background()
	hexb := func(s string) []byte {
		b := make([]byte, len(s)/2)
		for i := range b {
			if _, err := fmt.Sscanf(s[2*i:2*i+2], "%02x", &b[i]); err != nil {
				t.Fatal(err)
			}
		}
		return b
	}
	got, err := feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{
		Era:       v1.Era_ERA_CONWAY,
		Pparams:   &v1.ProtocolParameters{CostModelPlutusV3: []int64{1, 2, 3}},
		Redeemers: []byte{0xa0},
		Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V3},
	})
	feeMustOK(t, got, err)
	if want := hexb("a7fc912922b1280821cd2b1da8a40dc2773d39550d056490685a2d6b6e2a7cf9"); !bytes.Equal(got.GetHash(), want) {
		t.Fatalf("hash = %x, want %x", got.GetHash(), want)
	}

	got, err = feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{
		Era:       v1.Era_ERA_ALONZO,
		Pparams:   &v1.ProtocolParameters{CostModelPlutusV1: []int64{1, 2}},
		Redeemers: []byte{0x81, 0x00},
		Datums:    []byte{0x81, 0x01},
		Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V1},
	})
	feeMustOK(t, got, err)
	if want := hexb("f2a38cdd360e6d9afc13b72ca2f49df58b7e153a0d4c1d9521de4d1e8f1e2fe9"); !bytes.Equal(got.GetHash(), want) {
		t.Fatalf("hash = %x, want %x", got.GetHash(), want)
	}

	// Nothing to hash.
	got, err = feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{Era: v1.Era_ERA_CONWAY, Pparams: &v1.ProtocolParameters{}})
	feeMustOK(t, got, err)
	if got.GetHash() != nil {
		t.Fatalf("hash = %x, want none", got.GetHash())
	}

	_, err = feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{
		Era: v1.Era_ERA_CONWAY, Pparams: &v1.ProtocolParameters{}, Redeemers: []byte{0xa0},
		Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V4},
	})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestFeeRefScriptSize(t *testing.T) {
	ctx := context.Background()
	body := feeMustCbor(t, map[uint]any{
		0:  []any{feeTxIn(0x01, 0), feeTxIn(0x03, 1)},
		1:  []any{[]any{feeAddr(), uint64(2_000_000)}},
		2:  uint64(200_000),
		18: []any{feeTxIn(0x02, 0), feeTxIn(0x03, 1)},
	})
	tx := feeConwayTx(body)
	entries := []*v1.UtxoEntry{
		feeUtxoEntry(0x01, 0, feeBabbageOut(t, 5_000_000, 10)),
		feeUtxoEntry(0x02, 0, feeBabbageOut(t, 5_000_000, 20)),
		// Present in both the inputs and the reference inputs: counted once.
		feeUtxoEntry(0x03, 1, feeBabbageOut(t, 5_000_000, 7)),
	}
	got, err := feeRefScriptSize(ctx, &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_CONWAY, Tx: tx, Utxo: entries})
	feeMustOK(t, got, err)
	if got.GetSize() != 37 {
		t.Fatalf("size = %d, want 37", got.GetSize())
	}

	// An output without a script contributes nothing.
	plain := []*v1.UtxoEntry{
		feeUtxoEntry(0x01, 0, feeBabbageOut(t, 5_000_000, 0)),
		feeUtxoEntry(0x02, 0, feeBabbageOut(t, 5_000_000, 20)),
		feeUtxoEntry(0x03, 1, feeBabbageOut(t, 5_000_000, 0)),
	}
	got, err = feeRefScriptSize(ctx, &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_BABBAGE, Tx: tx, Utxo: plain})
	feeMustOK(t, got, err)
	if got.GetSize() != 20 {
		t.Fatalf("size = %d, want 20", got.GetSize())
	}

	_, err = feeRefScriptSize(ctx, &v1.FeeRefScriptSizeRequest{Era: v1.Era_ERA_ALONZO, Tx: tx, Utxo: plain})
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestRegistryReportsFeeFunctions(t *testing.T) {
	info, err := metaInfo(newRegistry())(context.Background(), &v1.MetaInfoRequest{})
	if err != nil {
		t.Fatal(err)
	}
	found := map[string]bool{}
	for _, f := range info.GetImplemented() {
		found[f] = true
	}
	for _, name := range []string{
		"fee_min_fee", "fee_ref_script_size", "fee_ref_script_fee", "fee_tx_size",
		"fee_script_fee", "fee_min_utxo", "fee_script_integrity_hash", "fee_value_balance",
	} {
		if !found[name] {
			t.Errorf("%s is not implemented", name)
		}
	}
}

// Real-length cost models, so the reference accepts them. Hashes computed
// independently: Conway map redeemers {[0, 0]: [0, [0, 0]]} with PlutusV2, and Alonzo
// one redeemer [0, 0, 0, [0, 0]] with datums [1] and PlutusV1 (tag 0x4100, indefinite-length
// model inside a byte string).
func TestFeeScriptIntegrityHashRealLengthModels(t *testing.T) {
	ctx := context.Background()
	unhex := func(s string) []byte {
		b := make([]byte, len(s)/2)
		for i := range b {
			if _, err := fmt.Sscanf(s[2*i:2*i+2], "%02x", &b[i]); err != nil {
				t.Fatal(err)
			}
		}
		return b
	}
	got, err := feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{
		Era:       v1.Era_ERA_CONWAY,
		Pparams:   &v1.ProtocolParameters{CostModelPlutusV2: feeCostModelV2},
		Redeemers: []byte{0xa1, 0x82, 0x00, 0x00, 0x82, 0x00, 0x82, 0x00, 0x00},
		Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V2},
	})
	feeMustOK(t, got, err)
	if want := unhex("6e9284e6d25df53370569c54c31a4a5a6bc7c35069638217e149aa0386353ab8"); !bytes.Equal(got.GetHash(), want) {
		t.Fatalf("v2 hash = %x, want %x", got.GetHash(), want)
	}
	got, err = feeScriptIntegrityHash(ctx, &v1.FeeScriptIntegrityHashRequest{
		Era:       v1.Era_ERA_ALONZO,
		Pparams:   &v1.ProtocolParameters{CostModelPlutusV1: feeCostModelV1},
		Redeemers: []byte{0x81, 0x84, 0x00, 0x00, 0x00, 0x82, 0x00, 0x00},
		Datums:    []byte{0x81, 0x01},
		Languages: []v1.PlutusVersion{v1.PlutusVersion_PLUTUS_VERSION_V1},
	})
	feeMustOK(t, got, err)
	if want := unhex("a08dacb8cb9a2c774e5a445ce066887169eb458798aff8bd989308c30bbbf24c"); !bytes.Equal(got.GetHash(), want) {
		t.Fatalf("v1 hash = %x, want %x", got.GetHash(), want)
	}
}
