package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// getConsumedMaryValue: inputs + withdrawals + refunds + positively minted
// assets. conwayProducedValue: outputs + fee + deposits + donation + burned
// assets. Deposits are keyDeposit per registration certificate, poolDeposit
// per newly registered pool and the proposal deposit per proposal.

func feeHdr(major byte, n int) []byte {
	if n < 24 {
		return []byte{major<<5 | byte(n)}
	}
	return []byte{major<<5 | 24, byte(n)}
}

func feeTwoAssets(t testing.TB, pa byte, na string, qa int64, pb byte, nb string, qb int64) []byte {
	one := func(p byte, n string, q int64) []byte {
		return feeCat(feeMustCbor(t, bytes.Repeat([]byte{p}, 28)), feeHdr(5, 1), feeMustCbor(t, []byte(n)), feeMustCbor(t, q))
	}
	return feeCat(feeHdr(5, 2), one(pa, na, qa), one(pb, nb, qb))
}

func feeValue(t testing.TB, coin uint64, assets []byte) []byte {
	return feeCat([]byte{0x82}, feeMustCbor(t, coin), assets)
}

func feeOutputArray(t testing.TB, value []byte) []byte {
	return feeCat([]byte{0x82}, feeMustCbor(t, feeAddr()), value)
}

func feeEntries(t testing.TB, entries ...[2][]byte) []byte {
	out := feeHdr(5, len(entries))
	for _, e := range entries {
		out = append(out, e[0]...)
		out = append(out, e[1]...)
	}
	return out
}

func TestFeeValueBalance(t *testing.T) {
	ctx := context.Background()
	txid := bytes.Repeat([]byte{0x01}, 32)
	inputs := feeTwoAssets(t, 0xaa, "a", 5, 0xbb, "b", 4)
	outputs := feeTwoAssets(t, 0xaa, "a", 8, 0xbb, "b", 2)
	mint := feeTwoAssets(t, 0xaa, "a", 3, 0xbb, "b", -2)
	reward := append([]byte{0xe1}, bytes.Repeat([]byte{0x22}, 28)...)
	withdrawals := feeCat(feeHdr(5, 1), feeMustCbor(t, reward), feeMustCbor(t, uint64(1_000_000)))
	// reg_cert: [7, [0, keyhash], deposit].
	cert := feeCat([]byte{0x83, 0x07, 0x82, 0x00}, feeMustCbor(t, bytes.Repeat([]byte{0x33}, 28)), feeMustCbor(t, uint64(2_000_000)))
	body := feeEntries(t,
		[2][]byte{{0x00}, feeMustCbor(t, []any{[]any{txid, uint(0)}})},
		[2][]byte{{0x01}, feeCat([]byte{0x81}, feeOutputArray(t, feeValue(t, 8_300_000, outputs)))},
		[2][]byte{{0x02}, feeMustCbor(t, uint64(500_000))},
		[2][]byte{{0x04}, feeCat([]byte{0x81}, cert)},
		[2][]byte{{0x05}, withdrawals},
		[2][]byte{{0x09}, mint},
		[2][]byte{{0x16}, feeMustCbor(t, uint64(200_000))},
	)
	tx := feeCat([]byte{0x84}, body, []byte{0xa0, 0xf5, 0xf6})
	req := &v1.FeeValueBalanceRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY},
		Pparams: &v1.ProtocolParameters{
			KeyDeposit: feeU64(2_000_000), PoolDeposit: feeU64(500_000_000),
			DrepDeposit: feeU64(500_000_000), GovActionDeposit: feeU64(100_000_000_000),
		},
		State: &v1.LedgerState{Utxo: []*v1.UtxoEntry{
			feeUtxoEntry(0x01, 0, feeOutputArray(t, feeValue(t, 10_000_000, inputs))),
		}},
		Tx: tx,
	}
	got, err := feeValueBalance(ctx, req)
	feeMustOK(t, got, err)
	if got.GetConsumedCoin() != 11_000_000 || got.GetProducedCoin() != 11_000_000 {
		t.Fatalf("coin consumed %d produced %d, want 11000000 both", got.GetConsumedCoin(), got.GetProducedCoin())
	}
	if got.GetDeposits() != 2_000_000 || got.GetRefunds() != 0 {
		t.Fatalf("deposits %d refunds %d", got.GetDeposits(), got.GetRefunds())
	}
	// Consumed: inputs plus the positive mint. Produced: outputs plus the burn.
	want := []*v1.Asset{
		{PolicyId: bytes.Repeat([]byte{0xaa}, 28), AssetName: []byte("a"), Quantity: 8},
		{PolicyId: bytes.Repeat([]byte{0xbb}, 28), AssetName: []byte("b"), Quantity: 4},
	}
	for name, got := range map[string][]*v1.Asset{"consumed": got.GetConsumedAssets(), "produced": got.GetProducedAssets()} {
		if len(got) != len(want) {
			t.Fatalf("%s assets = %v, want %v", name, got, want)
		}
		for i := range want {
			if !bytes.Equal(got[i].GetPolicyId(), want[i].GetPolicyId()) || !bytes.Equal(got[i].GetAssetName(), want[i].GetAssetName()) || got[i].GetQuantity() != want[i].GetQuantity() {
				t.Fatalf("%s asset %d = %v, want %v", name, i, got[i], want[i])
			}
		}
	}

	// An input missing from the UTxO counts nothing.
	req.State = &v1.LedgerState{}
	got, err = feeValueBalance(ctx, req)
	feeMustOK(t, got, err)
	if got.GetConsumedCoin() != 1_000_000 {
		t.Fatalf("consumed coin with an unresolved input = %d, want the withdrawal only", got.GetConsumedCoin())
	}

	req.Context.Era = v1.Era_ERA_BABBAGE
	_, err = feeValueBalance(ctx, req)
	feeWantCode(t, err, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED)
}

func TestFeeValueBalanceRefund(t *testing.T) {
	ctx := context.Background()
	txid := bytes.Repeat([]byte{0x01}, 32)
	hash := bytes.Repeat([]byte{0x33}, 28)
	// unreg_cert: [8, [0, keyhash], refund].
	cert := feeCat([]byte{0x83, 0x08, 0x82, 0x00}, feeMustCbor(t, hash), feeMustCbor(t, uint64(2_000_000)))
	body := feeEntries(t,
		[2][]byte{{0x00}, feeMustCbor(t, []any{[]any{txid, uint(0)}})},
		[2][]byte{{0x01}, feeCat([]byte{0x81}, feeOutputArray(t, feeMustCbor(t, uint64(6_800_000))))},
		[2][]byte{{0x02}, feeMustCbor(t, uint64(200_000))},
		[2][]byte{{0x04}, feeCat([]byte{0x81}, cert)},
	)
	req := &v1.FeeValueBalanceRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY},
		Pparams: &v1.ProtocolParameters{
			KeyDeposit: feeU64(2_000_000), PoolDeposit: feeU64(500_000_000),
			DrepDeposit: feeU64(500_000_000), GovActionDeposit: feeU64(100_000_000_000),
		},
		State: &v1.LedgerState{
			Utxo: []*v1.UtxoEntry{feeUtxoEntry(0x01, 0, feeOutputArray(t, feeMustCbor(t, uint64(5_000_000))))},
			Accounts: []*v1.Account{{
				Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: hash},
				Deposit:    2_000_000,
			}},
		},
		Tx: feeCat([]byte{0x84}, body, []byte{0xa0, 0xf5, 0xf6}),
	}
	got, err := feeValueBalance(ctx, req)
	feeMustOK(t, got, err)
	if got.GetRefunds() != 2_000_000 || got.GetConsumedCoin() != 7_000_000 || got.GetProducedCoin() != 7_000_000 {
		t.Fatalf("got %v", got)
	}
}
