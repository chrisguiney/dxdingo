package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// cborUint is the CBOR encoding of a small unsigned integer.
func cborUint(n uint64) []byte {
	switch {
	case n < 24:
		return []byte{byte(n)}
	case n < 256:
		return []byte{0x18, byte(n)}
	case n < 65536:
		return []byte{0x19, byte(n >> 8), byte(n)}
	}
	return []byte{0x1a, byte(n >> 24), byte(n >> 16), byte(n >> 8), byte(n)}
}

// shelleyOutput is the CBOR of a Shelley output [address, coin]. header is
// the address type and network byte; hashes follow it.
func shelleyOutput(header byte, hashes []byte, amount uint64) []byte {
	addr := append([]byte{header}, hashes...)
	out := []byte{0x82, 0x58, byte(len(addr))}
	out = append(out, addr...)
	return append(out, cborUint(amount)...)
}

func baseOut(header byte, stake byte, amount uint64) *v1.UtxoEntry {
	return &v1.UtxoEntry{
		Txin:  &v1.TxIn{TxId: bytes.Repeat([]byte{stake}, 32), Index: uint32(amount)},
		Txout: shelleyOutput(header, append(bytes.Repeat([]byte{0x70}, 28), bytes.Repeat([]byte{stake}, 28)...), amount),
	}
}

// K1 holds outputs of 10 and 5, K2 7, script credential S 9. An enterprise
// address and a zero-amount output add nothing. Header 0x00 is a key payment
// and key stake credential, 0x20 a key payment and script stake credential.
func instantStakeRequest() *v1.StakeInstantStakeRequest {
	enterprise := &v1.UtxoEntry{
		Txin:  &v1.TxIn{TxId: bytes.Repeat([]byte{0xee}, 32)},
		Txout: shelleyOutput(0x60, bytes.Repeat([]byte{0x70}, 28), 100),
	}
	return &v1.StakeInstantStakeRequest{Utxo: []*v1.UtxoEntry{
		baseOut(0x00, 0x01, 10), baseOut(0x00, 0x01, 5), baseOut(0x00, 0x02, 7), baseOut(0x20, 0x53, 9),
		enterprise, baseOut(0x00, 0x03, 0),
	}}
}

func TestInstantStake(t *testing.T) {
	got, err := instantStake(context.Background(), instantStakeRequest())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || len(got.GetStake()) != 3 {
		t.Fatalf("got %v", got)
	}
	want := []struct {
		kind   v1.CredentialKind
		first  byte
		amount uint64
	}{
		{v1.CredentialKind_CREDENTIAL_KIND_KEY, 0x01, 15},
		{v1.CredentialKind_CREDENTIAL_KIND_KEY, 0x02, 7},
		{v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, 0x53, 9},
	}
	for i, w := range want {
		e := got.GetStake()[i]
		if e.GetCredential().GetKind() != w.kind || e.GetCredential().GetHash()[0] != w.first || e.GetAmount() != w.amount {
			t.Errorf("stake[%d] = %v, want %+v", i, e, w)
		}
	}
}

func TestInstantStakeUndecodableOutputIsRejected(t *testing.T) {
	req := instantStakeRequest()
	req.Utxo[0].Txout = []byte{0xff}
	got, err := instantStake(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	f := got.GetRejection().GetFailures()
	if len(f) != 1 || f[0].GetRule() != "instant-stake" || f[0].GetName() != "DeserialiseFailure" {
		t.Fatalf("rejection = %v", got.GetRejection())
	}
}

func TestInstantStakeRepeatedInputIsBadRequest(t *testing.T) {
	req := instantStakeRequest()
	req.Utxo[1].Txin = req.Utxo[0].Txin
	if _, err := instantStake(context.Background(), req); err == nil || dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("err = %v, want BAD_REQUEST", err)
	}
}

// The active stake of a registered, delegated credential is its instant
// stake plus its reward balance. K1: 10 + 5 = 15 to pool A. K2 holds 7 and
// 3 in rewards, delegated to B: 10. K3 has no instant stake and a balance of
// 2: 2 to A. K4 is registered without a pool, K5 holds 50 but is not
// registered, and K6 is registered and delegated with nothing at all.
func snapshotRequest() *v1.StakeSnapshotRequest {
	return &v1.StakeSnapshotRequest{
		InstantStake: []*v1.StakeEntry{
			{Credential: keyCred(0x01), Amount: 10}, {Credential: keyCred(0x01), Amount: 5},
			{Credential: keyCred(0x02), Amount: 7}, {Credential: keyCred(0x04), Amount: 11}, {Credential: keyCred(0x05), Amount: 50},
		},
		Accounts: []*v1.Account{
			{Credential: keyCred(0x01), PoolId: poolIDRewardsStake(0xa1)},
			{Credential: keyCred(0x02), Balance: 3, PoolId: poolIDRewardsStake(0xb2)},
			{Credential: keyCred(0x03), Balance: 2, PoolId: poolIDRewardsStake(0xa1)},
			{Credential: keyCred(0x04)},
			{Credential: keyCred(0x06), PoolId: poolIDRewardsStake(0xa1)},
		},
		Pools: []*v1.Pool{stakePool(0xa1, v1.PoolStage_POOL_STAGE_CURRENT)},
	}
}

func TestStakeSnapshot(t *testing.T) {
	got, err := stakeSnapshot(context.Background(), snapshotRequest())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || got.GetTotal() != 27 || len(got.GetSnapshot()) != 3 {
		t.Fatalf("got %v", got)
	}
	want := []struct {
		first  byte
		amount uint64
		pool   byte
	}{{0x01, 15, 0xa1}, {0x02, 10, 0xb2}, {0x03, 2, 0xa1}}
	for i, w := range want {
		e := got.GetSnapshot()[i]
		if e.GetSnapshot() != v1.SnapshotKind_SNAPSHOT_KIND_MARK || e.GetCredential().GetHash()[0] != w.first ||
			e.GetAmount() != w.amount || !bytes.Equal(e.GetPoolId(), poolIDRewardsStake(w.pool)) {
			t.Errorf("snapshot[%d] = %v, want %+v", i, e, w)
		}
	}
}

func TestStakeSnapshotDomain(t *testing.T) {
	for name, tc := range map[string]struct {
		mutate func(*v1.StakeSnapshotRequest)
		code   v1.HarnessErrorCode
	}{
		"no active stake": {func(r *v1.StakeSnapshotRequest) { r.Accounts = r.Accounts[3:4] },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"repeated account": {func(r *v1.StakeSnapshotRequest) { r.Accounts = append(r.Accounts, r.Accounts[0]) },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id": {func(r *v1.StakeSnapshotRequest) { r.Accounts[0].PoolId = []byte{1} },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			req := snapshotRequest()
			tc.mutate(req)
			if _, err := stakeSnapshot(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}
