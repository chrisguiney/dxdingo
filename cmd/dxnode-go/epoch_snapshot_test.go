package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func rotKey(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: bytes.Repeat([]byte{b}, 28)}
}

func rotEntry(kind v1.SnapshotKind, k byte, amount uint64, pool byte) *v1.SnapshotEntry {
	return &v1.SnapshotEntry{Snapshot: kind, Credential: rotKey(k), Amount: amount, PoolId: bytes.Repeat([]byte{pool}, 28)}
}

// Same case as dxnode-hs test/Test/Epoch.hs. Before the boundary Mark has
// k1, Set has k2, Go has k3. k4 holds UTxO stake 40 and 2 and a reward
// balance of 5 and delegates to pool a0, so the new Mark is 47. k5 is
// registered but undelegated and k6 is delegated but unregistered: neither
// is in the new Mark. The fee pot is 77.
func rotateRequest() *v1.EpochSnapshotRotateRequest {
	return &v1.EpochSnapshotRotateRequest{
		Snapshots: []*v1.SnapshotEntry{
			rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 1, 100, 0xa0),
			rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_SET, 2, 200, 0xa0),
			rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_GO, 3, 300, 0xa0),
		},
		InstantStake: []*v1.StakeEntry{
			{Credential: rotKey(4), Amount: 40}, {Credential: rotKey(4), Amount: 2},
			{Credential: rotKey(5), Amount: 9}, {Credential: rotKey(6), Amount: 7},
		},
		Accounts: []*v1.Account{
			{Credential: rotKey(4), Balance: 5, PoolId: bytes.Repeat([]byte{0xa0}, 28)},
			{Credential: rotKey(5)},
		},
		Pools: []*v1.Pool{{PoolId: bytes.Repeat([]byte{0xa0}, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, Cost: 340_000_000, RewardAccount: rotKey(0xee), Margin: &v1.Rational{Numerator: "1", Denominator: "100"}}},
		Fees:  77,
	}
}

func TestEpochSnapshotRotate(t *testing.T) {
	got, err := epochSnapshotRotate(context.Background(), rotateRequest())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	want := []*v1.SnapshotEntry{
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 4, 47, 0xa0),
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_SET, 1, 100, 0xa0),
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_GO, 2, 200, 0xa0),
	}
	if len(got.GetSnapshots()) != len(want) {
		t.Fatalf("snapshots = %v", got.GetSnapshots())
	}
	for i, w := range want {
		g := got.GetSnapshots()[i]
		if g.GetSnapshot() != w.GetSnapshot() || !bytes.Equal(g.GetCredential().GetHash(), w.GetCredential().GetHash()) ||
			g.GetAmount() != w.GetAmount() || !bytes.Equal(g.GetPoolId(), w.GetPoolId()) {
			t.Errorf("snapshot[%d] = %v, want %v", i, g, w)
		}
	}
	if got.GetFeeSnapshot() != 77 {
		t.Errorf("fee snapshot = %d, want 77", got.GetFeeSnapshot())
	}
}

func TestEpochSnapshotRotateDomain(t *testing.T) {
	for name, tc := range map[string]struct {
		mutate func(*v1.EpochSnapshotRotateRequest)
		code   v1.HarnessErrorCode
	}{
		"repeated account": {func(r *v1.EpochSnapshotRotateRequest) { r.Accounts = append(r.Accounts, r.Accounts[0]) },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id": {func(r *v1.EpochSnapshotRotateRequest) { r.Accounts[0].PoolId = []byte{1} },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"repeated snapshot row": {func(r *v1.EpochSnapshotRotateRequest) { r.Snapshots = append(r.Snapshots, r.Snapshots[0]) },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"zero snapshot stake": {func(r *v1.EpochSnapshotRotateRequest) { r.Snapshots[0].Amount = 0 },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"undelegated snapshot row": {func(r *v1.EpochSnapshotRotateRequest) { r.Snapshots[0].PoolId = nil },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"unspecified snapshot kind": {func(r *v1.EpochSnapshotRotateRequest) {
			r.Snapshots[0].Snapshot = v1.SnapshotKind_SNAPSHOT_KIND_UNSPECIFIED
		},
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"stake overflow": {func(r *v1.EpochSnapshotRotateRequest) {
			r.InstantStake = append(r.InstantStake, &v1.StakeEntry{Credential: rotKey(4), Amount: ^uint64(0)})
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
	} {
		t.Run(name, func(t *testing.T) {
			req := rotateRequest()
			tc.mutate(req)
			if _, err := epochSnapshotRotate(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}
