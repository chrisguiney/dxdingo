package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func newEpochConwayPParams(t *testing.T) *v1.ProtocolParameters {
	t.Helper()
	req := forkReq(8, 9, 8)
	req.Pparams = babbagePParamsProto()
	req.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 8}
	req.ConwayGenesis = conwayGenesisInput()
	pp := run(t, req)
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: 10}
	return pp
}

// Same case as dxnode-hs test/Test/Epoch.hs. Pool a0 retires at epoch 11 with
// deposit 500 and reward account k1 (registered). k2 delegates to a0 with a
// reward balance of 5 and UTxO stake 40. The old mark holds k3 with 100.
func newEpochRequest(t *testing.T) *v1.EpochNewEpochRequest {
	retiring := uint64(11)
	return &v1.EpochNewEpochRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
		Pparams: newEpochConwayPParams(t),
		State: &v1.LedgerState{
			Accounts: []*v1.Account{
				{Credential: rotKey(1)},
				{Credential: rotKey(2), Balance: 5, PoolId: bytes.Repeat([]byte{0xa0}, 28)},
			},
			Pools: []*v1.Pool{{
				PoolId: bytes.Repeat([]byte{0xa0}, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, Deposit: 500,
				RewardAccount: rotKey(1), Margin: &v1.Rational{Numerator: "1", Denominator: "100"}, RetiringEpoch: &retiring,
			}},
			Pots: &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000},
		},
		Snapshots:    []*v1.SnapshotEntry{rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 3, 100, 0xa0)},
		InstantStake: []*v1.StakeEntry{{Credential: rotKey(2), Amount: 40}},
	}
}

func TestEpochNewEpoch(t *testing.T) {
	got, err := epochNewEpoch(context.Background(), newEpochRequest(t))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	d := got.GetDelta()
	if len(d.GetPoolsRemoved()) != 1 || !bytes.Equal(d.GetPoolsRemoved()[0], bytes.Repeat([]byte{0xa0}, 28)) {
		t.Errorf("pools removed = %x", d.GetPoolsRemoved())
	}
	if len(d.GetAccountsChanged()) != 2 {
		t.Fatalf("accounts changed = %v", d.GetAccountsChanged())
	}
	if a := d.GetAccountsChanged()[0]; a.GetCredential().GetHash()[0] != 1 || a.GetBalance() != 500 {
		t.Errorf("refunded account = %v", a)
	}
	if a := d.GetAccountsChanged()[1]; a.GetCredential().GetHash()[0] != 2 || a.GetBalance() != 5 || len(a.GetPoolId()) != 0 {
		t.Errorf("undelegated account = %v", a)
	}
	if d.GetReservesDelta() != 0 || d.GetTreasuryDelta() != 0 {
		t.Errorf("pot deltas = %d, %d", d.GetReservesDelta(), d.GetTreasuryDelta())
	}
	want := []*v1.SnapshotEntry{
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 2, 45, 0xa0),
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_SET, 3, 100, 0xa0),
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
}

func TestEpochNewEpochDomain(t *testing.T) {
	for name, tc := range map[string]struct {
		mutate func(*v1.EpochNewEpochRequest)
		code   v1.HarnessErrorCode
	}{
		"not Conway":       {func(r *v1.EpochNewEpochRequest) { r.Context.Era = v1.Era_ERA_BABBAGE }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"reward update":    {func(r *v1.EpochNewEpochRequest) { r.RewardUpdate = &v1.RewardUpdate{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"proposals":        {func(r *v1.EpochNewEpochRequest) { r.State.Proposals = []*v1.Proposal{{}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"utxo":             {func(r *v1.EpochNewEpochRequest) { r.State.Utxo = []*v1.UtxoEntry{{}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"repeated account": {func(r *v1.EpochNewEpochRequest) { r.State.Accounts = append(r.State.Accounts, r.State.Accounts[0]) }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id":    {func(r *v1.EpochNewEpochRequest) { r.State.Pools[0].PoolId = []byte{1} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			req := newEpochRequest(t)
			tc.mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}

// Donations of the ended epoch move into the treasury, and a retiring pool
// whose reward account is not registered returns its deposit there.
func TestEpochNewEpochTreasury(t *testing.T) {
	req := newEpochRequest(t)
	req.State.Pots.Donations = 5_000
	req.State.Accounts = req.State.Accounts[1:]
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	if d := got.GetDelta().GetTreasuryDelta(); d != 5_500 {
		t.Errorf("treasury delta = %d, want 5500 (donations plus the unclaimed deposit)", d)
	}
}
