package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func snapRow(c *v1.Credential, amount uint64, pool []byte) *v1.SnapshotEntry {
	return &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_MARK, Credential: c, Amount: amount, PoolId: pool}
}

func stakePool(id byte, stage v1.PoolStage) *v1.Pool {
	return &v1.Pool{
		PoolId: poolIDRewardsStake(id), Stage: stage, VrfKeyHash: bytes.Repeat([]byte{id}, 32), Margin: rat("1", "10"),
		RewardAccount: keyCred(0xee), Cost: 1,
	}
}

// Pool A holds 30 + 20 = 50 of an active stake of 100, pool B 25, and 25 is
// delegated to Z, which has no registration: its stake counts in the total
// but Z is not in the distribution. Pool C has no delegators and E is a
// future registration, so neither appears. The undelegated row is ignored.
func poolDistributionRequest() *v1.StakePoolDistributionRequest {
	return &v1.StakePoolDistributionRequest{
		Snapshot: []*v1.SnapshotEntry{
			snapRow(keyCred(0x01), 30, poolIDRewardsStake(0xa1)), snapRow(keyCred(0x02), 20, poolIDRewardsStake(0xa1)),
			snapRow(keyCred(0x03), 25, poolIDRewardsStake(0xb2)), snapRow(keyCred(0x04), 25, poolIDRewardsStake(0xff)),
			snapRow(keyCred(0x05), 999, nil),
		},
		Pools: []*v1.Pool{
			stakePool(0xa1, v1.PoolStage_POOL_STAGE_CURRENT), stakePool(0xb2, v1.PoolStage_POOL_STAGE_CURRENT),
			stakePool(0xc3, v1.PoolStage_POOL_STAGE_CURRENT), stakePool(0xe5, v1.PoolStage_POOL_STAGE_FUTURE),
		},
	}
}

func TestPoolDistribution(t *testing.T) {
	got, err := poolDistribution(context.Background(), poolDistributionRequest())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || got.GetTotalActive() != 100 || len(got.GetPools()) != 2 {
		t.Fatalf("got %v", got)
	}
	a, b := got.GetPools()[0], got.GetPools()[1]
	if !bytes.Equal(a.GetPoolId(), poolIDRewardsStake(0xa1)) || a.GetStake() != 50 || a.GetShare().GetNumerator() != "1" || a.GetShare().GetDenominator() != "2" {
		t.Errorf("pool A = %v", a)
	}
	if !bytes.Equal(b.GetPoolId(), poolIDRewardsStake(0xb2)) || b.GetStake() != 25 || b.GetShare().GetDenominator() != "4" {
		t.Errorf("pool B = %v", b)
	}
	if !bytes.Equal(a.GetVrfKeyHash(), bytes.Repeat([]byte{0xa1}, 32)) {
		t.Errorf("vrf = %x", a.GetVrfKeyHash())
	}
}

func TestPoolDistributionDomain(t *testing.T) {
	for name, tc := range map[string]struct {
		mutate func(*v1.StakePoolDistributionRequest)
		code   v1.HarnessErrorCode
	}{
		"no delegated stake": {func(r *v1.StakePoolDistributionRequest) {
			r.Snapshot = []*v1.SnapshotEntry{snapRow(keyCred(1), 5, nil)}
		},
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"zero stake": {func(r *v1.StakePoolDistributionRequest) { r.Snapshot[0].Amount = 0 },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"repeated credential": {func(r *v1.StakePoolDistributionRequest) { r.Snapshot[1].Credential = keyCred(0x01) },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"repeated pool": {func(r *v1.StakePoolDistributionRequest) {
			r.Pools = append(r.Pools, stakePool(0xa1, v1.PoolStage_POOL_STAGE_CURRENT))
		},
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id": {func(r *v1.StakePoolDistributionRequest) { r.Snapshot[0].PoolId = []byte{1} },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"margin above one": {func(r *v1.StakePoolDistributionRequest) { r.Pools[0].Margin = rat("3", "2") },
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
	} {
		t.Run(name, func(t *testing.T) {
			req := poolDistributionRequest()
			tc.mutate(req)
			_, err := poolDistribution(context.Background(), req)
			if err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}

// The DRep cases below repeat the Haskell checks in Test/Stake.hs.
func drepRequestRewardsStake() *v1.StakeDRepDistributionRequest {
	drepA := keyCred(0xa0)
	return &v1.StakeDRepDistributionRequest{
		Epoch: 10,
		InstantStake: []*v1.StakeEntry{
			{Credential: keyCred(0x01), Amount: 10}, {Credential: keyCred(0x02), Amount: 7}, {Credential: keyCred(0x03), Amount: 1000},
		},
		Accounts: []*v1.Account{
			{Credential: keyCred(0x01), Balance: 5, Drep: drepCred(drepA)},
			{Credential: keyCred(0x02), Drep: &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}},
			{Credential: keyCred(0x03), Balance: 1},
		},
		Dreps: []*v1.DRepRecord{{Credential: drepA, Expiry: 20}},
		Proposals: []*v1.Proposal{{
			Deposit: 100, ReturnAccount: keyCred(0x01), ProposedEpoch: 9, ExpiresEpoch: 15,
		}},
	}
}

func drepStakes(t *testing.T, req *v1.StakeDRepDistributionRequest) (drepA, abstain uint64, entries int) {
	t.Helper()
	got, err := drepDistribution(context.Background(), req)
	if err != nil || got.GetRejection() != nil {
		t.Fatalf("err = %v, rejection = %v", err, got.GetRejection())
	}
	for _, d := range got.GetDreps() {
		switch {
		case d.GetDrep().GetAlwaysAbstain():
			abstain = d.GetStake()
		case d.GetDrep().GetCredential() != nil:
			drepA = d.GetStake()
		}
	}
	return drepA, abstain, len(got.GetDreps())
}

func TestDRepDistributionActiveProposalWindow(t *testing.T) {
	for name, tc := range map[string]struct {
		expires uint64
		want    uint64
	}{
		"expired before the epoch": {9, 15},
		"last active epoch":        {10, 115},
	} {
		t.Run(name, func(t *testing.T) {
			req := drepRequestRewardsStake()
			req.Proposals[0].ExpiresEpoch = tc.expires
			if a, abs, _ := drepStakes(t, req); a != tc.want || abs != 7 {
				t.Fatalf("drepA = %d abstain = %d, want %d and 7", a, abs, tc.want)
			}
		})
	}
}

func TestDRepDistributionUnregisteredDRepCountsForNobody(t *testing.T) {
	req := drepRequestRewardsStake()
	req.Dreps = nil
	if a, abs, n := drepStakes(t, req); a != 0 || abs != 7 || n != 1 {
		t.Fatalf("drepA = %d abstain = %d entries = %d", a, abs, n)
	}
}

func TestDRepDistributionRepeatedAccountIsBadRequest(t *testing.T) {
	req := drepRequestRewardsStake()
	req.Accounts = append(req.Accounts, req.Accounts[0])
	if _, err := drepDistribution(context.Background(), req); err == nil || dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("err = %v, want BAD_REQUEST", err)
	}
}
