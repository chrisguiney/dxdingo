package main

import (
	"bytes"
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

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
		"not Conway":        {func(r *v1.EpochNewEpochRequest) { r.Context.Era = v1.Era_ERA_BABBAGE }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"reward update":     {func(r *v1.EpochNewEpochRequest) { r.RewardUpdate = &v1.RewardUpdate{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"proposals":         {func(r *v1.EpochNewEpochRequest) { r.State.Proposals = []*v1.Proposal{{}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"utxo":              {func(r *v1.EpochNewEpochRequest) { r.State.Utxo = []*v1.UtxoEntry{{}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"pointer":           {func(r *v1.EpochNewEpochRequest) { r.State.Accounts[0].Pointer = &v1.Pointer{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"drep target unset": {func(r *v1.EpochNewEpochRequest) { r.State.Accounts[0].Drep = &v1.DRep{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"repeated account":  {func(r *v1.EpochNewEpochRequest) { r.State.Accounts = append(r.State.Accounts, r.State.Accounts[0]) }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id":     {func(r *v1.EpochNewEpochRequest) { r.State.Pools[0].PoolId = []byte{1} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
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

// newEpochRewardRequest is the case of dxnode-hs test/Test/Epoch.hs for a
// pending reward round. Pool a0 holds all 4.4999e13 active stake (sigma =
// 1/1000 of the 4.4999e16 circulation) and made all 21600 blocks, the
// expected count for f = 1/20 over 432000 slots, so eta = 1. With rho =
// 3/1000 and tau = 1/5 on reserves of 10^12: deltaR1 = 3_000_000_000, the
// treasury cut is 600_000_000 and the pool pot 2_400_000_000. maxPool =
// floor(2.4e9 / 1.3 * 0.001) = 1_846_153 at performance 1; with margin 1/100,
// cost 0 and no owners the leader gets floor(1_846_153 / 100) = 18_461 and the
// member floor(1_846_153 * 99 / 100) = 1_827_691. The 1 left over stays in
// reserves: deltaR = -3e9 + (2.4e9 - 1_846_152).
func newEpochRewardRequest(t *testing.T) *v1.EpochNewEpochRequest {
	pool := bytes.Repeat([]byte{0xa0}, 28)
	const stake = 44_999_000_000_000
	return &v1.EpochNewEpochRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
		Pparams: newEpochConwayPParams(t),
		Globals: &v1.Globals{
			EpochLength: 432_000, SecurityParam: 2_160, MaxLovelaceSupply: 45_000_000_000_000_000,
			ActiveSlotCoeff: &v1.Rational{Numerator: "1", Denominator: "20"},
		},
		State: &v1.LedgerState{
			Accounts: []*v1.Account{
				{Credential: rotKey(1)},
				{Credential: rotKey(2), PoolId: pool},
			},
			Pools: []*v1.Pool{{
				PoolId: pool, Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: rotKey(1),
				Margin: &v1.Rational{Numerator: "1", Denominator: "100"},
			}},
			Pots: &v1.Pots{Reserves: 1_000_000_000_000, Treasury: 1_000_000},
		},
		Snapshots:      []*v1.SnapshotEntry{rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_GO, 2, stake, 0xa0)},
		BlocksPrevious: []*v1.BlocksMade{{PoolId: pool, Count: 21_600}},
		RewardUpdate: &v1.RewardUpdate{
			DeltaR: -601_846_152, DeltaT: 600_000_000,
			Rewards: []*v1.Reward{
				{Credential: rotKey(1), PoolId: pool, Kind: v1.RewardKind_REWARD_KIND_LEADER, Amount: 18_461},
				{Credential: rotKey(2), PoolId: pool, Kind: v1.RewardKind_REWARD_KIND_MEMBER, Amount: 1_827_691},
			},
		},
	}
}

func TestEpochNewEpochRewardApplication(t *testing.T) {
	t.Parallel()
	got, err := epochNewEpoch(context.Background(), newEpochRewardRequest(t))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	d := got.GetDelta()
	if d.GetReservesDelta() != -601_846_152 || d.GetTreasuryDelta() != 600_000_000 {
		t.Errorf("pot deltas = %d, %d", d.GetReservesDelta(), d.GetTreasuryDelta())
	}
	if len(d.GetAccountsChanged()) != 2 {
		t.Fatalf("accounts changed = %v", d.GetAccountsChanged())
	}
	if a := d.GetAccountsChanged()[0]; a.GetCredential().GetHash()[0] != 1 || a.GetBalance() != 18_461 {
		t.Errorf("leader account = %v", a)
	}
	if a := d.GetAccountsChanged()[1]; a.GetCredential().GetHash()[0] != 2 || a.GetBalance() != 1_827_691 {
		t.Errorf("member account = %v", a)
	}
	if len(got.GetSnapshots()) != 1 || got.GetSnapshots()[0].GetAmount() != 1_827_691 {
		t.Errorf("snapshots = %v, want the member's reward as the new mark", got.GetSnapshots())
	}
}

// Dingo recomputes the round at the boundary, so an update it would not have
// built, other globals, or no globals are outside the domain on both nodes.
func TestEpochNewEpochRewardDomain(t *testing.T) {
	t.Parallel()
	unsupported := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
	for name, mutate := range map[string]func(*v1.EpochNewEpochRequest){
		"update differs":    func(r *v1.EpochNewEpochRequest) { r.RewardUpdate.Rewards[0].Amount++ },
		"delta_t differs":   func(r *v1.EpochNewEpochRequest) { r.RewardUpdate.DeltaT++ },
		"other epoch size":  func(r *v1.EpochNewEpochRequest) { r.Globals.EpochLength = 100 },
		"no globals":        func(r *v1.EpochNewEpochRequest) { r.Globals = nil },
		"no blocks":         func(r *v1.EpochNewEpochRequest) { r.BlocksPrevious = nil },
		"pool not in state": func(r *v1.EpochNewEpochRequest) { r.BlocksPrevious[0].PoolId = bytes.Repeat([]byte{0xb0}, 28) },
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRewardRequest(t)
			mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != unsupported {
				t.Fatalf("err = %v, want UNSUPPORTED", err)
			}
		})
	}
}

// blocks_previous without a pending update feeds no round, so the boundary is
// the one without it.
func TestEpochNewEpochBlocksWithoutUpdate(t *testing.T) {
	t.Parallel()
	req := newEpochRewardRequest(t)
	req.RewardUpdate = nil
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if d := got.GetDelta(); d.GetReservesDelta() != 0 || d.GetTreasuryDelta() != 0 || len(d.GetAccountsChanged()) != 0 {
		t.Errorf("delta = %v, want none", d)
	}
}

// A DRep delegation is kept across the boundary and reported on a row that
// changed: the undelegated pool account keeps its DRep, a key DRep included.
func TestEpochNewEpochDRepDelegation(t *testing.T) {
	t.Parallel()
	for name, drep := range map[string]*v1.DRep{
		"abstain":       {Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}},
		"no confidence": {Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}},
		"key":           {Target: &v1.DRep_Credential{Credential: rotKey(0xd1)}},
		"script":        {Target: &v1.DRep_Credential{Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: bytes.Repeat([]byte{0xd2}, 28)}}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRequest(t)
			req.State.Accounts[1].Drep = drep
			got, err := epochNewEpoch(context.Background(), req)
			if err != nil {
				t.Fatal(err)
			}
			a := got.GetDelta().GetAccountsChanged()[1]
			if a.GetCredential().GetHash()[0] != 2 || !proto.Equal(a.GetDrep(), drep) {
				t.Errorf("account = %v, want drep %v", a, drep)
			}
			if first := got.GetDelta().GetAccountsChanged()[0]; first.GetDrep() != nil {
				t.Errorf("account without a delegation reports %v", first.GetDrep())
			}
		})
	}
}
