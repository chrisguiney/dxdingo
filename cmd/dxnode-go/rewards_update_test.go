package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func u32RewardsStake(n uint32) *uint32 { return &n }

func pvRewardsStake(major uint32) *v1.ProtocolVersion { return &v1.ProtocolVersion{Major: major} }

func poolIDRewardsStake(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

func scriptCred(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: bytes.Repeat([]byte{b}, 28)}
}

func isUnsupported(err error) bool {
	return err != nil && dispatch.Code(err) == v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
}

// One pool of total stake 100 out of a circulation of 1000 and an active
// stake of 500, a0 = 3/10, k = 500: both sigma and the pledge exceed
// z0 = 1/500, so maxPool' = R * z0 = 2_000_000 for R = 1e9. The pool made
// 5 of 10 blocks, so performance = (1/2) / (100/500) = 5/2 and the pool
// earns floor(5/2 * 2_000_000) = 5_000_000. Cost 1_000_000, margin 1/10:
//
//	leader = 1e6 + floor(4e6 * (0.1 + 0.9 * 20/100)) = 2_120_000
//	member = floor(4e6 * 0.9 * stake/100): 50 -> 1_800_000, 30 -> 1_080_000
func poolRewardRequest(major uint32) *v1.RewardsPoolRewardRequest {
	return &v1.RewardsPoolRewardRequest{
		ProtocolVersion: pvRewardsStake(major),
		Pparams:         &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: u32RewardsStake(500), D: rat("0", "1")},
		PoolPot:         1_000_000_000, TotalStake: 1000, TotalActiveStake: 500,
		Blocks: 5, BlocksTotal: 10,
		Pool: &v1.Pool{
			PoolId: poolIDRewardsStake(0xa1), Pledge: 10, Cost: 1_000_000, Margin: rat("1", "10"),
			RewardAccount: keyCred(0x01), Owners: [][]byte{keyCred(0x02).GetHash()},
		},
		Members: []*v1.StakeEntry{
			{Credential: keyCred(0x02), Amount: 20},
			{Credential: keyCred(0x03), Amount: 50},
			{Credential: keyCred(0x04), Amount: 30},
		},
		Registered: []*v1.Credential{keyCred(0x01), keyCred(0x02), keyCred(0x03), keyCred(0x04)},
	}
}

func rewardMap(rs []*v1.Reward) map[string]uint64 {
	out := map[string]uint64{}
	for _, r := range rs {
		k := string(r.GetCredential().GetHash()[:1])
		if r.GetKind() == v1.RewardKind_REWARD_KIND_LEADER {
			k += "L"
		}
		out[k] += r.GetAmount()
	}
	return out
}

func TestPoolReward(t *testing.T) {
	got, err := poolReward(context.Background(), poolRewardRequest(8))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || got.GetPoolReward() != 5_000_000 || got.GetLeaderReward() != 2_120_000 {
		t.Fatalf("got %v", got)
	}
	if p := got.GetPerformance(); p.GetNumerator() != "5" || p.GetDenominator() != "2" {
		t.Errorf("performance = %v, want 5/2", p)
	}
	want := map[string]uint64{"\x01L": 2_120_000, "\x03": 1_800_000, "\x04": 1_080_000}
	rm := rewardMap(got.GetRewards())
	if len(rm) != 3 || rm["\x01L"] != 2_120_000 || rm["\x03"] != 1_800_000 || rm["\x04"] != 1_080_000 {
		t.Errorf("rewards = %v, want %v", rm, want)
	}
}

// Before Babbage a member that is not registered is dropped before the
// reward is formed.
func TestPoolRewardPrefilterDropsUnregisteredMember(t *testing.T) {
	req := poolRewardRequest(6)
	req.Registered = []*v1.Credential{keyCred(0x01), keyCred(0x02), keyCred(0x03)}
	got, err := poolReward(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	rm := rewardMap(got.GetRewards())
	if _, ok := rm["\x04"]; ok || rm["\x03"] != 1_800_000 {
		t.Errorf("rewards = %v", rm)
	}
	req = poolRewardRequest(7)
	req.Registered = []*v1.Credential{keyCred(0x01), keyCred(0x02), keyCred(0x03)}
	got, err = poolReward(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if rm := rewardMap(got.GetRewards()); rm["\x04"] != 1_080_000 {
		t.Errorf("Babbage rewards = %v, want the unregistered member kept", rm)
	}
}

// A pool that does not meet its pledge earns nothing.
func TestPoolRewardPledgeNotMet(t *testing.T) {
	req := poolRewardRequest(8)
	req.Pool.Pledge = 21
	got, err := poolReward(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetPoolReward() != 0 || got.GetLeaderReward() != 0 || len(got.GetRewards()) != 0 {
		t.Fatalf("got %v", got)
	}
}

func TestPoolRewardDomain(t *testing.T) {
	for name, mutate := range map[string]func(*v1.RewardsPoolRewardRequest){
		"zero total stake":        func(r *v1.RewardsPoolRewardRequest) { r.TotalStake = 0 },
		"zero total active stake": func(r *v1.RewardsPoolRewardRequest) { r.TotalActiveStake = 0 },
		"margin above one":        func(r *v1.RewardsPoolRewardRequest) { r.Pool.Margin = rat("3", "2") },
		"zero member stake":       func(r *v1.RewardsPoolRewardRequest) { r.Members[1].Amount = 0 },
		"major below Shelley":     func(r *v1.RewardsPoolRewardRequest) { r.ProtocolVersion = pvRewardsStake(1) },
	} {
		t.Run(name, func(t *testing.T) {
			req := poolRewardRequest(8)
			mutate(req)
			if _, err := poolReward(context.Background(), req); !isUnsupported(err) {
				t.Fatalf("err = %v, want UNSUPPORTED", err)
			}
		})
	}
}

// rho = 3/1000, tau = 1/5, epoch 432000 slots, f = 1/20: expected blocks
// 21600. 10800 blocks give eta = 1/2, so deltaR1 = floor(1/2 * 3/1000 * 1e12)
// = 1.5e9; pot = 1.5e9 + 2e6; cut = floor(1/5 * pot) = 300_400_000.
func monetaryRequest(major uint32, d *v1.Rational, blocks uint64) *v1.RewardsMonetaryExpansionRequest {
	return &v1.RewardsMonetaryExpansionRequest{
		ProtocolVersion: pvRewardsStake(major),
		Pparams: &v1.ProtocolParameters{
			A0: rat("3", "10"), NOpt: u32RewardsStake(500), Rho: rat("3", "1000"), Tau: rat("1", "5"), D: d,
		},
		Globals: &v1.Globals{
			EpochLength: 432000, ActiveSlotCoeff: rat("1", "20"), SecurityParam: 2160,
			MaxLovelaceSupply: 45_000_000_000_000_000,
		},
		Reserves: 1_000_000_000_000, Fees: 2_000_000, BlocksTotal: blocks,
	}
}

func TestMonetaryExpansion(t *testing.T) {
	for name, tc := range map[string]struct {
		req                        *v1.RewardsMonetaryExpansionRequest
		deltaR1, pot, cut, poolPot uint64
	}{
		"half efficiency":       {monetaryRequest(8, rat("0", "1"), 10800), 1_500_000_000, 1_502_000_000, 300_400_000, 1_201_600_000},
		"efficiency capped":     {monetaryRequest(8, rat("0", "1"), 30000), 3_000_000_000, 3_002_000_000, 600_400_000, 2_401_600_000},
		"d at least 4/5 is one": {monetaryRequest(3, rat("9", "10"), 0), 3_000_000_000, 3_002_000_000, 600_400_000, 2_401_600_000},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := monetaryExpansion(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetDeltaR1() != tc.deltaR1 || got.GetRewardPot() != tc.pot ||
				got.GetTreasuryCut() != tc.cut || got.GetPoolPot() != tc.poolPot {
				t.Fatalf("got %v", got)
			}
		})
	}
}

func TestMonetaryExpansionZeroExpectedBlocksIsUnsupported(t *testing.T) {
	req := monetaryRequest(8, rat("0", "1"), 1)
	req.Globals.EpochLength = 10
	if _, err := monetaryExpansion(context.Background(), req); !isUnsupported(err) {
		t.Fatalf("err = %v, want UNSUPPORTED", err)
	}
}

// Two pools out of a circulation of 1000 (max supply 400_000_001_000 less
// reserves 4e11) and an active stake of 500. Pool A is the pool of
// poolRewardRequest and made every block of the epoch (21600, so eta = 1):
// deltaR1 = 3/1000 * 4e11 = 1.2e9, pot = 1.25e9 with 5e7 fees, cut = 2.5e8,
// R = 1e9. Its performance is 1 / (100/500) = 5, so it earns
// floor(5 * 2e6) = 1e7: leader 1e6 + floor(9e6 * 0.28) = 3_520_000, members
// 4_050_000 (stake 50) and 2_430_000 (stake 30). Pool B (stake 400) made no
// blocks.
func rewardUpdateRequest(major uint32) *v1.RewardsRewardUpdateRequest {
	req := poolRewardRequest(major)
	snap := func(c *v1.Credential, amount uint64, pool []byte) *v1.SnapshotEntry {
		return &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: c, Amount: amount, PoolId: pool}
	}
	poolB := &v1.Pool{
		PoolId: poolIDRewardsStake(0xb2), Cost: 340_000_000, Margin: rat("1", "20"),
		RewardAccount: keyCred(0x05), Owners: [][]byte{keyCred(0x06).GetHash()},
	}
	return &v1.RewardsRewardUpdateRequest{
		ProtocolVersion: pvRewardsStake(major),
		Pparams: &v1.ProtocolParameters{
			A0: rat("3", "10"), NOpt: u32RewardsStake(500), Rho: rat("3", "1000"), Tau: rat("1", "5"), D: rat("0", "1"),
		},
		Globals: &v1.Globals{
			EpochLength: 432000, ActiveSlotCoeff: rat("1", "20"), SecurityParam: 2160,
			MaxLovelaceSupply: 400_000_001_000,
		},
		Pots: &v1.Pots{Reserves: 400_000_000_000, Treasury: 7, Fees: 50_000_000},
		Snapshots: []*v1.SnapshotEntry{
			snap(keyCred(0x02), 20, poolIDRewardsStake(0xa1)), snap(keyCred(0x03), 50, poolIDRewardsStake(0xa1)), snap(keyCred(0x04), 30, poolIDRewardsStake(0xa1)),
			snap(keyCred(0x06), 400, poolIDRewardsStake(0xb2)),
		},
		Pools:      []*v1.Pool{req.Pool, poolB},
		Blocks:     []*v1.BlocksMade{{PoolId: poolIDRewardsStake(0xa1), Count: 21600}},
		Registered: req.Registered,
	}
}

func TestRewardUpdate(t *testing.T) {
	got, err := rewardUpdate(context.Background(), rewardUpdateRequest(8))
	if err != nil {
		t.Fatal(err)
	}
	u := got.GetUpdate()
	if got.GetRejection() != nil || u.GetDeltaR() != -210_000_000 || u.GetDeltaT() != 250_000_000 || u.GetDeltaF() != -50_000_000 {
		t.Fatalf("got %v", got)
	}
	rm := rewardMap(u.GetRewards())
	if len(rm) != 3 || rm["\x01L"] != 3_520_000 || rm["\x03"] != 4_050_000 || rm["\x04"] != 2_430_000 {
		t.Errorf("rewards = %v", rm)
	}
}

// Alonzo still prefilters: the unregistered member is dropped, so its
// 2_430_000 stays in reserves: deltaR = -1.2e9 + (1e9 - 7_570_000).
func TestRewardUpdatePrefilter(t *testing.T) {
	req := rewardUpdateRequest(6)
	req.Registered = []*v1.Credential{keyCred(0x01), keyCred(0x02), keyCred(0x03)}
	got, err := rewardUpdate(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	rm := rewardMap(got.GetUpdate().GetRewards())
	if got.GetUpdate().GetDeltaR() != -207_570_000 || len(rm) != 2 || rm["\x03"] != 4_050_000 {
		t.Fatalf("got %v", got)
	}
}

func TestRewardUpdateDomain(t *testing.T) {
	req := rewardUpdateRequest(8)
	req.Globals.MaxLovelaceSupply = 1
	if _, err := rewardUpdate(context.Background(), req); !isUnsupported(err) {
		t.Fatalf("err = %v, want UNSUPPORTED for reserves above the supply", err)
	}
}

// The update of TestRewardUpdate (Babbage) applied: reserves 4e11 - 2.1e8,
// treasury 7 + 2.5e8 + the unregistered member's 2_430_000, fees spent.
func applyUpdateRequest() *v1.RewardsApplyUpdateRequest {
	rew := func(c *v1.Credential, kind v1.RewardKind, amount uint64) *v1.Reward {
		return &v1.Reward{Credential: c, PoolId: poolIDRewardsStake(0xa1), Kind: kind, Amount: amount}
	}
	return &v1.RewardsApplyUpdateRequest{
		ProtocolVersion: pvRewardsStake(8),
		Pots:            &v1.Pots{Reserves: 400_000_000_000, Treasury: 7, Fees: 50_000_000},
		Update: &v1.RewardUpdate{
			DeltaR: -210_000_000, DeltaT: 250_000_000, DeltaF: -50_000_000,
			Rewards: []*v1.Reward{
				rew(keyCred(0x01), v1.RewardKind_REWARD_KIND_LEADER, 3_520_000),
				rew(keyCred(0x03), v1.RewardKind_REWARD_KIND_MEMBER, 4_050_000),
				rew(keyCred(0x04), v1.RewardKind_REWARD_KIND_MEMBER, 2_430_000),
			},
		},
		Registered:        []*v1.Credential{keyCred(0x01), keyCred(0x02), keyCred(0x03)},
		TotalRewardPot:    1_250_000_000,
		TreasuryExpansion: rat("1", "5"),
	}
}

func TestApplyUpdate(t *testing.T) {
	got, err := applyUpdate(context.Background(), applyUpdateRequest())
	if err != nil {
		t.Fatal(err)
	}
	p := got.GetPots()
	if got.GetRejection() != nil || p.GetReserves() != 399_790_000_000 || p.GetTreasury() != 252_430_007 || p.GetFees() != 0 {
		t.Fatalf("got %v", got)
	}
	if got.GetUnregisteredToTreasury() != 2_430_000 || len(got.GetCredited()) != 2 {
		t.Fatalf("got %v", got)
	}
	for _, c := range got.GetCredited() {
		switch c.GetCredential().GetHash()[0] {
		case 0x01:
			if c.GetAmount() != 3_520_000 {
				t.Errorf("leader credit = %d", c.GetAmount())
			}
		case 0x03:
			if c.GetAmount() != 4_050_000 {
				t.Errorf("member credit = %d", c.GetAmount())
			}
		default:
			t.Errorf("unexpected credit %v", c)
		}
	}
}

func TestApplyUpdateDomain(t *testing.T) {
	for name, mutate := range map[string]func(*v1.RewardsApplyUpdateRequest){
		"delta_t inconsistent with the pot": func(r *v1.RewardsApplyUpdateRequest) { r.Update.DeltaT++ },
		"fees increase":                     func(r *v1.RewardsApplyUpdateRequest) { r.Update.DeltaF = 1 },
		"before Allegra":                    func(r *v1.RewardsApplyUpdateRequest) { r.ProtocolVersion = pvRewardsStake(2) },
	} {
		t.Run(name, func(t *testing.T) {
			req := applyUpdateRequest()
			mutate(req)
			if _, err := applyUpdate(context.Background(), req); !isUnsupported(err) {
				t.Fatalf("err = %v, want UNSUPPORTED", err)
			}
		})
	}
}

// nOpt 2, a0 3/10, reward pot 1e9, circulation 1000, member stake 50. The
// derivation is in dingo's ledger/pure_logic_nonmyopic_test.go: pools A and B
// are the top pools, C earns nothing.
func nonMyopicRequest() *v1.RewardsNonMyopicRequest {
	pool := func(id byte, pledge, cost uint64, margin *v1.Rational) *v1.Pool {
		return &v1.Pool{PoolId: poolIDRewardsStake(id), Pledge: pledge, Cost: cost, Margin: margin, RewardAccount: keyCred(0xee)}
	}
	return &v1.RewardsNonMyopicRequest{
		Pparams:    &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: u32RewardsStake(2)},
		TotalStake: 1000, RewardPot: 1_000_000_000, MemberStake: 50,
		Pools: []*v1.Pool{
			pool(0xa1, 10, 1_000_000, rat("1", "10")), pool(0xb2, 5, 2_000_000, rat("1", "5")), pool(0xc3, 1, 400_000_000, rat("0", "1")),
		},
		PoolDistribution: []*v1.PoolStake{
			{PoolId: poolIDRewardsStake(0xa1), Stake: 100}, {PoolId: poolIDRewardsStake(0xb2), Stake: 50}, {PoolId: poolIDRewardsStake(0xc3), Stake: 10},
		},
		Performance: []*v1.PoolPerformance{
			{PoolId: poolIDRewardsStake(0xa1), Performance: rat("1", "1")}, {PoolId: poolIDRewardsStake(0xb2), Performance: rat("1", "2")},
			{PoolId: poolIDRewardsStake(0xc3), Performance: rat("1", "1")},
		},
		OwnerStake: []*v1.PoolOwnerStake{
			{PoolId: poolIDRewardsStake(0xa1), Amount: 20}, {PoolId: poolIDRewardsStake(0xb2), Amount: 5}, {PoolId: poolIDRewardsStake(0xc3), Amount: 1},
		},
	}
}

func nonMyopicRewards(t *testing.T, req *v1.RewardsNonMyopicRequest) map[byte]uint64 {
	t.Helper()
	got, err := nonMyopic(context.Background(), req)
	if err != nil || got.GetRejection() != nil {
		t.Fatalf("err = %v, rejection = %v", err, got.GetRejection())
	}
	out := map[byte]uint64{}
	for _, r := range got.GetRewards() {
		out[r.GetPoolId()[0]] = r.GetReward()
	}
	return out
}

func TestNonMyopic(t *testing.T) {
	got := nonMyopicRewards(t, nonMyopicRequest())
	if len(got) != 3 || got[0xa1] != 34_733_076 || got[0xb2] != 15_270_769 || got[0xc3] != 0 {
		t.Fatalf("rewards = %v", got)
	}
}

func TestNonMyopicPledgeNotMetPaysNothing(t *testing.T) {
	req := nonMyopicRequest()
	req.OwnerStake[0].Amount = 9
	if got := nonMyopicRewards(t, req); got[0xa1] != 0 || got[0xb2] != 15_270_769 {
		t.Fatalf("rewards = %v", got)
	}
}

func TestNonMyopicDomain(t *testing.T) {
	for name, mutate := range map[string]func(*v1.RewardsNonMyopicRequest){
		"zero total stake":     func(r *v1.RewardsNonMyopicRequest) { r.TotalStake = 0 },
		"zero n_opt":           func(r *v1.RewardsNonMyopicRequest) { r.Pparams.NOpt = u32RewardsStake(0) },
		"margin above one":     func(r *v1.RewardsNonMyopicRequest) { r.Pools[0].Margin = rat("3", "2") },
		"negative performance": func(r *v1.RewardsNonMyopicRequest) { r.Performance[0].Performance = rat("-1", "2") },
	} {
		t.Run(name, func(t *testing.T) {
			req := nonMyopicRequest()
			mutate(req)
			if _, err := nonMyopic(context.Background(), req); !isUnsupported(err) {
				t.Fatalf("err = %v, want UNSUPPORTED", err)
			}
		})
	}
}
