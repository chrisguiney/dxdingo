package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateRewardsCorpus(t *testing.T) { writeCorpus(t, "", rewardsCases()) }

func rewardsCases() []entry {
	leader := func(name, why string, f, c uint64, m *v1.Rational, owner, pool, total uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsLeaderReward{RewardsLeaderReward: &v1.RewardsLeaderRewardRequest{
			PoolReward: f, Cost: c, Margin: m, OwnerStake: owner, PoolStake: pool, TotalStake: total,
		}}}}
	}
	maxp := func(name, why string, a0 *v1.Rational, k uint32, r, total, pool, pledge uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsMaxPool{RewardsMaxPool: &v1.RewardsMaxPoolRequest{
			Pparams: &v1.ProtocolParameters{A0: a0, NOpt: &k}, PoolPot: r, TotalStake: total, PoolStake: pool, Pledge: pledge,
		}}}}
	}
	perf := func(name, why string, d *v1.Rational, pool, total, n, nbar uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsApparentPerformance{RewardsApparentPerformance: &v1.RewardsApparentPerformanceRequest{
			D: d, PoolStake: pool, TotalActiveStake: total, Blocks: n, BlocksTotal: nbar,
		}}}}
	}
	base := []entry{
		leader("rewards_leader_reward_owner_fifth", "340 + floor(660 * (0.01 + 0.99 * 0.2)) ADA", 1_000_000_000, 340_000_000, rat("1", "100"), 10, 50, 1000),
		leader("rewards_leader_reward_below_cost", "f <= cost: the operator takes f", 300_000_000, 340_000_000, rat("0", "1"), 10, 50, 1000),
		leader("rewards_leader_reward_odd_split", "unreduced margin, uneven owner share", 987_654_321, 1234, rat("50", "200"), 3, 13, 1_000_000),
		maxp("rewards_max_pool_basic", "a0 3/10, k 500, sigma 1/1000, pledge 1/2000: 812500", rat("3", "10"), 500, 1_000_000_000, 1_000_000, 1000, 500),
		maxp("rewards_max_pool_saturated", "sigma and pledge above z0 are capped at 1/k", rat("3", "10"), 500, 1_000_000_000, 1_000_000, 10_000, 9_000),
		maxp("rewards_max_pool_mainnet_like", "mainnet-sized pot and supply", rat("3", "10"), 500, 7_000_000_000_000, 34_000_000_000_000_000, 70_000_000_000_000, 1_000_000_000_000),
		perf("rewards_apparent_performance_decentralized", "beta / sigma = (3/10) / (1/4)", rat("0", "1"), 1, 4, 3, 10),
		perf("rewards_apparent_performance_federated", "d >= 0.8: performance is 1", rat("9", "10"), 1, 4, 3, 10),
		perf("rewards_apparent_performance_no_blocks_total", "beta divides by max 1 blocks_total", rat("1", "2"), 7, 100, 0, 0),
	}
	return append(base, rewardsUpdateCases()...)
}
