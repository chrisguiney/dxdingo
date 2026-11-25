package main

import (
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func rat(n, d string) *v1.Rational { return &v1.Rational{Numerator: n, Denominator: d} }

// Expected values follow Cardano.Ledger.Shelley.Rewards.memberRew:
// floor((f - c) * (1 - m) * t / sigma), and 0 when f <= c.
func TestMemberReward(t *testing.T) {
	for name, tc := range map[string]struct {
		req  *v1.RewardsMemberRewardRequest
		want uint64
	}{
		"half the pool, 1% margin": {&v1.RewardsMemberRewardRequest{
			PoolReward: 1_000_000_000, Cost: 340_000_000, Margin: rat("1", "100"),
			MemberStake: 25, PoolStake: 50, TotalStake: 1000,
		}, 326_700_000},
		"reward below cost": {&v1.RewardsMemberRewardRequest{
			PoolReward: 300_000_000, Cost: 340_000_000, Margin: rat("0", "1"),
			MemberStake: 25, PoolStake: 50, TotalStake: 1000,
		}, 0},
		"floors": {&v1.RewardsMemberRewardRequest{
			PoolReward: 10, Cost: 0, Margin: rat("0", "1"),
			MemberStake: 1, PoolStake: 3, TotalStake: 3,
		}, 3},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := memberReward(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetReward() != tc.want {
				t.Fatalf("got %v, want reward %d", got, tc.want)
			}
		})
	}
}

func TestMemberRewardMalformedMarginIsBadRequest(t *testing.T) {
	_, err := memberReward(context.Background(), &v1.RewardsMemberRewardRequest{
		PoolReward: 1, PoolStake: 1, Margin: rat("1", "0"),
	})
	if err == nil {
		t.Fatal("accepted a zero-denominator margin")
	}
}

func TestRegistryReportsImplementedFunctions(t *testing.T) {
	info, err := metaInfo(newRegistry())(context.Background(), &v1.MetaInfoRequest{})
	if err != nil {
		t.Fatal(err)
	}
	found := map[string]bool{}
	for _, f := range info.GetImplemented() {
		found[f] = true
	}
	if !found["meta_info"] || !found["rewards_member_reward"] {
		t.Fatalf("implemented = %v", info.GetImplemented())
	}
}
