package main

import (
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
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

// Inputs outside the reference's domain: StakeShare divides by total and
// pool stake, and the margin must be a UnitInterval (a [0,1] ratio of two
// Word64s). Both nodes reply UNSUPPORTED so the pair is skipped.
func TestMemberRewardOutsideDomainIsUnsupported(t *testing.T) {
	base := func() *v1.RewardsMemberRewardRequest {
		return &v1.RewardsMemberRewardRequest{
			PoolReward: 1000, Cost: 1, Margin: rat("1", "2"),
			MemberStake: 1, PoolStake: 2, TotalStake: 4,
		}
	}
	for name, mutate := range map[string]func(*v1.RewardsMemberRewardRequest){
		"zero pool stake":     func(r *v1.RewardsMemberRewardRequest) { r.PoolStake = 0 },
		"zero total stake":    func(r *v1.RewardsMemberRewardRequest) { r.TotalStake = 0 },
		"margin above one":    func(r *v1.RewardsMemberRewardRequest) { r.Margin = rat("3", "2") },
		"negative margin":     func(r *v1.RewardsMemberRewardRequest) { r.Margin = rat("-1", "2") },
		"margin over 64 bits": func(r *v1.RewardsMemberRewardRequest) { r.Margin = rat("1", "18446744073709551617") },
	} {
		t.Run(name, func(t *testing.T) {
			req := base()
			mutate(req)
			_, err := memberReward(context.Background(), req)
			if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
				t.Fatalf("err = %v (code %v), want UNSUPPORTED", err, code)
			}
		})
	}
}
