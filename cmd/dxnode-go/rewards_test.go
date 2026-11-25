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

// calcStakePoolOperatorReward: f if f <= c, else
// c + floor((f - c) * (m + (1 - m) * s / sigma)).
func TestLeaderReward(t *testing.T) {
	for name, tc := range map[string]struct {
		req  *v1.RewardsLeaderRewardRequest
		want uint64
	}{
		"owner fifth of pool": {&v1.RewardsLeaderRewardRequest{
			PoolReward: 1_000_000_000, Cost: 340_000_000, Margin: rat("1", "100"),
			OwnerStake: 10, PoolStake: 50, TotalStake: 1000,
		}, 477_280_000},
		"reward below cost goes to the operator": {&v1.RewardsLeaderRewardRequest{
			PoolReward: 300_000_000, Cost: 340_000_000, Margin: rat("0", "1"),
			OwnerStake: 10, PoolStake: 50, TotalStake: 1000,
		}, 300_000_000},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := leaderReward(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetReward() != tc.want {
				t.Fatalf("got %v, want %d", got, tc.want)
			}
		})
	}
	_, err := leaderReward(context.Background(), &v1.RewardsLeaderRewardRequest{
		PoolReward: 1, Margin: rat("1", "2"), OwnerStake: 1, PoolStake: 0, TotalStake: 4,
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("zero pool stake: err = %v, want UNSUPPORTED", err)
	}
}

// maxPool' with a0 = 3/10, nOpt = 500 (z0 = 1/500), sigma = 1/1000,
// pledge = 1/2000: factor2 = 0.00105625, r / 1.3 * factor2 = 812500.
func TestMaxPool(t *testing.T) {
	nOpt := uint32(500)
	got, err := maxPool(context.Background(), &v1.RewardsMaxPoolRequest{
		Pparams: &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: &nOpt},
		PoolPot: 1_000_000_000, TotalStake: 1_000_000, PoolStake: 1000, Pledge: 500,
	})
	if err != nil {
		t.Fatal(err)
	}
	if got.GetMaxPool() != 812_500 {
		t.Fatalf("max_pool = %d, want 812500", got.GetMaxPool())
	}
	_, err = maxPool(context.Background(), &v1.RewardsMaxPoolRequest{
		Pparams: &v1.ProtocolParameters{A0: rat("3", "10")}, PoolPot: 1, TotalStake: 1, PoolStake: 1,
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("missing n_opt: err = %v, want UNSUPPORTED", err)
	}
}

// mkApparentPerformance: 0 when sigma = 0, beta / sigma when d < 0.8,
// otherwise 1; beta = blocks / max 1 blocks_total.
func TestApparentPerformance(t *testing.T) {
	for name, tc := range map[string]struct {
		req  *v1.RewardsApparentPerformanceRequest
		want *v1.Rational
	}{
		"decentralized": {&v1.RewardsApparentPerformanceRequest{
			D: rat("0", "1"), PoolStake: 1, TotalActiveStake: 4, Blocks: 3, BlocksTotal: 10,
		}, rat("6", "5")},
		"federated": {&v1.RewardsApparentPerformanceRequest{
			D: rat("9", "10"), PoolStake: 1, TotalActiveStake: 4, Blocks: 3, BlocksTotal: 10,
		}, rat("1", "1")},
		"no stake": {&v1.RewardsApparentPerformanceRequest{
			D: rat("0", "1"), PoolStake: 0, TotalActiveStake: 4, Blocks: 3, BlocksTotal: 10,
		}, rat("0", "1")},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := apparentPerformance(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			p := got.GetPerformance()
			if p.GetNumerator() != tc.want.GetNumerator() || p.GetDenominator() != tc.want.GetDenominator() {
				t.Fatalf("performance = %v, want %v", p, tc.want)
			}
		})
	}
}
