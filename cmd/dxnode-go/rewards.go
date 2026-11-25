package main

import (
	"context"
	"errors"

	"github.com/blinklabs-io/dingo/ledger/rewards"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// memberReward maps to dingo rewards.MemberReward. Dingo takes stake
// amounts; total_stake only bounds the domain, because member/pool is the
// same ratio as Haskell's t/sigma.
//
// A dingo error where Haskell returns a value is reported as a rejection,
// not a harness error, so the comparator shows it as a mismatch.
func memberReward(_ context.Context, req *v1.RewardsMemberRewardRequest) (*v1.RewardsMemberRewardResponse, error) {
	margin, err := ratArg("margin", req.GetMargin())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("margin", margin); err != nil {
		return nil, err
	}
	if err := nonzero("pool_stake", req.GetPoolStake(), "total_stake", req.GetTotalStake()); err != nil {
		return nil, err
	}
	got, err := rewards.MemberReward(req.GetPoolReward(), req.GetCost(), margin, req.GetMemberStake(), req.GetPoolStake())
	if err != nil {
		return &v1.RewardsMemberRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	return &v1.RewardsMemberRewardResponse{Reward: got}, nil
}

// leaderReward maps to dingo rewards.LeaderReward
// (calcStakePoolOperatorReward).
func leaderReward(_ context.Context, req *v1.RewardsLeaderRewardRequest) (*v1.RewardsLeaderRewardResponse, error) {
	margin, err := ratArg("margin", req.GetMargin())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("margin", margin); err != nil {
		return nil, err
	}
	if err := nonzero("pool_stake", req.GetPoolStake(), "total_stake", req.GetTotalStake()); err != nil {
		return nil, err
	}
	got, err := rewards.LeaderReward(req.GetPoolReward(), req.GetCost(), margin, req.GetOwnerStake(), req.GetPoolStake())
	if err != nil {
		return &v1.RewardsLeaderRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	return &v1.RewardsLeaderRewardResponse{Reward: got}, nil
}

// maxPool maps to dingo rewards.ComputeMaxPool (maxPool'). Haskell takes
// nOpt as NonZero Word16.
func maxPool(_ context.Context, req *v1.RewardsMaxPoolRequest) (*v1.RewardsMaxPoolResponse, error) {
	pp := req.GetPparams()
	a0, err := ratArg("a0", pp.GetA0())
	if err != nil {
		return nil, err
	}
	if err := nonNegativeInterval("a0", a0); err != nil {
		return nil, err
	}
	if pp.NOpt == nil || pp.GetNOpt() == 0 || pp.GetNOpt() > 0xffff {
		return nil, dispatch.Unsupported(errors.New("n_opt must be in [1, 65535]"))
	}
	if err := nonzero("total_stake", req.GetTotalStake()); err != nil {
		return nil, err
	}
	got := rewards.ComputeMaxPool(
		rewards.Parameters{PledgeInfluence: a0, OptimalPoolCount: uint64(pp.GetNOpt())},
		req.GetPoolPot(), req.GetTotalStake(), req.GetPoolStake(), req.GetPledge(),
	)
	if got == nil || !got.IsUint64() {
		return &v1.RewardsMaxPoolResponse{Rejection: dingoRejection("rewards", errors.New("max pool not a uint64"))}, nil
	}
	return &v1.RewardsMaxPoolResponse{MaxPool: got.Uint64()}, nil
}

// apparentPerformance maps to dingo rewards.ComputeApparentPerformance
// (mkApparentPerformance), with sigma = pool / total active stake.
func apparentPerformance(_ context.Context, req *v1.RewardsApparentPerformanceRequest) (*v1.RewardsApparentPerformanceResponse, error) {
	d, err := ratArg("d", req.GetD())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("d", d); err != nil {
		return nil, err
	}
	if err := nonzero("total_active_stake", req.GetTotalActiveStake()); err != nil {
		return nil, err
	}
	got := rewards.ComputeApparentPerformance(d, req.GetPoolStake(), req.GetTotalActiveStake(), req.GetBlocks(), req.GetBlocksTotal())
	if got == nil {
		return &v1.RewardsApparentPerformanceResponse{Rejection: dingoRejection("rewards", errors.New("nil performance"))}, nil
	}
	return &v1.RewardsApparentPerformanceResponse{Performance: convert.RatProto(got)}, nil
}
