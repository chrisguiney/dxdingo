package main

import (
	"context"

	"github.com/blinklabs-io/dingo/ledger/rewards"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// memberReward maps to dingo rewards.MemberReward. Dingo takes stake
// amounts; total_stake is unused here because member/pool is the same ratio
// as Haskell's t/sigma.
//
// A dingo error where Haskell returns a value is reported as a rejection,
// not a harness error, so the comparator shows it as a mismatch.
func memberReward(_ context.Context, req *v1.RewardsMemberRewardRequest) (*v1.RewardsMemberRewardResponse, error) {
	margin, err := convert.Rat(req.GetMargin())
	if err != nil {
		return nil, dispatch.BadRequest(err)
	}
	got, err := rewards.MemberReward(req.GetPoolReward(), req.GetCost(), margin, req.GetMemberStake(), req.GetPoolStake())
	if err != nil {
		return &v1.RewardsMemberRewardResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
			Rule: "rewards", Name: "DingoError", Detail: err.Error(),
		}}}}, nil
	}
	return &v1.RewardsMemberRewardResponse{Reward: got}, nil
}
