package main

import (
	"context"
	"errors"
	"math/big"

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
	if err := memberRewardDomain(req, margin); err != nil {
		return nil, dispatch.Unsupported(err)
	}
	got, err := rewards.MemberReward(req.GetPoolReward(), req.GetCost(), margin, req.GetMemberStake(), req.GetPoolStake())
	if err != nil {
		return &v1.RewardsMemberRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	return &v1.RewardsMemberRewardResponse{Reward: got}, nil
}

var maxUint64 = new(big.Int).SetUint64(^uint64(0))

// memberRewardDomain rejects inputs the reference cannot represent: its
// stake shares divide by the total and pool stake, and its margin is a
// UnitInterval, a reduced [0,1] ratio of two Word64s. dxnode-hs applies the
// same checks, so both nodes skip these calls rather than one crashing.
func memberRewardDomain(req *v1.RewardsMemberRewardRequest, margin *big.Rat) error {
	if req.GetPoolStake() == 0 || req.GetTotalStake() == 0 {
		return errors.New("pool_stake and total_stake must be nonzero")
	}
	if margin.Sign() < 0 || margin.Cmp(big.NewRat(1, 1)) > 0 {
		return errors.New("margin outside [0, 1]")
	}
	if margin.Num().Cmp(maxUint64) > 0 || margin.Denom().Cmp(maxUint64) > 0 {
		return errors.New("margin does not fit a Word64 ratio")
	}
	return nil
}
