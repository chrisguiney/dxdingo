package main

import (
	"bytes"
	"context"
	"errors"
	"sort"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"github.com/blinklabs-io/dingo/ledger/rewards"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// minPoolCostMajor is the first protocol major whose non-myopic reward reads
// a minimum pool cost and margin. cardano-ledger has no such floor, so the
// request is UNSUPPORTED from there on.
const minPoolCostMajor = 12

// nonMyopic maps to ledger.NonMyopicMemberRewards, the logic behind dingo's
// ShelleyNonMyopicMemberRewards query. dingo computes the ranking and reward
// in float64 as the ledger does, and the replies are compared exactly.
func nonMyopic(_ context.Context, req *v1.RewardsNonMyopicRequest) (*v1.RewardsNonMyopicResponse, error) {
	pp := req.GetPparams()
	a0, err := ratArg("a0", pp.GetA0())
	if err != nil {
		return nil, err
	}
	if err := nonNegativeInterval("a0", a0); err != nil {
		return nil, err
	}
	if pp.NOpt == nil || pp.GetNOpt() == 0 || pp.GetNOpt() > 0xffff {
		return nil, unsupportedf("n_opt must be in [1, 65535]")
	}
	if pp.GetProtocolVersion().GetMajor() >= minPoolCostMajor {
		return nil, unsupportedf("protocol major %d reads a minimum pool cost", pp.GetProtocolVersion().GetMajor())
	}
	if err := nonzero("total_stake", req.GetTotalStake()); err != nil {
		return nil, err
	}
	stakes, err := nonMyopicPoolValues("pool_distribution", req.GetPoolDistribution(), (*v1.PoolStake).GetPoolId, (*v1.PoolStake).GetStake)
	if err != nil {
		return nil, err
	}
	owners, err := nonMyopicPoolValues("owner_stake", req.GetOwnerStake(), (*v1.PoolOwnerStake).GetPoolId, (*v1.PoolOwnerStake).GetAmount)
	if err != nil {
		return nil, err
	}
	performance := map[string]float64{}
	for _, p := range req.GetPerformance() {
		id, err := rewardPoolID("performance", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		if _, dup := performance[string(id[:])]; dup {
			return nil, dispatch.BadRequest(errors.New("performance: repeated pool"))
		}
		r, err := ratArg("performance", p.GetPerformance())
		if err != nil {
			return nil, err
		}
		if r.Sign() < 0 {
			return nil, unsupportedf("performance is negative")
		}
		performance[string(id[:])], _ = r.Float64()
	}
	defaultPerformance, err := epochengine.RewardPerformanceEstimate(make([]float32, epochengine.RewardLikelihoodSampleCount))
	if err != nil {
		return &v1.RewardsNonMyopicResponse{Rejection: dingoRejection("non-myopic", err)}, nil
	}
	seen := map[string]struct{}{}
	var pools []ledger.NonMyopicPool
	for _, p := range req.GetPools() {
		if p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE {
			continue
		}
		id, err := rewardPoolID("pools", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		if _, dup := seen[string(id[:])]; dup {
			return nil, dispatch.BadRequest(errors.New("pools: repeated pool"))
		}
		seen[string(id[:])] = struct{}{}
		margin, err := poolMargin(p)
		if err != nil {
			return nil, err
		}
		perf, has := performance[string(id[:])]
		if !has {
			perf = defaultPerformance
		}
		pools = append(pools, ledger.NonMyopicPool{
			Key: lcommon.PoolKeyHash(lcommon.NewBlake2b224(id[:])), Stake: stakes[string(id[:])], OwnerStake: owners[string(id[:])],
			Pledge: p.GetPledge(), Cost: p.GetCost(), Margin: margin, Performance: perf, HasLikelihood: has,
		})
	}
	params := rewards.Parameters{
		OptimalPoolCount: uint64(pp.GetNOpt()), PledgeInfluence: a0, ProtocolMajorVersion: uint64(pp.GetProtocolVersion().GetMajor()),
	}
	got, err := ledger.NonMyopicMemberRewards(params, req.GetRewardPot(), req.GetTotalStake(), 0, pools, req.GetMemberStake())
	if err != nil {
		return &v1.RewardsNonMyopicResponse{Rejection: dingoRejection("non-myopic", err)}, nil
	}
	resp := &v1.RewardsNonMyopicResponse{}
	for key, reward := range got {
		resp.Rewards = append(resp.Rewards, &v1.PoolMemberReward{PoolId: bytes.Clone(key.Bytes()), Reward: reward})
	}
	sort.Slice(resp.Rewards, func(i, j int) bool {
		return bytes.Compare(resp.Rewards[i].GetPoolId(), resp.Rewards[j].GetPoolId()) < 0
	})
	return resp, nil
}

// nonMyopicPoolValues indexes a per-pool amount list by pool id; a repeated
// pool is BAD_REQUEST.
func nonMyopicPoolValues[T any](name string, items []T, id func(T) []byte, amount func(T) uint64) (map[string]uint64, error) {
	out := map[string]uint64{}
	for _, it := range items {
		pid, err := rewardPoolID(name, id(it))
		if err != nil {
			return nil, err
		}
		if _, dup := out[string(pid[:])]; dup {
			return nil, dispatch.BadRequest(errors.New(name + ": repeated pool"))
		}
		out[string(pid[:])] = amount(it)
	}
	return out, nil
}
