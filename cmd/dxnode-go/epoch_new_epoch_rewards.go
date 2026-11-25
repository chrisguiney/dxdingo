package main

import (
	"context"
	"math/big"

	"github.com/blinklabs-io/dingo/dxoracle"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// newEpochGlobals are the genesis constants dingo's scratch ledger runs the
// boundary under, and so the only Globals a pending reward update can be
// checked against on both nodes.
const (
	newEpochEpochLength  = 432_000
	newEpochMaxSupply    = 45_000_000_000_000_000
	newEpochSecurityK    = 2_160
	newEpochActiveSlotsN = 1
	newEpochActiveSlotsD = 20
)

func newEpochGlobalsMatch(g *v1.Globals) bool {
	asc, err := ratArg("active_slot_coeff", g.GetActiveSlotCoeff())
	return err == nil && asc.Cmp(big.NewRat(newEpochActiveSlotsN, newEpochActiveSlotsD)) == 0 &&
		g.GetEpochLength() == newEpochEpochLength && g.GetMaxLovelaceSupply() == newEpochMaxSupply &&
		g.GetSecurityParam() == newEpochSecurityK
}

// newEpochRewardCall is the reward_update call whose answer a pending
// update must equal. It is built the same way in dxnode-hs.
func newEpochRewardCall(req *v1.EpochNewEpochRequest) *v1.RewardsRewardUpdateRequest {
	st := req.GetState()
	pv := req.GetContext().GetProtocolVersion()
	if pv == nil {
		pv = req.GetPparams().GetProtocolVersion()
	}
	rr := &v1.RewardsRewardUpdateRequest{
		ProtocolVersion: pv, Pparams: req.GetPparams(), Globals: req.GetGlobals(),
		Pots:      &v1.Pots{Reserves: st.GetPots().GetReserves(), Treasury: st.GetPots().GetTreasury(), Fees: st.GetPots().GetFees()},
		Snapshots: req.GetSnapshots(), Pools: st.GetPools(), Blocks: req.GetBlocksPrevious(),
	}
	for _, a := range st.GetAccounts() {
		rr.Registered = append(rr.Registered, a.GetCredential())
	}
	return rr
}

// pendingRewards checks req's reward_update against the update dingo's reward
// calculation derives from the same inputs, and returns the rows the
// boundary's reward round reads. Dingo recomputes the round at the boundary
// instead of taking an update, so an update it would not have built is outside
// the domain, as is any Globals other than the scratch ledger's. A non-nil
// rejection is dingo failing to derive an update the reference derives.
func pendingRewards(ctx context.Context, req *v1.EpochNewEpochRequest) (*dxoracle.NewEpochRewards, *v1.Rejection, error) {
	if !newEpochGlobalsMatch(req.GetGlobals()) {
		return nil, nil, unsupportedf("reward_update needs the scratch ledger's globals")
	}
	rr := newEpochRewardCall(req)
	got, err := rewardUpdate(ctx, rr)
	if err != nil {
		return nil, nil, err
	}
	if got.GetRejection() != nil {
		return nil, got.GetRejection(), nil
	}
	want := proto.Clone(req.GetRewardUpdate()).(*v1.RewardUpdate)
	sortRewards(want.Rewards)
	if !proto.Equal(got.GetUpdate(), want) {
		return nil, nil, unsupportedf("reward_update differs from the update the inputs produce")
	}
	in, err := buildRewardInputs(rr)
	if err != nil {
		return nil, nil, err
	}
	return &dxoracle.NewEpochRewards{
		Snapshot: in.snapshot, PoolInputs: in.poolInputs, StakeInputs: in.stakeInputs,
		BlockCounts: in.blockCounts, Fees: req.GetState().GetPots().GetFees(),
	}, nil, nil
}
