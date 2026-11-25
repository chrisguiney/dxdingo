package main

import (
	"context"
	"errors"
	"math"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/database/types"
	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"github.com/blinklabs-io/dingo/ledger/rewards"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// expectedBlocksIsZero is the domain check shared with dxnode-hs: with
// d < 4/5 the ledger divides the blocks made by floor((1-d) * f * epoch
// length), which is a zero division when that is zero.
func expectedBlocksIsZero(dom rewardDomain, glob rewardGlobals) bool {
	if dom.d.Cmp(big.NewRat(4, 5)) >= 0 {
		return false
	}
	e := new(big.Rat).Sub(big.NewRat(1, 1), dom.d)
	e.Mul(e, glob.asc)
	e.Mul(e, new(big.Rat).SetUint64(glob.epochLength))
	return new(big.Int).Quo(e.Num(), e.Denom()).Sign() == 0
}

// monetaryExpansion maps to dingo rewards.ComputeMonetaryExpansion.
func monetaryExpansion(_ context.Context, req *v1.RewardsMonetaryExpansionRequest) (*v1.RewardsMonetaryExpansionResponse, error) {
	dom, err := rewardPParams(req.GetProtocolVersion(), req.GetPparams(), true)
	if err != nil {
		return nil, err
	}
	glob, err := rewardGlobalsArg(req.GetGlobals())
	if err != nil {
		return nil, err
	}
	if req.GetReserves() > glob.maxSupply {
		return nil, unsupportedf("reserves exceed max_lovelace_supply")
	}
	if expectedBlocksIsZero(dom, glob) {
		return nil, unsupportedf("expected blocks per epoch is zero")
	}
	params, err := rewardParameters(dom, glob)
	if err != nil {
		return &v1.RewardsMonetaryExpansionResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	got, err := rewards.ComputeMonetaryExpansion(params, req.GetReserves(), req.GetFees(), req.GetBlocksTotal())
	if err != nil {
		return &v1.RewardsMonetaryExpansionResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	return &v1.RewardsMonetaryExpansionResponse{
		DeltaR1: got.DeltaR1, RewardPot: got.RewardPot, TreasuryCut: got.TreasuryCut, PoolPot: got.PoolPot,
	}, nil
}

// poolReward maps to dingo rewards.CalculatePoolReward for the pool-level
// amounts and rewards.ApplyPoolMemberRewards for the credited rewards.
// ApplyPoolMemberRewards splits one pool on its own, which dingo supports
// from Allegra on only; earlier majors are covered by reward_update.
func poolReward(_ context.Context, req *v1.RewardsPoolRewardRequest) (*v1.RewardsPoolRewardResponse, error) {
	dom, err := rewardPParams(req.GetProtocolVersion(), req.GetPparams(), false)
	if err != nil {
		return nil, err
	}
	if dom.major < 3 {
		return nil, unsupportedf("pool_reward needs protocol major >= 3")
	}
	if err := nonzero("total_stake", req.GetTotalStake(), "total_active_stake", req.GetTotalActiveStake()); err != nil {
		return nil, err
	}
	params, err := rewardParameters(dom, rewardGlobals{
		asc: big.NewRat(1, 20), epochLength: 432000, maxSupply: math.MaxUint64, securityK: 1,
	})
	if err != nil {
		return &v1.RewardsPoolRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	registered, err := registeredSet("registered", req.GetRegistered())
	if err != nil {
		return nil, err
	}
	pool, err := buildPool(req.GetPool(), req.GetMembers(), registered, req.GetBlocks(), req.GetBlocksTotal())
	if err != nil {
		return nil, err
	}
	base, err := rewards.CalculatePoolReward(
		pool, req.GetPoolPot(), req.GetTotalActiveStake(), req.GetTotalStake(), req.GetBlocksTotal(), params,
	)
	if err != nil {
		return &v1.RewardsPoolRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	split, err := rewards.ApplyPoolMemberRewards(pool, base, params)
	if err != nil {
		return &v1.RewardsPoolRewardResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	resp := &v1.RewardsPoolRewardResponse{PoolReward: base.PoolReward, LeaderReward: base.LeaderReward}
	perf := base.ApparentPerformance
	if perf == nil && req.GetBlocks() > 0 {
		// A pool below its pledge returns before dingo forms a performance;
		// the ledger still forms one for a pool that made blocks.
		perf = rewards.ComputeApparentPerformance(
			params.Decentralization, pool.DelegatedStake, req.GetTotalActiveStake(), req.GetBlocks(), req.GetBlocksTotal(),
		)
	}
	if perf != nil {
		resp.Performance = convert.RatProto(perf)
	}
	for _, r := range split.Rewards {
		if r.Amount > 0 {
			resp.Rewards = append(resp.Rewards, protoReward(r))
		}
	}
	sortRewards(resp.Rewards)
	return resp, nil
}

// rewardInputs assembles the stored reward-state rows dingo's calculator
// reads from the request's GO snapshot rows, pools and registered set.
type rewardInputs struct {
	snapshot    *models.RewardSnapshot
	poolInputs  []*models.RewardPoolInput
	stakeInputs []*models.RewardStakeInput
	blockCounts map[string]uint64
	totalBlocks uint64
	registered  map[string]struct{}
}

func buildRewardInputs(req *v1.RewardsRewardUpdateRequest) (*rewardInputs, error) {
	registered := map[string]struct{}{}
	for _, c := range req.GetRegistered() {
		tag, err := credentialTag(c)
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		if len(c.GetHash()) != rewards.CredentialHashSize {
			return nil, dispatch.BadRequest(errors.New("registered: credential hash is not 28 bytes"))
		}
		registered[models.NewStakeCredentialRef(tag, c.GetHash()).MapKey()] = struct{}{}
	}
	in := &rewardInputs{blockCounts: map[string]uint64{}, registered: registered}
	for _, b := range req.GetBlocks() {
		id, err := rewardPoolID("blocks", b.GetPoolId())
		if err != nil {
			return nil, err
		}
		if _, dup := in.blockCounts[string(id[:])]; dup {
			return nil, dispatch.BadRequest(errors.New("blocks: repeated pool"))
		}
		in.blockCounts[string(id[:])] = b.GetCount()
		in.totalBlocks += b.GetCount()
	}
	pools := map[string]*v1.Pool{}
	for _, p := range req.GetPools() {
		if p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE {
			continue
		}
		id, err := rewardPoolID("pools", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		if _, dup := pools[string(id[:])]; dup {
			return nil, dispatch.BadRequest(errors.New("pools: repeated pool"))
		}
		pools[string(id[:])] = p
	}
	for id := range in.blockCounts {
		if _, ok := pools[id]; !ok {
			return nil, unsupportedf("blocks: pool without a registration")
		}
	}
	owners := map[string]map[string]struct{}{}
	for id, p := range pools {
		set := map[string]struct{}{}
		for _, o := range p.GetOwners() {
			set[string(o)] = struct{}{}
		}
		owners[id] = set
	}

	seen := map[string]struct{}{}
	var totalActive, excluded uint64
	stakeByPool := map[string]uint64{}
	ownerByPool := map[string]uint64{}
	countByPool := map[string]uint64{}
	for _, row := range req.GetSnapshots() {
		if row.GetSnapshot() != v1.SnapshotKind_SNAPSHOT_KIND_GO || len(row.GetPoolId()) == 0 {
			continue
		}
		tag, err := credentialTag(row.GetCredential())
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		if len(row.GetCredential().GetHash()) != rewards.CredentialHashSize {
			return nil, dispatch.BadRequest(errors.New("snapshots: credential hash is not 28 bytes"))
		}
		if _, err := rewardPoolID("snapshots", row.GetPoolId()); err != nil {
			return nil, err
		}
		if _, dup := seen[credKeyRewardsStake(row.GetCredential())]; dup {
			return nil, dispatch.BadRequest(errors.New("snapshots: repeated credential"))
		}
		seen[credKeyRewardsStake(row.GetCredential())] = struct{}{}
		if row.GetAmount() == 0 {
			return nil, unsupportedf("snapshots: zero stake")
		}
		if totalActive > math.MaxUint64-row.GetAmount() {
			return nil, unsupportedf("snapshots: stake total exceeds Word64")
		}
		totalActive += row.GetAmount()
		pool := string(row.GetPoolId())
		if _, ok := pools[pool]; !ok {
			excluded += row.GetAmount()
			continue
		}
		_, registeredNow := registered[models.NewStakeCredentialRef(tag, row.GetCredential().GetHash()).MapKey()]
		_, isOwner := owners[pool][string(row.GetCredential().GetHash())]
		isOwner = isOwner && tag == 0
		in.stakeInputs = append(in.stakeInputs, &models.RewardStakeInput{
			PoolKeyHash: row.GetPoolId(), StakingKey: row.GetCredential().GetHash(), CredentialTag: tag,
			Stake: types.Uint64(row.GetAmount()), Owner: isOwner, Registered: registeredNow,
		})
		stakeByPool[pool] += row.GetAmount()
		countByPool[pool]++
		if isOwner {
			ownerByPool[pool] += row.GetAmount()
		}
	}
	var delegators uint64
	ids := make([]string, 0, len(pools))
	for id := range pools {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	for _, id := range ids {
		p := pools[id]
		margin, err := poolMargin(p)
		if err != nil {
			return nil, err
		}
		account, err := credentialTag(p.GetRewardAccount())
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		if len(p.GetRewardAccount().GetHash()) != rewards.CredentialHashSize {
			return nil, dispatch.BadRequest(errors.New("pools: reward account hash is not 28 bytes"))
		}
		blocks := in.blockCounts[id]
		in.poolInputs = append(in.poolInputs, &models.RewardPoolInput{
			Margin: &types.Rat{Rat: margin}, PoolKeyHash: p.GetPoolId(), RewardAccount: p.GetRewardAccount().GetHash(),
			RewardAccountCredentialTag: account, Pledge: types.Uint64(p.GetPledge()), Cost: types.Uint64(p.GetCost()),
			DelegatedStake: types.Uint64(stakeByPool[id]), OwnerStake: types.Uint64(ownerByPool[id]),
			DelegatorCount: countByPool[id], BlocksProduced: &blocks, TotalBlocksInEpoch: &in.totalBlocks,
		})
		delegators += countByPool[id]
	}
	in.snapshot = &models.RewardSnapshot{
		TotalActiveStake: types.Uint64(totalActive), ExcludedActiveStake: (*types.Uint64)(&excluded),
		TotalPoolCount: uint64(len(in.poolInputs)), TotalDelegators: delegators,
	}
	return in, nil
}

// rewardUpdate maps to epochengine.BuildRewardSnapshot and rewards.Calculate.
// The update's reserves delta is the change Calculate makes to reserves,
// its treasury delta the tax alone: unspendable rewards reach the treasury
// when the update is applied.
func rewardUpdate(_ context.Context, req *v1.RewardsRewardUpdateRequest) (*v1.RewardsRewardUpdateResponse, error) {
	dom, err := rewardPParams(req.GetProtocolVersion(), req.GetPparams(), true)
	if err != nil {
		return nil, err
	}
	glob, err := rewardGlobalsArg(req.GetGlobals())
	if err != nil {
		return nil, err
	}
	pots := req.GetPots()
	if pots.GetReserves() > glob.maxSupply {
		return nil, unsupportedf("reserves exceed max_lovelace_supply")
	}
	if expectedBlocksIsZero(dom, glob) {
		return nil, unsupportedf("expected blocks per epoch is zero")
	}
	in, err := buildRewardInputs(req)
	if err != nil {
		return nil, err
	}
	reject := func(err error) (*v1.RewardsRewardUpdateResponse, error) {
		return &v1.RewardsRewardUpdateResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	params, err := rewardParameters(dom, glob)
	if err != nil {
		return reject(err)
	}
	snap, err := epochengine.BuildRewardSnapshot(epochengine.RewardSnapshotInput{
		Snapshot: in.snapshot, PoolInputs: in.poolInputs, StakeInputs: in.stakeInputs,
		BlockCounts: in.blockCounts, TotalBlocks: in.totalBlocks,
		ActiveAccounts: in.registered, PrefilterAccounts: in.registered,
	})
	if err != nil {
		return reject(err)
	}
	res, err := rewards.Calculate(
		rewards.Pots{Reserves: pots.GetReserves(), Treasury: pots.GetTreasury(), Fees: pots.GetFees()}, snap, params,
	)
	if err != nil {
		return reject(err)
	}
	if res.UpdatedPots.Reserves > math.MaxInt64 || pots.GetReserves() > math.MaxInt64 ||
		res.TreasuryTax > math.MaxInt64 || pots.GetFees() > math.MaxInt64 {
		return nil, unsupportedf("pot amounts exceed Int64")
	}
	update := &v1.RewardUpdate{
		DeltaR: int64(res.UpdatedPots.Reserves) - int64(pots.GetReserves()),
		DeltaT: int64(res.TreasuryTax),
		DeltaF: -int64(pots.GetFees()),
	}
	for _, r := range res.AccountRewards {
		if r.Amount > 0 {
			update.Rewards = append(update.Rewards, protoReward(r))
		}
	}
	sortRewards(update.Rewards)
	return &v1.RewardsRewardUpdateResponse{Update: update}, nil
}

// applyUpdate maps to epochengine.ComputeRewardTotals and ComputeUpdatedPots.
// dingo never holds an abstract update: it recomputes the pots from the
// reward pot and tau, so the request carries both and delta_t must agree with
// them (see the proto comment).
func applyUpdate(_ context.Context, req *v1.RewardsApplyUpdateRequest) (*v1.RewardsApplyUpdateResponse, error) {
	if _, err := rewardMajor(req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	if req.GetProtocolVersion().GetMajor() < 3 {
		return nil, unsupportedf("apply_update needs protocol major >= 3")
	}
	tau, err := ratArg("treasury_expansion", req.GetTreasuryExpansion())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("treasury_expansion", tau); err != nil {
		return nil, err
	}
	up, pots := req.GetUpdate(), req.GetPots()
	cut := new(big.Rat).Mul(tau, new(big.Rat).SetUint64(req.GetTotalRewardPot()))
	if new(big.Int).Quo(cut.Num(), cut.Denom()).Cmp(big.NewInt(up.GetDeltaT())) != 0 {
		return nil, unsupportedf("delta_t differs from floor(treasury_expansion * total_reward_pot)")
	}
	if up.GetDeltaF() > 0 || uint64(-up.GetDeltaF()) > req.GetTotalRewardPot() || uint64(-up.GetDeltaF()) > pots.GetFees() {
		return nil, unsupportedf("delta_f must spend fees the pots hold and the pot covers")
	}
	fees := uint64(-up.GetDeltaF())
	registered, err := registeredSet("registered", req.GetRegistered())
	if err != nil {
		return nil, err
	}
	var outputs []epochengine.RewardOutputAmount
	var credited []*v1.Credit
	byCredential := map[rewards.Credential]uint64{}
	for _, r := range up.GetRewards() {
		cred, err := rewardCredential("update.rewards", r.GetCredential())
		if err != nil {
			return nil, err
		}
		if r.GetAmount() == 0 {
			continue
		}
		_, reg := registered[cred]
		outputs = append(outputs, epochengine.RewardOutputAmount{Amount: r.GetAmount(), Spendable: reg})
		if reg {
			byCredential[cred] += r.GetAmount()
		}
	}
	reject := func(err error) (*v1.RewardsApplyUpdateResponse, error) {
		return &v1.RewardsApplyUpdateResponse{Rejection: dingoRejection("rewards", err)}, nil
	}
	totals, err := epochengine.ComputeRewardTotals(epochengine.RewardTotalsInput{
		TotalRewardPot: req.GetTotalRewardPot(), Fees: fees, TreasuryExpansion: tau, Outputs: outputs,
	})
	if err != nil {
		return reject(err)
	}
	reserves, treasury, err := epochengine.ComputeUpdatedPots(epochengine.UpdatedPotsInput{
		TotalRewardPot: req.GetTotalRewardPot(), Fees: fees,
		PotsReserves: pots.GetReserves(), BaseReserves: pots.GetReserves(), BaseTreasury: pots.GetTreasury(),
		Undistributed: totals.Undistributed, Unspendable: totals.Unspendable, TreasuryExpansion: tau,
	})
	if err != nil {
		return reject(err)
	}
	for cred, amount := range byCredential {
		credited = append(credited, &v1.Credit{Credential: protoCredential(cred.Tag, cred.Hash[:]), Amount: amount})
	}
	sort.Slice(credited, func(i, j int) bool {
		return credKeyRewardsStake(credited[i].GetCredential()) < credKeyRewardsStake(credited[j].GetCredential())
	})
	return &v1.RewardsApplyUpdateResponse{
		Pots: &v1.Pots{
			Reserves: reserves, Treasury: treasury, Fees: pots.GetFees() - fees,
			Donations: pots.GetDonations(), Deposits: pots.GetDeposits(),
		},
		Credited:               credited,
		UnregisteredToTreasury: totals.Unspendable,
	}, nil
}
