package main

import (
	"context"
	"errors"
	"fmt"
	"math"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// rotateEpoch is the epoch the scratch boundary enters. Nothing in the
// request depends on it.
const rotateEpoch = 10

// epochSnapshotRotate runs dingo's SNAP capture over a scratch database.
// Dingo keeps no copy of the Set and Go snapshots: each is the Mark stored
// under an earlier epoch. The request's snapshots are therefore seeded as the
// Marks of the three epochs before the boundary, and the response reads the
// new Mark and the two older Marks the production rotation maps name.
//
// Domain, shared with dxnode-hs: a snapshot row is delegated, non-zero stake
// (the reference's ActiveStake holds nothing else), no credential repeats
// within a kind or among accounts, and no per-credential stake exceeds Word64.
func epochSnapshotRotate(ctx context.Context, req *v1.EpochSnapshotRotateRequest) (*v1.EpochSnapshotRotateResponse, error) {
	if _, err := currentPools(req.GetPools()); err != nil {
		return nil, err
	}
	in := dxoracle.SnapshotRotateInput{Epoch: rotateEpoch, Fees: req.GetFees()}
	var err error
	if in.PriorMark, in.PriorSet, in.PriorGo, err = priorSnapshots(req.GetSnapshots()); err != nil {
		return nil, err
	}
	totals := map[string]uint64{}
	add := func(what string, c *v1.Credential, amount uint64) error {
		key := credKeyRewardsStake(c)
		if totals[key] > math.MaxUint64-amount {
			return unsupportedf("%s: stake of one credential exceeds Word64", what)
		}
		totals[key] += amount
		return nil
	}
	for _, e := range req.GetInstantStake() {
		if _, err := rewardCredential("instant_stake", e.GetCredential()); err != nil {
			return nil, err
		}
		if err := add("instant_stake", e.GetCredential(), e.GetAmount()); err != nil {
			return nil, err
		}
		tag, _ := credentialTag(e.GetCredential())
		in.Stake = append(in.Stake, dxoracle.SnapshotStake{Tag: tag, Key: e.GetCredential().GetHash(), Amount: e.GetAmount()})
	}
	seenAccounts := map[string]struct{}{}
	for _, a := range req.GetAccounts() {
		if _, err := rewardCredential("accounts", a.GetCredential()); err != nil {
			return nil, err
		}
		if _, dup := seenAccounts[credKeyRewardsStake(a.GetCredential())]; dup {
			return nil, dispatch.BadRequest(errors.New("accounts: repeated credential"))
		}
		seenAccounts[credKeyRewardsStake(a.GetCredential())] = struct{}{}
		if len(a.GetPoolId()) != 0 {
			if _, err := rewardPoolID("accounts", a.GetPoolId()); err != nil {
				return nil, err
			}
		}
		if err := add("accounts", a.GetCredential(), a.GetBalance()); err != nil {
			return nil, err
		}
		tag, _ := credentialTag(a.GetCredential())
		in.Accounts = append(in.Accounts, dxoracle.LiveStakeAccount{
			Tag: tag, Key: a.GetCredential().GetHash(), Reward: a.GetBalance(), Active: true, Pool: a.GetPoolId(),
		})
	}
	for _, p := range req.GetPools() {
		if p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE {
			continue
		}
		in.Pools = append(in.Pools, dxoracle.SnapshotPool{Key: p.GetPoolId(), Pledge: p.GetPledge(), Cost: p.GetCost()})
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	out, err := oracle.SnapshotRotate(ctx, in)
	if err != nil {
		return &v1.EpochSnapshotRotateResponse{Rejection: dingoRejection("SNAP", err)}, nil
	}
	resp := &v1.EpochSnapshotRotateResponse{FeeSnapshot: out.FeeSnapshot}
	for _, part := range []struct {
		kind v1.SnapshotKind
		rows []dxoracle.SnapshotStake
	}{
		{v1.SnapshotKind_SNAPSHOT_KIND_MARK, out.Mark},
		{v1.SnapshotKind_SNAPSHOT_KIND_SET, out.Set},
		{v1.SnapshotKind_SNAPSHOT_KIND_GO, out.Go},
	} {
		start := len(resp.Snapshots)
		for _, r := range part.rows {
			resp.Snapshots = append(resp.Snapshots, &v1.SnapshotEntry{
				Snapshot: part.kind, Credential: protoCredential(r.Tag, r.Key), Amount: r.Amount, PoolId: r.Pool,
			})
		}
		added := resp.Snapshots[start:]
		sort.Slice(added, func(i, j int) bool {
			return credKeyRewardsStake(added[i].GetCredential()) < credKeyRewardsStake(added[j].GetCredential())
		})
	}
	return resp, nil
}

// priorSnapshots splits the snapshots in force before a boundary by kind and
// applies the domain: each row is delegated, non-zero stake and no credential
// repeats within a kind.
func priorSnapshots(entries []*v1.SnapshotEntry) (mark, set, goSnap []dxoracle.SnapshotStake, err error) {
	prior := map[v1.SnapshotKind]*[]dxoracle.SnapshotStake{
		v1.SnapshotKind_SNAPSHOT_KIND_MARK: &mark,
		v1.SnapshotKind_SNAPSHOT_KIND_SET:  &set,
		v1.SnapshotKind_SNAPSHOT_KIND_GO:   &goSnap,
	}
	seenRows := map[string]struct{}{}
	for _, e := range entries {
		dst, ok := prior[e.GetSnapshot()]
		if !ok {
			return nil, nil, nil, dispatch.BadRequest(errors.New("snapshots: kind is not mark, set or go"))
		}
		if _, err := rewardCredential("snapshots", e.GetCredential()); err != nil {
			return nil, nil, nil, err
		}
		if _, err := rewardPoolID("snapshots", e.GetPoolId()); len(e.GetPoolId()) != 0 && err != nil {
			return nil, nil, nil, err
		}
		key := fmt.Sprintf("%d/%s", e.GetSnapshot(), credKeyRewardsStake(e.GetCredential()))
		if _, dup := seenRows[key]; dup {
			return nil, nil, nil, dispatch.BadRequest(errors.New("snapshots: repeated credential"))
		}
		seenRows[key] = struct{}{}
		if len(e.GetPoolId()) == 0 {
			return nil, nil, nil, unsupportedf("snapshots: a snapshot row must be delegated")
		}
		if e.GetAmount() == 0 {
			return nil, nil, nil, unsupportedf("snapshots: a snapshot row must hold stake")
		}
		tag, _ := credentialTag(e.GetCredential())
		*dst = append(*dst, dxoracle.SnapshotStake{Tag: tag, Key: e.GetCredential().GetHash(), Amount: e.GetAmount(), Pool: e.GetPoolId()})
	}
	return mark, set, goSnap, nil
}
