package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/ledger/snapshot"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

const vrfHashSize = 32

// delegatedRow is a stake row with a pool, decoded and checked.
type delegatedRow struct {
	tag    uint8
	key    []byte
	pool   []byte
	amount uint64
}

// delegatedRows decodes stake rows the way dxnode-hs does: a row without a
// pool is not delegated and is dropped; a repeated credential is BAD_REQUEST;
// a zero amount, or a total above Word64, is UNSUPPORTED.
func delegatedRows(name string, rows []*v1.SnapshotEntry) ([]delegatedRow, uint64, error) {
	seen := map[string]struct{}{}
	var out []delegatedRow
	var total uint64
	for _, r := range rows {
		if len(r.GetPoolId()) == 0 {
			continue
		}
		cred, err := rewardCredential(name, r.GetCredential())
		if err != nil {
			return nil, 0, err
		}
		if _, err := rewardPoolID(name, r.GetPoolId()); err != nil {
			return nil, 0, err
		}
		if _, dup := seen[cred.Key()]; dup {
			return nil, 0, dispatch.BadRequest(fmt.Errorf("%s: repeated credential", name))
		}
		seen[cred.Key()] = struct{}{}
		if r.GetAmount() == 0 {
			return nil, 0, unsupportedf("%s: zero stake", name)
		}
		if total > math.MaxUint64-r.GetAmount() {
			return nil, 0, unsupportedf("%s: stake total exceeds Word64", name)
		}
		total += r.GetAmount()
		out = append(out, delegatedRow{cred.Tag, cred.Hash[:], r.GetPoolId(), r.GetAmount()})
	}
	return out, total, nil
}

// currentPools checks the pools in effect (a re-registration waiting for
// the next epoch is not part of a snapshot) and returns them by id with the
// VRF hash each reports. A repeated pool is BAD_REQUEST.
func currentPools(pools []*v1.Pool) (map[string][]byte, error) {
	out := map[string][]byte{}
	for _, p := range pools {
		if p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE {
			continue
		}
		id, err := rewardPoolID("pools", p.GetPoolId())
		if err != nil {
			return nil, err
		}
		vrf := p.GetVrfKeyHash()
		switch len(vrf) {
		case 0:
			vrf = make([]byte, vrfHashSize)
		case vrfHashSize:
		default:
			return nil, dispatch.BadRequest(errors.New("vrf_key_hash: hash is not 32 bytes"))
		}
		if _, err := poolMargin(p); err != nil {
			return nil, err
		}
		if _, err := rewardCredential("reward_account", p.GetRewardAccount()); err != nil {
			return nil, err
		}
		if _, err := poolOwners(p.GetOwners()); err != nil {
			return nil, err
		}
		if _, dup := out[string(id[:])]; dup {
			return nil, dispatch.BadRequest(errors.New("pools: repeated pool"))
		}
		out[string(id[:])] = vrf
	}
	return out, nil
}

// poolDistribution maps to snapshot.AggregateRewardStakeInputs, which sums
// the delegated rows per pool. The pools in effect and the share
// (stake / total active stake) are applied here: dingo keeps the raw stake
// and forms sigma where it uses it.
func poolDistribution(_ context.Context, req *v1.StakePoolDistributionRequest) (*v1.StakePoolDistributionResponse, error) {
	rows, total, err := delegatedRows("snapshot", req.GetSnapshot())
	if err != nil {
		return nil, err
	}
	if len(rows) == 0 {
		return nil, unsupportedf("snapshot: no delegated stake")
	}
	pools, err := currentPools(req.GetPools())
	if err != nil {
		return nil, err
	}
	dist := &snapshot.StakeDistribution{TotalActiveStake: total}
	for _, r := range rows {
		dist.StakeInputs = append(dist.StakeInputs, snapshot.StakeInput{
			PoolKeyHash: r.pool, CredentialTag: r.tag, StakingKey: r.key, Stake: r.amount, Registered: true,
		})
	}
	agg, err := snapshot.AggregateRewardStakeInputs(dist)
	if err != nil {
		return &v1.StakePoolDistributionResponse{Rejection: dingoRejection("pool-distribution", err)}, nil
	}
	resp := &v1.StakePoolDistributionResponse{TotalActive: agg.TotalActiveStake}
	for id, stake := range agg.PoolStakes {
		vrf, ok := pools[string(id[:])]
		if !ok || agg.DelegatorCount[lcommon.PoolKeyHash(id)] == 0 {
			continue
		}
		resp.Pools = append(resp.Pools, &v1.PoolStake{
			PoolId: bytes.Clone(id[:]), Stake: stake,
			Share:      convert.RatProto(new(big.Rat).SetFrac(new(big.Int).SetUint64(stake), new(big.Int).SetUint64(agg.TotalActiveStake))),
			VrfKeyHash: bytes.Clone(vrf),
		})
	}
	sort.Slice(resp.Pools, func(i, j int) bool { return bytes.Compare(resp.Pools[i].GetPoolId(), resp.Pools[j].GetPoolId()) < 0 })
	return resp, nil
}
