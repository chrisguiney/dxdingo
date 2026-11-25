package main

import (
	"bytes"
	"context"
	"errors"
	"math"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// epochNewEpoch runs dingo's production epoch rollover over a scratch ledger
// at the end of the epoch before req's, with the node's snapshot hooks wired.
//
// Domain, shared with dxnode-hs: Conway only; the request holds accounts,
// pools, pots, the snapshots in force and the instant stake. Governance
// state, a pending reward update, previous-epoch blocks and a UTxO table are
// UNSUPPORTED, since the rollover derives its own reward round and the request
// carries no state for them to read. The response reports only the pots both
// ledgers keep: reserves and treasury. Dingo keeps no fee, deposit or
// donation pot, so those deltas are left unset on both sides.
func epochNewEpoch(ctx context.Context, req *v1.EpochNewEpochRequest) (*v1.EpochNewEpochResponse, error) {
	if req.GetContext().GetEra() != v1.Era_ERA_CONWAY {
		return nil, unsupportedf("NEWEPOCH is answered for Conway only")
	}
	st := req.GetState()
	if req.GetRewardUpdate() != nil || len(req.GetBlocksPrevious()) > 0 || len(st.GetUtxo()) > 0 ||
		len(st.GetDreps()) > 0 || len(st.GetProposals()) > 0 || len(st.GetVotes()) > 0 ||
		len(st.GetPparamProposals()) > 0 || len(st.GetGenesisDelegations()) > 0 ||
		st.GetCommittee() != nil || st.GetEnactState() != nil {
		return nil, unsupportedf("reward update, blocks, UTxO and governance state are not read")
	}
	if req.GetContext().GetEpoch() < 5 {
		return nil, unsupportedf("epoch below 5 has no reward round to run")
	}
	pp, err := govFullPParams(req.GetPparams())
	if err != nil {
		return nil, err
	}
	in := dxoracle.NewEpochInput{
		Epoch:     req.GetContext().GetEpoch() - 1,
		PParams:   pp,
		Reserves:  st.GetPots().GetReserves(),
		Treasury:  st.GetPots().GetTreasury(),
		Donations: st.GetPots().GetDonations(),
	}
	if in.PriorMark, in.PriorSet, in.PriorGo, err = priorSnapshots(req.GetSnapshots()); err != nil {
		return nil, err
	}
	deposits := map[string]uint64{}
	seenAccounts := map[string]struct{}{}
	for _, a := range st.GetAccounts() {
		if _, err := rewardCredential("accounts", a.GetCredential()); err != nil {
			return nil, err
		}
		key := credKeyRewardsStake(a.GetCredential())
		if _, dup := seenAccounts[key]; dup {
			return nil, dispatch.BadRequest(errors.New("accounts: repeated credential"))
		}
		seenAccounts[key] = struct{}{}
		if len(a.GetPoolId()) != 0 {
			if _, err := rewardPoolID("accounts", a.GetPoolId()); err != nil {
				return nil, err
			}
		}
		if a.GetDrep() != nil || a.GetPointer() != nil {
			return nil, unsupportedf("accounts: DRep delegation and pointers are not read")
		}
		tag, _ := credentialTag(a.GetCredential())
		deposits[key] = a.GetDeposit()
		in.Accounts = append(in.Accounts, dxoracle.NewEpochAccount{
			Tag: tag, Key: a.GetCredential().GetHash(), Reward: a.GetBalance(), Pool: a.GetPoolId(),
		})
	}
	seenPools := map[string]struct{}{}
	for _, p := range st.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT {
			return nil, unsupportedf("pools: stage is not current")
		}
		if _, err := rewardPoolID("pools", p.GetPoolId()); err != nil {
			return nil, err
		}
		if _, dup := seenPools[string(p.GetPoolId())]; dup {
			return nil, dispatch.BadRequest(errors.New("pools: repeated pool"))
		}
		seenPools[string(p.GetPoolId())] = struct{}{}
		if _, err := rewardCredential("pools", p.GetRewardAccount()); err != nil {
			return nil, err
		}
		tag, _ := credentialTag(p.GetRewardAccount())
		pool := dxoracle.NewEpochPool{
			Key: p.GetPoolId(), Pledge: p.GetPledge(), Cost: p.GetCost(), Deposit: p.GetDeposit(),
			RewardTag: tag, RewardKey: p.GetRewardAccount().GetHash(),
		}
		if p.RetiringEpoch != nil {
			r := p.GetRetiringEpoch()
			pool.RetiringEpoch = &r
		}
		in.Pools = append(in.Pools, pool)
	}
	totals := map[string]uint64{}
	for _, e := range req.GetInstantStake() {
		if _, err := rewardCredential("instant_stake", e.GetCredential()); err != nil {
			return nil, err
		}
		key := credKeyRewardsStake(e.GetCredential())
		if totals[key] > math.MaxUint64-e.GetAmount() {
			return nil, unsupportedf("instant_stake: total exceeds Word64")
		}
		totals[key] += e.GetAmount()
		tag, _ := credentialTag(e.GetCredential())
		in.Stake = append(in.Stake, dxoracle.SnapshotStake{Tag: tag, Key: e.GetCredential().GetHash(), Amount: e.GetAmount()})
	}
	for _, a := range st.GetAccounts() {
		key := credKeyRewardsStake(a.GetCredential())
		if totals[key] > math.MaxUint64-a.GetBalance() {
			return nil, unsupportedf("accounts: stake of one credential exceeds Word64")
		}
	}
	out, err := dxoracle.NewEpoch(ctx, in)
	if err != nil {
		return &v1.EpochNewEpochResponse{Rejection: dingoRejection("NEWEPOCH", err)}, nil
	}
	delta := &v1.LedgerDelta{
		ReservesDelta: int64(out.Reserves) - int64(in.Reserves),
		TreasuryDelta: int64(out.Treasury) - int64(in.Treasury),
	}
	for i, a := range out.Accounts {
		before := in.Accounts[i]
		if a.Reward == before.Reward && bytes.Equal(a.Pool, before.Pool) {
			continue
		}
		cred := protoCredential(a.Tag, a.Key)
		delta.AccountsChanged = append(delta.AccountsChanged, &v1.Account{
			Credential: cred, Deposit: deposits[credKeyRewardsStake(cred)], Balance: a.Reward, PoolId: a.Pool,
		})
	}
	sort.Slice(delta.AccountsChanged, func(i, j int) bool {
		return credKeyRewardsStake(delta.AccountsChanged[i].GetCredential()) < credKeyRewardsStake(delta.AccountsChanged[j].GetCredential())
	})
	delta.PoolsRemoved = out.PoolsRemoved
	resp := &v1.EpochNewEpochResponse{Delta: delta}
	for _, part := range []struct {
		kind v1.SnapshotKind
		rows []dxoracle.SnapshotStake
	}{
		{v1.SnapshotKind_SNAPSHOT_KIND_MARK, out.Mark},
		{v1.SnapshotKind_SNAPSHOT_KIND_SET, out.Set},
		{v1.SnapshotKind_SNAPSHOT_KIND_GO, out.Go},
	} {
		for _, r := range part.rows {
			resp.Snapshots = append(resp.Snapshots, &v1.SnapshotEntry{
				Snapshot: part.kind, Credential: protoCredential(r.Tag, r.Key), Amount: r.Amount, PoolId: r.Pool,
			})
		}
	}
	return resp, nil
}
