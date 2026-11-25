package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochSnapshotRotateCorpus(t *testing.T) {
	writeCorpus(t, "", epochSnapshotRotateCases())
}

func epochSnapshotRotateCases() []entry {
	const (
		mark = v1.SnapshotKind_SNAPSHOT_KIND_MARK
		set  = v1.SnapshotKind_SNAPSHOT_KIND_SET
		goK  = v1.SnapshotKind_SNAPSHOT_KIND_GO
	)
	snap := func(k v1.SnapshotKind, c *v1.Credential, amount uint64, pool byte) *v1.SnapshotEntry {
		return &v1.SnapshotEntry{Snapshot: k, Credential: c, Amount: amount, PoolId: rep(pool, 28)}
	}
	stake := func(c *v1.Credential, amount uint64) *v1.StakeEntry {
		return &v1.StakeEntry{Credential: c, Amount: amount}
	}
	acct := func(c *v1.Credential, balance uint64, pool byte) *v1.Account {
		a := &v1.Account{Credential: c, Balance: balance}
		if pool != 0 {
			a.PoolId = rep(pool, 28)
		}
		return a
	}
	pool := func(id byte) *v1.Pool {
		return &v1.Pool{PoolId: rep(id, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: key(0xee), Margin: rat("1", "100"), Cost: 340_000_000}
	}
	rot := func(name, why string, r *v1.EpochSnapshotRotateRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochSnapshotRotate{EpochSnapshotRotate: r}}}
	}
	base := func() *v1.EpochSnapshotRotateRequest {
		return &v1.EpochSnapshotRotateRequest{
			Snapshots:    []*v1.SnapshotEntry{snap(mark, key(1), 100, 0xa0), snap(set, key(2), 200, 0xa0), snap(goK, key(3), 300, 0xa0)},
			InstantStake: []*v1.StakeEntry{stake(key(4), 40), stake(key(4), 2), stake(key(5), 9), stake(key(6), 7)},
			Accounts:     []*v1.Account{acct(key(4), 5, 0xa0), acct(key(5), 0, 0)},
			Pools:        []*v1.Pool{pool(0xa0)},
			Fees:         77,
		}
	}
	mutate := func(f func(*v1.EpochSnapshotRotateRequest)) *v1.EpochSnapshotRotateRequest {
		r := base()
		f(r)
		return r
	}
	return []entry{
		rot("epoch_snapshot_rotate_basic", "mark is the registered, delegated stake plus rewards; set and go are the older marks; undelegated and unregistered stake is dropped", base()),
		rot("epoch_snapshot_rotate_empty", "an empty boundary rotates nothing", &v1.EpochSnapshotRotateRequest{}),
		rot("epoch_snapshot_rotate_fees_only", "the fee pot is kept apart from the stake snapshots", &v1.EpochSnapshotRotateRequest{Fees: 1_000_000}),
		rot("epoch_snapshot_rotate_rewards_only", "reward balance alone is active stake for a delegated account", &v1.EpochSnapshotRotateRequest{
			Accounts: []*v1.Account{acct(key(4), 5_000, 0xa0)}, Pools: []*v1.Pool{pool(0xa0)}}),
		rot("epoch_snapshot_rotate_two_pools_script", "stake splits across pools and script credentials", &v1.EpochSnapshotRotateRequest{
			Snapshots:    []*v1.SnapshotEntry{snap(mark, scriptEpoch(7), 50, 0xb1), snap(set, key(1), 10, 0xa0)},
			InstantStake: []*v1.StakeEntry{stake(scriptEpoch(7), 20), stake(key(8), 30)},
			Accounts:     []*v1.Account{acct(scriptEpoch(7), 1, 0xb1), acct(key(8), 0, 0xa0)},
			Pools:        []*v1.Pool{pool(0xa0), pool(0xb1)}}),
		rot("epoch_snapshot_rotate_prior_only", "with no live stake the new mark is empty and the old marks shift", mutate(func(r *v1.EpochSnapshotRotateRequest) {
			r.InstantStake, r.Accounts = nil, nil
		})),
		rot("epoch_snapshot_rotate_zero_instant_stake", "a zero-amount output adds no stake", mutate(func(r *v1.EpochSnapshotRotateRequest) {
			r.InstantStake = append(r.InstantStake, stake(key(9), 0))
			r.Accounts = append(r.Accounts, acct(key(9), 0, 0xa0))
		})),
		rot("epoch_snapshot_rotate_zero_snapshot_stake", "a snapshot row of zero stake is outside the reference's active stake", mutate(func(r *v1.EpochSnapshotRotateRequest) {
			r.Snapshots[0].Amount = 0
		})),
		rot("epoch_snapshot_rotate_stake_overflow", "a credential's stake above Word64 cannot be carried", mutate(func(r *v1.EpochSnapshotRotateRequest) {
			r.InstantStake = append(r.InstantStake, stake(key(4), ^uint64(0)))
		})),
	}
}

func TestGenerateEpochSnapshotRotateDivergenceCorpus(t *testing.T) {
	writeCorpus(t, "divergences", epochSnapshotRotateDivergences())
}

func epochSnapshotRotateDivergences() []entry {
	req := &v1.EpochSnapshotRotateRequest{
		InstantStake: []*v1.StakeEntry{{Credential: key(4), Amount: 42}},
		Accounts:     []*v1.Account{{Credential: key(4), PoolId: rep(0xa0, 28)}},
	}
	return []entry{{
		"epoch_snapshot_rotate_unregistered_pool",
		"stake delegated to a pool absent from the pool set: the reference counts it as active stake, dingo's capture drops it",
		&v1.Call{Request: &v1.Call_EpochSnapshotRotate{EpochSnapshotRotate: req}},
	}}
}
