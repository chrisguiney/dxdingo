package corpusgen

import (
	"testing"

	"github.com/chrisguiney/dxdingo/internal/testgenesis"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochCases())
}

// epochConwayPParams are Babbage's parameters with the Conway genesis values
// applied, at protocol major 10.
func epochConwayPParams() *v1.ProtocolParameters {
	pp := epochBabbagePParams()
	g := epochConwayGenesis()
	pp.CostModelPlutusV1 = testgenesis.Alonzo().CostModels["PlutusV1"]
	pp.CostModelPlutusV3 = g.CostModelPlutusV3
	pp.PoolVotingThresholds, pp.DrepVotingThresholds = g.PoolVotingThresholds, g.DrepVotingThresholds
	cms, ctl, gal, gad, drd, dra := g.CommitteeMinSize, g.CommitteeMaxTermLength, g.GovActionLifetime, g.GovActionDeposit, g.DrepDeposit, g.DrepActivity
	pp.CommitteeMinSize, pp.CommitteeMaxTermLength, pp.GovActionLifetime = &cms, &ctl, &gal
	pp.GovActionDeposit, pp.DrepDeposit, pp.DrepActivity = &gad, &drd, &dra
	pp.MinFeeRefScriptCostPerByte = g.MinFeeRefScriptCostPerByte
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: 10}
	return pp
}

func epochNewEpochCases() []entry {
	const (
		mark = v1.SnapshotKind_SNAPSHOT_KIND_MARK
		set  = v1.SnapshotKind_SNAPSHOT_KIND_SET
		goK  = v1.SnapshotKind_SNAPSHOT_KIND_GO
	)
	snap := func(k v1.SnapshotKind, c *v1.Credential, amount uint64, pool byte) *v1.SnapshotEntry {
		return &v1.SnapshotEntry{Snapshot: k, Credential: c, Amount: amount, PoolId: rep(pool, 28)}
	}
	acct := func(c *v1.Credential, balance uint64, pool byte) *v1.Account {
		a := &v1.Account{Credential: c, Balance: balance}
		if pool != 0 {
			a.PoolId = rep(pool, 28)
		}
		return a
	}
	pool := func(id byte, reward *v1.Credential, deposit uint64, retireAt *uint64) *v1.Pool {
		return &v1.Pool{
			PoolId: rep(id, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: reward, Deposit: deposit,
			Margin: rat("1", "100"), Cost: 340_000_000, RetiringEpoch: retireAt,
		}
	}
	at := func(e uint64) *uint64 { return &e }
	stake := func(c *v1.Credential, amount uint64) *v1.StakeEntry {
		return &v1.StakeEntry{Credential: c, Amount: amount}
	}
	newEpoch := func(name, why string, r *v1.EpochNewEpochRequest) entry {
		r.Context = &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11}
		r.Pparams = epochConwayPParams()
		if r.State == nil {
			r.State = &v1.LedgerState{}
		}
		if r.State.Pots == nil {
			r.State.Pots = &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000}
		}
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: r}}}
	}
	state := func(accts []*v1.Account, pools []*v1.Pool) *v1.LedgerState {
		return &v1.LedgerState{Accounts: accts, Pools: pools}
	}
	return []entry{
		newEpoch("epoch_new_epoch_pool_retirement", "a pool retiring at the boundary is removed, its deposit refunds its registered reward account and its delegator is undelegated; the mark is taken before POOLREAP and the old mark becomes the set",
			&v1.EpochNewEpochRequest{
				State:        state([]*v1.Account{acct(key(1), 0, 0), acct(key(2), 5, 0xa0)}, []*v1.Pool{pool(0xa0, key(1), 500, at(11))}),
				Snapshots:    []*v1.SnapshotEntry{snap(mark, key(3), 100, 0xa0)},
				InstantStake: []*v1.StakeEntry{stake(key(2), 40)},
			}),
		newEpoch("epoch_new_epoch_empty", "a boundary over an empty ledger changes nothing", &v1.EpochNewEpochRequest{}),
		newEpoch("epoch_new_epoch_donations", "the donations of the ended epoch move into the treasury",
			&v1.EpochNewEpochRequest{State: &v1.LedgerState{Pots: &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000, Donations: 5_000}}}),
		newEpoch("epoch_new_epoch_unclaimed_deposit", "a retiring pool whose reward account is not registered returns its deposit to the treasury",
			&v1.EpochNewEpochRequest{State: state([]*v1.Account{acct(key(2), 0, 0xa0)}, []*v1.Pool{pool(0xa0, key(9), 500, at(11))}),
				InstantStake: []*v1.StakeEntry{stake(key(2), 7)}}),
		newEpoch("epoch_new_epoch_two_pools", "only the pool whose retirement epoch is the new epoch is reaped; both pools' delegators are in the mark",
			&v1.EpochNewEpochRequest{
				State: state([]*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0), acct(key(3), 4, 0xb1)},
					[]*v1.Pool{pool(0xa0, key(1), 500, at(11)), pool(0xb1, key(1), 500, nil)}),
				InstantStake: []*v1.StakeEntry{stake(key(2), 30), stake(key(3), 60)},
			}),
		newEpoch("epoch_new_epoch_future_retirement", "a retirement scheduled for a later epoch leaves the pool and its delegation in place",
			&v1.EpochNewEpochRequest{State: state([]*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, []*v1.Pool{pool(0xa0, key(1), 500, at(12))}),
				InstantStake: []*v1.StakeEntry{stake(key(2), 30)}}),
		newEpoch("epoch_new_epoch_rotation", "mark, set and go rotate: the live stake is the new mark, the old mark the set, the old set the go, and the old go is dropped",
			&v1.EpochNewEpochRequest{
				State:        state([]*v1.Account{acct(key(4), 2, 0xa0)}, []*v1.Pool{pool(0xa0, key(4), 500, nil)}),
				Snapshots:    []*v1.SnapshotEntry{snap(mark, key(1), 100, 0xa0), snap(set, key(2), 200, 0xa0), snap(goK, key(3), 300, 0xa0)},
				InstantStake: []*v1.StakeEntry{stake(key(4), 50)},
			}),
		newEpoch("epoch_new_epoch_script_reward_account", "a script-credential reward account is refunded like a key one",
			&v1.EpochNewEpochRequest{State: state([]*v1.Account{acct(scriptEpoch(5), 0, 0)}, []*v1.Pool{pool(0xa0, scriptEpoch(5), 500, at(11))})}),
		newEpoch("epoch_new_epoch_reward_update", "a pending reward update is outside the answered domain",
			&v1.EpochNewEpochRequest{RewardUpdate: &v1.RewardUpdate{}}),
	}
}
