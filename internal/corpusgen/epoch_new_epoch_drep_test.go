package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochDRepCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochDRepCases())
}

// epochNewEpochDRepCases are boundaries over accounts that delegate to a DRep.
// Neither node changes a delegation at a boundary with no governance state, so
// each case checks that the row reported for a changed account carries it.
func epochNewEpochDRepCases() []entry {
	const stake = 44_999_000_000_000
	abstain := &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	noConfidence := &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
	keyDRep := &v1.DRep{Target: &v1.DRep_Credential{Credential: key(0xd1)}}
	scriptDRep := &v1.DRep{Target: &v1.DRep_Credential{Credential: scriptEpoch(0xd2)}}
	acct := func(c *v1.Credential, balance uint64, pool byte, drep *v1.DRep) *v1.Account {
		a := &v1.Account{Credential: c, Balance: balance, Drep: drep}
		if pool != 0 {
			a.PoolId = rep(pool, 28)
		}
		return a
	}
	pool := func(retireAt *uint64) *v1.Pool {
		return &v1.Pool{
			PoolId: rep(0xa0, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: key(1),
			Margin: rat("1", "100"), Deposit: 500, RetiringEpoch: retireAt,
		}
	}
	newEpoch := func(name, why string, r *v1.EpochNewEpochRequest) entry {
		r.Context = &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11}
		r.Pparams = epochConwayPParams()
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: r}}}
	}
	retire := uint64(11)
	return []entry{
		newEpoch("epoch_new_epoch_drep_delegation", "delegators of a retiring pool are undelegated from it but keep their DRep, whether a predefined option, a key DRep or a script DRep, and the reported rows say so",
			&v1.EpochNewEpochRequest{State: &v1.LedgerState{
				Accounts: []*v1.Account{
					acct(key(1), 0, 0, nil), acct(key(2), 5, 0xa0, abstain), acct(key(3), 6, 0xa0, noConfidence),
					acct(key(4), 7, 0xa0, keyDRep), acct(scriptEpoch(5), 8, 0xa0, scriptDRep),
				},
				Pools: []*v1.Pool{pool(&retire)},
				Pots:  &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000},
			}}),
		newEpoch("epoch_new_epoch_drep_reward", "a delegator that also delegates to a DRep is credited its pending reward and keeps the DRep",
			&v1.EpochNewEpochRequest{
				Globals: &v1.Globals{
					EpochLength: 432_000, SecurityParam: 2_160, MaxLovelaceSupply: 45_000_000_000_000_000,
					ActiveSlotCoeff: rat("1", "20"),
				},
				State: &v1.LedgerState{
					Accounts: []*v1.Account{acct(key(1), 0, 0, nil), acct(key(2), 0, 0xa0, abstain)},
					Pools:    []*v1.Pool{pool(nil)},
					Pots:     &v1.Pots{Reserves: 1_000_000_000_000, Treasury: 1_000_000},
				},
				Snapshots:      []*v1.SnapshotEntry{{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: key(2), Amount: stake, PoolId: rep(0xa0, 28)}},
				BlocksPrevious: []*v1.BlocksMade{{PoolId: rep(0xa0, 28), Count: 21_600}},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					{Credential: key(1), PoolId: rep(0xa0, 28), Kind: v1.RewardKind_REWARD_KIND_LEADER, Amount: 18_461},
					{Credential: key(2), PoolId: rep(0xa0, 28), Kind: v1.RewardKind_REWARD_KIND_MEMBER, Amount: 1_827_691},
				}},
			}),
		newEpoch("epoch_new_epoch_pointer", "a registration pointer has no Conway account state: outside the answered domain",
			&v1.EpochNewEpochRequest{State: &v1.LedgerState{
				Accounts: []*v1.Account{{Credential: key(1), Pointer: &v1.Pointer{Slot: 1}}},
				Pots:     &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000},
			}}),
	}
}
