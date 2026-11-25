package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochRewardsCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochRewardCases())
}

// epochNewEpochRewardCases are boundaries with a pending reward update. The
// update must equal what the reward calculation derives from the blocks, the
// GO snapshot, the pools and the pots, so each case's numbers are worked out
// from the Shelley reward formulas with rho = 3/1000, tau = 1/5, a0 = 3/10,
// d = 0, f = 1/20, 432000 slots per epoch (21600 expected blocks) and the
// 4.5e16 supply:
//
//	eta     = min(1, blocks / 21600)
//	deltaR1 = floor(eta * rho * reserves)
//	pot     = fees + deltaR1;   deltaT = floor(tau * pot);   R = pot - deltaT
//	maxPool = floor(R / (1 + a0) * sigma)   (pledge 0, sigma = stake / (supply - reserves) < 1/n_opt)
//	reward  = floor(maxPool * (blocks / total) / (stake / active))
//	leader  = cost + floor((reward - cost) * margin)
//	member  = floor((reward - cost) * (1 - margin) * stake / pool stake)
//	deltaR  = -deltaR1 + (R - rewards paid);   deltaF = -fees
func epochNewEpochRewardCases() []entry {
	const goK = v1.SnapshotKind_SNAPSHOT_KIND_GO
	const stake = 44_999_000_000_000
	globals := &v1.Globals{
		EpochLength: 432_000, SecurityParam: 2_160, MaxLovelaceSupply: 45_000_000_000_000_000,
		ActiveSlotCoeff: rat("1", "20"),
	}
	acct := func(c *v1.Credential, balance uint64, pool byte) *v1.Account {
		a := &v1.Account{Credential: c, Balance: balance}
		if pool != 0 {
			a.PoolId = rep(pool, 28)
		}
		return a
	}
	pool := func(id byte, reward *v1.Credential, margin string, cost uint64, retireAt *uint64) *v1.Pool {
		return &v1.Pool{
			PoolId: rep(id, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: reward,
			Margin: rat(margin, "100"), Cost: cost, Deposit: 500, RetiringEpoch: retireAt,
		}
	}
	row := func(c *v1.Credential, amount uint64, pool byte) *v1.SnapshotEntry {
		return &v1.SnapshotEntry{Snapshot: goK, Credential: c, Amount: amount, PoolId: rep(pool, 28)}
	}
	made := func(pool byte, n uint64) *v1.BlocksMade { return &v1.BlocksMade{PoolId: rep(pool, 28), Count: n} }
	paid := func(c *v1.Credential, pool byte, kind v1.RewardKind, amount uint64) *v1.Reward {
		return &v1.Reward{Credential: c, PoolId: rep(pool, 28), Kind: kind, Amount: amount}
	}
	const leader, member = v1.RewardKind_REWARD_KIND_LEADER, v1.RewardKind_REWARD_KIND_MEMBER
	newEpoch := func(name, why string, reserves, fees uint64, r *v1.EpochNewEpochRequest) entry {
		r.Context = &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11}
		r.Pparams = epochConwayPParams()
		r.Globals = globals
		r.State.Pots = &v1.Pots{Reserves: reserves, Treasury: 1_000_000, Fees: fees}
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: r}}}
	}
	at := func(e uint64) *uint64 { return &e }
	const u = 10_000_000_000_000
	return []entry{
		newEpoch("epoch_new_epoch_reward_application", "the pending update is applied before SNAP: leader and member are credited, the treasury takes tau of the pot and what is not paid out returns to reserves; the credited member reward is in the new mark",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 18_461), paid(key(2), 0xa0, member, 1_827_691),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_fees", "the fee pot joins the reward pot and is spent by the update (delta_f); half the expected blocks halve eta",
			1_000_000_000_000, 1_000_000_000,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 10_800)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: 498_461_540, DeltaT: 500_000_000, DeltaF: -1_000_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 15_384), paid(key(2), 0xa0, member, 1_523_076),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_two_pools", "two pools with different block counts, margins and costs: performance is each pool's share of the blocks over its share of the active stake, and a pool's delegators split the member rewards by stake",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State: &v1.LedgerState{
					Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0), acct(key(3), 0, 0xa0), acct(key(4), 0, 0xb1), acct(key(5), 0, 0)},
					Pools:    []*v1.Pool{pool(0xa0, key(1), "1", 1000, nil), pool(0xb1, key(5), "10", 500, nil)},
				},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), 3*u, 0xa0), row(key(3), u, 0xa0), row(key(4), 2*u, 0xb1)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 12_960), made(0xb1, 8_640)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -602_461_589, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 15_759), paid(key(2), 0xa0, member, 1_095_896), paid(key(3), 0xa0, member, 365_298),
					paid(key(4), 0xb1, member, 885_723), paid(key(5), 0xb1, leader, 98_913),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_unregistered_member", "a delegator in the GO snapshot whose account is no longer registered is not credited: its reward goes to the treasury, and the update still lists it",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake/2, 0xa0), row(key(3), stake/2, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_151, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 18_461), paid(key(2), 0xa0, member, 913_845), paid(key(3), 0xa0, member, 913_845),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_unregistered_leader", "a pool whose reward account is not registered earns its leader reward for the treasury",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(9), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(9), 0xa0, leader, 18_461), paid(key(2), 0xa0, member, 1_827_691),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_no_blocks", "an epoch without blocks has eta 0: the round pays no rewards, the fee pot alone is split between the treasury and reserves",
			1_000_000_000_000, 2_000_000_000,
			&v1.EpochNewEpochRequest{
				State:        &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:    []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: 1_600_000_000, DeltaT: 400_000_000, DeltaF: -2_000_000_000},
			}),
		newEpoch("epoch_new_epoch_reward_retiring_pool", "a pool retiring at the boundary still earns its pending round: the reward is credited before POOLREAP, which then refunds the deposit on top and undelegates the member",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, at(11))}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 18_461), paid(key(2), 0xa0, member, 1_827_691),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_on_balance", "the reward adds to an existing balance, and the new mark is the live UTxO stake plus the credited balance",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 1_000, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				InstantStake:   []*v1.StakeEntry{{Credential: key(2), Amount: 5_000_000}},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 18_461), paid(key(2), 0xa0, member, 1_827_691),
				}},
			}),
		newEpoch("epoch_new_epoch_reward_blocks_without_update", "blocks_previous without a pending update feed no round, so the boundary changes nothing",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
			}),
		newEpoch("epoch_new_epoch_reward_update_mismatch", "an update the inputs do not produce is outside the domain: dingo recomputes the round instead of taking one",
			1_000_000_000_000, 0,
			&v1.EpochNewEpochRequest{
				State:          &v1.LedgerState{Accounts: []*v1.Account{acct(key(1), 0, 0), acct(key(2), 0, 0xa0)}, Pools: []*v1.Pool{pool(0xa0, key(1), "1", 0, nil)}},
				Snapshots:      []*v1.SnapshotEntry{row(key(2), stake, 0xa0)},
				BlocksPrevious: []*v1.BlocksMade{made(0xa0, 21_600)},
				RewardUpdate: &v1.RewardUpdate{DeltaR: -601_846_152, DeltaT: 600_000_000, Rewards: []*v1.Reward{
					paid(key(1), 0xa0, leader, 18_462), paid(key(2), 0xa0, member, 1_827_691),
				}},
			}),
	}
}
