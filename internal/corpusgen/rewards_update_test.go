package corpusgen

import (
	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// rewardsUpdateCases are the reward update cases. Small ones are hand-checked:
// the numbers are derived in cmd/dxnode-go/rewards_update_test.go.
func rewardsUpdateCases() []entry {
	var out []entry
	out = append(out, rwMonetaryCases()...)
	out = append(out, rwPoolRewardCases()...)
	out = append(out, rwRewardUpdateCases()...)
	out = append(out, rwApplyUpdateCases()...)
	out = append(out, rwNonMyopicCases()...)
	return out
}

func rwKey(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: rep(b, 28)}
}

func rwScript(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: rep(b, 28)}
}

func rwPV(major uint32) *v1.ProtocolVersion { return &v1.ProtocolVersion{Major: major} }

func rwU32(n uint32) *uint32 { return &n }

func rwPParams(d *v1.Rational) *v1.ProtocolParameters {
	return &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: rwU32(500), Rho: rat("3", "1000"), Tau: rat("1", "5"), D: d}
}

func rwGlobals(maxSupply uint64) *v1.Globals {
	return &v1.Globals{
		EpochLength: 432000, ActiveSlotCoeff: rat("1", "20"), SecurityParam: 2160,
		MaxLovelaceSupply: maxSupply, SlotLength: rat("1", "1"),
	}
}

func rwMonetaryCases() []entry {
	mk := func(name, why string, major uint32, d *v1.Rational, epoch, blocks uint64) entry {
		g := rwGlobals(45_000_000_000_000_000)
		g.EpochLength = epoch
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsMonetaryExpansion{RewardsMonetaryExpansion: &v1.RewardsMonetaryExpansionRequest{
			ProtocolVersion: rwPV(major), Pparams: rwPParams(d), Globals: g,
			Reserves: 1_000_000_000_000, Fees: 2_000_000, BlocksTotal: blocks,
		}}}}
	}
	return []entry{
		mk("rewards_monetary_expansion_half_efficiency", "eta 1/2: deltaR1 1.5e9, pot 1.502e9, cut 300400000", 8, rat("0", "1"), 432000, 10800),
		mk("rewards_monetary_expansion_efficiency_capped", "eta above 1 is capped: deltaR1 3e9", 8, rat("0", "1"), 432000, 30000),
		mk("rewards_monetary_expansion_decentralized_shelley", "d = 1/4 below 4/5: expected blocks 16200, eta 1/2", 5, rat("1", "4"), 432000, 8100),
		mk("rewards_monetary_expansion_d_at_least_four_fifths", "d >= 4/5: eta is 1 whatever the blocks", 3, rat("9", "10"), 432000, 0),
		mk("rewards_monetary_expansion_d_exactly_four_fifths", "boundary: d = 4/5 takes the eta = 1 branch", 3, rat("4", "5"), 432000, 0),
		mk("rewards_monetary_expansion_no_blocks", "no blocks at d = 0: eta 0, only fees in the pot", 8, rat("0", "1"), 432000, 0),
		mk("rewards_monetary_expansion_zero_expected_blocks", "floor((1-d) f L) is 0 at d < 4/5: UNSUPPORTED on both nodes", 8, rat("0", "1"), 10, 1),
	}
}

func rwPool(id byte, pledge, cost uint64, margin *v1.Rational, account *v1.Credential, owners ...*v1.Credential) *v1.Pool {
	p := &v1.Pool{PoolId: rep(id, 28), Pledge: pledge, Cost: cost, Margin: margin, RewardAccount: account}
	for _, o := range owners {
		p.Owners = append(p.Owners, o.GetHash())
	}
	return p
}

func rwMember(c *v1.Credential, amount uint64) *v1.StakeEntry {
	return &v1.StakeEntry{Credential: c, Amount: amount}
}

func rwPoolRewardReq(major uint32, d *v1.Rational) *v1.RewardsPoolRewardRequest {
	return &v1.RewardsPoolRewardRequest{
		ProtocolVersion: rwPV(major),
		Pparams:         &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: rwU32(500), D: d},
		PoolPot:         1_000_000_000, TotalStake: 1000, TotalActiveStake: 500, Blocks: 5, BlocksTotal: 10,
		Pool: rwPool(0xa1, 10, 1_000_000, rat("1", "10"), rwKey(0x01), rwKey(0x02)),
		Members: []*v1.StakeEntry{
			rwMember(rwKey(0x02), 20), rwMember(rwKey(0x03), 50), rwMember(rwKey(0x04), 30),
		},
		Registered: []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03), rwKey(0x04)},
	}
}

func rwPoolRewardEntry(name, why string, req *v1.RewardsPoolRewardRequest) entry {
	return entry{name, why, &v1.Call{Request: &v1.Call_RewardsPoolReward{RewardsPoolReward: req}}}
}

func rwPoolRewardCases() []entry {
	prefilter := rwPoolRewardReq(6, rat("0", "1"))
	prefilter.Registered = []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03)}
	babbageUnregistered := rwPoolRewardReq(7, rat("0", "1"))
	babbageUnregistered.Registered = []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03)}
	pledge := rwPoolRewardReq(8, rat("0", "1"))
	pledge.Pool.Pledge = 21
	pledgeEdge := rwPoolRewardReq(8, rat("0", "1"))
	pledgeEdge.Pool.Pledge = 20
	noBlocks := rwPoolRewardReq(8, rat("0", "1"))
	noBlocks.Blocks = 0
	shelley := rwPoolRewardReq(5, rat("1", "4"))
	federated := rwPoolRewardReq(5, rat("9", "10"))
	scriptMember := rwPoolRewardReq(8, rat("0", "1"))
	scriptMember.Members[2].Credential = rwScript(0x04)
	scriptMember.Registered[3] = rwScript(0x04)
	ownerNotDelegating := rwPoolRewardReq(8, rat("0", "1"))
	ownerNotDelegating.Pool.Owners = append(ownerNotDelegating.Pool.Owners, rep(0x09, 28))
	unaccounted := rwPoolRewardReq(6, rat("0", "1"))
	unaccounted.Registered = []*v1.Credential{rwKey(0x02), rwKey(0x03), rwKey(0x04)}
	belowCost := rwPoolRewardReq(8, rat("0", "1"))
	belowCost.Pool.Cost = 6_000_000
	zeroTotal := rwPoolRewardReq(8, rat("0", "1"))
	zeroTotal.TotalActiveStake = 0
	allegra := rwPoolRewardReq(3, rat("0", "1"))
	return []entry{
		rwPoolRewardEntry("rewards_pool_reward_babbage", "pool 5000000, leader 2120000, members 1800000 and 1080000, performance 5/2", rwPoolRewardReq(8, rat("0", "1"))),
		rwPoolRewardEntry("rewards_pool_reward_alonzo_prefilter", "major 6 drops the unregistered member before the reward is formed", prefilter),
		rwPoolRewardEntry("rewards_pool_reward_babbage_keeps_unregistered", "major 7 forgoes the prefilter: the unregistered member keeps its reward", babbageUnregistered),
		rwPoolRewardEntry("rewards_pool_reward_leader_account_unregistered_alonzo", "major 6: an unregistered reward account loses the leader reward", unaccounted),
		rwPoolRewardEntry("rewards_pool_reward_pledge_not_met", "pledge 21 above owner stake 20: the pool earns nothing", pledge),
		rwPoolRewardEntry("rewards_pool_reward_pledge_exactly_met", "boundary: pledge equals the owners' delegated stake", pledgeEdge),
		rwPoolRewardEntry("rewards_pool_reward_no_blocks", "a pool with no blocks gets no reward", noBlocks),
		rwPoolRewardEntry("rewards_pool_reward_shelley_decentralized", "d = 1/4: performance uses the block share", shelley),
		rwPoolRewardEntry("rewards_pool_reward_federated", "d >= 4/5: performance is 1", federated),
		rwPoolRewardEntry("rewards_pool_reward_script_member", "a script-credential member is paid like a key member", scriptMember),
		rwPoolRewardEntry("rewards_pool_reward_owner_not_delegating", "an owner that does not delegate to the pool is not a self-delegated owner", ownerNotDelegating),
		rwPoolRewardEntry("rewards_pool_reward_pot_below_cost", "poolR below cost: the operator takes it all and members get nothing", belowCost),
		rwPoolRewardEntry("rewards_pool_reward_allegra", "major 3: the first major with aggregated rewards", allegra),
		rwPoolRewardEntry("rewards_pool_reward_zero_active_stake", "zero total active stake: UNSUPPORTED on both nodes", zeroTotal),
	}
}

// rwRewardUpdateReq is the two-pool update of the Go test: pool A made every
// block of the epoch, pool B none.
func rwRewardUpdateReq(major uint32) *v1.RewardsRewardUpdateRequest {
	snap := func(c *v1.Credential, amount uint64, pool []byte) *v1.SnapshotEntry {
		return &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: c, Amount: amount, PoolId: pool}
	}
	return &v1.RewardsRewardUpdateRequest{
		ProtocolVersion: rwPV(major), Pparams: rwPParams(rat("0", "1")), Globals: rwGlobals(400_000_001_000),
		Pots: &v1.Pots{Reserves: 400_000_000_000, Treasury: 7, Fees: 50_000_000},
		Snapshots: []*v1.SnapshotEntry{
			snap(rwKey(0x02), 20, rep(0xa1, 28)), snap(rwKey(0x03), 50, rep(0xa1, 28)), snap(rwKey(0x04), 30, rep(0xa1, 28)),
			snap(rwKey(0x06), 400, rep(0xb2, 28)),
		},
		Pools: []*v1.Pool{
			rwPool(0xa1, 10, 1_000_000, rat("1", "10"), rwKey(0x01), rwKey(0x02)),
			rwPool(0xb2, 0, 340_000_000, rat("1", "20"), rwKey(0x05), rwKey(0x06)),
		},
		Blocks:     []*v1.BlocksMade{{PoolId: rep(0xa1, 28), Count: 21600}},
		Registered: []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03), rwKey(0x04)},
	}
}

func rwRewardUpdateEntry(name, why string, req *v1.RewardsRewardUpdateRequest) entry {
	return entry{name, why, &v1.Call{Request: &v1.Call_RewardsRewardUpdate{RewardsRewardUpdate: req}}}
}

// rwSharedCredential makes credential 0x07 the reward account of pool A and a
// member of pool B, both of which made blocks: below major 3 only one of its
// two rewards is paid.
func rwSharedCredential(major uint32) *v1.RewardsRewardUpdateRequest {
	req := rwRewardUpdateReq(major)
	req.Pools[0].RewardAccount = rwKey(0x07)
	req.Snapshots[3] = &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: rwKey(0x07), Amount: 400, PoolId: rep(0xb2, 28)}
	req.Pools[1].Owners = nil
	req.Pools[1].Cost = 1_000_000
	req.Blocks = []*v1.BlocksMade{{PoolId: rep(0xa1, 28), Count: 10800}, {PoolId: rep(0xb2, 28), Count: 10800}}
	req.Registered = append(req.Registered, rwKey(0x07))
	return req
}

// rwManyPools is a larger update: eight pools of different size, margin,
// cost and pledge, forty delegators, some unregistered, some script
// credentials, and block counts that give a spread of performances.
func rwManyPools(major uint32) *v1.RewardsRewardUpdateRequest {
	req := &v1.RewardsRewardUpdateRequest{
		ProtocolVersion: rwPV(major), Pparams: rwPParams(rat("0", "1")),
		Globals: rwGlobals(45_000_000_000_000_000),
		Pots:    &v1.Pots{Reserves: 13_000_000_000_000_000, Treasury: 1_000_000_000_000, Fees: 3_500_000_000},
	}
	for p := 0; p < 8; p++ {
		id := byte(0x10 + p)
		owner := rwKey(byte(0x40 + p))
		var stake uint64
		for m := 0; m < 5; m++ {
			var c *v1.Credential
			switch {
			case m == 0:
				c = owner
			case m == 4 && p%2 == 1:
				c = rwScript(byte(0x80 + p*8 + m))
			default:
				c = rwKey(byte(0x80 + p*8 + m))
			}
			amount := uint64(1_000_000_000_000 + p*137_000_000_000 + m*53_000_000_000)
			stake += amount
			req.Snapshots = append(req.Snapshots, &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: c, Amount: amount, PoolId: rep(id, 28)})
			if (p+m)%7 != 3 {
				req.Registered = append(req.Registered, c)
			}
		}
		margin := rat(string(rune('0'+p%5)), "20")
		pool := rwPool(id, uint64(500_000_000_000+p*10_000_000_000), uint64(340_000_000+p*20_000_000), margin, rwKey(byte(0x60+p)), owner)
		req.Pools = append(req.Pools, pool)
		req.Registered = append(req.Registered, rwKey(byte(0x60+p)))
		req.Blocks = append(req.Blocks, &v1.BlocksMade{PoolId: rep(id, 28), Count: uint64(1500 + p*300)})
	}
	return req
}

func rwRewardUpdateCases() []entry {
	prefilter := rwRewardUpdateReq(6)
	prefilter.Registered = []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03)}
	noBlocks := rwRewardUpdateReq(8)
	noBlocks.Blocks = nil
	futurePool := rwRewardUpdateReq(8)
	futurePool.Pools = append(futurePool.Pools, &v1.Pool{
		PoolId: rep(0xc3, 28), Stage: v1.PoolStage_POOL_STAGE_FUTURE, Margin: rat("1", "2"), RewardAccount: rwKey(0x08),
	})
	delegatedToUnregistered := rwRewardUpdateReq(8)
	delegatedToUnregistered.Snapshots = append(delegatedToUnregistered.Snapshots,
		&v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_GO, Credential: rwKey(0x09), Amount: 100, PoolId: rep(0xff, 28)})
	ignoreOtherSnapshots := rwRewardUpdateReq(8)
	ignoreOtherSnapshots.Snapshots = append(ignoreOtherSnapshots.Snapshots,
		&v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_MARK, Credential: rwKey(0x0a), Amount: 777, PoolId: rep(0xa1, 28)})
	overSupply := rwRewardUpdateReq(8)
	overSupply.Globals.MaxLovelaceSupply = 1
	return []entry{
		rwRewardUpdateEntry("rewards_reward_update_babbage", "deltaR -210000000, deltaT 250000000, deltaF -5e7; the unregistered member keeps its reward", rwRewardUpdateReq(8)),
		rwRewardUpdateEntry("rewards_reward_update_alonzo_prefilter", "major 6: the unregistered member is dropped, deltaR -207570000", prefilter),
		rwRewardUpdateEntry("rewards_reward_update_no_blocks", "no blocks made: eta 0 and every pool gets nothing", noBlocks),
		rwRewardUpdateEntry("rewards_reward_update_future_pool_ignored", "a pool registration waiting for the next epoch is not in the snapshot", futurePool),
		rwRewardUpdateEntry("rewards_reward_update_delegation_to_unregistered_pool", "stake delegated to an unregistered pool counts in the active stake only", delegatedToUnregistered),
		rwRewardUpdateEntry("rewards_reward_update_only_go_rows", "MARK rows are not read", ignoreOtherSnapshots),
		rwRewardUpdateEntry("rewards_reward_update_shared_credential_shelley", "major 2: one credential earning as leader and member is paid only one reward", rwSharedCredential(2)),
		rwRewardUpdateEntry("rewards_reward_update_shared_credential_allegra", "major 3: aggregated, both rewards are paid", rwSharedCredential(3)),
		rwRewardUpdateEntry("rewards_reward_update_shared_credential_alonzo", "major 5: aggregated with the prefilter", rwSharedCredential(5)),
		rwRewardUpdateEntry("rewards_reward_update_many_pools", "eight pools, forty delegators, mixed credentials and margins", rwManyPools(8)),
		rwRewardUpdateEntry("rewards_reward_update_many_pools_alonzo", "the larger update with the prefilter", rwManyPools(6)),
		rwRewardUpdateEntry("rewards_reward_update_reserves_above_supply", "reserves above the maximum supply: UNSUPPORTED on both nodes", overSupply),
	}
}

func rwApplyUpdateReq(major uint32) *v1.RewardsApplyUpdateRequest {
	rew := func(c *v1.Credential, kind v1.RewardKind, amount uint64) *v1.Reward {
		return &v1.Reward{Credential: c, PoolId: rep(0xa1, 28), Kind: kind, Amount: amount}
	}
	return &v1.RewardsApplyUpdateRequest{
		ProtocolVersion: rwPV(major),
		Pots:            &v1.Pots{Reserves: 400_000_000_000, Treasury: 7, Fees: 50_000_000, Donations: 11, Deposits: 13},
		Update: &v1.RewardUpdate{
			DeltaR: -210_000_000, DeltaT: 250_000_000, DeltaF: -50_000_000,
			Rewards: []*v1.Reward{
				rew(rwKey(0x01), v1.RewardKind_REWARD_KIND_LEADER, 3_520_000),
				rew(rwKey(0x03), v1.RewardKind_REWARD_KIND_MEMBER, 4_050_000),
				rew(rwKey(0x04), v1.RewardKind_REWARD_KIND_MEMBER, 2_430_000),
			},
		},
		Registered:     []*v1.Credential{rwKey(0x01), rwKey(0x02), rwKey(0x03)},
		TotalRewardPot: 1_250_000_000, TreasuryExpansion: rat("1", "5"),
	}
}

func rwApplyUpdateCases() []entry {
	entryOf := func(name, why string, req *v1.RewardsApplyUpdateRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsApplyUpdate{RewardsApplyUpdate: req}}}
	}
	allRegistered := rwApplyUpdateReq(8)
	allRegistered.Registered = append(allRegistered.Registered, rwKey(0x04))
	noneRegistered := rwApplyUpdateReq(8)
	noneRegistered.Registered = nil
	scriptAccount := rwApplyUpdateReq(8)
	scriptAccount.Update.Rewards[1].Credential = rwScript(0x03)
	scriptAccount.Registered[2] = rwScript(0x03)
	noRewards := rwApplyUpdateReq(8)
	noRewards.Update = &v1.RewardUpdate{DeltaR: -1_200_000_000 + 1_000_000_000, DeltaT: 250_000_000, DeltaF: -50_000_000}
	badTreasury := rwApplyUpdateReq(8)
	badTreasury.Update.DeltaT++
	allegra := rwApplyUpdateReq(3)
	preAllegra := rwApplyUpdateReq(2)
	return []entry{
		entryOf("rewards_apply_update_babbage", "registered accounts credited; the unregistered member's reward goes to the treasury", rwApplyUpdateReq(8)),
		entryOf("rewards_apply_update_all_registered", "nothing is unregistered: treasury grows by deltaT only", allRegistered),
		entryOf("rewards_apply_update_none_registered", "every reward is unregistered and goes to the treasury", noneRegistered),
		entryOf("rewards_apply_update_script_account", "script credentials are registered like key credentials", scriptAccount),
		entryOf("rewards_apply_update_no_rewards", "an update with no rewards still moves the pots", noRewards),
		entryOf("rewards_apply_update_allegra", "major 3: aggregated rewards", allegra),
		entryOf("rewards_apply_update_delta_t_mismatch", "delta_t differs from floor(tau * pot): UNSUPPORTED on both nodes", badTreasury),
		entryOf("rewards_apply_update_before_allegra", "major 2 selects one reward per credential at reward_update: UNSUPPORTED here", preAllegra),
	}
}

// rwNonMyopicBase is the request of the Go test TestNonMyopic: pools A and B
// are the top two, C earns nothing.
func rwNonMyopicBase() *v1.RewardsNonMyopicRequest {
	pool := func(id byte, pledge, cost uint64, margin *v1.Rational) *v1.Pool {
		return &v1.Pool{PoolId: rep(id, 28), Pledge: pledge, Cost: cost, Margin: margin, RewardAccount: rwKey(0xee)}
	}
	return &v1.RewardsNonMyopicRequest{
		Pparams:    &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: rwU32(2)},
		TotalStake: 1000, RewardPot: 1_000_000_000, MemberStake: 50,
		Pools: []*v1.Pool{
			pool(0xa1, 10, 1_000_000, rat("1", "10")), pool(0xb2, 5, 2_000_000, rat("1", "5")), pool(0xc3, 1, 400_000_000, rat("0", "1")),
		},
		PoolDistribution: []*v1.PoolStake{
			{PoolId: rep(0xa1, 28), Stake: 100}, {PoolId: rep(0xb2, 28), Stake: 50}, {PoolId: rep(0xc3, 28), Stake: 10},
		},
		Performance: []*v1.PoolPerformance{
			{PoolId: rep(0xa1, 28), Performance: rat("1", "1")}, {PoolId: rep(0xb2, 28), Performance: rat("1", "2")},
			{PoolId: rep(0xc3, 28), Performance: rat("1", "1")},
		},
		OwnerStake: []*v1.PoolOwnerStake{
			{PoolId: rep(0xa1, 28), Amount: 20}, {PoolId: rep(0xb2, 28), Amount: 5}, {PoolId: rep(0xc3, 28), Amount: 1},
		},
	}
}

// rwNonMyopicMany is a mainnet-sized ranking of sixteen pools with
// performances that are not dyadic fractions.
func rwNonMyopicMany(member uint64) *v1.RewardsNonMyopicRequest {
	req := &v1.RewardsNonMyopicRequest{
		Pparams:    &v1.ProtocolParameters{A0: rat("3", "10"), NOpt: rwU32(5)},
		TotalStake: 34_000_000_000_000_000, RewardPot: 7_000_000_000_000, MemberStake: member,
	}
	for p := 0; p < 16; p++ {
		id := rep(byte(0x20+p), 28)
		stake := uint64(60_000_000_000_000 + p*9_000_000_000_000)
		req.Pools = append(req.Pools, &v1.Pool{
			PoolId: id, Pledge: uint64(1_000_000_000_000 + p*400_000_000_000), Cost: uint64(340_000_000 + p*30_000_000),
			Margin: rat(string(rune('0'+p%6)), "100"), RewardAccount: rwKey(0xee),
		})
		req.PoolDistribution = append(req.PoolDistribution, &v1.PoolStake{PoolId: id, Stake: stake})
		req.OwnerStake = append(req.OwnerStake, &v1.PoolOwnerStake{PoolId: id, Amount: uint64(1_500_000_000_000 + p*400_000_000_000)})
		if p%5 != 4 {
			req.Performance = append(req.Performance, &v1.PoolPerformance{
				PoolId: id, Performance: rat(string(rune('5'+p%4)), "7"),
			})
		}
	}
	return req
}

func rwNonMyopicCases() []entry {
	mk := func(name, why string, req *v1.RewardsNonMyopicRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsNonMyopic{RewardsNonMyopic: req}}}
	}
	pledge := rwNonMyopicBase()
	pledge.OwnerStake[0].Amount = 9
	noLikelihood := rwNonMyopicBase()
	noLikelihood.Performance = noLikelihood.Performance[1:]
	single := rwNonMyopicBase()
	single.Pools, single.PoolDistribution, single.Performance, single.OwnerStake =
		single.Pools[:1], single.PoolDistribution[:1], single.Performance[:1], single.OwnerStake[:1]
	zeroMember := rwNonMyopicBase()
	zeroMember.MemberStake = 0
	zeroA0 := rwNonMyopicBase()
	zeroA0.Pparams = &v1.ProtocolParameters{A0: rat("0", "1"), NOpt: rwU32(1)}
	tie := rwNonMyopicBase()
	tie.Pools = tie.Pools[:2]
	tie.Pools[1].Pledge, tie.Pools[1].Cost, tie.Pools[1].Margin = tie.Pools[0].Pledge, tie.Pools[0].Cost, tie.Pools[0].Margin
	tie.PoolDistribution, tie.Performance, tie.OwnerStake = tie.PoolDistribution[:2], tie.Performance[:2], tie.OwnerStake[:2]
	tie.Performance[1].Performance = tie.Performance[0].Performance
	tie.OwnerStake[1].Amount = tie.OwnerStake[0].Amount
	tie.PoolDistribution[1].Stake = tie.PoolDistribution[0].Stake
	tie.Pparams.NOpt = rwU32(1)
	futurePool := rwNonMyopicBase()
	futurePool.Pools[2].Stage = v1.PoolStage_POOL_STAGE_FUTURE
	noPools := rwNonMyopicBase()
	noPools.Pools, noPools.PoolDistribution, noPools.Performance, noPools.OwnerStake = nil, nil, nil, nil
	zeroTotal := rwNonMyopicBase()
	zeroTotal.TotalStake = 0
	return []entry{
		mk("rewards_non_myopic_basic", "A and B are the top pools: 34733076 and 15270769; C earns nothing", rwNonMyopicBase()),
		mk("rewards_non_myopic_pledge_not_met", "owner stake below the pledge: the pool pays nothing", pledge),
		mk("rewards_non_myopic_no_likelihood", "a pool without a likelihood history is not a top pool", noLikelihood),
		mk("rewards_non_myopic_single_pool", "one pool", single),
		mk("rewards_non_myopic_zero_member_stake", "a delegator with no stake earns nothing anywhere", zeroMember),
		mk("rewards_non_myopic_zero_a0_one_pool", "a0 = 0 and nOpt = 1", zeroA0),
		mk("rewards_non_myopic_desirability_tie", "equal desirability: the lower pool id ranks first", tie),
		mk("rewards_non_myopic_future_pool_ignored", "a pool registration waiting for the next epoch is not ranked", futurePool),
		mk("rewards_non_myopic_no_pools", "no pools: an empty reply", noPools),
		mk("rewards_non_myopic_many_pools", "sixteen pools, mainnet-sized stake, performances in sevenths", rwNonMyopicMany(1_000_000_000_000)),
		mk("rewards_non_myopic_many_pools_small_member", "the same pools for a one-ADA delegator", rwNonMyopicMany(1_000_000)),
		mk("rewards_non_myopic_zero_total_stake", "zero total stake: UNSUPPORTED on both nodes", zeroTotal),
	}
}
