package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateStakeCorpus(t *testing.T) { writeCorpus(t, "", stakeCases()) }

func TestGenerateStakeDivergences(t *testing.T) { writeCorpus(t, "divergences", stakeDivergences()) }

func stkRow(c *v1.Credential, amount uint64, pool []byte) *v1.SnapshotEntry {
	return &v1.SnapshotEntry{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_MARK, Credential: c, Amount: amount, PoolId: pool}
}

func stkPool(id byte, stage v1.PoolStage, vrf byte) *v1.Pool {
	return &v1.Pool{
		PoolId: rep(id, 28), Stage: stage, VrfKeyHash: rep(vrf, 32), Margin: rat("1", "10"),
		RewardAccount: rwKey(0xee), Cost: 340_000_000,
	}
}

func stakeCases() []entry {
	var out []entry
	out = append(out, stkPoolDistributionCases()...)
	out = append(out, stkDRepCases()...)
	out = append(out, stkInstantStakeCases()...)
	out = append(out, stkSnapshotCases()...)
	return out
}

func stkPoolDistributionCases() []entry {
	mk := func(name, why string, rows []*v1.SnapshotEntry, pools ...*v1.Pool) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_StakePoolDistribution{StakePoolDistribution: &v1.StakePoolDistributionRequest{
			Snapshot: rows, Pools: pools,
		}}}}
	}
	cur := v1.PoolStage_POOL_STAGE_CURRENT
	basic := []*v1.SnapshotEntry{
		stkRow(rwKey(0x01), 30, rep(0xa1, 28)), stkRow(rwKey(0x02), 20, rep(0xa1, 28)),
		stkRow(rwKey(0x03), 25, rep(0xb2, 28)), stkRow(rwKey(0x04), 25, rep(0xff, 28)),
		stkRow(rwKey(0x05), 999, nil),
	}
	var big []*v1.SnapshotEntry
	var bigPools []*v1.Pool
	for p := 0; p < 12; p++ {
		bigPools = append(bigPools, stkPool(byte(0x20+p), cur, byte(0x90+p)))
		for m := 0; m < 6; m++ {
			c := rwKey(byte(0x30 + p*6 + m))
			if m == 5 {
				c = rwScript(byte(0x30 + p*6 + m))
			}
			big = append(big, stkRow(c, uint64(1_000_000+p*77_777+m*1234), rep(byte(0x20+p), 28)))
		}
	}
	return []entry{
		mk("stake_pool_distribution_basic", "A 50/100, B 25/100; Z (unregistered) counts in the total only; C has no delegators; E is a future pool; the undelegated row is ignored",
			basic,
			stkPool(0xa1, cur, 0xa1), stkPool(0xb2, cur, 0xb2), stkPool(0xc3, cur, 0xc3), stkPool(0xe5, v1.PoolStage_POOL_STAGE_FUTURE, 0xe5)),
		mk("stake_pool_distribution_single_pool", "one pool holds all stake: share 1/1", []*v1.SnapshotEntry{stkRow(rwKey(1), 7, rep(0xa1, 28))},
			stkPool(0xa1, cur, 0xa1)),
		mk("stake_pool_distribution_unreduced_share", "share 6/9 is reported reduced as 2/3",
			[]*v1.SnapshotEntry{stkRow(rwKey(1), 6, rep(0xa1, 28)), stkRow(rwKey(2), 3, rep(0xb2, 28))},
			stkPool(0xa1, cur, 0xa1), stkPool(0xb2, cur, 0xb2)),
		mk("stake_pool_distribution_many_pools", "twelve pools, seventy-two delegators", big, bigPools...),
		mk("stake_pool_distribution_no_registered_pool", "delegations only to unregistered pools: an empty distribution with the total",
			[]*v1.SnapshotEntry{stkRow(rwKey(1), 10, rep(0xff, 28))}, stkPool(0xa1, cur, 0xa1)),
		mk("stake_pool_distribution_no_delegations", "no delegated stake: UNSUPPORTED on both nodes",
			[]*v1.SnapshotEntry{stkRow(rwKey(1), 10, nil)}, stkPool(0xa1, cur, 0xa1)),
	}
}

func stkDRepOf(c *v1.Credential) *v1.DRep {
	return &v1.DRep{Target: &v1.DRep_Credential{Credential: c}}
}

func stkAbstain() *v1.DRep { return &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}} }

func stkNoConfidence() *v1.DRep {
	return &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
}

func stkProposal(deposit uint64, ret *v1.Credential, expires uint64) *v1.Proposal {
	return &v1.Proposal{Deposit: deposit, ReturnAccount: ret, ProposedEpoch: 1, ExpiresEpoch: expires}
}

// stkDRepBase is the request of the Go test TestDRepDistributionRunsDingoSQL:
// DRep A = 10 + 5 + the 100 deposit, abstain = 7.
func stkDRepBase() *v1.StakeDRepDistributionRequest {
	drepA := rwKey(0xa0)
	return &v1.StakeDRepDistributionRequest{
		Epoch: 10,
		InstantStake: []*v1.StakeEntry{
			rwMember(rwKey(0x01), 10), rwMember(rwKey(0x02), 7), rwMember(rwKey(0x03), 1000),
		},
		Accounts: []*v1.Account{
			{Credential: rwKey(0x01), Balance: 5, Drep: stkDRepOf(drepA)},
			{Credential: rwKey(0x02), Drep: stkAbstain()},
			{Credential: rwKey(0x03), Balance: 1},
		},
		Dreps:     []*v1.DRepRecord{{Credential: drepA, Expiry: 20}},
		Proposals: []*v1.Proposal{stkProposal(100, rwKey(0x01), 15)},
	}
}

func stkDRepCases() []entry {
	mk := func(name, why string, req *v1.StakeDRepDistributionRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_StakeDrepDistribution{StakeDrepDistribution: req}}}
	}
	expired := stkDRepBase()
	expired.Proposals[0].ExpiresEpoch = 9
	lastEpoch := stkDRepBase()
	lastEpoch.Proposals[0].ExpiresEpoch = 10
	unregistered := stkDRepBase()
	unregistered.Dreps = nil
	script := stkDRepBase()
	scriptDRep := rwScript(0xa1)
	script.Dreps = append(script.Dreps, &v1.DRepRecord{Credential: scriptDRep, Expiry: 30})
	script.InstantStake = append(script.InstantStake, rwMember(rwScript(0x04), 40))
	script.Accounts = append(script.Accounts, &v1.Account{Credential: rwScript(0x04), Balance: 2, Drep: stkDRepOf(scriptDRep)})
	noConfidence := stkDRepBase()
	noConfidence.Accounts[1].Drep = stkNoConfidence()
	noConfidence.Proposals = append(noConfidence.Proposals, stkProposal(50, rwKey(0x02), 12))
	multiUtxo := stkDRepBase()
	multiUtxo.InstantStake = append(multiUtxo.InstantStake, rwMember(rwKey(0x01), 90), rwMember(rwKey(0x01), 900))
	noProposals := stkDRepBase()
	noProposals.Proposals = nil
	zeroDeposit := stkDRepBase()
	zeroDeposit.Proposals[0].Deposit = 0
	depositOnly := stkDRepBase()
	depositOnly.InstantStake = nil
	depositOnly.Accounts[0].Balance = 0

	many := &v1.StakeDRepDistributionRequest{Epoch: 40}
	for d := 0; d < 6; d++ {
		many.Dreps = append(many.Dreps, &v1.DRepRecord{Credential: rwKey(byte(0xb0 + d)), Expiry: 50})
		for a := 0; a < 5; a++ {
			c := rwKey(byte(0x10 + d*5 + a))
			if a == 4 {
				c = rwScript(byte(0x10 + d*5 + a))
			}
			many.InstantStake = append(many.InstantStake, rwMember(c, uint64(1_000_000*(d+1)+a*17)))
			var target *v1.DRep
			switch {
			case (d+a)%7 == 0:
				target = stkAbstain()
			case (d+a)%7 == 1:
				target = stkNoConfidence()
			case (d+a)%7 == 2:
				target = nil
			default:
				target = stkDRepOf(rwKey(byte(0xb0 + (d+a)%6)))
			}
			many.Accounts = append(many.Accounts, &v1.Account{Credential: c, Balance: uint64(a * 1000), Drep: target})
		}
		many.Proposals = append(many.Proposals, stkProposal(uint64(500_000_000*(d+1)), rwKey(byte(0x10+d*5)), uint64(38+d)))
	}
	return []entry{
		mk("stake_drep_distribution_expired_proposal", "a proposal that expired before the epoch adds no deposit: DRep A 15", expired),
		mk("stake_drep_distribution_last_active_epoch", "expires_epoch equal to the epoch is still active: DRep A 115", lastEpoch),
		mk("stake_drep_distribution_unregistered_drep", "delegation to a DRep that is not registered counts for nobody", unregistered),
		mk("stake_drep_distribution_script_dreps", "script DRep and script credential account", script),
		mk("stake_drep_distribution_no_confidence", "a proposal deposit returned to a no-confidence delegator adds to that bucket", noConfidence),
		mk("stake_drep_distribution_multiple_utxos", "instant stake of a credential adds up across UTxOs", multiUtxo),
		mk("stake_drep_distribution_no_proposals", "no proposals", noProposals),
		mk("stake_drep_distribution_zero_deposit", "a zero deposit adds nothing", zeroDeposit),
		mk("stake_drep_distribution_many", "six DReps, thirty accounts with every delegation kind, six proposals", many),
	}
}

// stakeDivergences are the confirmed mismatches. dingo omits a DRep whose
// stake is zero; cardano-ledger's computeDRepDistr keeps the entry with
// stake 0 (Map.insertWith for every delegating account). See
// cmd/dxnode-go/divergence_stake_test.go and the "divergence:" check in
// Test/Stake.hs.
func stakeDivergences() []entry {
	zeroStake := stkDRepBase()
	zeroStake.Dreps = append(zeroStake.Dreps, &v1.DRepRecord{Credential: rwKey(0xa2), Expiry: 20})
	zeroStake.Accounts = append(zeroStake.Accounts, &v1.Account{Credential: rwKey(0x05), Drep: stkDRepOf(rwKey(0xa2))})
	depositOnly := stkDRepBase()
	depositOnly.InstantStake = nil
	depositOnly.Accounts[0].Balance = 0
	call := func(req *v1.StakeDRepDistributionRequest) *v1.Call {
		return &v1.Call{Request: &v1.Call_StakeDrepDistribution{StakeDrepDistribution: req}}
	}
	return []entry{
		{"stake_drep_distribution_zero_stake_drep", "a registered DRep whose delegator holds nothing: dingo omits it, the ledger lists it with stake 0", call(zeroStake)},
		{"stake_drep_distribution_zero_stake_abstain", "an always-abstain delegator holding nothing: dingo omits the bucket, the ledger lists it with stake 0", call(depositOnly)},
	}
}

func stkUint(n uint64) []byte {
	switch {
	case n < 24:
		return []byte{byte(n)}
	case n < 256:
		return []byte{0x18, byte(n)}
	case n < 65536:
		return []byte{0x19, byte(n >> 8), byte(n)}
	}
	return []byte{0x1a, byte(n >> 24), byte(n >> 16), byte(n >> 8), byte(n)}
}

// stkOutput is a Shelley output [address, coin].
func stkOutput(header byte, hashes []byte, amount uint64) []byte {
	addr := append([]byte{header}, hashes...)
	out := []byte{0x82, 0x58, byte(len(addr))}
	out = append(out, addr...)
	return append(out, stkUint(amount)...)
}

// stkMapOutput is a post-Alonzo output {0: address, 1: coin}.
func stkMapOutput(header byte, hashes []byte, amount uint64) []byte {
	addr := append([]byte{header}, hashes...)
	out := []byte{0xa2, 0x00, 0x58, byte(len(addr))}
	out = append(out, addr...)
	out = append(out, 0x01)
	return append(out, stkUint(amount)...)
}

func stkUtxo(n int, txout []byte) *v1.UtxoEntry {
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: rep(byte(n), 32), Index: uint32(n)}, Txout: txout}
}

func stkBase(header, stake byte) []byte { return append(rep(0x70, 28), rep(stake, 28)...) }

func stkInstantStakeCases() []entry {
	mk := func(name, why string, utxo ...*v1.UtxoEntry) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_StakeInstantStake{StakeInstantStake: &v1.StakeInstantStakeRequest{Utxo: utxo}}}}
	}
	var many []*v1.UtxoEntry
	for i := 1; i <= 60; i++ {
		header := byte(0x00)
		if i%5 == 0 {
			header = 0x20
		}
		many = append(many, stkUtxo(i, stkOutput(header, stkBase(header, byte(i%9+1)), uint64(i*977))))
	}
	return []entry{
		mk("stake_instant_stake_basic", "K1 10 + 5, K2 7, script credential 9; enterprise and zero-value outputs add nothing",
			stkUtxo(1, stkOutput(0x00, stkBase(0, 1), 10)), stkUtxo(2, stkOutput(0x00, stkBase(0, 1), 5)),
			stkUtxo(3, stkOutput(0x00, stkBase(0, 2), 7)), stkUtxo(4, stkOutput(0x20, stkBase(0x20, 0x53), 9)),
			stkUtxo(5, stkOutput(0x60, rep(0x70, 28), 100)), stkUtxo(6, stkOutput(0x00, stkBase(0, 3), 0))),
		mk("stake_instant_stake_map_output", "a post-Alonzo map output is read like a legacy array output",
			stkUtxo(1, stkMapOutput(0x00, stkBase(0, 1), 1000)), stkUtxo(2, stkOutput(0x00, stkBase(0, 1), 24))),
		mk("stake_instant_stake_script_payment", "a script payment credential with a key stake credential counts for the stake credential",
			stkUtxo(1, stkOutput(0x10, stkBase(0x10, 4), 300)), stkUtxo(2, stkOutput(0x30, stkBase(0x30, 5), 400))),
		mk("stake_instant_stake_pointer_address", "a pointer address names no credential: the ledger and dingo both leave it out",
			stkUtxo(1, stkOutput(0x40, append(rep(0x70, 28), 0x01, 0x02, 0x03), 500)), stkUtxo(2, stkOutput(0x00, stkBase(0, 1), 8))),
		mk("stake_instant_stake_no_outputs", "no outputs"),
		mk("stake_instant_stake_undecodable_output", "an output that is not CBOR: a DeserialiseFailure rejection from both nodes",
			stkUtxo(1, []byte{0xff})),
		mk("stake_instant_stake_many", "sixty outputs over nine credentials", many...),
	}
}

func stkSnapshotCases() []entry {
	mk := func(name, why string, req *v1.StakeSnapshotRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_StakeSnapshot{StakeSnapshot: req}}}
	}
	cur := v1.PoolStage_POOL_STAGE_CURRENT
	base := func() *v1.StakeSnapshotRequest {
		return &v1.StakeSnapshotRequest{
			InstantStake: []*v1.StakeEntry{
				rwMember(rwKey(0x01), 10), rwMember(rwKey(0x01), 5), rwMember(rwKey(0x02), 7),
				rwMember(rwKey(0x04), 11), rwMember(rwKey(0x05), 50),
			},
			Accounts: []*v1.Account{
				{Credential: rwKey(0x01), PoolId: rep(0xa1, 28)},
				{Credential: rwKey(0x02), Balance: 3, PoolId: rep(0xb2, 28)},
				{Credential: rwKey(0x03), Balance: 2, PoolId: rep(0xa1, 28)},
				{Credential: rwKey(0x04)},
				{Credential: rwKey(0x06), PoolId: rep(0xa1, 28)},
			},
			Pools: []*v1.Pool{stkPool(0xa1, cur, 0xa1)},
		}
	}
	scripts := base()
	scripts.InstantStake = append(scripts.InstantStake, rwMember(rwScript(0x07), 70))
	scripts.Accounts = append(scripts.Accounts, &v1.Account{Credential: rwScript(0x07), Balance: 1, PoolId: rep(0xb2, 28)})
	futurePool := base()
	futurePool.Pools = append(futurePool.Pools, stkPool(0xb2, v1.PoolStage_POOL_STAGE_FUTURE, 0xb2))
	balanceOnly := base()
	balanceOnly.InstantStake = nil
	none := base()
	none.Accounts = none.Accounts[3:4]
	zeroOnly := base()
	zeroOnly.InstantStake = []*v1.StakeEntry{rwMember(rwKey(0x01), 0)}
	zeroOnly.Accounts = zeroOnly.Accounts[:1]
	big := &v1.StakeSnapshotRequest{Pools: []*v1.Pool{stkPool(0xa1, cur, 0xa1), stkPool(0xb2, cur, 0xb2)}}
	for i := 1; i <= 50; i++ {
		c := rwKey(byte(i))
		if i%6 == 0 {
			c = rwScript(byte(i))
		}
		big.InstantStake = append(big.InstantStake, rwMember(c, uint64(i*10_007)))
		a := &v1.Account{Credential: c, Balance: uint64(i % 7 * 300)}
		if i%4 != 0 {
			a.PoolId = rep(byte(0xa1+(i%2)*0x11), 28)
		}
		if i%11 != 0 {
			big.Accounts = append(big.Accounts, a)
		}
	}
	return []entry{
		mk("stake_snapshot_basic", "K1 15 to A, K2 7 + 3 to B, K3 2 (balance only) to A; K4 has no pool, K5 is unregistered, K6 holds nothing", base()),
		mk("stake_snapshot_script_credential", "a script credential delegating to a pool is part of the snapshot", scripts),
		mk("stake_snapshot_future_pool", "a pool waiting for the next epoch changes no row", futurePool),
		mk("stake_snapshot_balance_only", "no instant stake: balances of delegated accounts only", balanceOnly),
		mk("stake_snapshot_zero_instant_stake", "a zero-amount instant stake entry adds nothing", zeroOnly),
		mk("stake_snapshot_no_active_stake", "no registered delegated credential holds stake: UNSUPPORTED on both nodes", none),
		mk("stake_snapshot_many", "fifty credentials, some script, some unregistered, some undelegated, over two pools", big),
	}
}
