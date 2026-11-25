package main

import (
	"bytes"
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func newEpochConwayPParams(t *testing.T) *v1.ProtocolParameters {
	t.Helper()
	req := forkReq(8, 9, 8)
	req.Pparams = babbagePParamsProto()
	req.Pparams.ProtocolVersion = &v1.ProtocolVersion{Major: 8}
	req.ConwayGenesis = conwayGenesisInput()
	pp := run(t, req)
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: 10}
	return pp
}

// Same case as dxnode-hs test/Test/Epoch.hs. Pool a0 retires at epoch 11 with
// deposit 500 and reward account k1 (registered). k2 delegates to a0 with a
// reward balance of 5 and UTxO stake 40. The old mark holds k3 with 100.
func newEpochRequest(t *testing.T) *v1.EpochNewEpochRequest {
	retiring := uint64(11)
	return &v1.EpochNewEpochRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
		Pparams: newEpochConwayPParams(t),
		State: &v1.LedgerState{
			Accounts: []*v1.Account{
				{Credential: rotKey(1)},
				{Credential: rotKey(2), Balance: 5, PoolId: bytes.Repeat([]byte{0xa0}, 28)},
			},
			Pools: []*v1.Pool{{
				PoolId: bytes.Repeat([]byte{0xa0}, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, Deposit: 500,
				RewardAccount: rotKey(1), Margin: &v1.Rational{Numerator: "1", Denominator: "100"}, RetiringEpoch: &retiring,
			}},
			Pots: &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000},
		},
		Snapshots:    []*v1.SnapshotEntry{rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 3, 100, 0xa0)},
		InstantStake: []*v1.StakeEntry{{Credential: rotKey(2), Amount: 40}},
	}
}

func TestEpochNewEpoch(t *testing.T) {
	got, err := epochNewEpoch(context.Background(), newEpochRequest(t))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	d := got.GetDelta()
	if len(d.GetPoolsRemoved()) != 1 || !bytes.Equal(d.GetPoolsRemoved()[0], bytes.Repeat([]byte{0xa0}, 28)) {
		t.Errorf("pools removed = %x", d.GetPoolsRemoved())
	}
	if len(d.GetAccountsChanged()) != 2 {
		t.Fatalf("accounts changed = %v", d.GetAccountsChanged())
	}
	if a := d.GetAccountsChanged()[0]; a.GetCredential().GetHash()[0] != 1 || a.GetBalance() != 500 {
		t.Errorf("refunded account = %v", a)
	}
	if a := d.GetAccountsChanged()[1]; a.GetCredential().GetHash()[0] != 2 || a.GetBalance() != 5 || len(a.GetPoolId()) != 0 {
		t.Errorf("undelegated account = %v", a)
	}
	if d.GetReservesDelta() != 0 || d.GetTreasuryDelta() != 0 {
		t.Errorf("pot deltas = %d, %d", d.GetReservesDelta(), d.GetTreasuryDelta())
	}
	want := []*v1.SnapshotEntry{
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_MARK, 2, 45, 0xa0),
		rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_SET, 3, 100, 0xa0),
	}
	if len(got.GetSnapshots()) != len(want) {
		t.Fatalf("snapshots = %v", got.GetSnapshots())
	}
	for i, w := range want {
		g := got.GetSnapshots()[i]
		if g.GetSnapshot() != w.GetSnapshot() || !bytes.Equal(g.GetCredential().GetHash(), w.GetCredential().GetHash()) ||
			g.GetAmount() != w.GetAmount() || !bytes.Equal(g.GetPoolId(), w.GetPoolId()) {
			t.Errorf("snapshot[%d] = %v, want %v", i, g, w)
		}
	}
}

func TestEpochNewEpochDomain(t *testing.T) {
	for name, tc := range map[string]struct {
		mutate func(*v1.EpochNewEpochRequest)
		code   v1.HarnessErrorCode
	}{
		"Byron": {func(r *v1.EpochNewEpochRequest) { r.Context.Era = v1.Era_ERA_BYRON }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"DRep delegation before Conway": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.State.Accounts[0].Drep = drepCred(cred(0xd1))
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"governance before Conway": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.State.Dreps = []*v1.DRepRecord{{Credential: cred(0xd1)}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"pointer before Conway": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.State.Accounts[0].Pointer = &v1.Pointer{}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"reward update":       {func(r *v1.EpochNewEpochRequest) { r.RewardUpdate = &v1.RewardUpdate{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"parameter proposals": {func(r *v1.EpochNewEpochRequest) { r.State.PparamProposals = []*v1.PParamProposal{{}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"utxo and instant stake": {func(r *v1.EpochNewEpochRequest) {
			r.State.Utxo = []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{1}, 32)}}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"utxo before Conway": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.InstantStake = nil
			r.State.Utxo = []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{1}, 32)}}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"utxo without tx id": {func(r *v1.EpochNewEpochRequest) {
			r.InstantStake = nil
			r.State.Utxo = []*v1.UtxoEntry{{}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"pointer":           {func(r *v1.EpochNewEpochRequest) { r.State.Accounts[0].Pointer = &v1.Pointer{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"drep target unset": {func(r *v1.EpochNewEpochRequest) { r.State.Accounts[0].Drep = &v1.DRep{} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"repeated account":  {func(r *v1.EpochNewEpochRequest) { r.State.Accounts = append(r.State.Accounts, r.State.Accounts[0]) }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id":     {func(r *v1.EpochNewEpochRequest) { r.State.Pools[0].PoolId = []byte{1} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			req := newEpochRequest(t)
			tc.mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}

// Donations of the ended epoch move into the treasury, and a retiring pool
// whose reward account is not registered returns its deposit there.
func TestEpochNewEpochTreasury(t *testing.T) {
	req := newEpochRequest(t)
	req.State.Pots.Donations = 5_000
	req.State.Accounts = req.State.Accounts[1:]
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	if d := got.GetDelta().GetTreasuryDelta(); d != 5_500 {
		t.Errorf("treasury delta = %d, want 5500 (donations plus the unclaimed deposit)", d)
	}
}

// newEpochRewardRequest is the case of dxnode-hs test/Test/Epoch.hs for a
// pending reward round. Pool a0 holds all 4.4999e13 active stake (sigma =
// 1/1000 of the 4.4999e16 circulation) and made all 21600 blocks, the
// expected count for f = 1/20 over 432000 slots, so eta = 1. With rho =
// 3/1000 and tau = 1/5 on reserves of 10^12: deltaR1 = 3_000_000_000, the
// treasury cut is 600_000_000 and the pool pot 2_400_000_000. maxPool =
// floor(2.4e9 / 1.3 * 0.001) = 1_846_153 at performance 1; with margin 1/100,
// cost 0 and no owners the leader gets floor(1_846_153 / 100) = 18_461 and the
// member floor(1_846_153 * 99 / 100) = 1_827_691. The 1 left over stays in
// reserves: deltaR = -3e9 + (2.4e9 - 1_846_152).
func newEpochRewardRequest(t *testing.T) *v1.EpochNewEpochRequest {
	pool := bytes.Repeat([]byte{0xa0}, 28)
	const stake = 44_999_000_000_000
	return &v1.EpochNewEpochRequest{
		Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
		Pparams: newEpochConwayPParams(t),
		Globals: &v1.Globals{
			EpochLength: 432_000, SecurityParam: 2_160, MaxLovelaceSupply: 45_000_000_000_000_000,
			ActiveSlotCoeff: &v1.Rational{Numerator: "1", Denominator: "20"},
		},
		State: &v1.LedgerState{
			Accounts: []*v1.Account{
				{Credential: rotKey(1)},
				{Credential: rotKey(2), PoolId: pool},
			},
			Pools: []*v1.Pool{{
				PoolId: pool, Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: rotKey(1),
				Margin: &v1.Rational{Numerator: "1", Denominator: "100"},
			}},
			Pots: &v1.Pots{Reserves: 1_000_000_000_000, Treasury: 1_000_000},
		},
		Snapshots:      []*v1.SnapshotEntry{rotEntry(v1.SnapshotKind_SNAPSHOT_KIND_GO, 2, stake, 0xa0)},
		BlocksPrevious: []*v1.BlocksMade{{PoolId: pool, Count: 21_600}},
		RewardUpdate: &v1.RewardUpdate{
			DeltaR: -601_846_152, DeltaT: 600_000_000,
			Rewards: []*v1.Reward{
				{Credential: rotKey(1), PoolId: pool, Kind: v1.RewardKind_REWARD_KIND_LEADER, Amount: 18_461},
				{Credential: rotKey(2), PoolId: pool, Kind: v1.RewardKind_REWARD_KIND_MEMBER, Amount: 1_827_691},
			},
		},
	}
}

func TestEpochNewEpochRewardApplication(t *testing.T) {
	t.Parallel()
	got, err := epochNewEpoch(context.Background(), newEpochRewardRequest(t))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	d := got.GetDelta()
	if d.GetReservesDelta() != -601_846_152 || d.GetTreasuryDelta() != 600_000_000 {
		t.Errorf("pot deltas = %d, %d", d.GetReservesDelta(), d.GetTreasuryDelta())
	}
	if len(d.GetAccountsChanged()) != 2 {
		t.Fatalf("accounts changed = %v", d.GetAccountsChanged())
	}
	if a := d.GetAccountsChanged()[0]; a.GetCredential().GetHash()[0] != 1 || a.GetBalance() != 18_461 {
		t.Errorf("leader account = %v", a)
	}
	if a := d.GetAccountsChanged()[1]; a.GetCredential().GetHash()[0] != 2 || a.GetBalance() != 1_827_691 {
		t.Errorf("member account = %v", a)
	}
	if len(got.GetSnapshots()) != 1 || got.GetSnapshots()[0].GetAmount() != 1_827_691 {
		t.Errorf("snapshots = %v, want the member's reward as the new mark", got.GetSnapshots())
	}
}

// Dingo recomputes the round at the boundary, so an update it would not have
// built, other globals, or no globals are outside the domain on both nodes.
func TestEpochNewEpochRewardDomain(t *testing.T) {
	t.Parallel()
	unsupported := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
	for name, mutate := range map[string]func(*v1.EpochNewEpochRequest){
		"update differs":    func(r *v1.EpochNewEpochRequest) { r.RewardUpdate.Rewards[0].Amount++ },
		"delta_t differs":   func(r *v1.EpochNewEpochRequest) { r.RewardUpdate.DeltaT++ },
		"other epoch size":  func(r *v1.EpochNewEpochRequest) { r.Globals.EpochLength = 100 },
		"no globals":        func(r *v1.EpochNewEpochRequest) { r.Globals = nil },
		"no blocks":         func(r *v1.EpochNewEpochRequest) { r.BlocksPrevious = nil },
		"pool not in state": func(r *v1.EpochNewEpochRequest) { r.BlocksPrevious[0].PoolId = bytes.Repeat([]byte{0xb0}, 28) },
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRewardRequest(t)
			mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != unsupported {
				t.Fatalf("err = %v, want UNSUPPORTED", err)
			}
		})
	}
}

// blocks_previous without a pending update feeds no round, so the boundary is
// the one without it.
func TestEpochNewEpochBlocksWithoutUpdate(t *testing.T) {
	t.Parallel()
	req := newEpochRewardRequest(t)
	req.RewardUpdate = nil
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if d := got.GetDelta(); d.GetReservesDelta() != 0 || d.GetTreasuryDelta() != 0 || len(d.GetAccountsChanged()) != 0 {
		t.Errorf("delta = %v, want none", d)
	}
}

// A DRep delegation is kept across the boundary and reported on a row that
// changed: the undelegated pool account keeps its DRep, a key DRep included.
func TestEpochNewEpochDRepDelegation(t *testing.T) {
	t.Parallel()
	for name, drep := range map[string]*v1.DRep{
		"abstain":       {Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}},
		"no confidence": {Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}},
		"key":           {Target: &v1.DRep_Credential{Credential: rotKey(0xd1)}},
		"script":        {Target: &v1.DRep_Credential{Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: bytes.Repeat([]byte{0xd2}, 28)}}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRequest(t)
			req.State.Accounts[1].Drep = drep
			got, err := epochNewEpoch(context.Background(), req)
			if err != nil {
				t.Fatal(err)
			}
			a := got.GetDelta().GetAccountsChanged()[1]
			if a.GetCredential().GetHash()[0] != 2 || !proto.Equal(a.GetDrep(), drep) {
				t.Errorf("account = %v, want drep %v", a, drep)
			}
			if first := got.GetDelta().GetAccountsChanged()[0]; first.GetDrep() != nil {
				t.Errorf("account without a delegation reports %v", first.GetDrep())
			}
		})
	}
}

func utxoEntry(t *testing.T, id byte, c *v1.Credential, amount uint64) *v1.UtxoEntry {
	t.Helper()
	out, err := stakeOutput(c, amount)
	if err != nil {
		t.Fatal(err)
	}
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{id}, 32)}, Txout: out}
}

// The UTxO table's instant stake is the stake_instant_stake answer: outputs of
// 25 and 15 to credential 2's base address are 40 for it, the same as the
// instant_stake entry newEpochRequest carries, and an output to credential 1,
// which delegates to no pool, adds nothing to the snapshot. The response must
// equal the instant_stake request's.
func TestEpochNewEpochUTxOInstantStake(t *testing.T) {
	t.Parallel()
	want, err := epochNewEpoch(context.Background(), newEpochRequest(t))
	if err != nil {
		t.Fatal(err)
	}
	req := newEpochRequest(t)
	req.InstantStake = nil
	req.State.Utxo = []*v1.UtxoEntry{
		utxoEntry(t, 1, rotKey(2), 25), utxoEntry(t, 2, rotKey(2), 15), utxoEntry(t, 3, rotKey(1), 7),
	}
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("UTxO answer\n%v\nwant the instant_stake answer\n%v", got, want)
	}
}

// An output that does not decode rejects the boundary as DeserialiseFailure,
// as stake_instant_stake does.
func TestEpochNewEpochUTxODecodeFailure(t *testing.T) {
	t.Parallel()
	req := newEpochRequest(t)
	req.InstantStake = nil
	req.State.Utxo = []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{1}, 32)}, Txout: []byte{0xff}}}
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	f := got.GetRejection().GetFailures()
	if len(f) != 1 || f[0].GetName() != "DeserialiseFailure" {
		t.Errorf("rejection %v, want DeserialiseFailure", got.GetRejection())
	}
}

// preConway puts req in an era before Conway under that era's parameters.
func preConway(req *v1.EpochNewEpochRequest, era v1.Era, pp *v1.ProtocolParameters) {
	req.Context.Era = era
	req.Pparams = pp
}

// The pool retirement boundary of TestEpochNewEpoch in each era before
// Conway, hand-derived the same way: the pool is removed, its deposit 500
// refunds account 1 and account 2 is undelegated; the new mark is credential
// 2's 45 (UTxO stake 40 and balance 5) and the set is the old mark. Same
// cases as haskell/dxnode-hs/test/Test/Epoch.hs.
func TestEpochNewEpochBeforeConway(t *testing.T) {
	t.Parallel()
	major := func(p *v1.ProtocolParameters, m uint32) *v1.ProtocolParameters {
		p = proto.Clone(p).(*v1.ProtocolParameters)
		p.ProtocolVersion = &v1.ProtocolVersion{Major: m}
		return p
	}
	for name, tc := range map[string]struct {
		era v1.Era
		pp  *v1.ProtocolParameters
	}{
		"shelley": {v1.Era_ERA_SHELLEY, shelleyPParams()},
		"allegra": {v1.Era_ERA_ALLEGRA, major(shelleyPParams(), 3)},
		"mary":    {v1.Era_ERA_MARY, major(shelleyPParams(), 4)},
		"alonzo":  {v1.Era_ERA_ALONZO, alonzoPParamsProto()},
		"babbage": {v1.Era_ERA_BABBAGE, babbagePParamsProto()},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRequest(t)
			preConway(req, tc.era, tc.pp)
			got := mustNewEpoch(t, req)
			d := got.GetDelta()
			if len(d.GetPoolsRemoved()) != 1 || !bytes.Equal(d.GetPoolsRemoved()[0], bytes.Repeat([]byte{0xa0}, 28)) {
				t.Errorf("pools removed = %x", d.GetPoolsRemoved())
			}
			if b := balances(d.GetAccountsChanged()); len(b) != 2 || b[1] != 500 || b[2] != 5 {
				t.Errorf("accounts changed %v, want 1=500 2=5", d.GetAccountsChanged())
			}
			if d.GetPparamsAfter() != nil || d.GetReservesDelta() != 0 || d.GetTreasuryDelta() != 0 {
				t.Errorf("delta %v, want no parameter or pot change", d)
			}
			var mark, set []uint64
			for _, e := range got.GetSnapshots() {
				switch e.GetSnapshot() {
				case v1.SnapshotKind_SNAPSHOT_KIND_MARK:
					mark = append(mark, e.GetAmount())
				case v1.SnapshotKind_SNAPSHOT_KIND_SET:
					set = append(set, e.GetAmount())
				}
			}
			if len(mark) != 1 || mark[0] != 45 || len(set) != 1 || set[0] != 100 {
				t.Errorf("mark %v set %v, want [45] [100]", mark, set)
			}
		})
	}
}

// The pending reward round of TestEpochNewEpochRewardApplication in each era
// before Conway, with d = 0 so the expected block count is the full 21600 and
// the hand-derived numbers hold: the leader is credited 18461, the member
// 1827691, and reserves and treasury move by the update's deltas.
func TestEpochNewEpochRewardBeforeConway(t *testing.T) {
	t.Parallel()
	zeroD := func(p *v1.ProtocolParameters, m uint32) *v1.ProtocolParameters {
		p = proto.Clone(p).(*v1.ProtocolParameters)
		p.D, p.ProtocolVersion = &v1.Rational{Numerator: "0", Denominator: "1"}, &v1.ProtocolVersion{Major: m}
		return p
	}
	for name, tc := range map[string]struct {
		era v1.Era
		pp  *v1.ProtocolParameters
	}{
		"shelley": {v1.Era_ERA_SHELLEY, zeroD(shelleyPParams(), 2)},
		"allegra": {v1.Era_ERA_ALLEGRA, zeroD(shelleyPParams(), 3)},
		"mary":    {v1.Era_ERA_MARY, zeroD(shelleyPParams(), 4)},
		"alonzo":  {v1.Era_ERA_ALONZO, zeroD(alonzoPParamsProto(), 5)},
		"babbage": {v1.Era_ERA_BABBAGE, babbagePParamsProto()},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRewardRequest(t)
			preConway(req, tc.era, tc.pp)
			got := mustNewEpoch(t, req)
			d := got.GetDelta()
			if d.GetReservesDelta() != -601_846_152 || d.GetTreasuryDelta() != 600_000_000 {
				t.Errorf("pot deltas = %d, %d", d.GetReservesDelta(), d.GetTreasuryDelta())
			}
			if b := balances(d.GetAccountsChanged()); len(b) != 2 || b[1] != 18_461 || b[2] != 1_827_691 {
				t.Errorf("accounts changed %v, want 1=18461 2=1827691", d.GetAccountsChanged())
			}
		})
	}
}

// babbageUpdateRequest is the Babbage boundary into epoch 11 with the
// update-system proposals: two genesis keys vote min_fee_a = 100 (key 0) for
// epoch 10, made during it. The reference adopts an update when `quorum` keys
// agree (votedFuturePParams); epoch_length is the scratch ledger's 432000.
func babbageUpdateRequest(t *testing.T, quorum uint64) *v1.EpochNewEpochRequest {
	t.Helper()
	req := newEpochRequest(t)
	preConway(req, v1.Era_ERA_BABBAGE, babbagePParamsProto())
	req.Globals = &v1.Globals{EpochLength: 432_000, Quorum: quorum}
	slot := uint64(10*432_000 + 10)
	req.State.PparamProposals = []*v1.PParamProposal{
		{GenesisKeyHash: bytes.Repeat([]byte{1}, 28), TargetEpoch: 10, AddedSlot: slot, Update: []byte{0xa1, 0x00, 0x18, 0x64}},
		{GenesisKeyHash: bytes.Repeat([]byte{2}, 28), TargetEpoch: 10, AddedSlot: slot + 1, Update: []byte{0xa1, 0x00, 0x18, 0x64}},
	}
	return req
}

// Two keys agree on min_fee_a = 100 and the quorum is 2: the boundary adopts
// it and reports the full parameters, every other value as it was. With a
// quorum of 3 nothing is adopted and pparams_after is unset.
func TestEpochNewEpochAdoptsParameterUpdate(t *testing.T) {
	t.Parallel()
	got := mustNewEpoch(t, babbageUpdateRequest(t, 2))
	after := got.GetDelta().GetPparamsAfter()
	if after.GetMinFeeA() != 100 || after.GetMaxBlockBodySize() != babbagePParamsProto().GetMaxBlockBodySize() {
		t.Errorf("pparams after %v, want min_fee_a 100 and the rest unchanged", after)
	}
	if rest := mustNewEpoch(t, babbageUpdateRequest(t, 3)); rest.GetDelta().GetPparamsAfter() != nil {
		t.Errorf("quorum 3: pparams after %v, want unset", rest.GetDelta().GetPparamsAfter())
	}
}

// An update that does not decode rejects the boundary as PPUP/DeserialiseFailure,
// as epoch_pparam_update does.
func TestEpochNewEpochParameterUpdateDecodeFailure(t *testing.T) {
	t.Parallel()
	req := babbageUpdateRequest(t, 2)
	req.State.PparamProposals[0].Update = []byte{0xff}
	got, err := epochNewEpoch(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	f := got.GetRejection().GetFailures()
	if len(f) != 1 || f[0].GetRule() != "PPUP" || f[0].GetName() != "DeserialiseFailure" {
		t.Errorf("rejection %v, want PPUP/DeserialiseFailure", got.GetRejection())
	}
}

// A MIR certificate of 150 from the reserves to registered account 1 is paid
// at the boundary, before POOLREAP refunds the retiring pool's 500 to the same
// account: balance 650, reserves down by 150. An unregistered target (account
// 9) gets nothing and its entry takes the whole certificate set with it only
// if over budget; here the 77 stays in the reserves.
func TestEpochNewEpochAppliesMIR(t *testing.T) {
	t.Parallel()
	req := newEpochRequest(t)
	preConway(req, v1.Era_ERA_BABBAGE, babbagePParamsProto())
	req.Mir = []*v1.MirEntry{
		{Source: v1.MirSource_MIR_SOURCE_RESERVES, Credential: rotKey(1), Amount: 150},
		{Source: v1.MirSource_MIR_SOURCE_RESERVES, Credential: rotKey(9), Amount: 77},
	}
	got := mustNewEpoch(t, req)
	d := got.GetDelta()
	if b := balances(d.GetAccountsChanged()); b[1] != 650 {
		t.Errorf("accounts changed %v, want 1=650", d.GetAccountsChanged())
	}
	if d.GetReservesDelta() != -150 || d.GetTreasuryDelta() != 0 {
		t.Errorf("pot deltas %d, %d, want -150, 0", d.GetReservesDelta(), d.GetTreasuryDelta())
	}
}

// Conway has neither update proposals nor MIR; genesis delegations are not
// read by NEWEPOCH on either node.
func TestEpochNewEpochUpdateSystemDomain(t *testing.T) {
	t.Parallel()
	unsupported := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED
	for name, tc := range map[string]struct {
		mutate func(*v1.EpochNewEpochRequest)
		code   v1.HarnessErrorCode
	}{
		"proposals in Conway": {func(r *v1.EpochNewEpochRequest) {
			r.State.PparamProposals = []*v1.PParamProposal{{GenesisKeyHash: bytes.Repeat([]byte{1}, 28), TargetEpoch: 10, Update: []byte{0xa0}}}
		}, unsupported},
		"MIR in Conway": {func(r *v1.EpochNewEpochRequest) {
			r.Mir = []*v1.MirEntry{{Source: v1.MirSource_MIR_SOURCE_RESERVES, Credential: rotKey(1), Amount: 1}}
		}, unsupported},
		"genesis delegations": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.State.GenesisDelegations = []*v1.GenesisDelegation{{}}
		}, unsupported},
		"other epoch length": {func(r *v1.EpochNewEpochRequest) {
			*r = *babbageUpdateRequest(t, 2)
			r.Globals.EpochLength = 100
		}, unsupported},
		"no globals": {func(r *v1.EpochNewEpochRequest) {
			*r = *babbageUpdateRequest(t, 2)
			r.Globals = nil
		}, unsupported},
		"proposal out of chain order": {func(r *v1.EpochNewEpochRequest) {
			*r = *babbageUpdateRequest(t, 2)
			r.State.PparamProposals[0].AddedSlot += 100
		}, unsupported},
		"short genesis key": {func(r *v1.EpochNewEpochRequest) {
			*r = *babbageUpdateRequest(t, 2)
			r.State.PparamProposals[0].GenesisKeyHash = []byte{1}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"negative MIR": {func(r *v1.EpochNewEpochRequest) {
			preConway(r, v1.Era_ERA_BABBAGE, babbagePParamsProto())
			r.Mir = []*v1.MirEntry{{Source: v1.MirSource_MIR_SOURCE_RESERVES, Credential: rotKey(1), Amount: -5}}
		}, unsupported},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := newEpochRequest(t)
			tc.mutate(req)
			if _, err := epochNewEpoch(context.Background(), req); err == nil || dispatch.Code(err) != tc.code {
				t.Fatalf("err = %v, want code %v", err, tc.code)
			}
		})
	}
}
