package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochUTxOCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochUTxOCases())
}

// epochNewEpochUTxOCases give the instant stake as a UTxO table instead of
// instant_stake. The boundary is the pool retirement one: pool a0 retires at
// epoch 11, credential 2 delegates to it and the old mark holds credential 3
// with 100, so credential 2's UTxO stake is what the new mark shows.
func epochNewEpochUTxOCases() []entry {
	base := func(utxo []*v1.UtxoEntry, instant ...*v1.StakeEntry) *v1.EpochNewEpochRequest {
		retire := uint64(11)
		return &v1.EpochNewEpochRequest{
			Context: &v1.LedgerContext{Era: v1.Era_ERA_CONWAY, Epoch: 11},
			Pparams: epochConwayPParams(),
			State: &v1.LedgerState{
				Accounts: []*v1.Account{{Credential: key(1)}, {Credential: key(2), Balance: 5, PoolId: rep(0xa0, 28)}},
				Pools: []*v1.Pool{{
					PoolId: rep(0xa0, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: key(1), Deposit: 500,
					Margin: rat("1", "100"), Cost: 340_000_000, RetiringEpoch: &retire,
				}},
				Pots: &v1.Pots{Reserves: 10_000_000_000, Treasury: 1_000_000},
				Utxo: utxo,
			},
			Snapshots:    []*v1.SnapshotEntry{{Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_MARK, Credential: key(3), Amount: 100, PoolId: rep(0xa0, 28)}},
			InstantStake: instant,
		}
	}
	mk := func(name, why string, r *v1.EpochNewEpochRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochNewEpoch{EpochNewEpoch: r}}}
	}
	return []entry{
		mk("epoch_new_epoch_utxo_base_outputs",
			"outputs of 25 and 15 to credential 2's base address are its instant stake of 40, as instant_stake would give; an output to credential 1, which delegates to no pool, adds nothing to the mark",
			base([]*v1.UtxoEntry{
				stkUtxo(1, stkOutput(0x00, stkBase(0, 2), 25)), stkUtxo(2, stkOutput(0x00, stkBase(0, 2), 15)),
				stkUtxo(3, stkOutput(0x00, stkBase(0, 1), 7)),
			})),
		mk("epoch_new_epoch_utxo_map_output",
			"a post-Alonzo map output is read like a legacy output",
			base([]*v1.UtxoEntry{stkUtxo(1, stkMapOutput(0x00, stkBase(0, 2), 1000))})),
		mk("epoch_new_epoch_utxo_pointer_and_enterprise",
			"pointer and enterprise addresses name no credential: they add no stake on either node",
			base([]*v1.UtxoEntry{
				stkUtxo(1, stkOutput(0x40, append(rep(0x70, 28), 0x01, 0x02, 0x03), 500)),
				stkUtxo(2, stkOutput(0x60, rep(0x70, 28), 100)), stkUtxo(3, stkOutput(0x00, stkBase(0, 2), 8)),
			})),
		mk("epoch_new_epoch_utxo_empty_table",
			"an empty table is no instant stake: only the account balance counts toward the mark",
			base(nil)),
		mk("epoch_new_epoch_utxo_undecodable_output",
			"an output that is not CBOR rejects the boundary as a DeserialiseFailure on both nodes",
			base([]*v1.UtxoEntry{stkUtxo(1, []byte{0xff})})),
		mk("epoch_new_epoch_utxo_with_instant_stake",
			"a UTxO table and instant_stake both give the instant stake: outside the answered domain",
			base([]*v1.UtxoEntry{stkUtxo(1, stkOutput(0x00, stkBase(0, 2), 25))}, &v1.StakeEntry{Credential: key(2), Amount: 40})),
	}
}
