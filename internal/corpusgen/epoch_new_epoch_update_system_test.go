package corpusgen

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochUpdateSystemCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochUpdateSystemCases())
}

// epochNewEpochUpdateSystemCases are boundaries before Conway that adopt a
// classic parameter update or pay MIR certificates. The base is the pool
// retirement boundary of epoch_new_epoch_pool_retirement in the named era,
// with the scratch ledger's epoch length 432000 and the update quorum in
// globals. A proposal made in epoch 10 is added at a slot from 4320000.
func epochNewEpochUpdateSystemCases() []entry {
	var pool entry
	for _, e := range epochNewEpochCases() {
		if e.name == "epoch_new_epoch_pool_retirement" {
			pool = e
		}
	}
	eraOf := func(name string) preConwayEra {
		for _, e := range preConwayEras() {
			if e.name == name {
				return e
			}
		}
		panic(name)
	}
	const made = 10*432_000 + 10
	prop := func(k byte, slot uint64, update []byte) *v1.PParamProposal {
		return &v1.PParamProposal{GenesisKeyHash: rep(k, 28), TargetEpoch: 10, AddedSlot: slot, Update: update}
	}
	upd := func(name, why, era string, quorum uint64, ps ...*v1.PParamProposal) entry {
		e := inEra(pool, eraOf(era), "")
		e.name, e.comment = "epoch_new_epoch_"+name+"_"+era, why+" ("+era+")"
		r := e.call.GetEpochNewEpoch()
		r.Globals = &v1.Globals{EpochLength: 432_000, Quorum: quorum}
		r.State.PparamProposals = ps
		return e
	}
	mir := func(name, why string, mutate func(*v1.EpochNewEpochRequest)) entry {
		e := inEra(pool, eraOf("babbage"), "")
		e.name, e.comment = "epoch_new_epoch_"+name, why
		mutate(e.call.GetEpochNewEpoch())
		return e
	}
	credit := func(src v1.MirSource, c *v1.Credential, amount int64) *v1.MirEntry {
		return &v1.MirEntry{Source: src, Credential: c, Amount: amount}
	}
	const reserves, treasury = v1.MirSource_MIR_SOURCE_RESERVES, v1.MirSource_MIR_SOURCE_TREASURY
	var out []entry
	for _, era := range []string{"shelley", "allegra", "mary", "alonzo", "babbage"} {
		out = append(out, upd("pparam_update_quorum_met", "two genesis keys agree on min_fee_a = 100 and the quorum is 2: the boundary adopts it", era, 2,
			prop(1, made, epochMinFeeA100), prop(2, made+1, epochMinFeeA100)))
	}
	out = append(out,
		upd("pparam_update_quorum_not_met", "the same two votes against a quorum of 3 adopt nothing and pparams_after is unset", "babbage", 3,
			prop(1, made, epochMinFeeA100), prop(2, made+1, epochMinFeeA100)),
		upd("pparam_update_conflict", "two values each reach the quorum: the reference adopts neither", "babbage", 2,
			prop(1, made, epochMinFeeA100), prop(2, made+1, epochMinFeeA100), prop(3, made+2, epochMinFeeA200), prop(4, made+3, epochMinFeeA200)),
		upd("pparam_update_latest_vote", "a genesis key's later proposal replaces its earlier one", "babbage", 2,
			prop(1, made, epochMinFeeA100), prop(1, made+1, epochMinFeeA200), prop(2, made+2, epochMinFeeA200)),
		upd("pparam_update_protocol_version", "an update to protocol 3.0 is adopted at the boundary into epoch 11", "shelley", 1,
			prop(1, made, epochPV30)),
		upd("pparam_update_carried_over", "a proposal made in epoch 9 for epoch 10 is carried over and counts", "babbage", 1,
			prop(1, 9*432_000+10, epochMinFeeA100)),
		upd("pparam_update_max_tx_size", "max_tx_size (key 3) set to 70000 is adopted and nothing else moves", "alonzo", 1,
			prop(1, made, epochMaxTx70000)),
		upd("pparam_update_undecodable", "an update that is not CBOR rejects the boundary as PPUP/DeserialiseFailure on both nodes", "babbage", 1,
			prop(1, made, []byte{0xff})),
	)
	out = append(out,
		mir("mir_reserves_credit", "150 from the reserves to registered account 1 is paid before POOLREAP refunds the retiring pool's 500 to the same account",
			func(r *v1.EpochNewEpochRequest) { r.Mir = []*v1.MirEntry{credit(reserves, key(1), 150)} }),
		mir("mir_treasury_credit", "200 from the treasury to registered account 2 is paid and the treasury shrinks by 200",
			func(r *v1.EpochNewEpochRequest) { r.Mir = []*v1.MirEntry{credit(treasury, key(2), 200)} }),
		mir("mir_unregistered_target", "a credit to an unregistered account is dropped and its amount stays in the reserves; the registered credit is paid",
			func(r *v1.EpochNewEpochRequest) {
				r.Mir = []*v1.MirEntry{credit(reserves, key(1), 150), credit(reserves, key(9), 77)}
			}),
		mir("mir_over_budget", "credits that exceed the treasury are skipped whole: nothing moves",
			func(r *v1.EpochNewEpochRequest) { r.Mir = []*v1.MirEntry{credit(treasury, key(1), 2_000_000)} }),
		mir("mir_pot_transfer", "a transfer of 400 from the reserves to the treasury moves both pots",
			func(r *v1.EpochNewEpochRequest) { r.ReservesToTreasury = 400 }),
		mir("mir_both_pots", "credits from both pots and a transfer in each direction",
			func(r *v1.EpochNewEpochRequest) {
				r.Mir = []*v1.MirEntry{credit(reserves, key(1), 100), credit(treasury, key(2), 60)}
				r.ReservesToTreasury, r.TreasuryToReserves = 300, 50
			}),
		mir("mir_in_conway", "MIR does not exist in Conway: outside the answered domain", func(r *v1.EpochNewEpochRequest) {
			r.Context.Era, r.Pparams = v1.Era_ERA_CONWAY, epochConwayPParams()
			r.Mir = []*v1.MirEntry{credit(reserves, key(1), 150)}
		}),
	)
	return out
}
