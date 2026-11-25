package corpusgen

import (
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochNewEpochPreConwayCorpus(t *testing.T) {
	writeCorpus(t, "", epochNewEpochPreConwayCases())
}

// preConwayEra is an era before Conway with its parameters. Shelley through
// Alonzo read d, which is set to zero so that the Conway reward numbers hold:
// the expected block count is the full 21600.
type preConwayEra struct {
	name string
	era  v1.Era
	pp   func() *v1.ProtocolParameters
}

func preConwayEras() []preConwayEra {
	noD := func(p *v1.ProtocolParameters, major uint32) *v1.ProtocolParameters {
		p.D, p.ProtocolVersion = rat("0", "1"), &v1.ProtocolVersion{Major: major}
		return p
	}
	return []preConwayEra{
		{"shelley", v1.Era_ERA_SHELLEY, func() *v1.ProtocolParameters { return noD(epochShelleyPParams(), 2) }},
		{"allegra", v1.Era_ERA_ALLEGRA, func() *v1.ProtocolParameters { return noD(epochShelleyPParams(), 3) }},
		{"mary", v1.Era_ERA_MARY, func() *v1.ProtocolParameters { return noD(epochShelleyPParams(), 4) }},
		{"alonzo", v1.Era_ERA_ALONZO, func() *v1.ProtocolParameters { return noD(epochAlonzoPParams(), 5) }},
		{"babbage", v1.Era_ERA_BABBAGE, epochBabbagePParams},
	}
}

// inEra is the Conway boundary e in era: the same ledger, the era's parameters.
func inEra(e entry, era preConwayEra, suffix string) entry {
	c := proto.Clone(e.call).(*v1.Call)
	r := c.GetEpochNewEpoch()
	r.Context.Era = era.era
	r.Pparams = era.pp()
	return entry{e.name + "_" + era.name, e.comment + " (" + era.name + ")", c}
}

// epochNewEpochPreConwayCases are the Conway pool retirement and reward
// boundaries in each era before Conway. Neither node has governance, a DRep
// delegation or a Conway update there, and the update system, MIR and
// pointers are outside the domain.
func epochNewEpochPreConwayCases() []entry {
	byName := map[string]entry{}
	for _, e := range append(epochNewEpochCases(), epochNewEpochRewardCases()...) {
		byName[e.name] = e
	}
	var out []entry
	for _, era := range preConwayEras() {
		out = append(out,
			inEra(byName["epoch_new_epoch_pool_retirement"], era, ""),
			inEra(byName["epoch_new_epoch_rotation"], era, ""),
			inEra(byName["epoch_new_epoch_reward_application"], era, ""),
			inEra(byName["epoch_new_epoch_reward_two_pools"], era, ""),
		)
	}
	// A DRep delegation, a pointer and governance state do not exist before
	// Conway: outside the domain.
	dele := inEra(byName["epoch_new_epoch_pool_retirement"], preConwayEras()[4], "")
	dele.name, dele.comment = "epoch_new_epoch_pre_conway_drep_delegation", "a DRep delegation does not exist before Conway: outside the answered domain"
	dele.call.GetEpochNewEpoch().State.Accounts[1].Drep = &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	gov := inEra(byName["epoch_new_epoch_pool_retirement"], preConwayEras()[4], "")
	gov.name, gov.comment = "epoch_new_epoch_pre_conway_governance", "governance state does not exist before Conway: outside the answered domain"
	gov.call.GetEpochNewEpoch().State.Dreps = []*v1.DRepRecord{{Credential: key(0xd1), Deposit: 500, Expiry: 100}}
	utxo := inEra(byName["epoch_new_epoch_pool_retirement"], preConwayEras()[4], "")
	utxo.name, utxo.comment = "epoch_new_epoch_pre_conway_utxo", "a UTxO table is read for Conway only: before it pointer addresses carry stake, which the nodes derive differently: outside the answered domain"
	utxo.call.GetEpochNewEpoch().InstantStake = nil
	utxo.call.GetEpochNewEpoch().State.Utxo = []*v1.UtxoEntry{stkUtxo(1, stkOutput(0x00, stkBase(0, 2), 40))}
	ptr := inEra(byName["epoch_new_epoch_pool_retirement"], preConwayEras()[4], "")
	ptr.name, ptr.comment = "epoch_new_epoch_pre_conway_pointer", "a registration pointer carries stake before Conway through pointer addresses, which dingo's instant stake leaves out: outside the answered domain"
	ptr.call.GetEpochNewEpoch().State.Accounts[1].Pointer = &v1.Pointer{Slot: 100, TxIndex: 1, CertIndex: 0}
	genesis := inEra(byName["epoch_new_epoch_pool_retirement"], preConwayEras()[4], "")
	genesis.name, genesis.comment = "epoch_new_epoch_pre_conway_genesis_delegation", "genesis delegations are not read at NEWEPOCH by either node: outside the answered domain"
	genesis.call.GetEpochNewEpoch().State.GenesisDelegations = []*v1.GenesisDelegation{{}}
	return append(out, dele, gov, utxo, ptr, genesis)
}
