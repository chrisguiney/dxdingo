// Package ledgerfixture builds the requests of the composed ledger rules
// (PPUP, GOV, LEDGER, BBODY) from hand-built transactions over small ledger
// states. Each case names the failures the reference reports and a digest of
// the delta an accepting rule produces; dxnode-go's tests, the corpus
// generator and the Haskell tests all run the same cases.
package ledgerfixture

import (
	"bytes"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// Hash returns a 28-byte hash made of b.
func Hash(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

// PPUP epoch geometry: epochs of 432000 slots, k=2160, f=1/20, so the stability
// window is 129600 and the point of no return of epoch 231 is slot 99964800.
const (
	ppupEpoch    = 231
	ppupSlot     = 99_793_000
	ppupNoReturn = 99_964_800
)

// GenesisKey and Delegate are the one genesis delegation of the PPUP state.
func GenesisKey() []byte { return Hash(0x91) }
func Delegate() []byte   { return common.Blake2b224Hash(delegateVkey()).Bytes() }

// delegateVkey is the verification key of the genesis delegate; PPUP checks
// its hash against the delegation, UTXOW checks its signature.
func delegateVkey() []byte { return bytes.Repeat([]byte{0xc1}, 32) }

// PpupCase is one PPUP scenario.
type PpupCase struct {
	Name   string
	Era    v1.Era
	Slot   uint64
	Mutate func(*v1.RulePpupRequest)
	// Update is the update field: genesis keys, the ProtocolParamUpdate map
	// each proposes and the target epoch.
	Keys   [][]byte
	Params map[uint]any
	Target uint64
	// Unsigned leaves the delegate's witness out.
	Unsigned bool
	Want     []string
	// Delta is Digest of the accepted delta.
	Delta string
}

// Tx builds an era's transaction whose body carries the update.
func ppupTx(era v1.Era, keys [][]byte, params map[uint]any, target uint64, witnessed bool) []byte {
	// A map keyed by byte strings is assembled by hand: a Go map cannot key on []byte.
	raw := []byte{0xa0 | byte(len(keys))}
	for _, k := range keys {
		kb, _ := cbor.Encode(k)
		vb, _ := cbor.Encode(params)
		raw = append(append(raw, kb...), vb...)
	}
	upd := []any{cbor.RawMessage(raw), target}
	body := map[uint]any{
		0: []any{[]any{bytes.Repeat([]byte{9}, 32), uint(0)}},
		1: []any{[]any{append([]byte{0x61}, Hash(0x77)...), uint(1_000_000)}},
		2: uint(200_000),
		3: uint(100_000_000),
		6: upd,
	}
	wits := map[uint]any{}
	if witnessed {
		wits[0] = []any{[]any{delegateVkey(), bytes.Repeat([]byte{0xd1}, 64)}}
	}
	var tx []any
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY:
		tx = []any{body, wits, nil}
	default:
		tx = []any{body, wits, true, nil}
	}
	out, err := cbor.Encode(tx)
	if err != nil {
		panic(err)
	}
	return out
}

// PpupRequest builds the request of a case.
func PpupRequest(c PpupCase) *v1.RulePpupRequest {
	ctx, g, pp := rulefixture.Env(c.Era)
	ctx.Slot = c.Slot
	ctx.Epoch = c.Slot / g.EpochLength
	req := &v1.RulePpupRequest{
		Context: ctx, Globals: g, Pparams: pp,
		State: &v1.LedgerState{GenesisDelegations: []*v1.GenesisDelegation{
			{GenesisKeyHash: GenesisKey(), DelegateKeyHash: Delegate(), VrfKeyHash: bytes.Repeat([]byte{0xb1}, 32)},
		}},
		Tx: ppupTx(c.Era, c.Keys, c.Params, c.Target, !c.Unsigned),
	}
	if c.Mutate != nil {
		c.Mutate(req)
	}
	return req
}

// PpupCases returns the PPUP scenarios.
func PpupCases() []PpupCase {
	gk := [][]byte{GenesisKey()}
	fee := map[uint]any{0: uint(45)}
	existing := func(r *v1.RulePpupRequest) {
		upd, _ := cbor.Encode(map[uint]any{0: uint(46)})
		r.State.PparamProposals = []*v1.PParamProposal{{GenesisKeyHash: GenesisKey(), TargetEpoch: ppupEpoch, Update: upd}}
	}
	var cases []PpupCase
	for _, era := range []v1.Era{v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY, v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE} {
		n := era.String()[4:]
		cases = append(cases,
			PpupCase{Name: "ppup " + n + " proposes for this epoch", Era: era, Slot: ppupSlot, Keys: gk, Params: fee, Target: ppupEpoch,
				Delta: "PP=1"},
			PpupCase{Name: "ppup " + n + " proposes for the wrong epoch", Era: era, Slot: ppupSlot, Keys: gk, Params: fee, Target: ppupEpoch + 2,
				Want: []string{"PPUP/PPUpdateWrongEpoch"}},
		)
	}
	sh := v1.Era_ERA_SHELLEY
	cases = append(cases,
		PpupCase{Name: "ppup proposes for the next epoch after the point of no return", Era: sh, Slot: ppupNoReturn, Keys: gk, Params: fee, Target: ppupEpoch + 1,
			Delta: "PP=1"},
		PpupCase{Name: "ppup one slot before the point of no return targets this epoch", Era: sh, Slot: ppupNoReturn - 1, Keys: gk, Params: fee, Target: ppupEpoch,
			Delta: "PP=1"},
		PpupCase{Name: "ppup this epoch target is late at the point of no return", Era: sh, Slot: ppupNoReturn, Keys: gk, Params: fee, Target: ppupEpoch,
			Want: []string{"PPUP/PPUpdateWrongEpoch"}},
		PpupCase{Name: "ppup next epoch target is early before the point of no return", Era: sh, Slot: ppupNoReturn - 1, Keys: gk, Params: fee, Target: ppupEpoch + 1,
			Want: []string{"PPUP/PPUpdateWrongEpoch"}},
		PpupCase{Name: "ppup proposal from a non genesis key", Era: sh, Slot: ppupSlot, Keys: [][]byte{Hash(0x92)}, Params: fee, Target: ppupEpoch,
			Want: []string{"PPUP/NonGenesisUpdatePPUP"}},
		PpupCase{Name: "ppup protocol version major increment with a nonzero minor", Era: sh, Slot: ppupSlot, Keys: gk, Params: map[uint]any{14: []any{uint(3), uint(1)}}, Target: ppupEpoch,
			Want: []string{"PPUP/PVCannotFollowPPUP"}},
		PpupCase{Name: "ppup protocol version major increment", Era: sh, Slot: ppupSlot, Keys: gk, Params: map[uint]any{14: []any{uint(3), uint(0)}}, Target: ppupEpoch,
			Delta: "PP=1"},
		PpupCase{Name: "ppup protocol version minor increment", Era: sh, Slot: ppupSlot, Keys: gk, Params: map[uint]any{14: []any{uint(2), uint(1)}}, Target: ppupEpoch,
			Delta: "PP=1"},
		PpupCase{Name: "ppup replaces an existing proposal", Era: sh, Slot: ppupSlot, Keys: gk, Params: fee, Target: ppupEpoch, Mutate: existing,
			Delta: "PP=1"},
		PpupCase{Name: "ppup resubmits an identical proposal", Era: sh, Slot: ppupSlot, Keys: gk, Params: map[uint]any{0: uint(46)}, Target: ppupEpoch, Mutate: existing,
			Delta: ""},
	)
	return cases
}

// PpupScenarios returns the cases as calls.
func PpupScenarios() []Scenario {
	var out []Scenario
	for _, c := range PpupCases() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_ppup", Want: c.Want, Delta: c.Delta,
			Call: &v1.Call{Request: &v1.Call_RulePpup{RulePpup: PpupRequest(c)}},
		})
	}
	return out
}

// PpupDivergences are the PPUP scenarios on which the nodes disagree.
func PpupDivergences() []Scenario {
	gk := [][]byte{GenesisKey()}
	c := PpupCase{Name: "ppup wrong epoch without the delegate witness", Era: v1.Era_ERA_SHELLEY, Slot: ppupSlot,
		Keys: gk, Params: map[uint]any{0: uint(45)}, Target: ppupEpoch + 2, Unsigned: true,
		Want: []string{"PPUP/PPUpdateWrongEpoch"}}
	return []Scenario{{
		Name: c.Name, Fn: "rule_ppup", Want: c.Want,
		Call: &v1.Call{Request: &v1.Call_RulePpup{RulePpup: PpupRequest(c)}},
	}}
}
