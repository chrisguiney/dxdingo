package ledgerfixture

import (
	"bytes"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// LegacyEras are the eras whose certificate rules the legacy scenarios run.
var LegacyEras = []v1.Era{
	v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY, v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE,
}

// LegacyCase is one certificate-rule scenario of a Shelley-family era over the
// certificate fixture's accounts and pool. Rule is "deleg", "pool" or "certs";
// Want and Delta are the same in every era unless Otherwise overrides them.
type LegacyCase struct {
	Name        string
	Rule        string
	Certs       []cf.Cert
	Withdrawals []cf.Withdrawal
	CertIndex   *uint32
	Want        []string
	// Delta is the Digest of the delta an accepting rule produces.
	Delta string
	// Otherwise lists the eras that answer differently: the pool reward
	// account network is checked from Alonzo on, so before it the rule accepts.
	Otherwise map[v1.Era]LegacyOutcome
}

// LegacyOutcome is the answer of an era that differs from the case's.
type LegacyOutcome struct {
	Want  []string
	Delta string
}

// outcome is the case's answer in an era.
func (c LegacyCase) outcome(era v1.Era) LegacyOutcome {
	if o, ok := c.Otherwise[era]; ok {
		return o
	}
	return LegacyOutcome{c.Want, c.Delta}
}

// LegacyTx builds a transaction of the era carrying the certificates and
// withdrawals: an input and an output on mainnet, a fee and a far ttl.
func LegacyTx(era v1.Era, certs []cf.Cert, withdrawals []cf.Withdrawal) []byte {
	body := map[uint]any{
		0: []any{[]any{bytes.Repeat([]byte{9}, 32), uint(0)}},
		1: []any{[]any{append([]byte{0x61}, cf.Hash(0x77)...), uint(1_000_000)}},
		2: uint(200_000),
		3: uint(1) << 40,
	}
	if len(certs) > 0 {
		body[4] = certs
	}
	if len(withdrawals) > 0 {
		kv := make([][2]raw, len(withdrawals))
		for i, w := range withdrawals {
			kv[i] = [2]raw{enc(cf.RewardAddr(w.Cred)), enc(w.Amount)}
		}
		body[5] = rmap(kv...)
	}
	parts := []any{body, map[uint]any{}, nil}
	if era == v1.Era_ERA_ALONZO || era == v1.Era_ERA_BABBAGE {
		parts = []any{body, map[uint]any{}, true, nil}
	}
	out, err := cbor.Encode(parts)
	if err != nil {
		panic(err)
	}
	return out
}

// LegacyState is the certificate fixture's accounts and pool.
func LegacyState() *v1.LedgerState {
	st := cf.State()
	st.Dreps, st.Committee = nil, nil
	return st
}

func legacyEnv(era v1.Era) (*v1.LedgerContext, *v1.ProtocolParameters) {
	ctx, _, pp := rulefixture.Env(era)
	ctx.Epoch, ctx.Slot = cf.Epoch, cf.Epoch*432_000+1_000
	return ctx, pp
}

// LegacyCases returns the scenarios.
func LegacyCases() []LegacyCase {
	b1, b2 := cf.Key(0xb1), cf.Key(0xb2)
	p2 := cf.Hash(0x52)
	one := uint32(1)
	return []LegacyCase{
		{Name: "deleg registers a new credential", Rule: "deleg", Certs: []cf.Cert{cf.StakeReg(b1)}, Delta: "A+=1"},
		{Name: "deleg re-registering a registered key", Rule: "deleg", Certs: []cf.Cert{cf.StakeReg(cf.Key(0xa1))},
			Want: []string{"DELEG/StakeKeyAlreadyRegisteredDELEG"}},
		{Name: "deleg deregistering a drained account", Rule: "deleg", Certs: []cf.Cert{cf.StakeDereg(cf.Key(0xa2))}, Delta: "A-=1"},
		{Name: "deleg deregistering an account with a balance", Rule: "deleg", Certs: []cf.Cert{cf.StakeDereg(cf.Key(0xa1))},
			Want: []string{"DELEG/StakeKeyNonZeroAccountBalanceDELEG"}},
		{Name: "deleg deregistering an unregistered key", Rule: "deleg", Certs: []cf.Cert{cf.StakeDereg(b1)},
			Want: []string{"DELEG/StakeKeyNotRegisteredDELEG"}},
		{Name: "deleg delegates to a registered pool", Rule: "deleg", Certs: []cf.Cert{cf.StakeDeleg(cf.Key(0xa2), cf.PoolID())}, Delta: "A~=1"},
		{Name: "deleg delegates to an unregistered pool", Rule: "deleg", Certs: []cf.Cert{cf.StakeDeleg(cf.Key(0xa2), cf.Hash(0x99))},
			Want: []string{"DELEG/DelegateeNotRegisteredDELEG"}},
		{Name: "deleg delegates an unregistered credential", Rule: "deleg", Certs: []cf.Cert{cf.StakeDeleg(b1, cf.PoolID())},
			Want: []string{"DELEG/StakeDelegationImpossibleDELEG"}},
		{Name: "deleg runs only its own certificates in order", Rule: "deleg",
			Certs: []cf.Cert{cf.StakeReg(b1), cf.StakeReg(b2), cf.StakeDeleg(b1, cf.PoolID())}, Delta: "A+=2"},
		{Name: "deleg reports every failing certificate", Rule: "deleg",
			Certs: []cf.Cert{cf.StakeReg(b1), cf.StakeReg(b1), cf.StakeDeleg(b2, cf.PoolID())},
			Want:  []string{"DELEG/StakeDelegationImpossibleDELEG", "DELEG/StakeKeyAlreadyRegisteredDELEG"}},
		{Name: "deleg cert_index selects one certificate", Rule: "deleg", CertIndex: &one,
			Certs: []cf.Cert{cf.StakeReg(cf.Key(0xa1)), cf.StakeReg(b1)}, Delta: "A+=1"},
		{Name: "pool registers a new pool", Rule: "pool", Certs: []cf.Cert{cf.PoolReg(p2, 0x62, 100, 170_000_000, 1, 0xb1)}, Delta: "P~=1"},
		{Name: "pool cost below the minimum", Rule: "pool", Certs: []cf.Cert{cf.PoolReg(p2, 0x62, 100, 1, 1, 0xb1)},
			Want: []string{"POOL/StakePoolCostTooLowPOOL"}},
		{Name: "pool reward account on the wrong network", Rule: "pool", Certs: []cf.Cert{cf.PoolReg(p2, 0x62, 100, 170_000_000, 0, 0xb1)},
			Want: []string{"POOL/WrongNetworkPOOL"},
			Otherwise: map[v1.Era]LegacyOutcome{
				v1.Era_ERA_SHELLEY: {Delta: "P~=1"}, v1.Era_ERA_ALLEGRA: {Delta: "P~=1"}, v1.Era_ERA_MARY: {Delta: "P~=1"},
			}},
		{Name: "pool re-registration becomes future parameters", Rule: "pool", Certs: []cf.Cert{cf.PoolReg(cf.PoolID(), 0x63, 200, 180_000_000, 1, 0xa1)}, Delta: "P~=1"},
		{Name: "pool retirement within emax", Rule: "pool", Certs: []cf.Cert{cf.PoolRetire(cf.PoolID(), 618)}, Delta: "P~=1"},
		{Name: "pool retirement at the current epoch", Rule: "pool", Certs: []cf.Cert{cf.PoolRetire(cf.PoolID(), 600)},
			Want: []string{"POOL/StakePoolRetirementWrongEpochPOOL"}},
		{Name: "pool retirement past emax", Rule: "pool", Certs: []cf.Cert{cf.PoolRetire(cf.PoolID(), 619)},
			Want: []string{"POOL/StakePoolRetirementWrongEpochPOOL"}},
		{Name: "pool retirement of an unregistered pool", Rule: "pool", Certs: []cf.Cert{cf.PoolRetire(p2, 610)},
			Want: []string{"POOL/StakePoolNotRegisteredOnKeyPOOL"}},
		// DELEGS runs the certificates only: the withdrawals of a transaction
		// are LEDGER's, so none of these fail here.
		{Name: "certs withdrawals are not checked by DELEGS", Rule: "certs",
			Withdrawals: []cf.Withdrawal{{Cred: cf.Key(0xa1), Amount: 1_000_000}, {Cred: b1, Amount: 7}}},
		{Name: "certs run every rule over the sequence", Rule: "certs",
			Certs: []cf.Cert{cf.StakeReg(b1), cf.StakeDeleg(b1, cf.PoolID()), cf.PoolReg(p2, 0x62, 100, 170_000_000, 1, 0xb1)}, Delta: "A+=1 P~=1"},
		{Name: "certs report failures from several rules", Rule: "certs",
			Certs: []cf.Cert{cf.StakeReg(b1), cf.StakeDeleg(b1, cf.Hash(0x99)), cf.PoolRetire(p2, 610)},
			Want:  []string{"DELEG/DelegateeNotRegisteredDELEG", "POOL/StakePoolNotRegisteredOnKeyPOOL"}},
	}
}

// LegacyRequest builds the call of a case in an era: the request type of its rule.
func LegacyRequest(era v1.Era, c LegacyCase) *v1.Call {
	ctx, pp := legacyEnv(era)
	st, tx := LegacyState(), LegacyTx(era, c.Certs, c.Withdrawals)
	switch c.Rule {
	case "deleg":
		return &v1.Call{Request: &v1.Call_RuleDeleg{RuleDeleg: &v1.RuleDelegRequest{Context: ctx, Pparams: pp, State: st, Tx: tx, CertIndex: c.CertIndex}}}
	case "pool":
		return &v1.Call{Request: &v1.Call_RulePool{RulePool: &v1.RulePoolRequest{Context: ctx, Pparams: pp, State: st, Tx: tx, CertIndex: c.CertIndex}}}
	}
	return &v1.Call{Request: &v1.Call_RuleCerts{RuleCerts: &v1.RuleCertsRequest{Context: ctx, Pparams: pp, State: st, Tx: tx}}}
}

// LegacyScenarios returns every case in every era as a call. The name has the
// era after the rule word.
func LegacyScenarios() []Scenario {
	var out []Scenario
	for _, era := range LegacyEras {
		short := strings.ToLower(strings.TrimPrefix(era.String(), "ERA_"))
		for _, c := range LegacyCases() {
			rule, rest, _ := strings.Cut(c.Name, " ")
			out = append(out, Scenario{
				Name: rule + " " + short + " " + rest, Fn: "rule_" + c.Rule, Want: c.outcome(era).Want, Delta: c.outcome(era).Delta,
				Call: LegacyRequest(era, c),
			})
		}
	}
	return out
}
