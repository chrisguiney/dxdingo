package ledgerfixture

import (
	"fmt"
	"strings"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Scenario is one call with the failures the reference reports (empty when
// the rule accepts) and, for an accepting rule, the digest of its delta.
type Scenario struct {
	Name string
	// Fn is the envelope name of the function: rule_ppup, rule_gov, ...
	Fn    string
	Call  *v1.Call
	Want  []string
	Delta string
}

// Digest summarises a delta as its non-empty parts, the form both nodes'
// tests compare: row counts per table, then the pot movements.
func Digest(d *v1.LedgerDelta) string {
	var parts []string
	add := func(k string, n int) {
		if n != 0 {
			parts = append(parts, fmt.Sprintf("%s=%d", k, n))
		}
	}
	add("U+", len(d.GetUtxoAdded()))
	add("U-", len(d.GetUtxoRemoved()))
	add("A+", len(d.GetAccountsAdded()))
	add("A-", len(d.GetAccountsRemoved()))
	add("A~", len(d.GetAccountsChanged()))
	add("P~", len(d.GetPoolsChanged()))
	add("P-", len(d.GetPoolsRemoved()))
	add("D~", len(d.GetDrepsChanged()))
	add("D-", len(d.GetDrepsRemoved()))
	if d.GetCommitteeAfter() != nil {
		add("C", 1)
	}
	add("G+", len(d.GetProposalsAdded()))
	add("G-", len(d.GetProposalsRemoved()))
	add("V+", len(d.GetVotesAdded()))
	add("PP", len(d.GetPparamProposalsChanged()))
	for _, p := range []struct {
		k string
		v int64
	}{
		{"res", d.GetReservesDelta()}, {"tre", d.GetTreasuryDelta()}, {"fee", d.GetFeesDelta()},
		{"dep", d.GetDepositsDelta()}, {"don", d.GetDonationsDelta()},
	} {
		if p.v != 0 {
			parts = append(parts, fmt.Sprintf("%s=%d", p.k, p.v))
		}
	}
	if d.GetEnactStateAfter() != nil {
		parts = append(parts, "enact")
	}
	if d.GetPparamsAfter() != nil {
		parts = append(parts, "pparams")
	}
	return strings.Join(parts, " ")
}

// Scenarios returns every shared scenario that both nodes accept or reject
// alike; the Go node's tests, the corpus generator and the Haskell tests run
// them.
func Scenarios() []Scenario {
	return append(append(append(PpupScenarios(), GovScenarios()...), LedgerScenarios()...), LegacyLedgerScenarios()...)
}

// Divergences returns scenarios on which the two nodes disagree, each
// confirmed by a direct test of both libraries.
func Divergences() []Scenario {
	return append(PpupDivergences(), LedgerDivergenceScenarios()...)
}
