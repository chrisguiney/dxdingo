package corpusgen

import (
	"testing"

	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

// TestGenerateRuleBlockBodyCorpus writes the BBODY scenarios: mainnet blocks
// whose body is replaced, with a header that commits to the new body or that
// is broken one way at a time (size, hash, protocol version), the block
// execution-unit and reference-script budgets at and one past their limits,
// and transactions that fail inside a block. The Haskell side runs the same
// calls from test/Test/RuleLedgerData.hs, written by TestGenerateRuleLedgerCorpus.
func TestGenerateRuleBlockBodyCorpus(t *testing.T) {
	writeCorpus(t, "", ledgerEntries(lf.BlockScenarios()))
	writeCorpus(t, "divergences", append(ledgerEntries(lf.BlockDivergenceScenarios()), ledgerEntries(ledgerLegacyBlockDivergences(t))...))
}
