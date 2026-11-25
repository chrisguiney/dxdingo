package corpusgen

import (
	"testing"

	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

// TestGenerateRuleCertsLegacyCorpus writes the certificate-rule scenarios of
// Shelley through Babbage: every case in every era, against the same
// accounts and pool. The pool reward-account network is the one rule the
// eras answer differently (checked from Alonzo on). The Haskell side runs the
// same calls from test/Test/RuleLedgerData.hs, written by
// TestGenerateRuleLedgerCorpus.
func TestGenerateRuleCertsLegacyCorpus(t *testing.T) {
	writeCorpus(t, "", ledgerEntries(lf.LegacyScenarios()))
}
