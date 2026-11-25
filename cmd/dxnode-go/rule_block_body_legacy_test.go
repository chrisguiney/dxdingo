package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

// TestRuleBlockBodyLegacyCases runs the Shelley-to-Babbage BBODY scenarios:
// each era's mainnet block (a TPraos header up to Alonzo, a Praos header in
// Babbage) with its body replaced and the header committing to it or broken
// one way at a time. The expected failures are those of the reference's
// ShelleyBbodyPredFailure and AlonzoBbodyPredFailure; the Haskell side runs
// the same calls from the generated data.
func TestRuleBlockBodyLegacyCases(t *testing.T) {
	t.Parallel()
	for _, s := range lf.LegacyBlockScenarios() {
		req := s.Call.GetRuleBlockBody()
		t.Run(s.Name, func(t *testing.T) {
			t.Parallel()
			resp, err := ruleBlockBody(context.Background(), req)
			require.NoError(t, err)
			var got []string
			for _, f := range resp.GetRejection().GetFailures() {
				got = append(got, f.GetRule()+"/"+f.GetName())
			}
			sort.Strings(got)
			require.Equal(t, s.Want, got)
			require.Equal(t, s.Delta, lf.BlockDigest(resp))
			if s.Want == nil {
				require.NotNil(t, resp.GetDelta())
			} else {
				require.Nil(t, resp.GetDelta())
			}
		})
	}
}
