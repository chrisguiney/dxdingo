package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

// runLegacyCall answers a deleg, pool or certs call.
func runLegacyCall(t *testing.T, call *v1.Call) (*v1.Rejection, *v1.LedgerDelta) {
	t.Helper()
	ctx := context.Background()
	switch r := call.GetRequest().(type) {
	case *v1.Call_RuleDeleg:
		resp, err := ruleDeleg(ctx, r.RuleDeleg)
		require.NoError(t, err)
		return resp.Rejection, resp.Delta
	case *v1.Call_RulePool:
		resp, err := rulePool(ctx, r.RulePool)
		require.NoError(t, err)
		return resp.Rejection, resp.Delta
	case *v1.Call_RuleCerts:
		resp, err := ruleCerts(ctx, r.RuleCerts)
		require.NoError(t, err)
		return resp.Rejection, resp.Delta
	}
	t.Fatalf("not a certificate call: %T", call.GetRequest())
	return nil, nil
}

// The certificate rules of Shelley through Babbage run the era's own rule
// list: each case has the failures the reference reports in every era.
func TestRuleCertsLegacyCases(t *testing.T) {
	t.Parallel()
	for _, s := range lf.LegacyScenarios() {
		t.Run(s.Name, func(t *testing.T) {
			t.Parallel()
			rej, delta := runLegacyCall(t, s.Call)
			var got []string
			for _, f := range rej.GetFailures() {
				got = append(got, f.GetRule()+"/"+f.GetName())
			}
			sort.Strings(got)
			require.Equal(t, s.Want, got)
			if s.Want == nil {
				require.NotNil(t, delta)
				require.Equal(t, s.Delta, lf.Digest(delta))
			} else {
				require.Nil(t, delta)
			}
		})
	}
}
