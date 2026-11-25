package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

func TestRuleLedgerCases(t *testing.T) {
	t.Parallel()
	for _, c := range lf.LedgerCases() {
		t.Run(c.Name, func(t *testing.T) {
			t.Parallel()
			resp, err := ruleLedger(context.Background(), lf.LedgerRequest(c))
			require.NoError(t, err)
			var got []string
			for _, f := range resp.GetRejection().GetFailures() {
				got = append(got, f.GetRule()+"/"+f.GetName())
			}
			sort.Strings(got)
			require.Equal(t, c.Want, got)
			if c.Want == nil {
				require.NotNil(t, resp.GetDelta())
				require.Equal(t, c.Delta, lf.Digest(resp.GetDelta()))
			} else {
				require.Nil(t, resp.GetDelta())
			}
		})
	}
}
