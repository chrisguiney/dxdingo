package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

func TestRuleBlockBodyCases(t *testing.T) {
	t.Parallel()
	for _, c := range lf.BlockCases() {
		t.Run(c.Name, func(t *testing.T) {
			t.Parallel()
			resp, err := ruleBlockBody(context.Background(), lf.BlockRequest(c))
			require.NoError(t, err)
			var got []string
			for _, f := range resp.GetRejection().GetFailures() {
				got = append(got, f.GetRule()+"/"+f.GetName())
			}
			sort.Strings(got)
			require.Equal(t, c.Want, got)
			require.Equal(t, c.Expected(), lf.BlockDigest(resp))
			if c.Want == nil {
				require.NotNil(t, resp.GetDelta())
			} else {
				require.Nil(t, resp.GetDelta())
			}
		})
	}
}
