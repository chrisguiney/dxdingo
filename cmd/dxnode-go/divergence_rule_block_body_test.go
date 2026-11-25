package main

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

func failureNames(resp *v1.RuleBlockBodyResponse) []string {
	var out []string
	for _, f := range resp.GetRejection().GetFailures() {
		out = append(out, f.GetRule()+"/"+f.GetName())
	}
	return out
}

// Dingo applies a block's transactions in order and stops at the first that
// fails, so a block whose first transaction lacks a witness and whose second
// does not balance reports only the first failure. The reference's LEDGERS
// rule reports both (the Haskell check "divergence: bbody two transactions
// failing different rules"). Run alone, the second transaction fails as the
// reference says, so the missing failure is the stop, not a blind spot in the
// balance rule.
func TestDivergenceBlockStopsAtTheFirstFailingTransaction(t *testing.T) {
	t.Parallel()
	div := lf.BlockDivergences()[0]
	resp, err := ruleBlockBody(context.Background(), lf.BlockRequest(div))
	require.NoError(t, err)
	require.Equal(t, []string{"UTXOW/MissingVKeyWitnessesUTXOW"}, failureNames(resp))
	require.NotEqual(t, div.Want, failureNames(resp))

	second := lf.BlockCase{
		Name: "second transaction alone", Txs: []int{2},
		Setup: func(r *v1.RuleBlockBodyRequest) {
			r.State.Utxo[len(r.State.Utxo)-1] = rulefixture.Rebalance(v1.Era_ERA_CONWAY, r.State.Utxo[len(r.State.Utxo)-1], 1)
		},
	}
	alone, err := ruleBlockBody(context.Background(), lf.BlockRequest(second))
	require.NoError(t, err)
	require.Equal(t, []string{"UTXO/ValueNotConservedUTxO"}, failureNames(alone))
}
