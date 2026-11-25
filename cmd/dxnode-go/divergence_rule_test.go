package main

import (
	"context"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/conway"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// With an input missing from the UTxO, the reference reports BadInputsUTxO
// and ValueNotConservedUTxO only: getScriptsNeeded skips unresolved inputs.
// gouroboros' required-redeemers rule fails on the unresolved input instead.
// This calls the rule directly, so no adapter code is involved.
func TestDivergenceRequiredRedeemersFailsOnUnresolvedInput(t *testing.T) {
	t.Parallel()
	r := ruleRequest(t)
	r.State.Utxo = nil
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, r.Tx)
	if err != nil {
		t.Fatal(err)
	}
	rs, err := newRuleState(r.Context, r.Globals, r.State)
	if err != nil {
		t.Fatal(err)
	}
	pp, err := ruleProtocolParameters(v1.Era_ERA_CONWAY, r.Pparams)
	if err != nil {
		t.Fatal(err)
	}
	if err := conway.UtxoValidateRequiredRedeemers(tx, r.Context.Slot, rs, pp); err == nil {
		t.Fatal("required-redeemers rule accepted a transaction with an unresolved input")
	}
	_ = context.Background
}
