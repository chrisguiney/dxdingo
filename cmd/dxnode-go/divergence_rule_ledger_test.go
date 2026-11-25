package main

import (
	"errors"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

// With a transaction that proposes for the wrong epoch and lacks the genesis
// delegate's witness, the reference's PPUP reports PPUpdateWrongEpoch: the
// witness is UTXOW's predicate. gouroboros runs the witness check first inside
// the one PPUP validator and returns its error, so the epoch check never runs.
// This calls the validator directly, so no adapter code is involved.
func TestDivergencePpupWitnessErrorHidesWrongEpoch(t *testing.T) {
	t.Parallel()
	req := lf.PpupRequest(lf.PpupCase{
		Name: "x", Era: v1.Era_ERA_SHELLEY, Slot: 99_793_000, Keys: [][]byte{lf.GenesisKey()},
		Params: map[uint]any{0: uint(45)}, Target: 233, Unsigned: true,
	})
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeShelley, req.Tx)
	require.NoError(t, err)
	rs, err := newRuleState(req.Context, req.Globals, req.State)
	require.NoError(t, err)
	window, err := ppupGlobals(req.Globals)
	require.NoError(t, err)
	pp, err := ruleProtocolParameters(v1.Era_ERA_SHELLEY, req.Pparams)
	require.NoError(t, err)
	got := shelley.UtxoValidateProtocolParameterUpdates(tx, req.Context.Slot, &ppupState{ruleState: rs, stabilityWindow: window}, pp)
	var witness common.ProtocolParameterUpdateWitnessError
	var epoch common.ProtocolParameterUpdateEpochError
	require.True(t, errors.As(got, &witness), "got %v", got)
	require.False(t, errors.As(got, &epoch))
}

// A withdrawal from an unregistered account is also a withdrawal by an
// account with no DRep delegation; the reference's LEDGER reports both
// (ConwayWithdrawalsMissingAccounts and ConwayWdrlNotDelegatedToDRep).
// gouroboros' withdrawal validator returns the unregistered-account error
// before it reaches the delegation check. This calls the validator directly.
func TestDivergenceWithdrawalValidatorStopsAtUnregisteredAccount(t *testing.T) {
	t.Parallel()
	c := lf.LedgerDivergences()[0]
	req := lf.LedgerRequest(c)
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, req.Tx)
	require.NoError(t, err)
	rs, err := newRuleState(req.Context, req.Globals, req.State)
	require.NoError(t, err)
	pp, err := ruleProtocolParameters(v1.Era_ERA_CONWAY, req.Pparams)
	require.NoError(t, err)
	got := conway.UtxoValidateWithdrawals(tx, req.Context.Slot, rs, pp)
	var missing shelley.WithdrawalFromUnregisteredRewardAccountError
	var undelegated conway.WithdrawalNotDelegatedToDRepError
	require.True(t, errors.As(got, &missing) || errors.As(got, new(conway.WithdrawalFromUnregisteredRewardAccountError)), "got %v", got)
	require.False(t, errors.As(got, &undelegated))
}
