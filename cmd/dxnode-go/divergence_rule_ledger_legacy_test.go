package main

import (
	"bytes"
	"context"
	"strings"
	"testing"

	"github.com/blinklabs-io/dingo/dxoracle"
	dledger "github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// babbageBase is the first plain mainnet transaction of the Babbage block with
// its synthesised state.
func babbageBase(t *testing.T) (*v1.LedgerContext, *v1.Globals, *v1.ProtocolParameters, *v1.LedgerState, []byte) {
	t.Helper()
	txs, err := rulefixture.BlockTxs(v1.Era_ERA_BABBAGE)
	require.NoError(t, err)
	for _, tx := range txs {
		if !rulefixture.Plain(v1.Era_ERA_BABBAGE, tx) {
			continue
		}
		st, err := rulefixture.State(v1.Era_ERA_BABBAGE, 1, tx)
		if err != nil {
			continue
		}
		slot, err := rulefixture.SlotFor(v1.Era_ERA_BABBAGE, tx)
		require.NoError(t, err)
		ctx, g, pp := rulefixture.Env(v1.Era_ERA_BABBAGE)
		ctx.Slot = slot
		return ctx, g, pp, st, tx
	}
	t.Fatal("no plain Babbage transaction")
	return nil, nil, nil, nil, nil
}

// With an input missing from the UTxO, the reference's LEDGER reports
// BadInputsUTxO and ValueNotConservedUTxO only: getScriptsNeeded skips
// unresolved inputs. Babbage's required-redeemers rule in gouroboros fails on
// the unresolved input. This calls the rule directly, so no adapter code is
// involved.
func TestDivergenceLegacyRequiredRedeemersFailsOnUnresolvedInput(t *testing.T) {
	t.Parallel()
	ctx, g, pp, st, txCbor := babbageBase(t)
	st.Utxo = nil
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeBabbage, txCbor)
	require.NoError(t, err)
	rs, err := newRuleState(ctx, g, st)
	require.NoError(t, err)
	params, err := ruleProtocolParameters(v1.Era_ERA_BABBAGE, pp)
	require.NoError(t, err)
	require.Error(t, babbage.UtxoValidateRequiredRedeemers(tx, ctx.Slot, rs, params),
		"required-redeemers rule accepted a transaction with an unresolved input")
}

// Dingo keeps the output of an applied transaction as the transaction wrote
// it. The first Babbage block transaction creates an output whose asset names
// are in canonical CBOR order (shortest first); the reference re-encodes an
// output with its names in byte order (the Haskell check "divergence: the
// reference re-encodes asset names in byte order"). This runs the production
// writer through dxoracle, so no adapter code is involved.
func TestDivergenceLegacyLedgerKeepsWireBytesOfCreatedOutputs(t *testing.T) {
	t.Parallel()
	ctx, _, pp, st, txCbor := babbageBase(t)
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeBabbage, txCbor)
	require.NoError(t, err)
	params, err := ruleProtocolParameters(v1.Era_ERA_BABBAGE, pp)
	require.NoError(t, err)
	tables, err := ledgerTablesOf(ctx, st)
	require.NoError(t, err)
	o, err := theCertOracle()
	require.NoError(t, err)
	res, err := o.RunLedger(context.Background(), dxoracle.LedgerRunInput{
		Slot: ctx.Slot, Epoch: ctx.Epoch, NetworkID: uint(ctx.NetworkId), Params: params, State: *tables, Tx: tx,
		LegacyEraID: babbage.EraIdBabbage,
	})
	require.NoError(t, err)
	require.Empty(t, res.Failures)
	require.Len(t, res.UtxoAdded, len(tx.Outputs()))
	var shortlex bool
	for _, added := range res.UtxoAdded {
		wire := tx.Outputs()[added.Index].Cbor()
		require.True(t, bytes.Equal(wire, added.Output.Cbor()), "output %d was re-encoded", added.Index)
		short, long := bytes.Index(wire, []byte("AdventurerOfThiolden868")), bytes.Index(wire, []byte("AdventurerOfThiolden2164"))
		shortlex = shortlex || (short >= 0 && long > short)
	}
	require.True(t, shortlex, "no output lists its asset names shortest first")
}

// Dingo refuses a block whose header's protocol major version is more than one
// ahead of the ledger's in every era; the reference's BBODY has that predicate
// from Conway only (HeaderProtVerTooHigh is a constructor of the Conway rule
// alone), so Shelley to Babbage blocks with such a header are accepted there
// (the Haskell check "BBODY alone accepts the block"). This calls dingo's
// validator on the decoded header, so no adapter code is involved.
func TestDivergenceLegacyBlockHeaderVersionLimitAppliesInEveryEra(t *testing.T) {
	t.Parallel()
	seen := 0
	for _, s := range lf.LegacyBlockDivergenceScenarios() {
		if !strings.Contains(s.Name, "header two major versions ahead") {
			continue
		}
		seen++
		req := s.Call.GetRuleBlockBody()
		era := req.GetContext().GetEra()
		typ, err := blockTypeOf(era)
		require.NoError(t, err)
		block, err := ledger.NewBlockFromCbor(typ, req.GetBlock(), lcommon.VerifyConfig{SkipBodyHashValidation: true})
		require.NoError(t, err)
		major := uint(req.GetPparams().GetProtocolVersion().GetMajor())
		require.Error(t, dledger.ValidateHeaderProtocolVersion(block.Header(), major, true), "%v", era)
		require.NoError(t, dledger.ValidateHeaderProtocolVersion(block.Header(), major+1, true), "%v", era)
	}
	require.Equal(t, len(lf.LegacyEras), seen)
}
