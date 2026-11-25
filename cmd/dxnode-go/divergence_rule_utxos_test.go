package main

import (
	"encoding/hex"
	"errors"
	"testing"
	"time"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// directState is the least gouroboros needs to evaluate scripts: the resolved
// UTxOs and a slot clock. It does not go through the adapter's ruleState.
type directState struct {
	common.LedgerState
	utxos common.ResolvedUtxos
}

func (d directState) UtxoById(in common.TransactionInput) (common.Utxo, error) {
	return d.utxos.UtxoById(in)
}

func (directState) SlotToTime(slot uint64) (time.Time, error) {
	return time.Unix(1596059091+int64(slot)-4492800, 0).UTC(), nil
}

// A spending script whose datum is not in the witness set: gouroboros
// refuses with MissingDatumForSpendingScriptError for PlutusV1 and V2. The
// reference's getSpendingDatum returns Nothing and the script runs on the
// remaining arguments (dxnode-hs "divergence: spending datum missing ...").
// UTXOW's MissingRequiredDatums rejects such a transaction first, so a full
// transaction validation cannot reach the difference.
func TestDivergenceSpendingDatumMissing(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		era  v1.Era
		lang int
		max  uint
		era_ string
		pv   uint
	}{
		"alonzo V1":  {v1.Era_ERA_ALONZO, 1, 1, "Alonzo", 6},
		"babbage V2": {v1.Era_ERA_BABBAGE, 2, 2, "Babbage", 8},
		"conway V2":  {v1.Era_ERA_CONWAY, 2, 3, "Conway", 10},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			raw, st := rulefixture.PlutusCase{
				Era: tc.era, Script: rulefixture.NewScript(tc.lang, false, false), IsValid: true,
				OmitDatum: true, NoRefInput: true,
			}.Build()
			tx, err := ledger.NewTransactionFromCbor(rulefixture.TxType(tc.era), raw)
			if err != nil {
				t.Fatal(err)
			}
			var utxos common.ResolvedUtxos
			for _, e := range st.Utxo {
				in, err := ledger.NewTransactionOutputFromCbor(e.Txout)
				if err != nil {
					t.Fatal(err)
				}
				id, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(e.Txin.TxId), int(e.Txin.Index))
				if err != nil {
					t.Fatal(err)
				}
				utxos = append(utxos, common.Utxo{Id: id, Output: in})
			}
			v1m, v2m, v3m := rulefixture.CostModels()
			models := map[uint][]int64{0: v1m, 1: v2m, 2: v3m}
			_, err = script.EvaluatePlutusScriptsPure(tx, directState{utxos: utxos}, tc.pv, models, tc.max, tc.era_, nil)
			var missing common.MissingDatumForSpendingScriptError
			if !errors.As(err, &missing) {
				t.Fatalf("err = %v, want MissingDatumForSpendingScriptError", err)
			}
		})
	}
}

// A spending redeemer that points at a key-locked input, with no Plutus script
// anywhere: the reference reports ExtraRedeemers only, because no script is
// needed. gouroboros' redeemer-and-script-witnesses rule also fails with
// MissingPlutusScriptWitnessesError, as it only asks whether a Plutus script
// is present when redeemers are (dxnode-hs "divergence: redeemer for a
// key-locked input"). The transaction is rejected by both, so a full
// validation sees the same accept or reject.
func TestDivergenceRedeemerForKeyLockedInput(t *testing.T) {
	t.Parallel()
	raw, st := rulefixture.PlutusCase{
		Era: v1.Era_ERA_CONWAY, Script: rulefixture.NewScript(3, false, false), IsValid: true,
		OmitScript: true, KeyInput: true, NoRefInput: true, CollateralReturn: true,
	}.Build()
	tx, ls := directTx(t, v1.Era_ERA_CONWAY, raw, st)
	pp := &conway.ConwayProtocolParameters{ProtocolVersion: common.ProtocolParametersProtocolVersion{Major: 10}}
	err := conway.UtxoValidateRedeemerAndScriptWitnesses(tx, 0, ls, pp)
	var missing common.MissingPlutusScriptWitnessesError
	if !errors.As(err, &missing) {
		t.Fatalf("err = %v, want MissingPlutusScriptWitnessesError", err)
	}
}

// An input missing from the UTxO: gouroboros' script run fails to resolve it
// and the UTXOS rule reports a collect error. The reference's collection
// skips unresolved inputs, so UTXOS has nothing to report (dxnode-hs
// "divergence: unresolved input ..."); BadInputsUTxO rejects the transaction
// in both nodes.
func TestDivergenceUnresolvedInputFailsScriptRun(t *testing.T) {
	t.Parallel()
	raw, _ := rulefixture.PlutusCase{
		Era: v1.Era_ERA_CONWAY, Script: rulefixture.NewScript(3, false, false), IsValid: true, NoRefInput: true, CollateralReturn: true,
	}.Build()
	tx, ls := directTx(t, v1.Era_ERA_CONWAY, raw, &v1.LedgerState{})
	v1m, v2m, v3m := rulefixture.CostModels()
	_, err := script.EvaluatePlutusScriptsPure(tx, ls, 10, map[uint][]int64{0: v1m, 1: v2m, 2: v3m}, 3, "Conway", nil)
	var unresolved common.InputResolutionError
	if !errors.As(err, &unresolved) {
		t.Fatalf("err = %v, want InputResolutionError", err)
	}
}

// directTx decodes raw and its resolved UTxOs with gouroboros alone.
func directTx(t *testing.T, era v1.Era, raw []byte, st *v1.LedgerState) (common.Transaction, directState) {
	t.Helper()
	tx, err := ledger.NewTransactionFromCbor(rulefixture.TxType(era), raw)
	if err != nil {
		t.Fatal(err)
	}
	var utxos common.ResolvedUtxos
	for _, e := range st.Utxo {
		out, err := ledger.NewTransactionOutputFromCbor(e.Txout)
		if err != nil {
			t.Fatal(err)
		}
		id, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(e.Txin.TxId), int(e.Txin.Index))
		if err != nil {
			t.Fatal(err)
		}
		utxos = append(utxos, common.Utxo{Id: id, Output: out})
	}
	return tx, directState{utxos: utxos}
}
