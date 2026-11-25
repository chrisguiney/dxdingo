package main

import (
	"bytes"
	"testing"

	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/ledger"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
)

// Divergence confirmations call dingo and gouroboros directly, bypassing the
// dxnode-go adapters, so a reported mismatch cannot come from request
// translation. Each has the matching "divergence:" check in test/Test/Fee.hs
// on the reference side.

// fee.min-utxo, Mary multi-asset output. The reference scales the flat
// minUTxOValue by the size of the value (scaledMinDeposit): for one policy and
// one one-byte asset name, quot(1000000, 27) * (27 + 12) = 1444443. gouroboros
// MinCoinTxOut takes no output at all in Mary and returns the parameter.
//
// The UTXO rule never applies the scaled figure to a multi-asset output: the
// Allegra/Mary check is Val.pointwise (<) v (inject minCoin), false for any
// output holding a positive asset quantity, and gouroboros skips those outputs
// the same way (outputHasPositiveAsset). A real chain therefore cannot observe
// the difference.
func TestDivergenceMaryMinCoinIgnoresValueSize(t *testing.T) {
	t.Parallel()
	const reference = 1_444_443
	got, err := mary.MinCoinTxOut(nil, &mary.MaryProtocolParameters{MinUtxoValue: 1_000_000})
	if err != nil {
		t.Fatal(err)
	}
	if got == reference {
		t.Fatal("dingo now matches the reference; move the corpus case out of corpus/divergences")
	}
	if got != 1_000_000 {
		t.Fatalf("mary.MinCoinTxOut = %d, want the flat parameter 1000000", got)
	}
}

// fee.script-fee, units above int64. A redeemer's ExUnits are Word64 in the
// reference; gouroboros holds them as int64, so a transaction whose redeemer
// declares 2^63 memory does not decode. The reference prices it:
// 2^63 / 10^9 rounded up = 9223372037.
//
// A real chain reaches this only if maxTxExUnits is set to 2^63 or more: the
// UTXO rules reject a transaction whose units exceed it (ExUnitsTooBigUTxO),
// and mainnet's limit is 14 million memory units.
func TestDivergenceExUnitsAboveInt64DoNotDecode(t *testing.T) {
	t.Parallel()
	body := feeBody(t, 200_000)
	// Redeemers as a map: {[tag 0, index 0]: [data 0, [2^63, 0]]}.
	redeemers := []byte{0xa1, 0x82, 0x00, 0x00, 0x82, 0x00, 0x82, 0x1b, 0x80, 0, 0, 0, 0, 0, 0, 0, 0x00}
	wits := append([]byte{0xa1, 0x05}, redeemers...)
	tx := feeCat([]byte{0x84}, body, wits, []byte{0xf5, 0xf6})
	if _, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, tx); err == nil {
		t.Fatal("gouroboros now decodes redeemer units above int64; revisit the adapter bound")
	}
}

// fee.ref-script-size, absent inputs. The reference reads an absent input as
// carrying no script. dingo fails on an absent spending or reference input;
// gouroboros ignores an absent spending input and fails on an absent reference
// input.
//
// The transaction is invalid either way: the UTXO rule requires every spending
// and reference input to exist (validateBadInputsUTxO over allInputsTxBodyF in
// Babbage/Rules/Utxo.hs), so no accepted transaction reaches the difference.
func TestDivergenceAbsentInputsFailReferenceScriptSize(t *testing.T) {
	t.Parallel()
	body := feeMustCbor(t, map[uint]any{
		0:  []any{feeTxIn(0x01, 0)},
		1:  []any{[]any{feeAddr(), uint64(2_000_000)}},
		2:  uint64(200_000),
		18: []any{feeTxIn(0x02, 0)},
	})
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, feeConwayTx(body))
	if err != nil {
		t.Fatal(err)
	}
	empty := resolvedState{}
	if _, err := eras.ReferencedScriptSize(tx, empty); err == nil {
		t.Fatal("dingo now ignores absent inputs; move the corpus cases out of corpus/divergences")
	}
	if _, err := lcommon.ConsumedReferenceScriptSize(tx, lcommon.ResolvedUtxos{}); err == nil {
		t.Fatal("gouroboros now ignores an absent reference input")
	}
	// Only the spending input absent: gouroboros counts nothing, dingo fails.
	refOnly := lcommon.ResolvedUtxos{{Id: tx.ReferenceInputs()[0], Output: mustOut(t, feeBabbageOut(t, 5_000_000, 100))}}
	size, err := lcommon.ConsumedReferenceScriptSize(tx, refOnly)
	if err != nil || size != 100 {
		t.Fatalf("gouroboros size = %d, %v; want 100 with the spending input skipped", size, err)
	}
	if _, err := eras.ReferencedScriptSize(tx, resolvedState{utxos: refOnly}); err == nil {
		t.Fatal("dingo now skips an absent spending input")
	}
}

func mustOut(t *testing.T, raw []byte) lcommon.TransactionOutput {
	t.Helper()
	o, err := ledger.NewTransactionOutputFromCbor(raw)
	if err != nil {
		t.Fatal(err)
	}
	return o
}

// fee.script-integrity-hash, a language without a cost model. The reference
// hashes the language view with a null cost model (getLanguageView encodes
// null for a model the parameters lack); gouroboros EncodeLangViews fails with
// a missing-cost-model error.
//
// A transaction using that language is rejected by both: the reference raises
// NoCostModel while collecting the Plutus scripts (Alonzo/Rules/Utxow.hs), and
// gouroboros returns MissingCostModelError from the script-data-hash rule.
func TestDivergenceMissingCostModelFailsLangViews(t *testing.T) {
	t.Parallel()
	_, err := lcommon.EncodeLangViews(map[uint]struct{}{2: {}}, map[uint][]int64{})
	if err == nil {
		t.Fatal("gouroboros now encodes a missing cost model; move the corpus case out of corpus/divergences")
	}
}

// fee.value-balance, deposit and refund validation inside the balance. The
// reference sums the deposits the parameters and state define (getTotalDeposits,
// getTotalRefunds) and leaves a certificate's own amount to the DELEG and GOV
// rules (IncorrectDepositDELEG, DepositIncorrectDELEG, StakeKeyNotRegisteredDELEG,
// ProposalDepositIncorrect). gouroboros checks them while summing and fails.
//
// The transaction is rejected by both nodes; only the rule that reports it
// differs.
func TestDivergenceBalanceValidatesCertificateAmounts(t *testing.T) {
	t.Parallel()
	hash := bytes.Repeat([]byte{0x33}, 28)
	txid := bytes.Repeat([]byte{0x01}, 32)
	reward := feeMustCbor(t, append([]byte{0xe1}, bytes.Repeat([]byte{0x22}, 28)...))
	anchor := feeCat([]byte{0x82}, feeMustCbor(t, "https://example.com/a"), feeMustCbor(t, bytes.Repeat([]byte{0x77}, 32)))
	cred := feeCat([]byte{0x82, 0x00}, feeMustCbor(t, hash))
	ls := &balanceState{
		utxos:    lcommon.ResolvedUtxos{},
		accounts: map[balanceCred]uint64{{lcommon.CredentialTypeAddrKeyHash, lcommon.Blake2b224(hash)}: 2_000_000},
		pools:    map[lcommon.PoolKeyHash]*uint64{},
	}
	pp := &conway.ConwayProtocolParameters{KeyDeposit: 2_000_000, GovActionDeposit: 100_000_000_000}
	for name, extra := range map[string][2][]byte{
		"registration deposit":    {{0x04}, feeCat([]byte{0x81, 0x83, 0x07}, cred, feeMustCbor(t, uint64(1_000_000)))},
		"deregistration refund":   {{0x04}, feeCat([]byte{0x81, 0x83, 0x08}, cred, feeMustCbor(t, uint64(1_000_000)))},
		"unregistered credential": {{0x04}, feeCat([]byte{0x81, 0x82, 0x01}, feeCat([]byte{0x82, 0x00}, feeMustCbor(t, bytes.Repeat([]byte{0x44}, 28))))},
		"proposal deposit":        {{0x14}, feeCat([]byte{0x81, 0x84}, feeMustCbor(t, uint64(1_000_000)), reward, []byte{0x81, 0x06}, anchor)},
	} {
		body := feeEntries(t,
			[2][]byte{{0x00}, feeMustCbor(t, []any{[]any{txid, uint(0)}})},
			[2][]byte{{0x01}, feeCat([]byte{0x81}, feeOutputArray(t, feeMustCbor(t, uint64(1_000_000))))},
			[2][]byte{{0x02}, feeMustCbor(t, uint64(200_000))},
			extra,
		)
		tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, feeCat([]byte{0x84}, body, []byte{0xa0, 0xf5, 0xf6}))
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		if _, err := conway.ComputeValueBalance(tx, 0, ls, pp); err == nil {
			t.Fatalf("%s: gouroboros now only sums; move the corpus case out of corpus/divergences", name)
		}
	}
}
