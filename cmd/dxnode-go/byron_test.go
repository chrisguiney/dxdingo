package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func byronRat(n, d string) *v1.Rational { return &v1.Rational{Numerator: n, Denominator: d} }

// calculateTxSizeLinear (TxSizeLinear a b) size = a + ceiling (b * size).
// Mainnet's policy is a = 155381, b = 43946000 nano = 0.043946.
func TestByronMinFee(t *testing.T) {
	t.Parallel()
	mainnetB := byronRat("43946", "1000000")
	for name, tc := range map[string]struct {
		a, b *v1.Rational
		size uint64
		want uint64
	}{
		"mainnet 200 bytes: ceil(8.7892) = 9": {byronRat("155381", "1"), mainnetB, 200, 155390},
		"zero size is the summand":            {byronRat("155381", "1"), mainnetB, 0, 155381},
		"exact product is not rounded up":     {byronRat("0", "1"), byronRat("1", "2"), 10, 5},
		"half a lovelace rounds up":           {byronRat("0", "1"), byronRat("1", "2"), 11, 6},
		"zero multiplier":                     {byronRat("7", "1"), byronRat("0", "1"), 1_000_000, 7},
		"summand at the Lovelace bound":       {byronRat("45000000000000000", "1"), byronRat("0", "1"), 5, 45000000000000000},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := byronMinFee(context.Background(), &v1.ByronMinFeeRequest{TxFeeA: tc.a, TxFeeB: tc.b, Size: tc.size})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetFee() != tc.want {
				t.Fatalf("fee = %v, want %d", got, tc.want)
			}
		})
	}
}

func TestByronMinFeeRejectsOverflow(t *testing.T) {
	t.Parallel()
	got, err := byronMinFee(context.Background(), &v1.ByronMinFeeRequest{
		TxFeeA: byronRat("45000000000000000", "1"), TxFeeB: byronRat("1", "1"), Size: 1,
	})
	if err != nil {
		t.Fatal(err)
	}
	if f := got.GetRejection().GetFailures(); len(f) != 1 || f[0].GetName() != "LovelaceOverflow" {
		t.Fatalf("a + b*size above maxLovelaceVal: %v, want LovelaceOverflow", got)
	}
}

func TestByronMinFeeDomain(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		a, b *v1.Rational
		want v1.HarnessErrorCode
	}{
		"fractional summand":     {byronRat("1", "2"), byronRat("0", "1"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"negative summand":       {byronRat("-1", "1"), byronRat("0", "1"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"summand above Lovelace": {byronRat("45000000000000001", "1"), byronRat("0", "1"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"negative multiplier":    {byronRat("0", "1"), byronRat("-1", "2"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"multiplier below Nano":  {byronRat("0", "1"), byronRat("1", "3"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"malformed rational":     {byronRat("x", "1"), byronRat("0", "1"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"missing multiplier":     {byronRat("0", "1"), nil, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			_, err := byronMinFee(context.Background(), &v1.ByronMinFeeRequest{TxFeeA: tc.a, TxFeeB: tc.b, Size: 1})
			if dispatch.Code(err) != tc.want {
				t.Fatalf("code = %v (%v), want %v", dispatch.Code(err), err, tc.want)
			}
		})
	}
}

const byronMainnetMagic = 764824073

func byronTestParams() *v1.ByronProtocolParameters {
	return &v1.ByronProtocolParameters{
		TxFeeA: byronRat("155381", "1"), TxFeeB: byronRat("43946", "1000000"),
		MaxTxSize: 4096, MaxBlockSize: 2000000, MaxHeaderSize: 2000000,
	}
}

type byronTxCase struct {
	req *v1.ByronTxValidateRequest
	// spend is the TxIn of the funded output.
	spend byronfixture.TxIn
}

type byronTxOpts struct {
	spend     *byronfixture.TxIn // overrides the funded output's TxIn
	outs      [][]byte           // overrides the single output
	signer    *byronfixture.Key  // overrides key 1
	signID    *[32]byte          // overrides the signed TxId
	maxTxSize uint64             // overrides the limit
}

// byronFundedTx funds one 10 ADA output at key 1's address and builds a
// transaction spending it to key 2's address, leaving `fee` lovelace, with the
// witness re-signed for the final body unless an option says otherwise.
func byronFundedTx(fee uint64, o byronTxOpts) *byronTxCase {
	k1, k2 := byronfixture.NewKey(1), byronfixture.NewKey(2)
	const funded = 10_000_000
	funding := byronfixture.TxIn{ID: [32]byte{0x11}, Index: 0}
	utxo := []*v1.UtxoEntry{{
		Txin:  &v1.TxIn{TxId: funding.ID[:], Index: 0},
		Txout: byronfixture.TxOut(k1.Address(nil), funded),
	}}
	spend := funding
	if o.spend != nil {
		spend = *o.spend
	}
	outs := o.outs
	if outs == nil {
		outs = [][]byte{byronfixture.TxOut(k2.Address(nil), funded-fee)}
	}
	signer := k1
	if o.signer != nil {
		signer = *o.signer
	}
	body, id := byronfixture.Tx([]byronfixture.TxIn{spend}, outs)
	if o.signID != nil {
		id = *o.signID
	}
	pp := byronTestParams()
	if o.maxTxSize != 0 {
		pp.MaxTxSize = o.maxTxSize
	}
	return &byronTxCase{spend: spend, req: &v1.ByronTxValidateRequest{
		ProtocolMagic: byronMainnetMagic, Pparams: pp, Utxo: utxo,
		Tx: byronfixture.TxAux(body, signer.Witness(byronMainnetMagic, id)),
	}}
}

func byronNames(r *v1.Rejection) []string {
	var out []string
	for _, f := range r.GetFailures() {
		out = append(out, f.GetName())
	}
	return out
}

func TestByronTxValidateAccepts(t *testing.T) {
	t.Parallel()
	c := byronFundedTx(1_000_000, byronTxOpts{})
	got, err := byronTxValidate(context.Background(), c.req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || got.GetFee() != 1_000_000 {
		t.Fatalf("response = %v, want fee 1000000 and no rejection", got)
	}
	if len(got.GetUtxoRemoved()) != 1 || !bytes.Equal(got.GetUtxoRemoved()[0].GetTxId(), c.spend.ID[:]) {
		t.Fatalf("removed = %v, want the spent input", got.GetUtxoRemoved())
	}
	if len(got.GetUtxoAdded()) != 1 || got.GetUtxoAdded()[0].GetTxin().GetIndex() != 0 {
		t.Fatalf("added = %v, want one output", got.GetUtxoAdded())
	}
}

// Each case violates exactly one rule of validateTxAux, validateTx or
// updateUTxOTxWitness. The expected names are the reference constructors.
func TestByronTxValidateRejects(t *testing.T) {
	t.Parallel()
	k2 := byronfixture.NewKey(2)
	testnet := uint32(42)
	wrongID := [32]byte{0xee}
	absent := byronfixture.TxIn{ID: [32]byte{0x99}}
	for name, tc := range map[string]struct {
		c    *byronTxCase
		want string
	}{
		"fee below the minimum":              {byronFundedTx(1_000, byronTxOpts{}), "TxValidationFeeTooSmall"},
		"outputs exceed inputs":              {byronFundedTx(0, byronTxOpts{outs: [][]byte{byronfixture.TxOut(k2.Address(nil), 10_000_001)}}), "TxValidationLovelaceError"},
		"witness signs another transaction":  {byronFundedTx(1_000_000, byronTxOpts{signID: &wrongID}), "TxValidationWitnessWrongSignature"},
		"witness key does not own the input": {byronFundedTx(1_000_000, byronTxOpts{signer: &k2}), "TxValidationWitnessWrongKey"},
		"output on another network":          {byronFundedTx(0, byronTxOpts{outs: [][]byte{byronfixture.TxOut(k2.Address(&testnet), 9_000_000)}}), "TxValidationNetworkMagicMismatch"},
		"input not in the UTxO":              {byronFundedTx(1_000_000, byronTxOpts{spend: &absent}), "UTxOMissingInput"},
		"transaction over the size limit":    {byronFundedTx(1_000_000, byronTxOpts{maxTxSize: 100}), "TxValidationTxTooLarge"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := byronTxValidate(context.Background(), tc.c.req)
			if err != nil {
				t.Fatal(err)
			}
			if names := byronNames(got.GetRejection()); len(names) != 1 || names[0] != tc.want {
				t.Fatalf("rejection = %v, want only %s (%v)", names, tc.want, got)
			}
		})
	}
	t.Run("undecodable transaction", func(t *testing.T) {
		t.Parallel()
		c := byronFundedTx(1_000_000, byronTxOpts{})
		c.req.Tx = []byte{0xff}
		got, err := byronTxValidate(context.Background(), c.req)
		if err != nil {
			t.Fatal(err)
		}
		if names := byronNames(got.GetRejection()); len(names) != 1 || names[0] != "DeserialiseFailure" {
			t.Fatalf("rejection = %v, want DeserialiseFailure", names)
		}
	})
}

// The reference reaches failures in a fixed order and stops at the first, so a
// transaction that fails several rules reports the earliest: an unresolved
// input before a bad witness, and a balance failure before the fee comparison.
func TestByronTxValidateReportsFirstFailureInReferenceOrder(t *testing.T) {
	t.Parallel()
	k2 := byronfixture.NewKey(2)
	absent := byronfixture.TxIn{ID: [32]byte{0x99}}
	for name, tc := range map[string]struct {
		c    *byronTxCase
		want string
	}{
		"missing input also fails balance and fee":             {byronFundedTx(1_000_000, byronTxOpts{spend: &absent}), "UTxOMissingInput"},
		"negative fee is a balance failure, not a fee failure": {byronFundedTx(0, byronTxOpts{outs: [][]byte{byronfixture.TxOut(k2.Address(nil), 10_000_001)}}), "TxValidationLovelaceError"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := byronTxValidate(context.Background(), tc.c.req)
			if err != nil {
				t.Fatal(err)
			}
			if names := byronNames(got.GetRejection()); len(names) != 1 || names[0] != tc.want {
				t.Fatalf("rejection = %v, want only %s", names, tc.want)
			}
		})
	}
}
