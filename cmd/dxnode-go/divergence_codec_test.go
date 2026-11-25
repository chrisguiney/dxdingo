package main

import (
	"bytes"
	"encoding/hex"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/byron"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	"github.com/chrisguiney/dxdingo/internal/byronfixture"
)

// Divergence confirmations call gouroboros directly, bypassing dxnode-go, so
// a reported mismatch cannot come from request translation. The counterpart
// checks are the "divergence:" entries in haskell/dxnode-hs/test/Test/Codec.hs.

// The reference decodes an output and re-serialises it from its value;
// gouroboros keeps the bytes it was given. Three mainnet-shaped encodings the
// reference rewrites: an empty multi-asset after the coin, a map-format
// output that only carries a datum hash, and a multi-asset map in
// canonical-CBOR order.
func TestDivergenceCodecOutputKeepsWireBytes(t *testing.T) {
	t.Parallel()
	addr := append([]byte{0x58, 0x1d}, testAddr()...)
	emptyMultiAsset := append(append([]byte{0x82}, addr...), 0x82, 0x1a, 0x1a, 0x1e, 0x85, 0x28, 0xa0)
	datumHashMap := append(append([]byte{0xa3, 0x00}, addr...), 0x01, 0x1a, 0x00, 0x16, 0xe3, 0x60, 0x02, 0x82, 0x00, 0x58, 0x20)
	datumHashMap = append(datumHashMap, bytes.Repeat([]byte{0x33}, 32)...)
	// {policy: {"b": 7, "aa": 5}}: length-first key order on the wire.
	assets := append(append([]byte{0x82}, addr...), 0x82, 0x1a, 0x00, 0x1e, 0x84, 0x80, 0xa1, 0x58, 0x1c)
	assets = append(assets, h28...)
	assets = append(assets, 0xa2, 0x41, 'b', 0x07, 0x42, 'a', 'a', 0x05)
	for name, wire := range map[string][]byte{
		"empty multi-asset": emptyMultiAsset,
		"datum hash map":    datumHashMap,
		"asset order":       assets,
	} {
		out, err := ledger.NewBabbageTransactionOutputFromCbor(wire)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		if !bytes.Equal(out.Cbor(), wire) {
			t.Fatalf("%s: Cbor() = %x, want the wire bytes %x", name, out.Cbor(), wire)
		}
	}
}

// A three-element transaction is accepted as Conway and kept as three
// elements; the reference re-encodes it with the IsValid element.
func TestDivergenceCodecThreeElementTx(t *testing.T) {
	t.Parallel()
	wire := unhex(t, "83 a3 00 80 01 80 02 00 a0 f6")
	tx, err := ledger.NewConwayTransactionFromCbor(wire)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(tx.Cbor(), wire) || !tx.IsValid() {
		t.Fatalf("Cbor() = %x, IsValid %v", tx.Cbor(), tx.IsValid())
	}
}

// {18: {0: [1, 2, 3]}}: gouroboros takes a PlutusV1 cost model of any length
// in an Alonzo update; the reference decoder rejects it.
func TestDivergenceCodecAlonzoCostModelLength(t *testing.T) {
	t.Parallel()
	var u alonzo.AlonzoProtocolParameterUpdate
	if _, err := cbor.DecodeExact(unhex(t, "a1 12 a1 00 83 01 02 03"), &u); err != nil {
		t.Fatal(err)
	}
	if got := u.CostModels[0]; len(got) != 3 {
		t.Fatalf("cost model = %v", got)
	}
}

// A Dijkstra block body is three components in gouroboros, [transactions,
// leios certificate, peras certificate]. The reference writes and reads four,
// with the invalid-transaction set first, so no block is accepted by both.
func TestDivergenceDijkstraBlockBodyComponents(t *testing.T) {
	t.Parallel()
	block := func(body ...byte) []byte {
		return append(append([]byte{0x82}, dijkstraHeader(t)...), body...)
	}
	three := block(0x83, 0x80, 0xf6, 0xf6)
	four := block(0x84, 0xf6, 0x80, 0xf6, 0xf6)
	if _, err := ledger.NewBlockFromCbor(ledger.BlockTypeDijkstra, three, common.VerifyConfig{SkipBodyHashValidation: true}); err != nil {
		t.Fatalf("three-component body: %v, want accepted", err)
	}
	if _, err := ledger.NewBlockFromCbor(ledger.BlockTypeDijkstra, four, common.VerifyConfig{SkipBodyHashValidation: true}); err == nil {
		t.Fatal("four-component body accepted, want rejected")
	}
}

// The Dijkstra CDDL drops the deposit-less registration and deregistration
// (tags 0 and 1) and genesis delegation (tag 5). gouroboros decodes them as
// certificates whatever the era; the reference rejects them in Dijkstra.
func TestDivergenceDijkstraExpungedCertificates(t *testing.T) {
	t.Parallel()
	key := append([]byte{0x82, 0x00, 0x58, 0x1c}, h28...)
	for name, wire := range map[string][]byte{
		"tag 0 registration":     append([]byte{0x82, 0x00}, key...),
		"tag 1 deregistration":   append([]byte{0x82, 0x01}, key...),
		"tag 5 genesis delegate": unhex(t, "84 05 581c"+hex.EncodeToString(h28)+" 581c"+hex.EncodeToString(bytes.Repeat([]byte{0x22}, 28))+" 5820"+hex.EncodeToString(bytes.Repeat([]byte{0x33}, 32))),
	} {
		var w common.CertificateWrapper
		if _, err := cbor.DecodeExact(wire, &w); err != nil {
			t.Fatalf("%s: %v, want accepted", name, err)
		}
	}
}

// A Dijkstra transaction in the mempool form [body, witnesses, true, auxiliary
// data] keeps its four elements in gouroboros; the reference re-encodes the
// three-element transaction.
func TestDivergenceDijkstraMempoolTxKeepsWireBytes(t *testing.T) {
	t.Parallel()
	wire := unhex(t, "84 a3 00 80 01 80 02 00 a0 f5 f6")
	tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeDijkstra, wire)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(tx.Cbor(), wire) {
		t.Fatalf("Cbor() = %x, want the wire bytes %x", tx.Cbor(), wire)
	}
}

// A Byron transaction with definite-length input and output lists decodes in
// gouroboros; the reference reads both lists as indefinite-length only.
func TestDivergenceByronTxDefiniteLists(t *testing.T) {
	t.Parallel()
	k := byronfixture.NewKey(1)
	in := byronfixture.TxIn{ID: [32]byte{9}, Index: 1}
	out := byronfixture.TxOut(k.Address(nil), 1_000_000)
	inWire := byronfixture.Array(byronfixture.Uint(0), byronfixture.Tag24(byronfixture.Array(byronfixture.Bytes(in.ID[:]), byronfixture.Uint(1))))
	body := byronfixture.Array(byronfixture.Array(inWire), byronfixture.Array(out), byronfixture.EmptyMap)
	_, id := byronfixture.Tx([]byronfixture.TxIn{in}, [][]byte{out})
	wire := byronfixture.TxAux(body, k.Witness(764824073, id))
	if _, err := byron.NewByronTransactionFromCbor(wire); err != nil {
		t.Fatalf("definite lists: %v, want accepted", err)
	}
}
