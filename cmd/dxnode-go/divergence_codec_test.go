package main

import (
	"bytes"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
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
