package main

import (
	"bytes"
	"testing"

	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
)

// Divergence confirmations call gouroboros directly, bypassing the dxnode-go
// adapters, so a reported mismatch cannot come from request translation.

// praos.nonce-rolling and praos.nonce-candidate, neutral evolving nonce. The
// reference computes NeutralNonce ⭒ Nonce c = Nonce c. Gouroboros
// CalculateRollingNonce reads 32 zero bytes as neutral but, instead of
// returning the contribution, hashes it as blake2b_256(c).
func TestDivergenceNeutralEvolvingIsHashed(t *testing.T) {
	t.Parallel()
	c := bytes.Repeat([]byte{0x77}, 32)
	got, err := lcommon.CalculateRollingNonce(make([]byte, 32), c)
	if err != nil {
		t.Fatal(err)
	}
	if bytes.Equal(got.Bytes(), c) {
		t.Fatal("gouroboros now returns the contribution for a neutral evolving nonce; move the corpus cases out of corpus/divergences")
	}
	if want := b2b256(c); !bytes.Equal(got.Bytes(), want) {
		t.Fatalf("result %x is neither the contribution nor its hash %x; re-derive the divergence", got.Bytes(), want)
	}
	// A non-neutral evolving nonce folds as the reference does: a ⭒ c =
	// blake2b_256(a || c).
	a := bytes.Repeat([]byte{0xaa}, 32)
	folded, err := lcommon.CalculateRollingNonce(a, c)
	if err != nil {
		t.Fatal(err)
	}
	if want := b2b256(a, c); !bytes.Equal(folded.Bytes(), want) {
		t.Fatalf("a ⭒ c = %x, want %x", folded.Bytes(), want)
	}
}
