package main

import (
	"bytes"
	"testing"

	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"golang.org/x/crypto/blake2b"
)

// Divergence confirmations call dingo directly, bypassing the dxnode-go
// adapters, so a reported mismatch cannot come from request translation.

// praos.nonce-epoch, neutral candidate. The reference computes
// (NeutralNonce ⭒ lab) ⭒ NeutralNonce = lab. Dingo has no separate neutral
// value for the candidate: AssembleEpochNonce requires 32 bytes, and
// IsNeutralNonce classifies 32 zero bytes as neutral. With a lab present it
// nonetheless hashes those zero bytes as a real nonce.
func TestDivergenceNeutralCandidateIsHashed(t *testing.T) {
	zero := make([]byte, 32)
	lab := bytes.Repeat([]byte{0xbb}, 32)

	if !epochengine.IsNeutralNonce(zero) {
		t.Fatal("dingo no longer treats 32 zero bytes as the neutral nonce; revisit the adapter mapping")
	}
	if _, err := epochengine.AssembleEpochNonce(nil, lab, nil); err == nil {
		t.Fatal("dingo now accepts an empty candidate; the adapter should pass the neutral nonce through as empty")
	}

	got, err := epochengine.AssembleEpochNonce(zero, lab, nil)
	if err != nil {
		t.Fatal(err)
	}
	hashed := blake2b.Sum256(append(bytes.Clone(zero), lab...))
	if !bytes.Equal(got, hashed[:]) {
		t.Fatalf("dingo result %x is neither the hash of the zero candidate nor expected; re-derive the divergence", got)
	}
	if bytes.Equal(got, lab) {
		t.Fatal("dingo now matches the reference; move the corpus case out of corpus/divergences")
	}

	// The same zero candidate without a lab is treated as neutral, so dingo
	// is inconsistent with itself, not only with the reference.
	alone, err := epochengine.AssembleEpochNonce(zero, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(alone, zero) {
		t.Fatalf("zero candidate alone = %x, want it returned as neutral", alone)
	}
	extra := bytes.Repeat([]byte{0xcc}, 32)
	withExtra, err := epochengine.AssembleEpochNonce(zero, nil, extra)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(withExtra, extra) {
		t.Fatalf("zero candidate with extra entropy = %x, want extra entropy (neutral candidate)", withExtra)
	}
}
