package main

import (
	"bytes"
	"context"
	"testing"

	"github.com/blinklabs-io/dingo/chain"
	"github.com/chrisguiney/dxdingo/internal/praosfixtures"

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

// praos.header-envelope, header on the empty chain. The reference
// (validateEnvelope at Origin) needs block number 0 and the genesis hash as
// previous hash. Dingo's chain queues the first header of an empty chain
// without a position check, whatever its block number and previous hash.
// A real chain reaches it only as a chain that starts after Byron, such as a
// devnet beginning in a later era.
func TestDivergenceOriginHasNoPositionCheck(t *testing.T) {
	t.Parallel()
	for _, name := range []string{"shelley", "conway_a"} {
		h := praosfixtures.ByName(name)
		d := decode(t, h)
		if d.blockNo == 0 {
			t.Fatalf("%s: fixture has block number 0", name)
		}
		cm, err := chain.NewManager(context.Background(), nil, nil)
		if err != nil {
			t.Fatal(err)
		}
		if err := cm.PrimaryChain().AddBlockHeader(context.Background(), d.hdr); err != nil {
			t.Fatalf("%s: dingo now rejects a first header (%v); move the corpus cases out of corpus/divergences", name, err)
		}
	}
}
