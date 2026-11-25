package main

import (
	"bytes"
	"testing"

	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
)

// crypto.script-hash, native script followed by one extra byte. The reference
// decodes with decodeFullAnnotator, which requires the whole input to be
// consumed, so the bytes are a DeserialiseFailure. gouroboros'
// NativeScript.UnmarshalCBOR parses the first item and ignores what follows.
// Called directly, so the adapter cannot be the source of the difference.
func TestDivergenceNativeScriptTrailingByte(t *testing.T) {
	t.Parallel()
	script := append([]byte{0x82, 0x00, 0x58, 0x1c}, bytes.Repeat([]byte{0x01}, 28)...)

	var exact lcommon.NativeScript
	if err := exact.UnmarshalCBOR(script); err != nil {
		t.Fatalf("the script alone no longer decodes: %v", err)
	}
	var padded lcommon.NativeScript
	if err := padded.UnmarshalCBOR(append(bytes.Clone(script), 0x00)); err != nil {
		t.Fatal("gouroboros now rejects trailing bytes after a native script; move the corpus case out of corpus/divergences")
	}
	if padded.Hash() == exact.Hash() {
		t.Fatal("the padded script hashes like the exact one; re-derive the divergence")
	}
}
