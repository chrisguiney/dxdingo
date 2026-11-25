package main

import (
	"bytes"
	"strings"
	"testing"

	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"
	"github.com/blinklabs-io/gouroboros/ledger/byron"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
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

// byron.header-validate, header signed for another network. gouroboros'
// ValidatePBFTHeader compares the header's embedded magic with the
// configuration before looking at the signature and reports it as such. The
// reference's PBFT check has no such comparison (the ledger's updateBlock
// compares the magic, outside PBFT): the signing tag carries the node's magic,
// so the same header fails as an invalid signature. Called directly.
func TestDivergenceHeaderProtocolMagicChecked(t *testing.T) {
	t.Parallel()
	c := loadPreprod(t)
	header, err := byron.NewByronMainBlockHeaderFromCbor(c.headers[4])
	if err != nil {
		t.Fatal(err)
	}
	cfg, err := byronPBFTConfig(1, 2160, &v1.ByronChainState{Delegations: c.dlg})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := byronconsensus.ValidatePBFTHeader(header, cfg); err != nil {
		t.Fatalf("the header no longer validates under its own magic: %v", err)
	}
	cfg.ProtocolMagic = 764824073
	_, err = byronconsensus.ValidatePBFTHeader(header, cfg)
	if err == nil || !strings.Contains(err.Error(), "protocol magic") {
		t.Fatalf("error = %v, want a protocol magic error; if gouroboros now fails the signature, move the corpus case out of corpus/divergences", err)
	}
}
