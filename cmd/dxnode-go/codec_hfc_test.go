package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

var (
	gouroborosDirOnce sync.Once
	gouroborosDir     string
)

// gouroborosBlock reads a mainnet block hex file from the gouroboros
// checkout the build resolves.
func gouroborosBlock(t *testing.T, name string) []byte {
	t.Helper()
	gouroborosDirOnce.Do(func() {
		out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", "github.com/blinklabs-io/gouroboros").Output()
		if err != nil {
			t.Fatal(err)
		}
		gouroborosDir = strings.TrimSpace(string(out))
	})
	raw, err := os.ReadFile(filepath.Join(gouroborosDir, "internal", "testdata", name))
	if err != nil {
		t.Fatal(err)
	}
	b, err := hex.DecodeString(strings.TrimSpace(string(raw)))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// The Cardano disk-form tags: Byron boundary blocks are 0 and regular blocks
// 1, so the Byron encoding is kept as is, and each later era follows.
func TestHfcTags(t *testing.T) {
	t.Parallel()
	want := map[uint]struct {
		era   v1.Era
		block uint
	}{
		0: {v1.Era_ERA_BYRON, ledger.BlockTypeByronEbb}, 1: {v1.Era_ERA_BYRON, ledger.BlockTypeByronMain},
		2: {v1.Era_ERA_SHELLEY, ledger.BlockTypeShelley}, 3: {v1.Era_ERA_ALLEGRA, ledger.BlockTypeAllegra},
		4: {v1.Era_ERA_MARY, ledger.BlockTypeMary}, 5: {v1.Era_ERA_ALONZO, ledger.BlockTypeAlonzo},
		6: {v1.Era_ERA_BABBAGE, ledger.BlockTypeBabbage}, 7: {v1.Era_ERA_CONWAY, ledger.BlockTypeConway},
		8: {v1.Era_ERA_DIJKSTRA, ledger.BlockTypeDijkstra},
	}
	for tag, w := range want {
		got, ok := hfcTags[tag]
		if !ok || got.era != w.era || got.block != w.block {
			t.Errorf("tag %d: %+v, want %+v", tag, got, w)
		}
	}
	if len(hfcTags) != len(want) {
		t.Errorf("%d tags, want %d", len(hfcTags), len(want))
	}
}

func hfcCall(t *testing.T, version uint32, b []byte) *v1.CodecHfcBlockResponse {
	t.Helper()
	got, err := codecHfcBlock(context.Background(), &v1.CodecHfcBlockRequest{NodeToNodeVersion: version, Cbor: b})
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func disk(idx uint, inner []byte) []byte {
	out := []byte{0x82}
	out = append(out, mustEnc(idx)...)
	return append(out, inner...)
}

func wrap24(b []byte) []byte { return mustEnc(cbor.WrappedCbor(b)) }

func mustEnc(v any) []byte {
	b, err := cbor.Encode(v)
	if err != nil {
		panic(err)
	}
	return b
}

func TestCodecHfcBlockEras(t *testing.T) {
	t.Parallel()
	for _, c := range []struct {
		file string
		idx  uint
		era  v1.Era
		blk  uint
	}{
		{"shelley_block.hex", 2, v1.Era_ERA_SHELLEY, ledger.BlockTypeShelley},
		{"allegra_block.hex", 3, v1.Era_ERA_ALLEGRA, ledger.BlockTypeAllegra},
		{"mary_block.hex", 4, v1.Era_ERA_MARY, ledger.BlockTypeMary},
		{"alonzo_block.hex", 5, v1.Era_ERA_ALONZO, ledger.BlockTypeAlonzo},
		{"babbage_block.hex", 6, v1.Era_ERA_BABBAGE, ledger.BlockTypeBabbage},
		{"conway_block.hex", 7, v1.Era_ERA_CONWAY, ledger.BlockTypeConway},
	} {
		t.Run(c.file, func(t *testing.T) {
			t.Parallel()
			inner := gouroborosBlock(t, c.file)
			blk, err := decodeBlock(c.blk, inner)
			if err != nil {
				t.Fatal(err)
			}
			hash := blk.Hash()
			wire := disk(c.idx, inner)
			for version, form := range map[uint32][]byte{0: wire, 14: wrap24(wire)} {
				got := hfcCall(t, version, form)
				mustAccept(t, got.GetRejection())
				if got.GetEra() != c.era || !bytes.Equal(got.GetHeaderHash(), hash[:]) || !bytes.Equal(got.GetCbor(), form) {
					t.Fatalf("version %d: era %v hash %x cbor equal %v", version, got.GetEra(), got.GetHeaderHash(), bytes.Equal(got.GetCbor(), form))
				}
			}
		})
	}
}

// Byron blocks keep the Byron encoding: tag 0 is a boundary block, tag 1 a
// regular one.
func TestCodecHfcBlockByron(t *testing.T) {
	t.Parallel()
	inner := gouroborosBlock(t, "byron_block.hex")
	wire := disk(1, inner)
	got := hfcCall(t, 0, wire)
	mustAccept(t, got.GetRejection())
	blk, err := decodeBlock(ledger.BlockTypeByronMain, inner)
	if err != nil {
		t.Fatal(err)
	}
	hash := blk.Hash()
	if got.GetEra() != v1.Era_ERA_BYRON || !bytes.Equal(got.GetHeaderHash(), hash[:]) || !bytes.Equal(got.GetCbor(), wire) {
		t.Fatalf("era %v hash %x", got.GetEra(), got.GetHeaderHash())
	}
	mustReject(t, hfcCall(t, 0, disk(0, inner)).GetRejection(), "regular block under the boundary tag")
}

func TestCodecHfcBlockRejections(t *testing.T) {
	t.Parallel()
	conway := gouroborosBlock(t, "conway_block.hex")
	good := disk(7, conway)
	for name, c := range map[string]struct {
		version uint32
		b       []byte
	}{
		"disk truncated":       {0, good[:len(good)-1]},
		"disk trailing":        {0, append(bytes.Clone(good), 0)},
		"era index mismatch":   {0, disk(5, conway)},
		"era tag out of range": {0, disk(9, conway)},
		"not an array":         {0, []byte{0x01}},
		"three elements":       {0, append([]byte{0x83, 0x07}, append(bytes.Clone(conway), 0x00)...)},
		"n2n without tag 24":   {14, good},
		"n2n truncated":        {14, wrap24(good)[:len(wrap24(good))-1]},
		"disk form as n2n tag": {0, wrap24(good)},
	} {
		mustReject(t, hfcCall(t, c.version, c.b).GetRejection(), name)
	}
}

// dijkstraHeader is the header of the consensus Dijkstra golden block.
func dijkstraHeader(t *testing.T) []byte {
	t.Helper()
	raw, err := os.ReadFile("testdata/dijkstra_header.hex")
	if err != nil {
		t.Fatal(err)
	}
	b, err := hex.DecodeString(strings.TrimSpace(string(raw)))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// dijkstraBlock is [header, body]; the body is gouroboros's three-component
// layout [transactions, leios certificate, peras certificate], here empty.
func dijkstraBlock(t *testing.T) []byte {
	t.Helper()
	b := append([]byte{0x82}, dijkstraHeader(t)...)
	return append(b, 0x83, 0x80, 0xf6, 0xf6)
}

// Tag 8 is Dijkstra, in the disk and the node-to-node form. The header hash
// is blake2b-256 of the header bytes.
func TestCodecHfcBlockDijkstra(t *testing.T) {
	t.Parallel()
	inner := dijkstraBlock(t)
	want, err := hex.DecodeString("1a68d6908bdfec726530a7da46bdda2a459e12ea386679e3807dff01d2e1ecb3")
	if err != nil {
		t.Fatal(err)
	}
	wire := disk(8, inner)
	for version, form := range map[uint32][]byte{0: wire, 14: wrap24(wire)} {
		got := hfcCall(t, version, form)
		mustAccept(t, got.GetRejection())
		if got.GetEra() != v1.Era_ERA_DIJKSTRA || !bytes.Equal(got.GetHeaderHash(), want) || !bytes.Equal(got.GetCbor(), form) {
			t.Fatalf("version %d: era %v hash %x cbor equal %v", version, got.GetEra(), got.GetHeaderHash(), bytes.Equal(got.GetCbor(), form))
		}
	}
}

// A payload that is no Dijkstra block under tag 8 is a decode failure, not
// UNSUPPORTED.
func TestCodecHfcBlockDijkstraRejections(t *testing.T) {
	t.Parallel()
	good := dijkstraBlock(t)
	for name, b := range map[string][]byte{
		"empty payload": disk(8, []byte{0x80}),
		"no payload":    {0x82, 0x08},
		"junk payload":  {0x82, 0x08, 0xff},
		"truncated":     disk(8, good[:len(good)-1]),
		"trailing":      append(disk(8, good), 0),
		"conway block":  disk(8, gouroborosBlock(t, "conway_block.hex")),
	} {
		mustReject(t, hfcCall(t, 0, b).GetRejection(), name)
	}
	mustReject(t, hfcCall(t, 14, wrap24(disk(8, []byte{0x80}))).GetRejection(), "n2n empty payload")
}
