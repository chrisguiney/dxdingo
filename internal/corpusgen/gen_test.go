// Package corpusgen writes the seed corpus. The crypto cases need real keys,
// proofs and signatures, so they are generated here from fixed seeds rather
// than written by hand. Regenerate with:
//
//	DXDINGO_GEN_CORPUS=1 go test ./internal/corpusgen/
//
// Divergence cases (confirmed mismatches) go in corpus/divergences, which the
// default replay does not read.
package corpusgen

import (
	"bytes"
	"math/big"
	"os"
	"path/filepath"
	"testing"

	"google.golang.org/protobuf/encoding/prototext"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func rat(n, d string) *v1.Rational { return &v1.Rational{Numerator: n, Denominator: d} }

func rep(b byte, n int) []byte { return bytes.Repeat([]byte{b}, n) }

func flip(b []byte, i int) []byte {
	c := bytes.Clone(b)
	c[i] ^= 1
	return c
}

// fraction returns floor(num/den * 2^(8*size)) as size big-endian bytes.
func fraction(num, den int64, size int) []byte {
	v := new(big.Int).Lsh(big.NewInt(num), uint(8*size))
	v.Quo(v, big.NewInt(den))
	return v.FillBytes(make([]byte, size))
}

type entry struct {
	name, comment string
	call          *v1.Call
}

// writeCorpus writes entries to corpus/<sub>. Each area has its own
// TestGenerate<Area>Corpus in its own file, all gated by DXDINGO_GEN_CORPUS.
func writeCorpus(t *testing.T, sub string, entries []entry) {
	t.Helper()
	if os.Getenv("DXDINGO_GEN_CORPUS") == "" {
		t.Skip("set DXDINGO_GEN_CORPUS=1 to regenerate corpus/")
	}
	dir := filepath.Join("..", "..", "corpus", sub)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		b, err := prototext.MarshalOptions{Multiline: true}.Marshal(e.call)
		if err != nil {
			t.Fatal(err)
		}
		out := append([]byte("# "+e.comment+"\n"), b...)
		if err := os.WriteFile(filepath.Join(dir, e.name+".txtpb"), out, 0o644); err != nil {
			t.Fatal(err)
		}
	}
}
