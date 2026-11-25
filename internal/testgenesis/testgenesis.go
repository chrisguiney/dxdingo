// Package testgenesis reads full-length Plutus cost models from the preview
// network's genesis files, for fork tests and the corpus generator.
package testgenesis

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"runtime"

	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
)

// dir finds the dingo checkout the module builds against through the
// go.work replace, since the worktree's own third_party is not populated.
func dir() string {
	_, file, _, _ := runtime.Caller(0)
	root := filepath.Join(filepath.Dir(file), "..", "..")
	b, err := os.ReadFile(filepath.Join(root, "go.work"))
	if err != nil {
		panic(err)
	}
	m := regexp.MustCompile(`github.com/blinklabs-io/dingo\s*=>\s*(\S+)`).FindSubmatch(b)
	if m == nil {
		panic("go.work has no dingo replace")
	}
	return filepath.Join(string(m[1]), "config", "cardano", "preview")
}

func load(name string, into any) {
	b, err := os.ReadFile(filepath.Join(dir(), name))
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(b, into); err != nil {
		panic(err)
	}
}

// Alonzo is the preview Alonzo genesis.
func Alonzo() *alonzo.AlonzoGenesis {
	var g alonzo.AlonzoGenesis
	load("alonzo-genesis.json", &g)
	return &g
}

// Conway is the preview Conway genesis.
func Conway() *conway.ConwayGenesis {
	var g conway.ConwayGenesis
	load("conway-genesis.json", &g)
	return &g
}
