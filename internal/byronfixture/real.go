package byronfixture

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
)

// ModuleDir returns the source directory of a module in the workspace, so
// tests and the corpus generator read the real chain data those modules ship.
func ModuleDir(module string) (string, error) {
	out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", module).Output()
	if err != nil {
		return "", fmt.Errorf("locate %s: %w", module, err)
	}
	return strings.TrimSpace(string(out)), nil
}

// Delegation is a genesis key and the delegate that signs blocks for it, each
// as the 64-byte extended verification key.
type Delegation struct{ Issuer, Delegate []byte }

// PreprodDelegations reads the heavy delegation certificates of the preprod
// Byron genesis.
func PreprodDelegations(dingoDir string) ([]Delegation, error) {
	raw, err := os.ReadFile(filepath.Join(dingoDir, "config/cardano/preprod/byron-genesis.json"))
	if err != nil {
		return nil, err
	}
	var g struct {
		HeavyDelegation map[string]struct {
			IssuerPk   string `json:"issuerPk"`
			DelegatePk string `json:"delegatePk"`
		} `json:"heavyDelegation"`
	}
	if err := json.Unmarshal(raw, &g); err != nil {
		return nil, err
	}
	var keys []string
	for k := range g.HeavyDelegation {
		keys = append(keys, k)
	}
	sortStrings(keys)
	var out []Delegation
	for _, k := range keys {
		v := g.HeavyDelegation[k]
		iss, err := base64.StdEncoding.DecodeString(v.IssuerPk)
		if err != nil {
			return nil, err
		}
		del, err := base64.StdEncoding.DecodeString(v.DelegatePk)
		if err != nil {
			return nil, err
		}
		out = append(out, Delegation{Issuer: iss, Delegate: del})
	}
	return out, nil
}

func sortStrings(s []string) {
	for i := 1; i < len(s); i++ {
		for j := i; j > 0 && s[j] < s[j-1]; j-- {
			s[j], s[j-1] = s[j-1], s[j]
		}
	}
}

// Block is one preprod Byron block: its era block type (0 boundary, 1 main)
// and wire CBOR.
type Block struct {
	Type uint
	CBOR []byte
}

// PreprodBlocks returns the preprod Byron chain from the genesis boundary
// block through slot 84242, as served by a preprod relay.
func PreprodBlocks(dingoDir string) ([]Block, error) {
	raw, err := os.ReadFile(filepath.Join(dingoDir, "ledger/testdata/preprod-byron-chain.cbor"))
	if err != nil {
		return nil, err
	}
	var entries []struct {
		cbor.StructAsArray
		Type  uint
		Block []byte
	}
	if _, err := cbor.Decode(raw, &entries); err != nil {
		return nil, err
	}
	out := make([]Block, 0, len(entries))
	for _, e := range entries {
		out = append(out, Block{Type: e.Type, CBOR: e.Block})
	}
	return out, nil
}
