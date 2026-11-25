package rulefixture

import (
	"encoding/hex"
	"fmt"
	"strings"

	"github.com/blinklabs-io/gouroboros/ledger"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// BlockCbor returns the era's embedded mainnet block and its block type.
func BlockCbor(era v1.Era) ([]byte, uint, error) {
	name, typ := "", uint(0)
	switch era {
	case v1.Era_ERA_SHELLEY:
		name, typ = "shelley_block.hex", ledger.BlockTypeShelley
	case v1.Era_ERA_ALLEGRA:
		name, typ = "allegra_block.hex", ledger.BlockTypeAllegra
	case v1.Era_ERA_MARY:
		name, typ = "mary_block.hex", ledger.BlockTypeMary
	case v1.Era_ERA_ALONZO:
		name, typ = "alonzo_block.hex", ledger.BlockTypeAlonzo
	case v1.Era_ERA_BABBAGE:
		name, typ = "babbage_block.hex", ledger.BlockTypeBabbage
	case v1.Era_ERA_CONWAY:
		name, typ = "conway_block.hex", ledger.BlockTypeConway
	default:
		return nil, 0, fmt.Errorf("no block for %v", era)
	}
	raw, err := blocks.ReadFile("testdata/" + name)
	if err != nil {
		return nil, 0, err
	}
	data, err := hex.DecodeString(strings.TrimSpace(string(raw)))
	return data, typ, err
}
