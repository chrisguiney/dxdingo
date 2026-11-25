package byronfixture

import (
	"fmt"

	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/byron"
)

// Chain is the preprod Byron era decoded: the genesis epoch boundary block at
// index 0 and the main blocks after it, with the wire form of each.
type Chain struct {
	Raw    []Block
	Blocks []gledger.Block
}

// LoadPreprodChain decodes the preprod Byron chain shipped with dingo.
func LoadPreprodChain(dingoDir string) (*Chain, error) {
	raw, err := PreprodBlocks(dingoDir)
	if err != nil {
		return nil, err
	}
	c := &Chain{Raw: raw}
	for i, b := range raw {
		blk, err := gledger.NewBlockFromCbor(b.Type, b.CBOR)
		if err != nil {
			return nil, fmt.Errorf("block %d: %w", i, err)
		}
		c.Blocks = append(c.Blocks, blk)
	}
	return c, nil
}

// Wrapped is block i as the reference's ABlockOrBoundary encodes it: the
// block type tag (0 boundary, 1 main) and the block.
func (c *Chain) Wrapped(i int) []byte {
	return Array(Uint(uint64(c.Raw[i].Type)), c.Raw[i].CBOR)
}

// Main returns block i as a main block; it panics on a boundary block.
func (c *Chain) Main(i int) *byron.ByronMainBlock {
	m, ok := c.Blocks[i].(*byron.ByronMainBlock)
	if !ok {
		panic(fmt.Sprintf("block %d is not a main block", i))
	}
	return m
}

// DelegateVkey is the extended verification key of the delegate that signed
// main block i, the block issuer the update rules endorse with.
func (c *Chain) DelegateVkey(i int) []byte {
	sig := c.Main(i).BlockHeader.ConsensusData.BlockSig[1].([]any)
	return sig[0].([]any)[2].([]byte)
}

// PrevSlot is the slot of the main block before block i, or 0 before the
// first: the reference's last slot, which a boundary block does not move.
func (c *Chain) PrevSlot(i int) uint64 {
	for j := i - 1; j >= 0; j-- {
		if c.Raw[j].Type == byron.BlockTypeByronMain {
			return c.Blocks[j].SlotNumber()
		}
	}
	return 0
}

// PrevHash is the hash of block i-1, or nil before the first block.
func (c *Chain) PrevHash(i int) []byte {
	if i == 0 {
		return nil
	}
	h := c.Blocks[i-1].Hash()
	return h[:]
}
