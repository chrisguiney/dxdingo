package main

import (
	"errors"
	"math/big"
	"testing"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/eras"
	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
)

// Divergence: a main block that does not extend the tip. dingo's block
// application rejects it; the reference's updateChainBlockOrBoundary compares
// a previous hash only for a boundary block (see the matching "divergence:"
// check in Test/Byron.hs), because the consensus layer's header envelope check
// owns the main-block link. A real chain cannot reach it: the consensus layer
// rejects the header before the ledger sees the block. This calls dingo
// directly, bypassing the adapter.
func TestDivergenceByronChainMainBlockPrevHash(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	cfg := byronconsensus.ByronConfig{
		ProtocolMagic: chainMagic, SlotsPerEpoch: 21600, SecurityParam: 2160, NumGenesisKeys: len(dlg),
		GenesisDelegations: map[lcommon.Blake2b224]lcommon.Blake2b224{},
	}
	for _, d := range dlg {
		iss, err := byronconsensus.PBFTVerificationKeyHash(d.IssuerVkey)
		if err != nil {
			t.Fatal(err)
		}
		del, err := byronconsensus.PBFTVerificationKeyHash(d.DelegateVkey)
		if err != nil {
			t.Fatal(err)
		}
		cfg.GenesisKeyHashes = append(cfg.GenesisKeyHashes, iss.Bytes())
		cfg.GenesisDelegations[iss] = del
	}
	pbft, err := ledger.NewByronPBFTState(cfg, &eras.ByronProtocolParameters{
		MaxBlockSize: big.NewInt(2_000_000), MaxHeaderSize: big.NewInt(2_000_000), MaxTxSize: big.NewInt(4096),
		TxFeeMultiplierNano: big.NewInt(0),
	})
	if err != nil {
		t.Fatal(err)
	}
	right := c.PrevHash(3)
	if _, err := ledger.ApplyByronBlock(cfg, ledger.ByronChainState{PBFT: pbft, PrevHash: right}, c.Blocks[3], false); err != nil {
		t.Fatalf("with the right previous hash: %v", err)
	}
	wrong := append([]byte(nil), right...)
	wrong[0] ^= 1
	_, err = ledger.ApplyByronBlock(cfg, ledger.ByronChainState{PBFT: pbft, PrevHash: wrong}, c.Blocks[3], false)
	if !errors.Is(err, ledger.ErrStaleChainIterator) {
		t.Fatalf("with a wrong previous hash: %v, want the stale-chain error", err)
	}
}
