package main

import (
	"context"
	"fmt"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// hfcTag is what a tag of the Cardano disk form stands for.
type hfcTag struct {
	era   v1.Era
	block uint
}

// hfcTags are the tags of Cardano's encodeDiskHfcBlock: Byron blocks keep
// their own encoding, so a boundary block is 0 and a regular block 1, and
// every later era follows at the next tag.
var hfcTags = map[uint]hfcTag{
	0: {v1.Era_ERA_BYRON, ledger.BlockTypeByronEbb},
	1: {v1.Era_ERA_BYRON, ledger.BlockTypeByronMain},
	2: {v1.Era_ERA_SHELLEY, ledger.BlockTypeShelley},
	3: {v1.Era_ERA_ALLEGRA, ledger.BlockTypeAllegra},
	4: {v1.Era_ERA_MARY, ledger.BlockTypeMary},
	5: {v1.Era_ERA_ALONZO, ledger.BlockTypeAlonzo},
	6: {v1.Era_ERA_BABBAGE, ledger.BlockTypeBabbage},
	7: {v1.Era_ERA_CONWAY, ledger.BlockTypeConway},
	8: {v1.Era_ERA_DIJKSTRA, ledger.BlockTypeDijkstra},
}

// codecHfcBlock decodes a Cardano hard-fork-combinator block. Version 0 is
// the disk form of encodeDiskHfcBlock, [tag, block]. A non-zero
// node_to_node_version is the node-to-node form, that array wrapped in CBOR
// tag 24 (wrapCBORinCBOR); the reference ignores the version otherwise.
func codecHfcBlock(_ context.Context, req *v1.CodecHfcBlockRequest) (*v1.CodecHfcBlockResponse, error) {
	reject := func(err error) (*v1.CodecHfcBlockResponse, error) {
		return &v1.CodecHfcBlockResponse{Rejection: decodeFailure(err)}, nil
	}
	n2n := req.GetNodeToNodeVersion() != 0
	diskForm := req.GetCbor()
	if n2n {
		var w cbor.WrappedCbor
		if _, err := cbor.DecodeExact(req.GetCbor(), &w); err != nil {
			return reject(err)
		}
		diskForm = w
	}
	var outer struct {
		cbor.StructAsArray
		Tag uint
		Raw cbor.RawMessage
	}
	if _, err := cbor.DecodeExact(diskForm, &outer); err != nil {
		return reject(err)
	}
	t, ok := hfcTags[outer.Tag]
	if !ok {
		return reject(fmt.Errorf("era tag %d", outer.Tag))
	}
	blk, err := decodeBlock(t.block, outer.Raw)
	if err != nil {
		return reject(err)
	}
	hash := blk.Hash()
	// Byron blocks are echoed as received, as codec_byron_block does:
	// gouroboros rebuilds a Byron block's wire form only partly.
	inner := outer.Raw
	if t.era != v1.Era_ERA_BYRON {
		inner = blk.Cbor()
	}
	out := append([]byte{0x82, byte(outer.Tag)}, inner...)
	if n2n {
		wrapped, err := cbor.Encode(cbor.WrappedCbor(out))
		if err != nil {
			return nil, err
		}
		out = wrapped
	}
	return &v1.CodecHfcBlockResponse{Cbor: out, Era: t.era, HeaderHash: hash[:]}, nil
}
