package main

import (
	"fmt"

	"github.com/blinklabs-io/gouroboros/ledger/byron"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// byronProtocolVersionArg accepts the Byron protocol versions, 0 and 1; an
// unset version is the era's own.
func byronProtocolVersionArg(pv *v1.ProtocolVersion) error {
	if major := pv.GetMajor(); major > 1 {
		return dispatch.Unsupported(fmt.Errorf("protocol version %d outside Byron", major))
	}
	return nil
}

// codecByronTx decodes a submitted Byron transaction, [tx, [witness...]].
// The size is the wire size, which the Byron fee is computed from.
func codecByronTx(req *v1.CodecTxRequest) (*v1.CodecTxResponse, error) {
	if err := byronProtocolVersionArg(req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	tx, err := byron.NewByronTransactionFromCbor(req.GetCbor())
	if err != nil {
		return &v1.CodecTxResponse{Rejection: decodeFailure(err)}, nil
	}
	size, err := common.TxSize(tx)
	if err != nil {
		return &v1.CodecTxResponse{Rejection: dingoRejection("codec", err)}, nil
	}
	id := tx.Hash()
	return &v1.CodecTxResponse{Cbor: tx.Cbor(), TxId: id[:], Size: uint64(size), IsValid: true}, nil
}

// codecByronTxOutput decodes [address, lovelace]; the address is reported in
// its wire form.
func codecByronTxOutput(req *v1.CodecTxOutputRequest) (*v1.CodecTxOutputResponse, error) {
	if err := byronProtocolVersionArg(req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	out, err := byron.NewByronTransactionOutputFromCbor(req.GetCbor())
	if err != nil {
		return &v1.CodecTxOutputResponse{Rejection: decodeFailure(err)}, nil
	}
	addr, err := out.Address().Bytes()
	if err != nil {
		return &v1.CodecTxOutputResponse{Rejection: dingoRejection("codec", err)}, nil
	}
	return &v1.CodecTxOutputResponse{Cbor: out.Cbor(), Address: addr, Coin: out.Amount().Uint64()}, nil
}
