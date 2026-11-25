package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// A Byron transaction is [tx, [witness...]] with tx = [inputs, outputs,
// attributes]; its id is blake2b-256 of the tx bytes and its size the wire
// size of the whole.
func byronTxFixture() (wire []byte, id [32]byte, out []byte) {
	k := byronfixture.NewKey(1)
	out = byronfixture.TxOut(k.Address(nil), 1_000_000)
	body, id := byronfixture.Tx([]byronfixture.TxIn{{ID: [32]byte{9}, Index: 1}}, [][]byte{out})
	return byronfixture.TxAux(body, k.Witness(764824073, id)), id, out
}

func TestCodecByronTx(t *testing.T) {
	t.Parallel()
	wire, id, _ := byronTxFixture()
	got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_BYRON, Cbor: wire})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetTxId(), id[:]) || got.GetSize() != uint64(len(wire)) || !got.GetIsValid() || !bytes.Equal(got.GetCbor(), wire) {
		t.Fatalf("tx_id %x size %d valid %v cbor equal %v", got.GetTxId(), got.GetSize(), got.GetIsValid(), bytes.Equal(got.GetCbor(), wire))
	}
	for name, b := range map[string][]byte{
		"truncated":     wire[:len(wire)-1],
		"trailing byte": append(bytes.Clone(wire), 0),
		"not an array":  {0x01},
		"empty":         nil,
	} {
		got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_BYRON, Cbor: b})
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustReject(t, got.GetRejection(), name)
	}
	_, err = codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_BYRON, ProtocolVersion: &v1.ProtocolVersion{Major: 2}, Cbor: wire})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("version 2: err = %v, want UNSUPPORTED", err)
	}
}

func TestCodecByronTxOutput(t *testing.T) {
	t.Parallel()
	_, _, out := byronTxFixture()
	k := byronfixture.NewKey(1)
	addr := k.Address(nil)
	got, err := codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_BYRON, Cbor: out})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if got.GetCoin() != 1_000_000 || !bytes.Equal(got.GetAddress(), addr) || !bytes.Equal(got.GetCbor(), out) {
		t.Fatalf("coin %d address %x cbor %x", got.GetCoin(), got.GetAddress(), got.GetCbor())
	}
	for name, b := range map[string][]byte{
		"truncated":     out[:len(out)-1],
		"trailing byte": append(bytes.Clone(out), 0),
		"integer":       {0x01},
	} {
		got, _ := codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_BYRON, Cbor: b})
		mustReject(t, got.GetRejection(), name)
	}
}
