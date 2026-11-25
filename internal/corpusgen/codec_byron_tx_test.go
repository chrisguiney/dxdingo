package corpusgen

import (
	"fmt"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
)

// byronTxCodecCases covers codec_tx and codec_tx_output for the Byron era:
// the two transactions of the mainnet block (the preprod chain, through slot
// 84242, holds none), transactions and outputs built with byronfixture, and
// malformed encodings.
func byronTxCodecCases(t *testing.T, out []entry) []entry {
	const era = v1.Era_ERA_BYRON
	add := func(name, why string, call *v1.Call) { out = append(out, entry{name, why, call}) }
	tx := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecTx{CodecTx: &v1.CodecTxRequest{Era: era, Cbor: b}}}
	}
	txOut := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecTxOutput{CodecTxOutput: &v1.CodecTxOutputRequest{Era: era, Cbor: b}}}
	}

	blk, err := ledger.NewBlockFromCbor(ledger.BlockTypeByronMain, readHex(t, gouroborosTestdata(t), "byron_block.hex"), common.VerifyConfig{SkipBodyHashValidation: true})
	if err != nil {
		t.Fatal(err)
	}
	for i, x := range blk.Transactions() {
		add(fmt.Sprintf("codec_tx_byron_mainnet_%d", i), "a real mainnet Byron transaction", tx(x.Cbor()))
		for j, u := range x.Produced() {
			add(fmt.Sprintf("codec_tx_output_byron_mainnet_%d_%d", i, j), "a real mainnet Byron output", txOut(u.Output.Cbor()))
		}
	}

	k, k2 := byronfixture.NewKey(1), byronfixture.NewKey(2)
	preprod := uint32(1)
	const mainnetMagic = 764824073
	in1, in2 := byronfixture.TxIn{ID: [32]byte{9}, Index: 1}, byronfixture.TxIn{ID: [32]byte{7}, Index: 70000}
	o1, o2 := byronfixture.TxOut(k.Address(nil), 1_000_000), byronfixture.TxOut(k2.Address(&preprod), 3_000_000)
	body, id := byronfixture.Tx([]byronfixture.TxIn{in1}, [][]byte{o1})
	wire := byronfixture.TxAux(body, k.Witness(mainnetMagic, id))
	add("codec_tx_byron_minimal", "one input, one output, one witness, lists of indefinite length as Byron writes them", tx(wire))
	body2, id2 := byronfixture.Tx([]byronfixture.TxIn{in1, in2}, [][]byte{o1, o2})
	add("codec_tx_byron_two_inputs_outputs", "two inputs, one with an index above 65535, a mainnet and a testnet-magic output", tx(byronfixture.TxAux(body2, k.Witness(mainnetMagic, id2), k2.Witness(mainnetMagic, id2))))
	add("codec_tx_byron_no_witnesses", "an empty witness list decodes", tx(byronfixture.TxAux(body)))
	add("codec_tx_byron_bad_signature", "a witness with a wrong signature still decodes: validation is another function", tx(byronfixture.TxAux(body, k.WitnessSignature(make([]byte, 64)))))
	definite := byronfixture.Array(byronfixture.Array(in1Wire(in1)), byronfixture.Array(o1), byronfixture.EmptyMap)
	add("codec_tx_byron_definite_lists", "the same transaction with definite-length lists", tx(byronfixture.TxAux(definite, k.Witness(mainnetMagic, id))))
	tr, tl := malformed(wire)
	add("codec_tx_byron_truncated", "cut by one byte is rejected", tx(tr))
	add("codec_tx_byron_trailing", "a trailing byte is rejected", tx(tl))
	add("codec_tx_byron_empty", "no bytes", tx(nil))
	add("codec_tx_byron_integer", "an integer is no transaction", tx([]byte{0x01}))
	add("codec_tx_byron_three_elements", "an envelope of three elements is rejected", tx(byronfixture.Array(body, byronfixture.Array(), byronfixture.Array())))
	add("codec_tx_byron_input_tag_1", "an input of type 1 is not an outpoint", tx(byronfixture.TxAux(byronfixture.Array(byronfixture.IndefArray(byronfixture.Array(byronfixture.Uint(1), byronfixture.Tag24(byronfixture.Array(byronfixture.Bytes(in1.ID[:]), byronfixture.Uint(1))))), byronfixture.IndefArray(o1), byronfixture.EmptyMap))))
	add("codec_tx_byron_short_txid", "an input naming a 31-byte transaction id", tx(byronfixture.TxAux(byronfixture.Array(byronfixture.IndefArray(byronfixture.Array(byronfixture.Uint(0), byronfixture.Tag24(byronfixture.Array(byronfixture.Bytes(in1.ID[:31]), byronfixture.Uint(1))))), byronfixture.IndefArray(o1), byronfixture.EmptyMap))))
	add("codec_tx_byron_attributes", "a non-empty transaction attribute map", tx(byronfixture.TxAux(byronfixture.Array(byronfixture.IndefArray(in1Wire(in1)), byronfixture.IndefArray(o1), []byte{0xa1, 0x00, 0x41, 0x01}))))
	for _, v := range []uint32{0, 1, 2} {
		add(fmt.Sprintf("codec_tx_byron_version_%d", v), "protocol version "+fmt.Sprint(v)+": Byron has versions 0 and 1", &v1.Call{Request: &v1.Call_CodecTx{CodecTx: &v1.CodecTxRequest{
			Era: era, ProtocolVersion: &v1.ProtocolVersion{Major: v}, Cbor: wire,
		}}})
	}

	add("codec_tx_output_byron_mainnet_address", "a mainnet address output", txOut(o1))
	add("codec_tx_output_byron_testnet_address", "an address carrying a network magic", txOut(o2))
	add("codec_tx_output_byron_zero", "zero lovelace", txOut(byronfixture.TxOut(k.Address(nil), 0)))
	add("codec_tx_output_byron_max", "the largest lovelace amount, 45e15", txOut(byronfixture.TxOut(k.Address(nil), 45_000_000_000_000_000)))
	add("codec_tx_output_byron_above_max", "one lovelace above the maximum", txOut(byronfixture.TxOut(k.Address(nil), 45_000_000_000_000_001)))
	otr, otl := malformed(o1)
	add("codec_tx_output_byron_truncated", "cut by one byte is rejected", txOut(otr))
	add("codec_tx_output_byron_trailing", "a trailing byte is rejected", txOut(otl))
	add("codec_tx_output_byron_integer", "an integer is no output", txOut([]byte{0x01}))
	badCRC := byronfixture.Array(byronfixture.Array(byronfixture.Tag24(byronfixture.Array(byronfixture.Bytes(rep(1, 28)), byronfixture.EmptyMap, byronfixture.Uint(0))), byronfixture.Uint(1)), byronfixture.Uint(5))
	add("codec_tx_output_byron_bad_crc", "an address whose CRC does not match its payload", txOut(badCRC))
	add("codec_tx_output_byron_three_elements", "an output of three elements", txOut(byronfixture.Array(k.Address(nil), byronfixture.Uint(1), byronfixture.Uint(2))))
	return out
}

func in1Wire(i byronfixture.TxIn) []byte {
	return byronfixture.Array(byronfixture.Uint(0), byronfixture.Tag24(byronfixture.Array(byronfixture.Bytes(i.ID[:]), byronfixture.Uint(uint64(i.Index)))))
}
