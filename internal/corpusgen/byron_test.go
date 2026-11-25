package corpusgen

import (
	"bytes"
	"math/big"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"
	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/byron"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
)

const (
	mainnetMagic = 764824073
	preprodMagic = 1
)

func TestGenerateByronCorpus(t *testing.T) {
	var entries []entry
	entries = append(entries, byronMinFeeCases()...)
	entries = append(entries, byronTxCases(t)...)
	entries = append(entries, byronHeaderCases(t)...)
	entries = append(entries, byronChainCases(t)...)
	entries = append(entries, byronUpdateCases(t)...)
	writeCorpus(t, "", entries)
	writeCorpus(t, "divergences", byronDivergences(t))
}

// byronDivergences are confirmed mismatches; see divergence_byron_crypto_test.go
// and the "divergence:" checks in Test/Byron.hs.
func byronDivergences(t *testing.T) []entry {
	t.Helper()
	dir, err := byronfixture.ModuleDir("github.com/blinklabs-io/dingo")
	if err != nil {
		t.Fatal(err)
	}
	dlgs, err := byronfixture.PreprodDelegations(dir)
	if err != nil {
		t.Fatal(err)
	}
	blocks, err := byronfixture.PreprodBlocks(dir)
	if err != nil {
		t.Fatal(err)
	}
	var protoDlg []*v1.ByronDelegation
	for _, d := range dlgs {
		protoDlg = append(protoDlg, &v1.ByronDelegation{IssuerVkey: d.Issuer, DelegateVkey: d.Delegate})
	}
	blk, err := gledger.NewBlockFromCbor(blocks[4].Type, blocks[4].CBOR)
	if err != nil {
		t.Fatal(err)
	}
	return append(byronChainDivergences(t), []entry{{
		"byron_header_validate_wrong_magic",
		"mainnet magic against a preprod header: gouroboros compares the header's magic first (a non-PBFT error), the reference's PBFT check fails the signature",
		&v1.Call{Request: &v1.Call_ByronHeaderValidate{ByronHeaderValidate: &v1.ByronHeaderValidateRequest{
			ProtocolMagic: mainnetMagic, SecurityParam: 2160, Pparams: byronParams(65536),
			ChainState: &v1.ByronChainState{Delegations: protoDlg}, Header: blk.Header().Cbor(),
		}}},
	}}...)
}

// moduleFile reads a file from a module in go.work, so the corpus can use the
// real chain data the production code ships tests for.
func moduleFile(t *testing.T, module, rel string) []byte {
	t.Helper()
	out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", module).Output()
	if err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(filepath.Join(strings.TrimSpace(string(out)), rel))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func byronMinFeeCases() []entry {
	mk := func(name, why string, a, b *v1.Rational, size uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronMinFee{ByronMinFee: &v1.ByronMinFeeRequest{
			TxFeeA: a, TxFeeB: b, Size: size,
		}}}}
	}
	a, b := rat("155381", "1"), rat("43946", "1000000") // mainnet policy
	return []entry{
		mk("byron_min_fee_mainnet_200", "mainnet policy, 200 bytes: ceil(8.7892) = 9", a, b, 200),
		mk("byron_min_fee_mainnet_max_tx", "mainnet policy at a 16 KiB transaction", a, b, 16384),
		mk("byron_min_fee_zero_size", "size 0 is the summand alone", a, b, 0),
		mk("byron_min_fee_exact_product", "b*size is an integer: no rounding up", rat("0", "1"), rat("1", "2"), 10),
		mk("byron_min_fee_half_rounds_up", "b*size = 5.5 rounds up to 6", rat("0", "1"), rat("1", "2"), 11),
		mk("byron_min_fee_one_nano", "smallest multiplier, 1e-9 per byte", rat("0", "1"), rat("1", "1000000000"), 1_000_000_001),
		mk("byron_min_fee_zero_policy", "free policy", rat("0", "1"), rat("0", "1"), 1000),
		mk("byron_min_fee_summand_at_bound", "a at maxLovelaceVal, size 0", rat("45000000000000000", "1"), rat("1", "1"), 0),
		mk("byron_min_fee_overflow", "a + b*size one lovelace above maxLovelaceVal", rat("45000000000000000", "1"), rat("1", "1"), 1),
		mk("byron_min_fee_large_size", "size 2^40 with the mainnet multiplier", a, b, 1<<40),
		mk("byron_min_fee_fractional_summand", "a is not an integer: UNSUPPORTED on both", rat("1", "2"), b, 10),
		mk("byron_min_fee_below_nano", "b = 1/3 is not a Nano: UNSUPPORTED on both", a, rat("1", "3"), 10),
		mk("byron_min_fee_negative_multiplier", "negative b: UNSUPPORTED on both", a, rat("-1", "2"), 10),
	}
}

// txFixture builds transactions over a fixed funded UTxO.
type txFixture struct {
	magic uint32
	keys  []byronfixture.Key
	utxo  []*v1.UtxoEntry
	ins   []byronfixture.TxIn
}

func newTxFixture(magic uint32, values ...uint64) *txFixture {
	f := &txFixture{magic: magic}
	var net *uint32
	if magic != mainnetMagic {
		net = &magic
	}
	for i, v := range values {
		k := byronfixture.NewKey(byte(i + 1))
		in := byronfixture.TxIn{ID: [32]byte{0x11 * byte(i+1)}, Index: uint32(i)}
		f.keys = append(f.keys, k)
		f.ins = append(f.ins, in)
		f.utxo = append(f.utxo, &v1.UtxoEntry{
			Txin:  &v1.TxIn{TxId: in.ID[:], Index: in.Index},
			Txout: byronfixture.TxOut(k.Address(net), v),
		})
	}
	return f
}

func (f *txFixture) out(n byte, lovelace uint64) []byte {
	var net *uint32
	if f.magic != mainnetMagic {
		net = &f.magic
	}
	return byronfixture.TxOut(byronfixture.NewKey(0x40+n).Address(net), lovelace)
}

// aux builds the TxAux spending ins (witnessed by keys at the same positions).
func (f *txFixture) aux(ins []byronfixture.TxIn, signers []byronfixture.Key, outs ...[]byte) []byte {
	body, id := byronfixture.Tx(ins, outs)
	var ws [][]byte
	for _, k := range signers {
		ws = append(ws, k.Witness(f.magic, id))
	}
	return byronfixture.TxAux(body, ws...)
}

func byronParams(maxTx uint64) *v1.ByronProtocolParameters {
	return &v1.ByronProtocolParameters{
		TxFeeA: rat("155381", "1"), TxFeeB: rat("43946", "1000000"),
		MaxTxSize: maxTx, MaxBlockSize: 2000000, MaxHeaderSize: 2000000,
	}
}

// minFeeFor is the mainnet fee policy a + ceil(b*size) for a transaction of
// the given size, used to place a case exactly on the fee boundary.
func minFeeFor(size int) uint64 {
	n := new(big.Int).Mul(big.NewInt(43946000), big.NewInt(int64(size)))
	q, r := new(big.Int).DivMod(n, big.NewInt(1_000_000_000), new(big.Int))
	if r.Sign() > 0 {
		q.Add(q, big.NewInt(1))
	}
	return 155381 + q.Uint64()
}

func byronTxCases(t *testing.T) []entry {
	t.Helper()
	const ada = 1_000_000
	req := func(name, why string, f *txFixture, maxTx uint64, tx []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronTxValidate{ByronTxValidate: &v1.ByronTxValidateRequest{
			ProtocolMagic: f.magic, Pparams: byronParams(maxTx), Utxo: f.utxo, Tx: tx,
		}}}}
	}
	f := newTxFixture(mainnetMagic, 10*ada)
	k1 := f.keys[0]
	wrongID := [32]byte{0xee}
	_, wrongSigID := byronfixture.Tx(f.ins, [][]byte{f.out(1, 9*ada)})
	_ = wrongSigID

	valid := f.aux(f.ins, f.keys, f.out(1, 9*ada))
	// A one-input, one-output transaction has a size independent of the fee
	// value's width as long as the fee stays in the same CBOR integer class.
	exact := minFeeFor(len(valid))
	atMin := f.aux(f.ins, f.keys, f.out(1, 10*ada-exact))
	belowMin := f.aux(f.ins, f.keys, f.out(1, 10*ada-exact+1))

	two := newTxFixture(mainnetMagic, 6*ada, 5*ada)
	twoTx := two.aux(two.ins, two.keys, two.out(1, 7*ada), two.out(2, 3*ada+500_000))

	dup := f.aux([]byronfixture.TxIn{f.ins[0], f.ins[0]}, []byronfixture.Key{k1, k1}, f.out(1, 12*ada))
	zeroOut := f.aux(f.ins, f.keys, f.out(1, 0), f.out(2, 9*ada))

	testnet := newTxFixture(preprodMagic, 10*ada)
	wrongNet := newTxFixture(mainnetMagic, 10*ada)
	otherNetOut := byronfixture.TxOut(byronfixture.NewKey(0x41).Address(&[]uint32{preprodMagic}[0]), 9*ada)

	entries := []entry{
		req("byron_tx_validate_valid", "one input, one output, 1 ADA fee", f, 4096, valid),
		req("byron_tx_validate_two_inputs", "two inputs, two outputs, fee 500000", two, 4096, twoTx),
		req("byron_tx_validate_preprod", "preprod network: addresses carry the protocol magic", testnet, 4096,
			testnet.aux(testnet.ins, testnet.keys, testnet.out(1, 9*ada))),
		req("byron_tx_validate_fee_exact_min", "fee equal to the minimum is accepted", f, 4096, atMin),
		req("byron_tx_validate_fee_one_below_min", "one lovelace below the minimum fee", f, 4096, belowMin),
		req("byron_tx_validate_outputs_exceed_inputs", "outputs worth more than inputs", f, 4096,
			f.aux(f.ins, f.keys, f.out(1, 10*ada+1))),
		req("byron_tx_validate_bad_signature", "witness signed over another TxId", f, 4096,
			byronfixture.TxAux(mustBody(f, f.out(1, 9*ada)), k1.Witness(mainnetMagic, wrongID))),
		req("byron_tx_validate_wrong_key", "witness is a valid signature by a key that does not own the input", f, 4096,
			f.aux(f.ins, []byronfixture.Key{byronfixture.NewKey(9)}, f.out(1, 9*ada))),
		req("byron_tx_validate_wrong_network_output", "mainnet transaction paying a preprod address", wrongNet, 4096,
			wrongNet.aux(wrongNet.ins, wrongNet.keys, otherNetOut)),
		req("byron_tx_validate_missing_input", "input absent from the UTxO", f, 4096,
			f.aux([]byronfixture.TxIn{{ID: [32]byte{0x99}}}, f.keys, f.out(1, 9*ada))),
		req("byron_tx_validate_size_at_limit", "max_tx_size equal to the transaction size is accepted", f, uint64(len(valid)), valid),
		req("byron_tx_validate_size_over_limit", "max_tx_size one below the transaction size", f, uint64(len(valid))-1, valid),
		req("byron_tx_validate_duplicate_input", "the same input listed twice: the reference counts it once", f, 4096, dup),
		req("byron_tx_validate_zero_output", "a zero-value output is valid in Byron", f, 4096, zeroOut),
		req("byron_tx_validate_garbage", "bytes that are not a TxAux", f, 4096, []byte{0xff, 0x00}),
		req("byron_tx_validate_no_witness", "valid body with no witnesses: the reference zips inputs with an empty witness list", f, 4096,
			byronfixture.TxAux(mustBody(f, f.out(1, 9*ada)))),
	}
	return append(entries, realByronTxCases(t)...)
}

func mustBody(f *txFixture, outs ...[]byte) []byte {
	b, _ := byronfixture.Tx(f.ins, outs)
	return b
}

// realByronTxCases replays transactions from real mainnet blocks against an
// empty UTxO: their inputs are not available, so both nodes must report the
// first unresolved input.
func realByronTxCases(t *testing.T) []entry {
	t.Helper()
	raw := moduleFile(t, "github.com/blinklabs-io/dingo", "ledger/testdata/mainnet-byron-4471207.cbor")
	var wrapped struct {
		cbor.StructAsArray
		Type  uint
		Block []byte
	}
	var block gledger.Block
	if _, err := cbor.Decode(raw, &wrapped); err == nil && len(wrapped.Block) > 0 {
		block, _ = gledger.NewBlockFromCbor(wrapped.Type, wrapped.Block)
	}
	if block == nil {
		var err error
		block, err = gledger.NewBlockFromCbor(byron.BlockTypeByronMain, raw)
		if err != nil {
			t.Fatal(err)
		}
	}
	var out []entry
	for i, tx := range block.Transactions() {
		if i >= 3 {
			break
		}
		out = append(out, entry{
			name:    "byron_tx_validate_real_mainnet_" + string(rune('a'+i)),
			comment: "transaction from mainnet Byron block 4471207 against an empty UTxO",
			call: &v1.Call{Request: &v1.Call_ByronTxValidate{ByronTxValidate: &v1.ByronTxValidateRequest{
				ProtocolMagic: mainnetMagic, Pparams: byronParams(65536), Tx: tx.Cbor(),
			}}},
		})
	}
	return out
}

// byronHeaderCases validate real preprod Byron main-block headers against the
// preprod genesis delegations, and tampered variants of them.
func byronHeaderCases(t *testing.T) []entry {
	t.Helper()
	dir, err := byronfixture.ModuleDir("github.com/blinklabs-io/dingo")
	if err != nil {
		t.Fatal(err)
	}
	dlgs, err := byronfixture.PreprodDelegations(dir)
	if err != nil {
		t.Fatal(err)
	}
	blocks, err := byronfixture.PreprodBlocks(dir)
	if err != nil {
		t.Fatal(err)
	}
	var protoDlg []*v1.ByronDelegation
	for _, d := range dlgs {
		protoDlg = append(protoDlg, &v1.ByronDelegation{IssuerVkey: d.Issuer, DelegateVkey: d.Delegate})
	}
	var headers [][]byte
	var ebb []byte
	var main []*byron.ByronMainBlockHeader
	for _, b := range blocks {
		blk, err := gledger.NewBlockFromCbor(b.Type, b.CBOR)
		if err != nil {
			t.Fatal(err)
		}
		if b.Type == byron.BlockTypeByronEbb {
			ebb = blk.Header().Cbor()
			continue
		}
		headers = append(headers, blk.Header().Cbor())
		main = append(main, blk.Header().(*byron.ByronMainBlockHeader))
	}
	req := func(name, why string, magic uint32, k uint64, dlg []*v1.ByronDelegation, header []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronHeaderValidate{ByronHeaderValidate: &v1.ByronHeaderValidateRequest{
			ProtocolMagic: magic, SecurityParam: k, Pparams: byronParams(65536),
			ChainState: &v1.ByronChainState{Delegations: dlg}, Header: header,
		}}}}
	}
	h := headers[3]
	prev := main[3].PrevHash()
	flipAt := func(needle []byte, off int) []byte {
		at := bytes.Index(h, needle)
		if at < 0 {
			t.Fatal("needle not in header")
		}
		return flip(h, at+off)
	}
	sig := main[3].ConsensusData.BlockSig[1].([]any)[1].([]byte)
	delegate := main[3].ConsensusData.BlockSig[1].([]any)[0].([]any)[2].([]byte)
	var without []*v1.ByronDelegation
	for _, d := range protoDlg {
		if !bytes.Equal(d.GetDelegateVkey(), delegate) {
			without = append(without, d)
		}
	}
	issuer, err := byronconsensus.PBFTIssuerFromHeader(main[3])
	if err != nil {
		t.Fatal(err)
	}
	g := issuer.GenesisKeyHash[:]
	slotID := main[3].ConsensusData.SlotId
	headerSlot := slotID.Epoch*21600 + slotID.Slot
	filler := func(i int) []byte {
		k := make([]byte, 28)
		k[0], k[1] = byte(1+i%7), 0xee
		return k
	}
	// window builds n signers in slots 1..n; the header's genesis key signs
	// where isG says.
	window := func(n int, isG func(i int) bool) []*v1.ByronSigner {
		out := make([]*v1.ByronSigner, n)
		for i := range out {
			key := filler(i)
			if isG(i) {
				key = g
			}
			out[i] = &v1.ByronSigner{Key: key, Slot: uint64(i + 1)}
		}
		return out
	}
	reqW := func(name, why string, signers []*v1.ByronSigner) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronHeaderValidate{ByronHeaderValidate: &v1.ByronHeaderValidateRequest{
			ProtocolMagic: preprodMagic, SecurityParam: 2160, Pparams: byronParams(65536),
			ChainState: &v1.ByronChainState{Delegations: protoDlg, Signers: signers}, Header: h,
		}}}}
	}
	// k = 2160 and the threshold 0.22 allow floor(0.22 * 2160) = 475 signatures
	// per genesis key in the window.
	return []entry{
		reqW("byron_header_validate_window_below_threshold", "474 earlier signatures by the header's genesis key: 475 with this one is at the limit", window(474, func(int) bool { return true })),
		reqW("byron_header_validate_window_threshold_exceeded", "475 earlier signatures by the header's genesis key: this one is the 476th", window(475, func(int) bool { return true })),
		reqW("byron_header_validate_window_slides_past_old_signature", "a full window whose oldest entry is the issuer's 475th: it leaves as this block enters", window(2160, func(i int) bool { return i < 475 })),
		reqW("byron_header_validate_window_oldest_is_another", "a full window with 475 signatures by the issuer, the oldest being another's: 476 after this block", window(2160, func(i int) bool { return i >= 1 && i < 476 })),
		reqW("byron_header_validate_window_slot_before_last", "the last signer signed one slot after this header", []*v1.ByronSigner{{Key: filler(0), Slot: headerSlot + 1}}),
		reqW("byron_header_validate_window_slot_equal_last", "the last signer signed in this header's slot: the reference compares non-strictly", []*v1.ByronSigner{{Key: filler(0), Slot: headerSlot}}),
		reqW("byron_header_validate_window_slots_decrease", "signers out of slot order: not a window the reference can hold, UNSUPPORTED on both", []*v1.ByronSigner{{Key: filler(0), Slot: 5}, {Key: filler(1), Slot: 4}}),
		reqW("byron_header_validate_window_larger_than_k", "2161 signers for k = 2160: UNSUPPORTED on both", window(2161, func(int) bool { return false })),
		req("byron_header_validate_preprod_first", "first preprod main block header", preprodMagic, 2160, protoDlg, headers[0]),
		req("byron_header_validate_preprod_3", "preprod main block 3: the case the unit tests use", preprodMagic, 2160, protoDlg, h),
		req("byron_header_validate_preprod_22", "mid-chain preprod header", preprodMagic, 2160, protoDlg, headers[22]),
		req("byron_header_validate_preprod_last", "last Byron header on preprod, slot 84242", preprodMagic, 2160, protoDlg, headers[len(headers)-1]),
		req("byron_header_validate_tampered_prev_hash", "one bit flipped in the signed previous hash", preprodMagic, 2160, protoDlg, flipAt(prev[:], 5)),
		req("byron_header_validate_tampered_signature", "one bit flipped in the delegate's block signature", preprodMagic, 2160, protoDlg, flipAt(sig, 10)),
		req("byron_header_validate_signer_not_delegate", "the signer's delegation removed from the active set", preprodMagic, 2160, without, h),
		req("byron_header_validate_wrong_k", "k = 1 changes the epoch length, so the signed slot differs", preprodMagic, 1, protoDlg, h),
		req("byron_header_validate_boundary_header", "epoch boundary header is not a main-block header", preprodMagic, 2160, protoDlg, ebb),
		req("byron_header_validate_garbage", "bytes that are not a header", preprodMagic, 2160, protoDlg, []byte{0xff}),
		req("byron_header_validate_trailing_byte", "valid header plus a trailing byte", preprodMagic, 2160, protoDlg, append(bytes.Clone(h), 0x00)),
		req("byron_header_validate_zero_k", "security_param 0: UNSUPPORTED on both", preprodMagic, 0, protoDlg, h),
		req("byron_header_validate_no_delegations", "empty delegation set: UNSUPPORTED on both", preprodMagic, 2160, nil, h),
	}
}
