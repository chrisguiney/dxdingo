package rulefixture

import (
	"bytes"
	"embed"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"sort"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/plutigo/syn"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

//go:embed testdata/costmodels.json testdata/oneshot_mint_*.hex testdata/mainnet_tx_oneshot_mint.hex
var plutusData embed.FS

// CostModels returns the mainnet PlutusV1 and PlutusV2 cost models (as of
// protocol version 8) and the epoch 653 PlutusV3 model.
func CostModels() (v1m, v2m, v3m []int64) {
	raw, err := plutusData.ReadFile("testdata/costmodels.json")
	if err != nil {
		panic(err)
	}
	var m map[string][]int64
	if err := json.Unmarshal(raw, &m); err != nil {
		panic(err)
	}
	return m["PlutusV1"], m["PlutusV2"], m["PlutusV3"]
}

// WithCostModels sets the three Plutus cost models on pp.
func WithCostModels(pp *v1.ProtocolParameters) {
	pp.CostModelPlutusV1, pp.CostModelPlutusV2, pp.CostModelPlutusV3 = CostModels()
}

// MainnetEraHistory is Byron (20s slots, 4492800 slots) followed by an
// unbounded Shelley-and-later era with 1s slots.
func MainnetEraHistory() *v1.EraHistory {
	return &v1.EraHistory{
		SystemStartUnix: 1506203091,
		Eras: []*v1.EraSummary{
			{
				Era: v1.Era_ERA_BYRON, StartSlot: 0, StartEpoch: 0, StartTime: rat("0", "1"),
				EndSlot: u64(4492800), EndEpoch: u64(208), EndTime: rat("89856000", "1"),
				EpochLength: 21600, SlotLength: rat("20", "1"), SafeZone: 4320,
			},
			{
				Era: v1.Era_ERA_SHELLEY, StartSlot: 4492800, StartEpoch: 208, StartTime: rat("89856000", "1"),
				EpochLength: 432000, SlotLength: rat("1", "1"), SafeZone: 129600,
			},
		},
	}
}

// Script is a Plutus script as stored on chain.
type Script struct {
	Lang    int    // 1, 2 or 3
	Wrapper []byte // CBOR byte string around the flat-encoded program
}

// Hash is the script hash: blake2b-224 of the language tag and the wrapper.
func (s Script) Hash() common.Blake2b224 {
	switch s.Lang {
	case 1:
		return common.PlutusV1Script(s.Wrapper).Hash()
	case 2:
		return common.PlutusV2Script(s.Wrapper).Hash()
	}
	return common.PlutusV3Script(s.Wrapper).Hash()
}

// NewScript returns a script that ignores its arguments and either returns
// unit or fails. V1 and V2 spending scripts take datum, redeemer and context
// and minting scripts redeemer and context; V3 scripts take the context alone.
func NewScript(lang int, fail, mint bool) Script {
	var body syn.Term[syn.DeBruijn] = &syn.Constant{Con: &syn.Unit{}}
	if fail {
		body = &syn.Error{}
	}
	args, ver := 1, [3]uint32{1, 1, 0}
	if lang != 3 {
		args, ver = 3, [3]uint32{1, 0, 0}
		if mint {
			args = 2
		}
	}
	for range args {
		body = &syn.Lambda[syn.DeBruijn]{Body: body}
	}
	flat, err := syn.Encode(&syn.Program[syn.DeBruijn]{Version: ver, Term: body})
	if err != nil {
		panic(err)
	}
	w, err := cbor.Encode(flat)
	if err != nil {
		panic(err)
	}
	return Script{Lang: lang, Wrapper: w}
}

// A minimal CBOR emitter for the transactions built here; the bodies are
// maps with integer keys, which the typed gouroboros encoders do not expose.
type raw []byte

func head(major byte, n uint64) []byte {
	switch {
	case n < 24:
		return []byte{major<<5 | byte(n)}
	case n < 1<<8:
		return []byte{major<<5 | 24, byte(n)}
	case n < 1<<16:
		return binary.BigEndian.AppendUint16([]byte{major<<5 | 25}, uint16(n))
	case n < 1<<32:
		return binary.BigEndian.AppendUint32([]byte{major<<5 | 26}, uint32(n))
	}
	return binary.BigEndian.AppendUint64([]byte{major<<5 | 27}, n)
}

func cUint(n uint64) raw       { return head(0, n) }
func cBytes(b []byte) raw      { return append(head(2, uint64(len(b))), b...) }
func cTag(n uint64, r raw) raw { return append(head(6, n), r...) }
func cArr(items ...raw) raw {
	out := head(4, uint64(len(items)))
	for _, i := range items {
		out = append(out, i...)
	}
	return out
}

type kv struct {
	k uint64
	v raw
}

func cMap(items ...kv) raw {
	out := head(5, uint64(len(items)))
	for _, i := range items {
		out = append(out, cUint(i.k)...)
		out = append(out, i.v...)
	}
	return out
}

// PlutusCase describes one built transaction.
type PlutusCase struct {
	Era     v1.Era
	Script  Script
	Mint    bool // the script is a minting policy; otherwise it locks the spent input
	IsValid bool // the transaction's is_valid flag
	// Babbage and later.
	RefScript        bool // the script sits in a reference input's output, not the witness set
	InlineDatum      bool // the locked output carries its datum inline
	CollateralReturn bool // a collateral return and total collateral
	OmitDatum        bool // the datum is not in the witness set
	OmitScript       bool // the script is neither in the witness set nor referenced
	NoRefInput       bool // Babbage and later: no reference input at all
	KeyInput         bool // the spent input is key-locked although the transaction carries a spending redeemer
}

var (
	plutusDatum    = []byte{0xd8, 0x79, 0x80} // Constr 0 []
	plutusRedeemer = []byte{0xd8, 0x79, 0x80}
)

func keyAddr() []byte {
	a := []byte{0x61}
	for i := 0; i < 28; i++ {
		a = append(a, 0x11)
	}
	return a
}

func scriptAddr(h common.Blake2b224) []byte { return append([]byte{0x71}, h.Bytes()...) }

func txid(b byte) []byte {
	out := make([]byte, 32)
	for i := range out {
		out[i] = b
	}
	return out
}

// output encodes a TxOut in the era's format.
func output(era v1.Era, addr []byte, coin uint64, datumHash []byte, inline, scriptRef raw) raw {
	if era == v1.Era_ERA_ALONZO {
		items := []raw{cBytes(addr), cUint(coin)}
		if datumHash != nil {
			items = append(items, cBytes(datumHash))
		}
		return cArr(items...)
	}
	m := []kv{{0, cBytes(addr)}, {1, cUint(coin)}}
	switch {
	case inline != nil:
		m = append(m, kv{2, cArr(cUint(1), cTag(24, cBytes(inline)))})
	case datumHash != nil:
		m = append(m, kv{2, cArr(cUint(0), cBytes(datumHash))})
	}
	if scriptRef != nil {
		m = append(m, kv{3, scriptRef})
	}
	return cMap(m...)
}

// Build returns the transaction CBOR and the ledger state holding the
// outputs it spends, uses as collateral and references.
func (c PlutusCase) Build() ([]byte, *v1.LedgerState) {
	sh := c.Script.Hash()
	datumHash := common.Blake2b256Hash(plutusDatum).Bytes()
	babbage := c.Era != v1.Era_ERA_ALONZO

	var scriptRef raw
	if c.RefScript && !c.OmitScript {
		scriptRef = cTag(24, cBytes(cArr(cUint(uint64(c.Script.Lang)), cBytes(c.Script.Wrapper))))
	}
	lockAddr := keyAddr()
	var lockHash []byte
	var lockInline []byte
	if !c.Mint && !c.KeyInput {
		lockAddr = scriptAddr(sh)
		lockHash = datumHash
		if c.InlineDatum && babbage {
			lockHash, lockInline = nil, plutusDatum
		}
	}
	in := output(c.Era, lockAddr, 10_000_000, lockHash, lockInline, nil)
	col := output(c.Era, keyAddr(), 5_000_000, nil, nil, nil)
	ref := output(c.Era, keyAddr(), 2_000_000, nil, nil, scriptRef)

	body := []kv{
		{0, cArr(cArr(cBytes(txid(0xa1)), cUint(0)))},
		{1, cArr(output(c.Era, keyAddr(), 9_800_000, nil, nil, nil))},
		{2, cUint(200_000)},
		{13, cArr(cArr(cBytes(txid(0xb2)), cUint(0)))},
	}
	if c.Mint {
		// {policy: {"T": 1}}: keyed by byte string, which cMap does not do.
		assets := append(append(head(5, 1), cBytes([]byte("T"))...), cUint(1)...)
		body = append(body, kv{9, append(append(head(5, 1), cBytes(sh.Bytes())...), assets...)})
	}
	refInput := babbage && !c.NoRefInput
	if refInput {
		body = append(body, kv{18, cArr(cArr(cBytes(txid(0xc3)), cUint(0)))})
	}
	if babbage && c.CollateralReturn {
		body = append(body, kv{16, output(c.Era, keyAddr(), 2_000_000, nil, nil, nil)}, kv{17, cUint(3_000_000)})
	}
	sortKV(body)

	tag, idx := uint64(0), uint64(0)
	if c.Mint {
		tag = 1
	}
	redeemer := cArr(cArr(cUint(tag), cUint(idx), raw(plutusRedeemer), cArr(cUint(1_000_000), cUint(500_000_000))))
	wits := []kv{{5, redeemer}}
	if !c.Mint && !c.OmitDatum {
		wits = append(wits, kv{4, cArr(raw(plutusDatum))})
	}
	if !c.RefScript && !c.OmitScript {
		key := map[int]uint64{1: 3, 2: 6, 3: 7}[c.Script.Lang]
		wits = append(wits, kv{key, cArr(cBytes(c.Script.Wrapper))})
	}
	sortKV(wits)

	valid := raw{0xf5}
	if !c.IsValid {
		valid = raw{0xf4}
	}
	tx := cArr(cMap(body...), cMap(wits...), valid, raw{0xf6})

	st := &v1.LedgerState{Pots: &v1.Pots{Treasury: 1_000_000_000_000}}
	st.Utxo = append(st.Utxo,
		&v1.UtxoEntry{Txin: &v1.TxIn{TxId: txid(0xa1)}, Txout: in},
		&v1.UtxoEntry{Txin: &v1.TxIn{TxId: txid(0xb2)}, Txout: col},
	)
	if refInput {
		st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: txid(0xc3)}, Txout: ref})
	}
	return tx, st
}

func sortKV(m []kv) {
	for i := 1; i < len(m); i++ {
		for j := i; j > 0 && m[j].k < m[j-1].k; j-- {
			m[j], m[j-1] = m[j-1], m[j]
		}
	}
}

// UtxosEnv returns the context, globals and parameters for a UTXOS run: the
// era's Env with the three cost models and the mainnet era history.
func UtxosEnv(era v1.Era) (*v1.LedgerContext, *v1.Globals, *v1.ProtocolParameters, *v1.EraHistory) {
	ctx, g, pp := Env(era)
	WithCostModels(pp)
	return ctx, g, pp, MainnetEraHistory()
}

// OneshotMint is a real mainnet Conway transaction minting with a PlutusV3
// policy, with its on-chain inputs, reference input and collateral. It ran
// under epoch 653 parameters (protocol version 11).
func OneshotMint() ([]byte, *v1.LedgerState, error) {
	read := func(name string) ([]byte, error) {
		s, err := plutusData.ReadFile("testdata/" + name)
		if err != nil {
			return nil, err
		}
		return hex.DecodeString(strings.TrimSpace(string(s)))
	}
	tx, err := read("mainnet_tx_oneshot_mint.hex")
	if err != nil {
		return nil, nil, err
	}
	st := &v1.LedgerState{Pots: &v1.Pots{Treasury: 1_000_000_000_000}}
	for _, p := range [][2]string{
		{"oneshot_mint_inputs.hex", "oneshot_mint_input_outputs.hex"},
		{"oneshot_mint_ref_input.hex", "oneshot_mint_ref_output.hex"},
		{"oneshot_mint_collateral_input.hex", "oneshot_mint_collateral_output.hex"},
	} {
		ins, err := splitArray(read, p[0])
		if err != nil {
			return nil, nil, err
		}
		outs, err := splitArray(read, p[1])
		if err != nil {
			return nil, nil, err
		}
		if len(ins) != len(outs) {
			return nil, nil, fmt.Errorf("%s: %d inputs, %d outputs", p[0], len(ins), len(outs))
		}
		for i := range ins {
			var in []cbor.RawMessage
			if _, err := cbor.Decode(ins[i], &in); err != nil || len(in) != 2 {
				return nil, nil, fmt.Errorf("%s: input %d: %v", p[0], i, err)
			}
			var id []byte
			var idx uint32
			if _, err := cbor.Decode(in[0], &id); err != nil {
				return nil, nil, err
			}
			if _, err := cbor.Decode(in[1], &idx); err != nil {
				return nil, nil, err
			}
			st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: id, Index: idx}, Txout: outs[i]})
		}
	}
	return tx, st, nil
}

// splitArray returns the elements of a CBOR array, or the item itself when
// the file holds one input or output rather than an array of them.
func splitArray(read func(string) ([]byte, error), name string) ([][]byte, error) {
	b, err := read(name)
	if err != nil {
		return nil, err
	}
	var items []cbor.RawMessage
	if _, err := cbor.Decode(b, &items); err != nil {
		return nil, fmt.Errorf("%s: %w", name, err)
	}
	var out [][]byte
	for _, i := range items {
		out = append(out, []byte(i))
	}
	return out, nil
}

// AlonzoPlutusV1 returns the transaction of the Alonzo mainnet block that
// carries a PlutusV1 script, with a synthesised ledger state: the input its
// spending redeemer points at is locked by the transaction's own script and
// holds the datum the transaction supplies; the other inputs and the
// collateral are plain key-locked outputs.
func AlonzoPlutusV1() ([]byte, *v1.LedgerState, error) {
	txs, err := BlockTxs(v1.Era_ERA_ALONZO)
	if err != nil {
		return nil, nil, err
	}
	for _, raw := range txs {
		tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeAlonzo, raw)
		if err != nil {
			return nil, nil, err
		}
		w := tx.Witnesses()
		if len(w.PlutusV1Scripts()) == 0 || len(w.PlutusData()) == 0 || w.Redeemers() == nil {
			continue
		}
		st, err := State(v1.Era_ERA_ALONZO, 1, raw)
		if err != nil {
			return nil, nil, err
		}
		inputs := tx.Inputs()
		sort.Slice(inputs, func(i, j int) bool {
			if c := bytes.Compare(inputs[i].Id().Bytes(), inputs[j].Id().Bytes()); c != 0 {
				return c < 0
			}
			return inputs[i].Index() < inputs[j].Index()
		})
		for key := range w.Redeemers().Iter() {
			if key.Tag != common.RedeemerTagSpend || int(key.Index) >= len(inputs) {
				continue
			}
			in := inputs[key.Index]
			for _, e := range st.Utxo {
				if !bytes.Equal(e.Txin.TxId, in.Id().Bytes()) || e.Txin.Index != in.Index() {
					continue
				}
				old, err := ledger.NewTransactionOutputFromCbor(e.Txout)
				if err != nil {
					return nil, nil, err
				}
				h := common.PlutusV1Script(w.PlutusV1Scripts()[0]).Hash()
				d := w.PlutusData()[0].Hash()
				e.Txout = output(v1.Era_ERA_ALONZO, scriptAddr(h), old.Amount().Uint64(), d.Bytes(), nil, nil)
			}
		}
		return raw, st, nil
	}
	return nil, nil, fmt.Errorf("no PlutusV1 transaction in the Alonzo block")
}
