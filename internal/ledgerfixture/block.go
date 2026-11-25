package ledgerfixture

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// BlockCase is one BBODY scenario: a mainnet Conway block whose body is
// replaced, over a UTxO table synthesised for the body's transactions.
type BlockCase struct {
	Name string
	// Txs picks the transactions of the mainnet block by index; Own, when
	// set, supplies them instead (with the state they spend from).
	Txs   []int
	Own   func() (Body, []*v1.UtxoEntry)
	Edit  Edit
	Setup func(*v1.RuleBlockBodyRequest)
	Want  []string
	// Delta is the digest of the accepted delta.
	Delta string
	// Totals is what the response reports of the block's execution units and
	// reference-script bytes, accepted or not: "ex=<mem>/<steps> ref=<bytes>".
	// Empty means a block that declares none of either.
	Totals string
}

// Expected is the digest a node's answer to the case must have: the delta's
// digest, then the totals.
func (c BlockCase) Expected() string {
	totals := c.Totals
	if totals == "" {
		totals = "ex=0/0 ref=0"
	}
	return strings.TrimSpace(c.Delta + " " + totals)
}

// BlockDigest is Digest of the delta and the response's totals.
func BlockDigest(r *v1.RuleBlockBodyResponse) string {
	return strings.TrimSpace(fmt.Sprintf("%s ex=%d/%d ref=%d", Digest(r.GetDelta()), r.GetExMemTotal(), r.GetExStepsTotal(), r.GetRefScriptBytes()))
}

// BlockRequest builds the request of a case.
func BlockRequest(c BlockCase) *v1.RuleBlockBodyRequest {
	raw, typ, err := rulefixture.BlockCbor(v1.Era_ERA_CONWAY)
	if err != nil {
		panic(err)
	}
	ctx, g, pp := rulefixture.Env(v1.Era_ERA_CONWAY)
	st := &v1.LedgerState{Pots: &v1.Pots{Treasury: 1_000_000_000_000}}
	var body Body
	if c.Own != nil {
		var utxo []*v1.UtxoEntry
		body, utxo = c.Own()
		st.Utxo = utxo
	} else {
		mainnet, err := BodyOf(raw)
		if err != nil {
			panic(err)
		}
		for _, i := range c.Txs {
			body = append(body, mainnet[i])
			// A transaction with no vkey witness (a Byron bootstrap spend)
			// has no synthesised inputs; its inputs are left unresolved.
			if one, err := rulefixture.State(v1.Era_ERA_CONWAY, 1, mainnet[i]); err == nil {
				st.Utxo = append(st.Utxo, one.Utxo...)
			}
		}
	}
	blk, err := BlockWith(raw, body, c.Edit)
	if err != nil {
		panic(err)
	}
	decoded, err := ledger.NewBlockFromCbor(typ, raw)
	if err != nil {
		panic(err)
	}
	ctx.Slot = decoded.SlotNumber()
	ctx.Epoch = ctx.Slot / 432_000
	req := &v1.RuleBlockBodyRequest{Context: ctx, Globals: g, Pparams: pp, State: st, Block: blk}
	if c.Setup != nil {
		c.Setup(req)
	}
	return req
}

// chainedPayments is two payments by the payer, the second spending the
// first's output: valid only against the state the first leaves.
func chainedPayments() (Body, []*v1.UtxoEntry) {
	const total = 300_000_000_000
	first := LedgerBody{Input: 0x31, Output: total - Fee, Fee: Fee, OutputAddr: payer.Addr(), Signers: []Signer{payer}}.Tx()
	id := common.Blake2b256Hash(txBody(first)).Bytes()
	second := LedgerBody{InputTx: id, Output: total - 2*Fee, Fee: Fee, OutputAddr: payer.Addr(), Signers: []Signer{payer}}.Tx()
	return Body{first, second}, []*v1.UtxoEntry{LedgerUtxo(0x31, payer.Addr(), total)}
}

// txBody is the body bytes of a transaction.
func txBody(tx []byte) []byte {
	parts, err := splitTx(tx)
	if err != nil {
		panic(err)
	}
	return parts[0]
}

// BlockCases returns the BBODY scenarios both nodes agree on.
func BlockCases() []BlockCase {
	return []BlockCase{
		{Name: "bbody two mainnet transactions", Txs: []int{0, 2},
			Delta: mainnetDigest(0, 2)},
		{Name: "bbody whose second transaction spends the first", Own: chainedPayments,
			Delta: "U+=1 U-=1 fee=400000"},
		{Name: "bbody declaring a body size one too large", Txs: []int{0, 2}, Edit: Edit{SizeDelta: 1},
			Want: []string{"BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody declaring a body size one too small", Txs: []int{0, 2}, Edit: Edit{SizeDelta: -1},
			Want: []string{"BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody whose header commits to another body", Txs: []int{0, 2}, Edit: Edit{FlipHash: true},
			Want: []string{"BBODY/InvalidBodyHashBBODY"}},
		{Name: "bbody with a wrong size and a wrong hash", Txs: []int{0, 2}, Edit: Edit{SizeDelta: 1, FlipHash: true},
			Want: []string{"BBODY/InvalidBodyHashBBODY", "BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody header two major versions ahead", Txs: []int{0, 2}, Edit: Edit{HeaderMajor: 12},
			Want: []string{"BBODY/HeaderProtVerTooHigh"}},
		{Name: "bbody header one major version ahead", Txs: []int{0, 2}, Edit: Edit{HeaderMajor: 11},
			Delta: mainnetDigest(0, 2)},
		{Name: "bbody reference scripts exactly at the block limit", Own: refScriptBlock(refTxMax, refTxMax, refTxMax, refTxMax, refTxMax, 24_576),
			Delta: "U+=6 U-=6 fee=60000000", Totals: "ex=0/0 ref=1048576"},
		{Name: "bbody reference scripts one byte over the block limit", Own: refScriptBlock(refTxMax, refTxMax, refTxMax, refTxMax, refTxMax, 24_577),
			Want: []string{"BBODY/BodyRefScriptsSizeTooBig"}, Totals: "ex=0/0 ref=1048577"},
		{Name: "bbody execution units within the block budget", Own: plutusMint, Setup: withCostModels,
			Want: plutusMintFailures, Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution memory one over the block budget", Own: plutusMint,
			Setup: func(r *v1.RuleBlockBodyRequest) { withCostModels(r); r.Pparams.MaxBlockExMem = u64p(999_999) },
			Want:  append([]string{"BBODY/TooManyExUnits"}, plutusMintFailures...), Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution memory exactly at the block budget", Own: plutusMint,
			Setup: func(r *v1.RuleBlockBodyRequest) { withCostModels(r); r.Pparams.MaxBlockExMem = u64p(1_000_000) },
			Want:  plutusMintFailures, Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution steps one over the block budget", Own: plutusMint,
			Setup: func(r *v1.RuleBlockBodyRequest) { withCostModels(r); r.Pparams.MaxBlockExSteps = u64p(499_999_999) },
			Want:  append([]string{"BBODY/TooManyExUnits"}, plutusMintFailures...), Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody with a transaction that fails", Txs: []int{0, 2},
			Setup: func(r *v1.RuleBlockBodyRequest) {
				r.State.Utxo[0] = rulefixture.Rekey(v1.Era_ERA_CONWAY, r.State.Utxo[0])
			},
			Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "bbody with a failing transaction and a wrong size", Txs: []int{0, 2}, Edit: Edit{SizeDelta: 1},
			Setup: func(r *v1.RuleBlockBodyRequest) {
				r.State.Utxo[0] = rulefixture.Rekey(v1.Era_ERA_CONWAY, r.State.Utxo[0])
			},
			Want: []string{"BBODY/WrongBlockBodySizeBBODY", "UTXOW/MissingVKeyWitnessesUTXOW"}},
	}
}

// refTxMax is the largest reference script a transaction may consume.
const refTxMax = 200 * 1024

// plutusMintFailures are the failures of the Plutus minting fixture, which is
// unsigned and unbalanced: it is there for its declared execution units.
var plutusMintFailures = []string{
	"UTXO/FeeTooSmallUTxO", "UTXO/ValueNotConservedUTxO", "UTXOW/MissingVKeyWitnessesUTXOW", "UTXOW/PPViewHashesDontMatch",
}

func withCostModels(r *v1.RuleBlockBodyRequest) { rulefixture.WithCostModels(r.Pparams) }

func plutusMint() (Body, []*v1.UtxoEntry) {
	tx, st := rulefixture.PlutusCase{
		Era: v1.Era_ERA_CONWAY, Script: rulefixture.NewScript(2, false, true), Mint: true, IsValid: true, CollateralReturn: true,
	}.Build()
	return Body{tx}, st.Utxo
}

func refScriptBlock(sizes ...int) func() (Body, []*v1.UtxoEntry) {
	return func() (Body, []*v1.UtxoEntry) {
		const total, fee = 300_000_000_000, 10_000_000
		var body Body
		var utxo []*v1.UtxoEntry
		for i, size := range sizes {
			in, ref := byte(0x40+i), bytes.Repeat([]byte{byte(0x60 + i)}, 32)
			utxo = append(utxo, LedgerUtxo(in, payer.Addr(), total), refScriptUtxo(ref, size))
			body = append(body, LedgerBody{Input: in, Output: total - fee, Fee: fee, OutputAddr: payer.Addr(), RefInput: ref, Signers: []Signer{payer}}.Tx())
		}
		return body, utxo
	}
}

// refScriptUtxo is an output holding a PlutusV2 reference script of size bytes.
func refScriptUtxo(txid []byte, size int) *v1.UtxoEntry {
	script := enc([]any{uint(2), bytes.Repeat([]byte{0x01}, size)})
	out := enc(map[uint]any{0: payer.Addr(), 1: uint(2_000_000), 3: cbor.Tag{Number: 24, Content: []byte(script)}})
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: txid}, Txout: out}
}

func u64p(x uint64) *uint64 { return &x }

// mainnetDigest is the delta digest of the mainnet block's transactions at
// idx applied in order, read from the transactions themselves: every input is
// consumed, every output created, every fee collected.
func mainnetDigest(idx ...int) string {
	raw, _, err := rulefixture.BlockCbor(v1.Era_ERA_CONWAY)
	if err != nil {
		panic(err)
	}
	body, err := BodyOf(raw)
	if err != nil {
		panic(err)
	}
	var in, out int
	var fee uint64
	for _, i := range idx {
		tx, err := ledger.NewTransactionFromCbor(ledger.TxTypeConway, body[i])
		if err != nil {
			panic(err)
		}
		in += len(tx.Inputs())
		out += len(tx.Outputs())
		fee += tx.Fee().Uint64()
	}
	return fmt.Sprintf("U+=%d U-=%d fee=%d", out, in, fee)
}

// BlockScenarios returns the cases as calls.
func BlockScenarios() []Scenario {
	var out []Scenario
	for _, c := range BlockCases() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_block_body", Want: c.Want, Delta: c.Expected(),
			Call: &v1.Call{Request: &v1.Call_RuleBlockBody{RuleBlockBody: BlockRequest(c)}},
		})
	}
	return append(out, LegacyBlockScenarios()...)
}

// BlockDivergences are the BBODY scenarios on which the nodes disagree, with
// the failures the reference reports.
func BlockDivergences() []BlockCase {
	return []BlockCase{
		// LEDGERS runs every transaction of the block and reports the failures
		// of each; Dingo applies a block's transactions in order and stops at
		// the first that fails.
		{Name: "bbody two transactions failing different rules", Txs: []int{0, 2},
			Setup: func(r *v1.RuleBlockBodyRequest) {
				r.State.Utxo[0] = rulefixture.Rekey(v1.Era_ERA_CONWAY, r.State.Utxo[0])
				n := len(r.State.Utxo) - 1
				r.State.Utxo[n] = rulefixture.Rebalance(v1.Era_ERA_CONWAY, r.State.Utxo[n], 1)
			},
			Want: []string{"UTXO/ValueNotConservedUTxO", "UTXOW/MissingVKeyWitnessesUTXOW"}},
	}
}

// BlockDivergenceScenarios returns BlockDivergences as calls.
func BlockDivergenceScenarios() []Scenario {
	var out []Scenario
	for _, c := range BlockDivergences() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_block_body", Want: c.Want, Delta: c.Expected(),
			Call: &v1.Call{Request: &v1.Call_RuleBlockBody{RuleBlockBody: BlockRequest(c)}},
		})
	}
	return out
}
