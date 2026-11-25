package ledgerfixture

import (
	"fmt"
	"strings"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// LegacyBlockCase is one BBODY scenario of a Shelley-family era: the era's
// mainnet block with its body replaced, over a UTxO table synthesised for the
// body's transactions. Want and Delta are the answer unless Only restricts the
// eras the case runs in.
type LegacyBlockCase struct {
	Name string
	// Txs is how many plain mainnet transactions of the era's block form the
	// body (fewer when the block holds fewer); Own supplies the body and the
	// outputs it spends instead.
	Txs int
	// FirstTx keeps the first eligible transaction of the Babbage block, whose
	// created output the nodes encode differently; other cases skip it.
	FirstTx bool
	Own     func(v1.Era) (Body, []*v1.UtxoEntry)
	Edit    func(v1.Era) Edit
	Setup   func(v1.Era, *v1.RuleBlockBodyRequest)
	Want    []string
	// Delta is the digest of the accepted delta; empty with Txs set means the
	// transactions' own digest.
	Delta string
	// Totals is as in BlockCase.
	Totals string
	Only   []v1.Era
	// Diverges lists the eras in which dxnode-go answers differently from the
	// reference: the case is a divergence there, not a shared scenario.
	Diverges []v1.Era
}

func (c LegacyBlockCase) divergesIn(era v1.Era) bool {
	for _, e := range c.Diverges {
		if e == era {
			return true
		}
	}
	return false
}

func (c LegacyBlockCase) runsIn(era v1.Era) bool {
	if len(c.Only) == 0 {
		return true
	}
	for _, e := range c.Only {
		if e == era {
			return true
		}
	}
	return false
}

// mainnetBody is the first n transactions of the era's block that need nothing
// beyond key witnesses and whose inputs can be synthesised, with those inputs.
func mainnetBody(era v1.Era, n int, firstTx bool) (Body, []*v1.UtxoEntry, string) {
	raw, _, err := rulefixture.BlockCbor(era)
	if err != nil {
		panic(err)
	}
	all, err := BodyOf(raw)
	if err != nil {
		panic(err)
	}
	var body Body
	var utxo []*v1.UtxoEntry
	var in, out int
	var fee uint64
	eligible := 0
	for _, tx := range all {
		if len(body) == n {
			break
		}
		if !rulefixture.Plain(era, tx) {
			continue
		}
		st, err := rulefixture.State(era, 1, tx)
		if err != nil {
			continue
		}
		eligible++
		if era == v1.Era_ERA_BABBAGE && eligible == 1 && !firstTx {
			continue
		}
		parsed, err := ledger.NewTransactionFromCbor(rulefixture.TxType(era), tx)
		if err != nil {
			panic(err)
		}
		body = append(body, tx)
		utxo = append(utxo, st.Utxo...)
		in += len(parsed.Inputs())
		out += len(parsed.Outputs())
		fee += parsed.Fee().Uint64()
	}
	return body, utxo, fmt.Sprintf("U+=%d U-=%d fee=%d", out, in, fee)
}

// LegacyBlockRequest builds the request of a case in an era.
func LegacyBlockRequest(era v1.Era, c LegacyBlockCase) (*v1.RuleBlockBodyRequest, string) {
	raw, typ, err := rulefixture.BlockCbor(era)
	if err != nil {
		panic(err)
	}
	ctx, g, pp := rulefixture.Env(era)
	st := &v1.LedgerState{Pots: &v1.Pots{Treasury: 1_000_000_000_000}}
	var body Body
	delta := c.Delta
	if c.Own != nil {
		body, st.Utxo = c.Own(era)
	} else {
		var d string
		body, st.Utxo, d = mainnetBody(era, c.Txs, c.FirstTx)
		if delta == "" && len(c.Want) == 0 {
			delta = d
		}
	}
	var edit Edit
	if c.Edit != nil {
		edit = c.Edit(era)
	}
	blk, err := BlockWith(raw, body, edit)
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
		c.Setup(era, req)
	}
	totals := c.Totals
	if totals == "" {
		totals = "ex=0/0 ref=0"
	}
	return req, strings.TrimSpace(delta + " " + totals)
}

// chainedLegacyPayments is two payments by the payer in the era's wire shape,
// the second spending the first's output.
func chainedLegacyPayments(era v1.Era) (Body, []*v1.UtxoEntry) {
	const total = 300_000_000_000
	first := LegacyLedgerBody{Input: 0x31, Output: total - Fee, Fee: Fee, PayToPayer: true, Signers: []Signer{payer}}.Tx(era)
	id := common.Blake2b256Hash(txBody(first)).Bytes()
	second := LegacyLedgerBody{InputTx: id, Output: total - 2*Fee, Fee: Fee, Signers: []Signer{payer}}.Tx(era)
	return Body{first, second}, []*v1.UtxoEntry{LedgerUtxo(0x31, payer.Addr(), total)}
}

// legacyPlutusMint is a minting transaction with a Plutus script declaring
// 1000000 memory and 500000000 steps. Like the Conway fixture it is unsigned
// and unbalanced: it is there for its declared execution units.
func legacyPlutusMint(era v1.Era) (Body, []*v1.UtxoEntry) {
	lang := 2
	if era == v1.Era_ERA_ALONZO {
		lang = 1
	}
	tx, st := rulefixture.PlutusCase{
		Era: era, Script: rulefixture.NewScript(lang, false, true), Mint: true, IsValid: true, CollateralReturn: era != v1.Era_ERA_ALONZO,
	}.Build()
	return Body{tx}, st.Utxo
}

var alonzoOn = []v1.Era{v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE}

// legacyMintFailures are the failures of the Plutus minting fixture: it is
// unsigned and unbalanced.
var legacyMintFailures = []string{
	"UTXO/FeeTooSmallUTxO", "UTXO/ValueNotConservedUTxO", "UTXOW/MissingVKeyWitnessesUTXOW", "UTXOW/PPViewHashesDontMatch",
}

func withMintBudget(mem, steps *uint64) func(v1.Era, *v1.RuleBlockBodyRequest) {
	return func(_ v1.Era, r *v1.RuleBlockBodyRequest) {
		rulefixture.WithCostModels(r.Pparams)
		if mem != nil {
			r.Pparams.MaxBlockExMem = mem
		}
		if steps != nil {
			r.Pparams.MaxBlockExSteps = steps
		}
	}
}

func rekeyFirst(era v1.Era, r *v1.RuleBlockBodyRequest) {
	r.State.Utxo[0] = rulefixture.Rekey(era, r.State.Utxo[0])
}

// headerAhead sets the header's protocol major version n ahead of the
// ledger's.
func headerAhead(n uint64) func(v1.Era) Edit {
	return func(era v1.Era) Edit {
		ctx, _, _ := rulefixture.Env(era)
		return Edit{HeaderMajor: uint64(ctx.GetProtocolVersion().GetMajor()) + n}
	}
}

// LegacyBlockDivergences are the Shelley-to-Babbage BBODY scenarios on which
// the nodes disagree, with the reference's answer. Dingo applies the
// header-version limit to every era; the reference's BBODY has it from Conway
// (HeaderProtVerTooHigh is a constructor of the Conway rule alone).
func LegacyBlockDivergences() []LegacyBlockCase {
	return []LegacyBlockCase{
		{Name: "bbody header two major versions ahead", Txs: 2, Edit: headerAhead(2), Diverges: LegacyEras},
	}
}

// LegacyBlockCases returns the Shelley-to-Babbage BBODY scenarios both nodes
// agree on.
func LegacyBlockCases() []LegacyBlockCase {
	size := func(d int) func(v1.Era) Edit { return func(v1.Era) Edit { return Edit{SizeDelta: d} } }
	return []LegacyBlockCase{
		// Babbage's first transaction creates an output whose asset names are
		// in canonical CBOR order, which Dingo stores as written and the
		// reference re-encodes in byte order; the other cases leave it out.
		{Name: "bbody two mainnet transactions", Txs: 2, FirstTx: true, Diverges: []v1.Era{v1.Era_ERA_BABBAGE}},
		{Name: "bbody header one major version ahead", Txs: 2, Edit: headerAhead(1)},
		{Name: "bbody whose second transaction spends the first", Own: chainedLegacyPayments, Delta: "U+=1 U-=1 fee=400000"},
		{Name: "bbody declaring a body size one too large", Txs: 2, Edit: size(1), Want: []string{"BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody declaring a body size one too small", Txs: 2, Edit: size(-1), Want: []string{"BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody whose header commits to another body", Txs: 2, Edit: func(v1.Era) Edit { return Edit{FlipHash: true} },
			Want: []string{"BBODY/InvalidBodyHashBBODY"}},
		{Name: "bbody with a wrong size and a wrong hash", Txs: 2, Edit: func(v1.Era) Edit { return Edit{SizeDelta: 1, FlipHash: true} },
			Want: []string{"BBODY/InvalidBodyHashBBODY", "BBODY/WrongBlockBodySizeBBODY"}},
		{Name: "bbody with a transaction that fails", Txs: 2, Setup: rekeyFirst, Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "bbody with a fee below the minimum", Txs: 2,
			Setup: func(_ v1.Era, r *v1.RuleBlockBodyRequest) { r.Pparams.MinFeeB = u64p(10_000_000) },
			Want:  []string{"UTXO/FeeTooSmallUTxO"}},
		{Name: "bbody with an unbalanced transaction", Txs: 2,
			Setup: func(era v1.Era, r *v1.RuleBlockBodyRequest) {
				r.State.Utxo[0] = rulefixture.Rebalance(era, r.State.Utxo[0], 1)
			},
			Want: []string{"UTXO/ValueNotConservedUTxO"}},
		{Name: "bbody on the wrong network", Txs: 2,
			Setup: func(_ v1.Era, r *v1.RuleBlockBodyRequest) { r.Context.NetworkId = 0 },
			Want:  []string{"UTXO/WrongNetwork"}},
		{Name: "bbody with transactions over the size limit", Txs: 2,
			Setup: func(_ v1.Era, r *v1.RuleBlockBodyRequest) { r.Pparams.MaxTxSize = ptr32(100) },
			Want:  []string{"UTXO/MaxTxSizeUTxO"}},
		{Name: "bbody with a failing transaction and a wrong size", Txs: 2, Edit: size(1), Setup: rekeyFirst,
			Want: []string{"BBODY/WrongBlockBodySizeBBODY", "UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "bbody execution units within the block budget", Own: legacyPlutusMint, Setup: withMintBudget(nil, nil), Only: alonzoOn,
			Want: legacyMintFailures, Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution memory one over the block budget", Own: legacyPlutusMint, Setup: withMintBudget(u64p(999_999), nil), Only: alonzoOn,
			Want: append([]string{"BBODY/TooManyExUnits"}, legacyMintFailures...), Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution memory exactly at the block budget", Own: legacyPlutusMint, Setup: withMintBudget(u64p(1_000_000), nil), Only: alonzoOn,
			Want: legacyMintFailures, Totals: "ex=1000000/500000000 ref=0"},
		{Name: "bbody execution steps one over the block budget", Own: legacyPlutusMint, Setup: withMintBudget(nil, u64p(499_999_999)), Only: alonzoOn,
			Want: append([]string{"BBODY/TooManyExUnits"}, legacyMintFailures...), Totals: "ex=1000000/500000000 ref=0"},
	}
}

// legacyBlockName is the scenario name of a case in an era.
func legacyBlockName(era v1.Era, c LegacyBlockCase) string {
	short := strings.ToLower(strings.TrimPrefix(era.String(), "ERA_"))
	_, rest, _ := strings.Cut(c.Name, " ")
	return "bbody " + short + " " + rest
}

// LegacyBlockScenarios returns every case in every era as a call, except the
// ones on which the nodes disagree.
func LegacyBlockScenarios() []Scenario { return legacyBlockScenarios(LegacyBlockCases(), false) }

// LegacyBlockDivergenceScenarios returns the case-era pairs on which the nodes
// disagree as calls, with the reference's answer.
func LegacyBlockDivergenceScenarios() []Scenario {
	return legacyBlockScenarios(append(LegacyBlockCases(), LegacyBlockDivergences()...), true)
}

func legacyBlockScenarios(cases []LegacyBlockCase, diverging bool) []Scenario {
	var out []Scenario
	for _, era := range LegacyEras {
		for _, c := range cases {
			if !c.runsIn(era) || c.divergesIn(era) != diverging {
				continue
			}
			req, delta := LegacyBlockRequest(era, c)
			out = append(out, Scenario{
				Name: legacyBlockName(era, c), Fn: "rule_block_body", Want: c.Want, Delta: delta,
				Call: &v1.Call{Request: &v1.Call_RuleBlockBody{RuleBlockBody: req}},
			})
		}
	}
	return out
}
