package corpusgen

import (
	"fmt"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

func TestGenerateRuleCorpus(t *testing.T) {
	writeCorpus(t, "", ruleCases(t))
	writeCorpus(t, "divergences", ruleDivergenceCases(t))
}

// ruleBase is one real mainnet transaction with a synthesised UTxO table.
type ruleBase struct {
	ctx *v1.LedgerContext
	g   *v1.Globals
	pp  *v1.ProtocolParameters
	st  *v1.LedgerState
	tx  []byte
	ttl bool
}

func (b ruleBase) clone() ruleBase {
	b.ctx = proto.Clone(b.ctx).(*v1.LedgerContext)
	b.pp = proto.Clone(b.pp).(*v1.ProtocolParameters)
	b.st = proto.Clone(b.st).(*v1.LedgerState)
	return b
}

func (b ruleBase) utxo() *v1.RuleUtxoRequest {
	return &v1.RuleUtxoRequest{Context: b.ctx, Globals: b.g, Pparams: b.pp, State: b.st, Tx: b.tx}
}

func (b ruleBase) utxow() *v1.RuleUtxowRequest {
	return &v1.RuleUtxowRequest{Context: b.ctx, Globals: b.g, Pparams: b.pp, State: b.st, Tx: b.tx}
}

func ruleBases(t *testing.T, era v1.Era, max int) []ruleBase {
	t.Helper()
	txs, err := rulefixture.BlockTxs(era)
	if err != nil {
		t.Fatal(err)
	}
	var out []ruleBase
	for _, tx := range txs {
		if len(out) == max {
			break
		}
		if !rulefixture.Plain(era, tx) {
			continue
		}
		st, err := rulefixture.State(era, 1, tx)
		if err != nil {
			continue
		}
		slot, err := rulefixture.SlotFor(era, tx)
		if err != nil {
			t.Fatal(err)
		}
		ctx, g, pp := rulefixture.Env(era)
		ctx.Slot = slot
		out = append(out, ruleBase{ctx: ctx, g: g, pp: pp, st: st, tx: tx})
	}
	return out
}

type ruleMutation struct {
	name, why string
	apply     func(*ruleBase)
}

func ruleMutations() []ruleMutation {
	u64 := func(x uint64) *uint64 { return &x }
	return []ruleMutation{
		{"valid", "unmodified: the synthesised state balances the transaction", func(*ruleBase) {}},
		{"fee_too_small", "min_fee_b raised far above the tx fee", func(b *ruleBase) { b.pp.MinFeeB = u64(10_000_000) }},
		{"value_not_conserved", "first input holds a different amount than outputs plus fee", func(b *ruleBase) {
			b.st.Utxo[0] = rulefixture.Rebalance(b.ctx.GetEra(), b.st.GetUtxo()[0], 1)
		}},
		{"bad_inputs", "first input is missing from the UTxO", func(b *ruleBase) { b.st.Utxo = b.st.Utxo[1:] }},
		{"max_tx_size", "max_tx_size below the transaction size", func(b *ruleBase) { b.pp.MaxTxSize = proto.Uint32(100) }},
		{"wrong_network", "ledger network is testnet, addresses are mainnet", func(b *ruleBase) { b.ctx.NetworkId = 0 }},
		{"output_too_small", "coins_per_utxo_byte raised so every output is below the minimum", func(b *ruleBase) {
			if b.ctx.GetEra() == v1.Era_ERA_ALONZO {
				b.pp.CoinsPerUtxoWord = u64(1_000_000_000)
			} else {
				b.pp.CoinsPerUtxoByte = u64(1_000_000_000)
			}
		}},
		{"missing_witness", "first input belongs to a key that did not sign", func(b *ruleBase) {
			b.st.Utxo[0] = rulefixture.Rekey(b.ctx.GetEra(), b.st.GetUtxo()[0])
		}},
		{"outside_validity_upper", "slot equals the ttl: the interval is half open", func(b *ruleBase) { b.ctx.Slot++ }},
	}
}

func ruleCases(t *testing.T) []entry { return buildRuleCases(t, false) }

// ruleDivergenceCases holds the confirmed mismatches: with an input missing
// from the UTxO the reference reports BadInputsUTxO and ValueNotConservedUTxO,
// gouroboros adds MissingRedeemers (TestDivergenceRequiredRedeemersFailsOnUnresolvedInput).
// A transaction with an unresolved input is refused by both nodes, and the
// production rule order reaches bad-inputs first, so the extra failure is
// not reachable as a different accept or reject.
func ruleDivergenceCases(t *testing.T) []entry { return buildRuleCases(t, true) }

func buildRuleCases(t *testing.T, divergences bool) []entry {
	var out []entry
	for _, era := range []v1.Era{v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE, v1.Era_ERA_CONWAY} {
		eraName := strings.ToLower(strings.TrimPrefix(era.String(), "ERA_"))
		for i, base := range ruleBases(t, era, 3) {
			for _, m := range ruleMutations() {
				b := base.clone()
				m.apply(&b)
				for _, fn := range []string{"utxo", "utxow"} {
					if fn == "utxo" && m.name == "missing_witness" {
						continue
					}
					if divergent := fn == "utxow" && m.name == "bad_inputs" && era != v1.Era_ERA_ALONZO; divergent != divergences {
						continue
					}
					name := fmt.Sprintf("rule_%s_%s_tx%d_%s", fn, eraName, i, m.name)
					why := fmt.Sprintf("%s tx %d of the %s mainnet block: %s", fn, i, eraName, m.why)
					var call *v1.Call
					if fn == "utxo" {
						call = &v1.Call{Request: &v1.Call_RuleUtxo{RuleUtxo: b.utxo()}}
					} else {
						call = &v1.Call{Request: &v1.Call_RuleUtxow{RuleUtxow: b.utxow()}}
					}
					out = append(out, entry{name, why, call})
				}
			}
		}
	}
	return out
}
