package main

import (
	"context"
	"encoding/hex"
	"sort"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// A mainnet Conway transaction (one input, two outputs, fee 169417, validity
// start 55449912, ttl 159842130) whose input is synthesised to hold exactly
// outputs plus fee. It needs only a key witness.
const (
	ruleTxHex      = "84a50081825820876087c685570cd91a6e6ff60120cc55a9105f56576b6b519136850ab66845e401018282583901228ef2e891696d804b705b75660503d8cc1ec4d01a8af20a3697f12d33d1dc483f1c0db883685f10360ba04719f23222d4951e288188e03c1a013de7e78258390162e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de5128c1198de6eba555d96ba1d71435099083ce5c156359525a6407e2e6c1b00000040b4cda07c021a000295c9031a0986ff52081a034e1938a1008182582020fc80626e97534d41073ad5067bbca2eb2b43b4679462c3b69d7f68e7e582b65840aa6d087d15a428279687dbebbae62d3c5f2e6dfeb07ce6684fee69afa685edc28d46a5d126bcafea9e3e1b891431c4d8272bf25ada33bd581cf8f7f75150960bf5f6"
	ruleInputTxID  = "876087c685570cd91a6e6ff60120cc55a9105f56576b6b519136850ab66845e4"
	ruleInputOut   = "a200581d6162e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de512011b00000040b60e1e2c"
	ruleSlot       = 159842129
	ruleTTL        = 159842130
	ruleInputCoins = 0x40b60e1e2c
)

func ruleRequest(t *testing.T) *v1.RuleUtxowRequest {
	t.Helper()
	ctx, g, pp := rulefixture.Env(v1.Era_ERA_CONWAY)
	ctx.Slot = ruleSlot
	tx, _ := hex.DecodeString(ruleTxHex)
	txid, _ := hex.DecodeString(ruleInputTxID)
	out, _ := hex.DecodeString(ruleInputOut)
	return &v1.RuleUtxowRequest{
		Context: ctx, Globals: g, Pparams: pp, Tx: tx,
		State: &v1.LedgerState{Utxo: []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: txid, Index: 1}, Txout: out}}},
	}
}

func failuresOf(rej *v1.Rejection) []string {
	var out []string
	for _, f := range rej.GetFailures() {
		out = append(out, f.GetRule()+"/"+f.GetName())
	}
	sort.Strings(out)
	return out
}

func runUtxow(t *testing.T, req *v1.RuleUtxowRequest) string {
	t.Helper()
	resp, err := ruleUtxow(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Join(failuresOf(resp.GetRejection()), ",")
}

// Expected failures are the constructors of Cardano.Ledger.Conway.Rules.Utxo
// and Utxow, each under the rule that owns it.
func TestRuleUtxow(t *testing.T) {
	t.Parallel()
	u64 := func(x uint64) *uint64 { return &x }
	for name, tc := range map[string]struct {
		mutate func(*v1.RuleUtxowRequest)
		want   string
	}{
		"valid": {func(*v1.RuleUtxowRequest) {}, ""},
		// validateOutsideValidityIntervalUTxO: the interval is [start, ttl).
		"last slot before ttl": {func(r *v1.RuleUtxowRequest) { r.Context.Slot = ruleTTL - 1 }, ""},
		"slot equals ttl":      {func(r *v1.RuleUtxowRequest) { r.Context.Slot = ruleTTL }, "UTXO/OutsideValidityIntervalUTxO"},
		"before validity start": {func(r *v1.RuleUtxowRequest) { r.Context.Slot = 55449911 },
			"UTXO/OutsideValidityIntervalUTxO"},
		"fee too small":    {func(r *v1.RuleUtxowRequest) { r.Pparams.MinFeeB = u64(10_000_000) }, "UTXO/FeeTooSmallUTxO"},
		"max tx size":      {func(r *v1.RuleUtxowRequest) { r.Pparams.MaxTxSize = proto.Uint32(100) }, "UTXO/MaxTxSizeUTxO"},
		"wrong network":    {func(r *v1.RuleUtxowRequest) { r.Context.NetworkId = 0 }, "UTXO/WrongNetwork"},
		"output too small": {func(r *v1.RuleUtxowRequest) { r.Pparams.CoinsPerUtxoByte = u64(1_000_000_000) }, "UTXO/BabbageOutputTooSmallUTxO"},
		// The input's lovelace no longer equals outputs plus fee.
		"value not conserved": {func(r *v1.RuleUtxowRequest) {
			r.State.Utxo[0] = rulefixture.Rebalance(v1.Era_ERA_CONWAY, r.State.Utxo[0], 1)
		}, "UTXO/ValueNotConservedUTxO"},
		// The input key did not sign.
		"missing witness": {func(r *v1.RuleUtxowRequest) {
			r.State.Utxo[0] = rulefixture.Rekey(v1.Era_ERA_CONWAY, r.State.Utxo[0])
		}, "UTXOW/MissingVKeyWitnessesUTXOW"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req := ruleRequest(t)
			tc.mutate(req)
			if got := runUtxow(t, req); got != tc.want {
				t.Fatalf("failures = %q, want %q", got, tc.want)
			}
		})
	}
}

// UTXO alone does not run the witness checks, so a missing key witness passes.
func TestRuleUtxoSkipsWitnessChecks(t *testing.T) {
	t.Parallel()
	r := ruleRequest(t)
	r.State.Utxo[0] = rulefixture.Rekey(v1.Era_ERA_CONWAY, r.State.Utxo[0])
	resp, err := ruleUtxo(context.Background(), &v1.RuleUtxoRequest{Context: r.Context, Globals: r.Globals, Pparams: r.Pparams, State: r.State, Tx: r.Tx})
	if err != nil {
		t.Fatal(err)
	}
	if got := failuresOf(resp.GetRejection()); len(got) != 0 {
		t.Fatalf("failures = %v, want none", got)
	}
}

func TestRuleBadTxIsBadRequest(t *testing.T) {
	t.Parallel()
	r := ruleRequest(t)
	r.Tx = r.Tx[:10]
	if _, err := ruleUtxow(context.Background(), r); err == nil {
		t.Fatal("truncated tx accepted")
	}
}
