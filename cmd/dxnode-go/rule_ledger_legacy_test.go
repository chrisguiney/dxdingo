package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// TestRuleLedgerLegacyCases runs the Shelley-to-Babbage LEDGER scenarios, whose
// expected failures and delta digests are derived from the reference's
// ShelleyLedgerPredFailure and the Shelley-era rules, and which the Haskell
// side runs from the same generated data.
func TestRuleLedgerLegacyCases(t *testing.T) {
	t.Parallel()
	for _, era := range lf.LegacyEras {
		for _, s := range lf.LegacyLedgerScenarios() {
			req := s.Call.GetRuleLedger()
			if req.GetContext().GetEra() != era {
				continue
			}
			t.Run(s.Name, func(t *testing.T) {
				t.Parallel()
				resp, err := ruleLedger(context.Background(), req)
				require.NoError(t, err)
				var got []string
				for _, f := range resp.GetRejection().GetFailures() {
					got = append(got, f.GetRule()+"/"+f.GetName())
				}
				sort.Strings(got)
				require.Equal(t, s.Want, got)
				if s.Want == nil {
					require.NotNil(t, resp.GetDelta())
					require.Equal(t, s.Delta, lf.Digest(resp.GetDelta()))
					require.True(t, resp.GetScriptsValid())
				} else {
					require.Nil(t, resp.GetDelta())
				}
			})
		}
	}
}

// A real transaction with no certificates or withdrawals is judged by UTXOW
// alone, and LEDGER embeds UTXOW: the two rules must name the same failures.
func TestRuleLedgerLegacyAgreesWithUtxowOnMainnetTransactions(t *testing.T) {
	t.Parallel()
	for _, era := range lf.LegacyEras {
		txs, err := rulefixture.BlockTxs(era)
		require.NoError(t, err)
		n := 0
		for _, tx := range txs {
			if n == 2 || !rulefixture.Plain(era, tx) {
				continue
			}
			st, err := rulefixture.State(era, 1, tx)
			if err != nil {
				continue
			}
			n++
			slot, err := rulefixture.SlotFor(era, tx)
			require.NoError(t, err)
			for name, mutate := range map[string]func(*v1.LedgerContext, *v1.ProtocolParameters, *v1.LedgerState){
				"valid": func(*v1.LedgerContext, *v1.ProtocolParameters, *v1.LedgerState) {},
				"fee too small": func(_ *v1.LedgerContext, pp *v1.ProtocolParameters, _ *v1.LedgerState) {
					b := uint64(10_000_000)
					pp.MinFeeB = &b
				},
				"missing witness": func(c *v1.LedgerContext, _ *v1.ProtocolParameters, s *v1.LedgerState) {
					s.Utxo[0] = rulefixture.Rekey(c.GetEra(), s.GetUtxo()[0])
				},
				"wrong network": func(c *v1.LedgerContext, _ *v1.ProtocolParameters, _ *v1.LedgerState) { c.NetworkId = 0 },
			} {
				t.Run(era.String()+"/"+name, func(t *testing.T) {
					ctx, g, pp := rulefixture.Env(era)
					ctx.Slot = slot
					state := proto.Clone(st).(*v1.LedgerState)
					mutate(ctx, pp, state)
					ledgerResp, err := ruleLedger(context.Background(), &v1.RuleLedgerRequest{Context: ctx, Globals: g, Pparams: pp, State: state, Tx: tx})
					require.NoError(t, err)
					utxowResp, err := ruleUtxow(context.Background(), &v1.RuleUtxowRequest{Context: ctx, Globals: g, Pparams: pp, State: state, Tx: tx})
					require.NoError(t, err)
					require.Equal(t, rejectionNames(utxowResp.GetRejection()), rejectionNames(ledgerResp.GetRejection()))
					require.Equal(t, name != "valid", ledgerResp.GetRejection() != nil)
				})
			}
		}
		require.NotZero(t, n, "no eligible transaction in the %v block", era)
	}
}

func rejectionNames(r *v1.Rejection) []string {
	var out []string
	for _, f := range r.GetFailures() {
		out = append(out, f.GetRule()+"/"+f.GetName())
	}
	sort.Strings(out)
	return out
}
