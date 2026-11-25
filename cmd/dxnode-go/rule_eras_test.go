package main

import (
	"context"
	"encoding/hex"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// One mainnet transaction per pre-Alonzo era, each with a single synthesised
// input holding exactly outputs plus fee. The same cases run in dxnode-hs
// (test/Test/Rule.hs).
type legacyEra struct {
	era        v1.Era
	tx         string
	inTxID     string
	inIndex    uint32
	inOut      string
	slot       uint64 // last slot inside the validity interval
	ttl        uint64
	expiryName string // the constructor for a slot past the ttl
}

var legacyEras = map[string]legacyEra{
	"shelley": {
		era:    v1.Era_ERA_SHELLEY,
		tx:     "83a4008182582080266661df8a01b93e9e20de7232e3b34b609a1ee4a630442db9209485d2293300018282583901491f94fa0985f2c34ccfb94085dcd9763c8eb947737dd380b52e37d8ab93b30d186d41b7b9bc22c336fdbb75dd1ab1ddf58ee9ba906bec821a713fb300825839013ada263c5ccda43559b21a9cd59c1e24f36993d8c579e1d7e8815aae7704a3cd5ae21fed6914ec6fc9d9879ee34008c541fa00d9ae571f5f1b000001748b231e6f021a0002a751031a00f68b55a10081825820e9f3aaf15c651471ae0d5fe633a0e995c52a7d10f1b842ca2ccee768e64c5c0f5840664e923c5ee2cf5bc3aab03c2ccd963c11606014b8934548f65bb449c396fe7aa737c0758ca5c6bbb0efc7b6b3d27f1b3089b7144eff291b641b48ccf0d10e03f6",
		inTxID: "80266661df8a01b93e9e20de7232e3b34b609a1ee4a630442db9209485d22933", inIndex: 0,
		inOut: "82581d613ada263c5ccda43559b21a9cd59c1e24f36993d8c579e1d7e8815aae1b00000174fc6578c0",
		slot:  16157525, ttl: 16157525, expiryName: "UTXO/ExpiredUTxO",
	},
	"allegra": {
		era:    v1.Era_ERA_ALLEGRA,
		tx:     "83a40081825820d53ea8c652a58a8d0b4f62b92acdb5dd71a7b49c68cd3a686bec22e6e088005d01018282584c82d818584283581cf6afb6b5ffa538cba54edb1513a4aa031d280f9448861273b1a1bd6fa101581e581c2b0b011ba3683d15ad5eef2a4a1e16bc63fa2df24653334ea986ae10001a53d31d6b1a10a133c0825839012d1f6eb196eb8c352ad8512c39b2ef980fd9ff11f61f13b327cb3f06aba8304aa2f0dbce3c7b1201659b8872fdcfc2bbbc9144853f4bdace1a0f3efedb021a0002c30a031a01600d72a100818258203c693f0da4e77b94bd0890d82ccd62066662ab7e5a60043854301be914b22b3d584000ef4847489cefcd1c77e70f7d30d2ef98cbd0b5177515add351308b4111c69ade63f5e98565108c3ffa82bebc3a331bdc4583112e8d6738dcf9eb5430673700f6",
		inTxID: "d53ea8c652a58a8d0b4f62b92acdb5dd71a7b49c68cd3a686bec22e6e088005d", inIndex: 1,
		inOut: "82581d612d1f6eb196eb8c352ad8512c39b2ef980fd9ff11f61f13b327cb3f061a1fe2f5a5",
		slot:  23072113, ttl: 23072114, expiryName: "UTXO/OutsideValidityIntervalUTxO",
	},
	"mary": {
		era:    v1.Era_ERA_MARY,
		tx:     "83a50081825820d9557efbf84ffa6dd363661c1b5e7313ed29e50823da16baedc1794f2f616207000181825839015d3c84a4ab34244ca865475c13eabf2e862d5a30c612360f4f0b7b2405fbcf0e74565278f93b2b82ab69bcb9f08706ddd4a00d225d5d678f1a00217dad021a00029d2d031a02613b8a0800a100818258205f565a66dee8a092bd11e6196ce1f974195bc4183cc883699d3f04c87a603b6f58400354c4b89d6d2eacec89eb2b1cea400309edef1ed707b406455f8c405a2a7c7cfda44565c00dfbb8a35c3a9d7a804d6dfbea074ef197d0de7ad5f66c175db105f6",
		inTxID: "d9557efbf84ffa6dd363661c1b5e7313ed29e50823da16baedc1794f2f616207", inIndex: 0,
		inOut: "82581d61a8ab4c5d55e86795f3af683f107d1442ec77762842ce5fd956a69b431a00241ada",
		slot:  39926665, ttl: 39926666, expiryName: "UTXO/OutsideValidityIntervalUTxO",
	},
}

func (l legacyEra) request(t *testing.T) *v1.RuleUtxowRequest {
	t.Helper()
	ctx, g, pp := rulefixture.Env(l.era)
	ctx.Slot = l.slot
	tx, _ := hex.DecodeString(l.tx)
	txid, _ := hex.DecodeString(l.inTxID)
	out, _ := hex.DecodeString(l.inOut)
	return &v1.RuleUtxowRequest{
		Context: ctx, Globals: g, Pparams: pp, Tx: tx,
		State: &v1.LedgerState{Utxo: []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: txid, Index: l.inIndex}, Txout: out}}},
	}
}

// Expected names are the constructors of Cardano.Ledger.Shelley.Rules.Utxo
// and Utxow, and of the Allegra Utxo rule the Mary era reuses.
func TestRuleLegacyEras(t *testing.T) {
	t.Parallel()
	u64 := func(x uint64) *uint64 { return &x }
	for eraName, l := range legacyEras {
		for name, tc := range map[string]struct {
			mutate func(*legacyEra, *v1.RuleUtxowRequest)
			want   string
		}{
			"valid": {func(*legacyEra, *v1.RuleUtxowRequest) {}, ""},
			// Shelley's ttl is inclusive (ttl >= slot); Allegra's interval is half open.
			"slot past ttl": {func(l *legacyEra, r *v1.RuleUtxowRequest) { r.Context.Slot = l.ttl + 1 }, l.expiryName},
			"fee too small": {func(_ *legacyEra, r *v1.RuleUtxowRequest) { r.Pparams.MinFeeB = u64(10_000_000) }, "UTXO/FeeTooSmallUTxO"},
			"max tx size":   {func(_ *legacyEra, r *v1.RuleUtxowRequest) { r.Pparams.MaxTxSize = proto.Uint32(100) }, "UTXO/MaxTxSizeUTxO"},
			"wrong network": {func(_ *legacyEra, r *v1.RuleUtxowRequest) { r.Context.NetworkId = 0 }, "UTXO/WrongNetwork"},
			"output too small": {func(_ *legacyEra, r *v1.RuleUtxowRequest) {
				r.Pparams.MinUtxoValue = u64(1_000_000_000_000)
			}, "UTXO/OutputTooSmallUTxO"},
			"value not conserved": {func(l *legacyEra, r *v1.RuleUtxowRequest) {
				r.State.Utxo[0] = rulefixture.Rebalance(l.era, r.State.Utxo[0], 1)
			}, "UTXO/ValueNotConservedUTxO"},
			"missing witness": {func(l *legacyEra, r *v1.RuleUtxowRequest) {
				r.State.Utxo[0] = rulefixture.Rekey(l.era, r.State.Utxo[0])
			}, "UTXOW/MissingVKeyWitnessesUTXOW"},
		} {
			t.Run(eraName+"/"+name, func(t *testing.T) {
				t.Parallel()
				req := l.request(t)
				tc.mutate(&l, req)
				resp, err := ruleUtxow(context.Background(), req)
				if err != nil {
					t.Fatal(err)
				}
				if got := strings.Join(failuresOf(resp.GetRejection()), ","); got != tc.want {
					t.Fatalf("failures = %q, want %q", got, tc.want)
				}
			})
		}
	}
}

func TestRuleEraOutsideShelleyToConwayIsUnsupported(t *testing.T) {
	t.Parallel()
	r := legacyEras["mary"].request(t)
	r.Context.Era = v1.Era_ERA_BYRON
	if _, err := ruleUtxow(context.Background(), r); err == nil {
		t.Fatal("Byron accepted")
	}
}
