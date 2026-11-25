package corpusgen

import (
	"testing"

	"github.com/blinklabs-io/dingo/config/cardano"
	"github.com/blinklabs-io/dingo/ledger/eras"
)

// mainnetConfig loads the shipped mainnet genesis files through dingo's own
// loader, so the cost models are the ones dingo starts from.
func mainnetConfig(t *testing.T) *cardano.CardanoNodeConfig {
	t.Helper()
	cfg, err := cardano.NewCardanoNodeConfigFromEmbedFS(cardano.EmbeddedConfigFS, cardano.EmbeddedConfigPath("mainnet"))
	if err != nil {
		t.Fatal(err)
	}
	return cfg
}

// mainnetCostModels are the PlutusV1 (Alonzo genesis), PlutusV2 (dingo's
// default, the mainnet value before an on-chain model replaces it) and
// PlutusV3 (Conway genesis) cost models.
func mainnetCostModels(t *testing.T) (v1, v2, v3 []int64) {
	t.Helper()
	cfg := mainnetConfig(t)
	return append([]int64(nil), cfg.AlonzoGenesis().CostModels["PlutusV1"]...),
		append([]int64(nil), eras.DefaultPlutusV2CostModel...),
		append([]int64(nil), cfg.ConwayGenesis().PlutusV3CostModel...)
}
