package corpusgen

import (
	"slices"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGeneratePlutusCorpus(t *testing.T) {
	writeCorpus(t, "", slices.Concat(plutusCostModelCases(t), plutusChainCases(t), plutusEdgeCases(t), plutusContextCases(t),
		plutusMultiEvalCases(t), plutusCollectCases(t)))
	writeCorpus(t, "divergences", slices.Concat(plutusCostModelDivergences(t), plutusEvalDivergences(t), plutusContextDivergences(t),
		plutusMultiEvalDivergences(t)))
}

// plutusCostModelDivergences are confirmed mismatches, each pinned by a direct
// test in dxnode-go (TestDivergence...) and the Haskell "divergence:" checks.
func plutusCostModelDivergences(t *testing.T) []entry {
	m1, _, _ := mainnetCostModels(t)
	cm := func(name, why string, params []int64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PlutusCostModel{PlutusCostModel: &v1.PlutusCostModelRequest{
			PlutusVersion: v1.PlutusVersion_PLUTUS_VERSION_V1, ProtocolVersion: &v1.ProtocolVersion{Major: 9}, Params: params,
		}}}}
	}
	return []entry{
		cm("plutus_cost_model_v1_zero_costs", "reference accepts a zero CEK step cost; plutigo requires at least 1 (TestDivergenceZeroCekCostRejected)", make([]int64, 166)),
		cm("plutus_cost_model_v1_extra_zeros", "reference drops entries past the 166 V1 names; plutigo applies them to later-language CEK costs (TestDivergenceExtraParamsAreApplied)", append(append([]int64(nil), m1...), make([]int64, 100)...)),
	}
}

func plutusCostModelCases(t *testing.T) []entry {
	m1, m2, m3 := mainnetCostModels(t)
	pv := func(major uint32) *v1.ProtocolVersion { return &v1.ProtocolVersion{Major: major} }
	cm := func(name, why string, ver v1.PlutusVersion, major uint32, params []int64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PlutusCostModel{PlutusCostModel: &v1.PlutusCostModelRequest{
			PlutusVersion: ver, ProtocolVersion: pv(major), Params: params,
		}}}}
	}
	v1c, v2c, v3c := v1.PlutusVersion_PLUTUS_VERSION_V1, v1.PlutusVersion_PLUTUS_VERSION_V2, v1.PlutusVersion_PLUTUS_VERSION_V3
	return []entry{
		cm("plutus_cost_model_v1_mainnet", "PlutusV1 model of the Alonzo genesis, 166 parameters", v1c, 6, m1),
		cm("plutus_cost_model_v2_mainnet", "PlutusV2 default model, 175 parameters", v2c, 8, m2),
		cm("plutus_cost_model_v3_mainnet", "PlutusV3 model of the Conway genesis, 251 parameters", v3c, 9, m3),
		cm("plutus_cost_model_v3_too_few", "fewer parameters than the language names: missing ones cost maxBound or are rejected", v3c, 9, m3[:100]),
		cm("plutus_cost_model_v1_too_many", "more parameters than the language names, extras nonzero: ignored by the reference, stored as supplied", v1c, 6, append(append([]int64(nil), m1...), ones(400)...)),
		cm("plutus_cost_model_v2_empty", "an empty list: every parameter missing", v2c, 8, nil),
		cm("plutus_cost_model_v1_negative", "a negative cost parameter", v1c, 6, append([]int64{-1}, m1[1:]...)),
	}
}

func ones(n int) []int64 {
	out := make([]int64, n)
	for i := range out {
		out[i] = 1
	}
	return out
}
