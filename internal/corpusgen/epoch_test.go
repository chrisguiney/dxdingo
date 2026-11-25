package corpusgen

import (
	"math"
	"testing"

	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/testgenesis"
)

func TestGenerateEpochCorpus(t *testing.T) { writeCorpus(t, "", epochCases()) }

func key(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: rep(b, 28)}
}

func scriptEpoch(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: rep(b, 28)}
}

func TestGenerateEpochDivergenceCorpus(t *testing.T) {
	writeCorpus(t, "divergences", epochDivergences())
}

func u64pEpoch(x uint64) *uint64 { return &x }
func u32pEpoch(x uint32) *uint32 { return &x }

func epochShelleyPParams() *v1.ProtocolParameters {
	return &v1.ProtocolParameters{
		MinFeeA: u64pEpoch(44), MinFeeB: u64pEpoch(155381), MaxBlockBodySize: u32pEpoch(65536), MaxTxSize: u32pEpoch(16384),
		MaxBlockHeaderSize: u32pEpoch(1100), KeyDeposit: u64pEpoch(2_000_000), PoolDeposit: u64pEpoch(500_000_000),
		MaxEpoch: u64pEpoch(18), NOpt: u32pEpoch(150), A0: rat("3", "10"), Rho: rat("3", "1000"), Tau: rat("1", "5"),
		D: rat("1", "2"), ExtraEntropy: &v1.Nonce{}, ProtocolVersion: &v1.ProtocolVersion{Major: 2},
		MinUtxoValue: u64pEpoch(1_000_000), MinPoolCost: u64pEpoch(340_000_000),
	}
}

// Shelley ProtocolParamUpdate CBOR: a map from parameter index to value.
var (
	epochMinFeeA100 = []byte{0xa1, 0x00, 0x18, 0x64}                   // {0: 100}
	epochMinFeeA200 = []byte{0xa1, 0x00, 0x18, 0xc8}                   // {0: 200}
	epochPV30       = []byte{0xa1, 0x0e, 0x82, 0x03, 0x00}             // {14: [3, 0]}
	epochPV40       = []byte{0xa1, 0x0e, 0x82, 0x04, 0x00}             // {14: [4, 0]}
	epochMaxTx70000 = []byte{0xa1, 0x03, 0x1a, 0x00, 0x01, 0x11, 0x70} // {3: 70000}
)

// epochProp is a proposal made during the epoch it targets (epoch length 100).
func epochProp(k byte, target uint64, update []byte) *v1.PParamProposal {
	return epochPropAt(k, target, target*100+10, update)
}

func epochPropAt(k byte, target, slot uint64, update []byte) *v1.PParamProposal {
	return &v1.PParamProposal{GenesisKeyHash: rep(k, 28), TargetEpoch: target, AddedSlot: slot, Update: update}
}

func epochPlutus(p *v1.ProtocolParameters) *v1.ProtocolParameters {
	p.PriceMem, p.PriceSteps = rat("577", "10000"), rat("721", "10000000")
	p.MaxTxExMem, p.MaxTxExSteps = u64pEpoch(14_000_000), u64pEpoch(10_000_000_000)
	p.MaxBlockExMem, p.MaxBlockExSteps = u64pEpoch(62_000_000), u64pEpoch(40_000_000_000)
	p.MaxValueSize, p.CollateralPercentage, p.MaxCollateralInputs = u32pEpoch(5000), u32pEpoch(150), u32pEpoch(3)
	return p
}

func epochAlonzoPParams() *v1.ProtocolParameters {
	p := epochPlutus(epochShelleyPParams())
	p.MinUtxoValue, p.CoinsPerUtxoWord = nil, u64pEpoch(34482)
	p.ProtocolVersion = &v1.ProtocolVersion{Major: 5}
	return p
}

func epochBabbagePParams() *v1.ProtocolParameters {
	p := epochPlutus(epochShelleyPParams())
	p.MinUtxoValue, p.D, p.ExtraEntropy, p.CoinsPerUtxoByte = nil, nil, nil, u64pEpoch(4310)
	p.ProtocolVersion = &v1.ProtocolVersion{Major: 7}
	return p
}

var epochPV25 = []byte{0xa1, 0x0e, 0x82, 0x02, 0x05} // {14: [2, 5]}: decodes, but cannot follow 2.0

func epochPParamCall(era v1.Era, quorum uint64, ps ...*v1.PParamProposal) *v1.Call {
	return &v1.Call{Request: &v1.Call_EpochPparamUpdate{EpochPparamUpdate: &v1.EpochPParamUpdateRequest{
		Context: &v1.LedgerContext{Era: era, Epoch: 11}, Globals: &v1.Globals{Quorum: quorum, EpochLength: 100},
		Pparams: epochShelleyPParams(), Proposals: ps,
	}}}
}

func epochForkCall(from, to, major uint32) *v1.Call {
	pp := epochShelleyPParams()
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: major}
	return &v1.Call{Request: &v1.Call_EpochHardFork{EpochHardFork: &v1.EpochHardForkRequest{
		From: &v1.ProtocolVersion{Major: from}, To: &v1.ProtocolVersion{Major: to}, Pparams: pp,
	}}}
}

func epochPParamCallEra(era v1.Era, pp *v1.ProtocolParameters, ps ...*v1.PParamProposal) *v1.Call {
	c := epochPParamCall(era, 1, ps...)
	c.GetEpochPparamUpdate().Pparams = pp
	return c
}

func epochGR(r *cbor.Rat) *v1.Rational {
	return &v1.Rational{Numerator: r.Num().String(), Denominator: r.Denom().String()}
}

func epochAlonzoGenesis() *v1.AlonzoGenesisInput {
	g := testgenesis.Alonzo()
	return &v1.AlonzoGenesisInput{
		CoinsPerUtxoWord: g.LovelacePerUtxoWord,
		PriceMem:         epochGR(g.ExecutionPrices.Mem), PriceSteps: epochGR(g.ExecutionPrices.Steps),
		MaxTxExMem: g.MaxTxExUnits.Mem, MaxTxExSteps: g.MaxTxExUnits.Steps,
		MaxBlockExMem: g.MaxBlockExUnits.Mem, MaxBlockExSteps: g.MaxBlockExUnits.Steps,
		MaxValueSize: uint32(g.MaxValueSize), CollateralPercentage: uint32(g.CollateralPercentage),
		MaxCollateralInputs: uint32(g.MaxCollateralInputs), CostModelPlutusV1: g.CostModels["PlutusV1"],
	}
}

func epochConwayGenesis() *v1.ConwayGenesisInput {
	g := testgenesis.Conway()
	p, d := g.PoolVotingThresholds, g.DRepVotingThresholds
	return &v1.ConwayGenesisInput{
		PoolVotingThresholds: &v1.PoolVotingThresholds{
			MotionNoConfidence: epochGR(p.MotionNoConfidence), CommitteeNormal: epochGR(p.CommitteeNormal),
			CommitteeNoConfidence: epochGR(p.CommitteeNoConfidence), HardForkInitiation: epochGR(p.HardForkInitiation), PpSecurityGroup: epochGR(p.PpSecurityGroup),
		},
		DrepVotingThresholds: &v1.DRepVotingThresholds{
			MotionNoConfidence: epochGR(d.MotionNoConfidence), CommitteeNormal: epochGR(d.CommitteeNormal), CommitteeNoConfidence: epochGR(d.CommitteeNoConfidence),
			UpdateToConstitution: epochGR(d.UpdateToConstitution), HardForkInitiation: epochGR(d.HardForkInitiation), PpNetworkGroup: epochGR(d.PpNetworkGroup),
			PpEconomicGroup: epochGR(d.PpEconomicGroup), PpTechnicalGroup: epochGR(d.PpTechnicalGroup), PpGovGroup: epochGR(d.PpGovGroup), TreasuryWithdrawal: epochGR(d.TreasuryWithdrawal),
		},
		CommitteeMinSize: uint32(g.MinCommitteeSize), CommitteeMaxTermLength: g.CommitteeTermLimit,
		GovActionLifetime: g.GovActionValidityPeriod, GovActionDeposit: g.GovActionDeposit,
		DrepDeposit: g.DRepDeposit, DrepActivity: g.DRepInactivityPeriod,
		MinFeeRefScriptCostPerByte: epochGR(g.MinFeeRefScriptCostPerByte), CostModelPlutusV3: g.PlutusV3CostModel,
	}
}

func epochForkAlonzo() *v1.Call {
	c := epochForkCall(4, 5, 4)
	c.GetEpochHardFork().AlonzoGenesis = epochAlonzoGenesis()
	return c
}

func epochForkBabbage(major uint32, withV2 bool) *v1.Call {
	pp := epochAlonzoPParams()
	if withV2 {
		pp.CostModelPlutusV2 = eras.DefaultPlutusV2CostModel
	}
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: major}
	pp.CostModelPlutusV1 = testgenesis.Alonzo().CostModels["PlutusV1"]
	return &v1.Call{Request: &v1.Call_EpochHardFork{EpochHardFork: &v1.EpochHardForkRequest{
		From: &v1.ProtocolVersion{Major: 6}, To: &v1.ProtocolVersion{Major: 7}, Pparams: pp,
	}}}
}

func epochForkConway() *v1.Call {
	pp := epochBabbagePParams()
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: 8}
	pp.CostModelPlutusV1 = testgenesis.Alonzo().CostModels["PlutusV1"]
	return &v1.Call{Request: &v1.Call_EpochHardFork{EpochHardFork: &v1.EpochHardForkRequest{
		From: &v1.ProtocolVersion{Major: 8}, To: &v1.ProtocolVersion{Major: 9}, Pparams: pp, ConwayGenesis: epochConwayGenesis(),
	}}}
}

func epochDivergences() []entry {
	return []entry{
		{"epoch_hard_fork_babbage_default_v2", "dingo's Babbage fork inserts DefaultPlutusV2CostModel when the input has no V2 model; the reference translation adds none", epochForkBabbage(6, false)},
		{"epoch_hard_fork_allegra_major_2", "dingo raises a protocol major below 3 to 3 entering Allegra; the reference translateEra keeps it. Reachable only by a schedule-driven fork on a test network", epochForkCall(2, 3, 2)},
		{"epoch_pparam_update_shelley_pv4", "dingo's Shelley decoder accepts protocol version 4; the reference's ShelleyEra decoder caps it at 3 (decodeProtVer)",
			epochPParamCall(v1.Era_ERA_SHELLEY, 1, epochProp(1, 10, epochPV40))},
	}
}

func epochCases() []entry {
	sh := v1.Era_ERA_SHELLEY
	pu := func(name, why string, c *v1.Call) entry { return entry{name, why, c} }
	donate := func(name, why string, treasury, donations uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochDonations{EpochDonations: &v1.EpochDonationsRequest{Treasury: treasury, Donations: donations}}}}
	}
	cur := v1.PoolStage_POOL_STAGE_CURRENT
	pool := func(id byte, acct *v1.Credential, deposit, retireAt uint64) *v1.Pool {
		return &v1.Pool{PoolId: rep(id, 28), Stage: cur, RewardAccount: acct, Deposit: deposit, RetiringEpoch: &retireAt}
	}
	acct := func(c *v1.Credential, pool []byte) *v1.Account { return &v1.Account{Credential: c, PoolId: pool} }
	reap := func(name, why string, epoch uint64, pools []*v1.Pool, accts []*v1.Account) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochPoolReap{EpochPoolReap: &v1.EpochPoolReapRequest{Epoch: epoch, Pools: pools, Accounts: accts}}}}
	}
	R, T := v1.MirSource_MIR_SOURCE_RESERVES, v1.MirSource_MIR_SOURCE_TREASURY
	me := func(s v1.MirSource, c *v1.Credential, amt int64) *v1.MirEntry {
		return &v1.MirEntry{Source: s, Credential: c, Amount: amt}
	}
	mir := func(name, why string, major uint32, reserves, treasury uint64, es []*v1.MirEntry, reg []*v1.Credential, r2t, t2r uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochMir{EpochMir: &v1.EpochMirRequest{
			ProtocolVersion: &v1.ProtocolVersion{Major: major}, Reserves: reserves, Treasury: treasury, Mir: es,
			Registered: reg, ReservesToTreasury: r2t, TreasuryToReserves: t2r,
		}}}}
	}
	return []entry{
		donate("epoch_donations_basic", "treasury plus donations", 1_000_000, 234_000),
		donate("epoch_donations_zero", "no donations leaves the treasury", 5, 0),
		donate("epoch_donations_exact_max", "sum exactly Word64 max still representable", math.MaxUint64-7, 7),
		donate("epoch_donations_overflow", "sum above Word64: Coin is unbounded, the response is not", math.MaxUint64, 1),

		reap("epoch_pool_reap_basic", "pools 1,2 share account A (refunds merge), pool 3 has an unregistered account, pool 4 retires later; account C is delegated to a retiring pool and is cleared", 5,
			[]*v1.Pool{pool(1, key(0xa), 500, 5), pool(2, key(0xa), 300, 5), pool(3, key(0xb), 200, 5), pool(4, key(0xa), 900, 6)},
			[]*v1.Account{acct(key(0xa), nil), acct(key(0xc), rep(1, 28)), acct(key(0xd), rep(4, 28))}),
		reap("epoch_pool_reap_nothing_retiring", "no pool retires this epoch: nothing moves", 7,
			[]*v1.Pool{pool(1, key(0xa), 500, 8)}, []*v1.Account{acct(key(0xa), rep(1, 28))}),
		reap("epoch_pool_reap_script_account", "a script reward account is refunded; credential kind is part of identity (key A is not script A)", 3,
			[]*v1.Pool{pool(1, scriptEpoch(0xa), 500, 3), pool(2, key(0xa), 100, 3)}, []*v1.Account{acct(scriptEpoch(0xa), nil)}),
		reap("epoch_pool_reap_zero_deposit", "a zero deposit still yields a (zero) refund entry for a registered account", 2,
			[]*v1.Pool{pool(1, key(0xa), 0, 2)}, []*v1.Account{acct(key(0xa), nil)}),
		reap("epoch_pool_reap_all_unclaimed", "every reward account unregistered: all deposits go to the treasury", 4,
			[]*v1.Pool{pool(1, key(0xa), 500_000_000, 4), pool(2, key(0xb), 500_000_000, 4)}, nil),
		reap("epoch_pool_reap_deposit_overflow", "retiring deposits exceed Word64: UNSUPPORTED on both", 1,
			[]*v1.Pool{pool(1, key(0xa), math.MaxUint64, 1), pool(2, key(0xb), 1, 1)}, nil),
		reap("epoch_pool_reap_duplicate_pool", "a repeated pool id has no meaning in a map: UNSUPPORTED on both", 1,
			[]*v1.Pool{pool(1, key(0xa), 5, 1), pool(1, key(0xb), 6, 1)}, nil),

		mir("epoch_mir_applied", "credit from both pots; credential 3 unregistered and skipped; reserves-to-treasury transfer applied before the budget check", 6, 1000, 500,
			[]*v1.MirEntry{me(R, key(1), 100), me(T, key(1), 50), me(R, key(2), 200), me(R, key(3), 999)}, []*v1.Credential{key(1), key(2)}, 100, 0),
		mir("epoch_mir_over_budget", "reserves cannot cover the total: the whole boundary is a no-op, transfer included", 6, 100, 0,
			[]*v1.MirEntry{me(R, key(1), 150)}, []*v1.Credential{key(1)}, 10, 0),
		mir("epoch_mir_treasury_over_budget", "treasury pot too small for its total", 5, 1000, 10,
			[]*v1.MirEntry{me(T, key(1), 11)}, []*v1.Credential{key(1)}, 0, 0),
		mir("epoch_mir_transfer_funds_credit", "treasury-to-reserves transfer makes a reserves credit fit", 4, 50, 500,
			[]*v1.MirEntry{me(R, key(1), 100)}, []*v1.Credential{key(1)}, 0, 60),
		mir("epoch_mir_empty", "no entries and no transfers: pots unchanged, applied", 7, 10, 20, nil, nil, 0, 0),
		mir("epoch_mir_script_credential", "script credential registered and credited", 6, 1000, 1000,
			[]*v1.MirEntry{me(R, scriptEpoch(1), 7), me(R, key(1), 9)}, []*v1.Credential{scriptEpoch(1)}, 0, 0),
		mir("epoch_mir_exact_budget", "total exactly equals the pot: fits (<=)", 6, 100, 0,
			[]*v1.MirEntry{me(R, key(1), 60), me(R, key(2), 40)}, []*v1.Credential{key(1), key(2)}, 0, 0),
		mir("epoch_mir_conway_unsupported", "MIR does not exist from protocol 9: UNSUPPORTED on both", 9, 10, 10, nil, nil, 0, 0),
		pu("epoch_pparam_update_quorum", "two of two genesis keys agree: min_fee_a becomes 100", epochPParamCall(sh, 2, epochProp(1, 10, epochMinFeeA100), epochProp(2, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_below_quorum", "one vote of quorum two: nothing adopted", epochPParamCall(sh, 2, epochProp(1, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_two_values", "two values both reach quorum: the reference requires exactly one, so nothing is adopted", epochPParamCall(sh, 2,
			epochProp(1, 10, epochMinFeeA100), epochProp(2, 10, epochMinFeeA100), epochProp(3, 10, epochMinFeeA200), epochProp(4, 10, epochMinFeeA200))),
		pu("epoch_pparam_update_latest_per_key", "a key's later proposal replaces its earlier one", epochPParamCall(sh, 2,
			epochProp(1, 10, epochMinFeeA100), epochProp(1, 10, epochMinFeeA200), epochProp(2, 10, epochMinFeeA200))),
		pu("epoch_pparam_update_other_epoch", "a proposal targeting another epoch does not vote", epochPParamCall(sh, 2, epochProp(1, 10, epochMinFeeA100), epochProp(2, 11, epochMinFeeA100))),
		pu("epoch_pparam_update_protocol_version", "protocol version 2.0 to 3.0 is adopted", epochPParamCall(sh, 1, epochProp(1, 10, epochPV30))),
		pu("epoch_pparam_update_block_size_guard", "maxTxSize 70000 + header 1100 is not below body 65536: the reference refuses the update", epochPParamCall(sh, 1, epochProp(1, 10, epochMaxTx70000))),
		pu("epoch_pparam_update_carried", "a proposal made in epoch 9 for epoch 10 is carried over and counts", epochPParamCall(sh, 1, epochPropAt(1, 10, 950, epochMinFeeA100))),
		pu("epoch_pparam_update_carried_dropped", "one carried proposal's version cannot follow 2.0, so every carried proposal is dropped", epochPParamCall(sh, 1,
			epochPropAt(1, 10, 950, epochMinFeeA100), epochPropAt(2, 10, 960, epochPV25))),
		pu("epoch_pparam_update_carried_overridden", "a key's proposal made in the epoch overrides its carried one", epochPParamCall(sh, 1,
			epochPropAt(1, 10, 950, epochMinFeeA100), epochPropAt(1, 10, 1010, epochMinFeeA200))),
		pu("epoch_pparam_update_dropped_carry_keeps_new", "carry-over dropped, but a proposal made this epoch still counts", epochPParamCall(sh, 1,
			epochPropAt(2, 10, 950, epochPV25), epochPropAt(1, 10, 1010, epochMinFeeA100))),
		pu("epoch_pparam_update_allegra", "the same update system in Allegra", epochPParamCallEra(v1.Era_ERA_ALLEGRA, epochShelleyPParams(), epochProp(1, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_mary", "the same update system in Mary", epochPParamCallEra(v1.Era_ERA_MARY, epochShelleyPParams(), epochProp(1, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_alonzo", "Alonzo: Plutus parameters carry through the update", epochPParamCallEra(v1.Era_ERA_ALONZO, epochAlonzoPParams(), epochProp(1, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_babbage", "Babbage: no d, extra entropy or min_utxo_value", epochPParamCallEra(v1.Era_ERA_BABBAGE, epochBabbagePParams(), epochProp(1, 10, epochMinFeeA100))),
		pu("epoch_pparam_update_undecodable", "bytes that are not a pparam update: both nodes reject with PPUP DeserialiseFailure", epochPParamCall(sh, 1, epochProp(1, 10, []byte{0xff}))),
		pu("epoch_pparam_update_conway_unsupported", "the classic update system ends before Conway", epochPParamCall(v1.Era_ERA_CONWAY, 1)),
		pu("epoch_hard_fork_allegra", "Shelley to Allegra with the version already enacted: every parameter carries over", epochForkCall(2, 3, 3)),
		pu("epoch_hard_fork_mary", "Allegra to Mary: every parameter carries over", epochForkCall(3, 4, 3)),
		pu("epoch_hard_fork_alonzo", "Mary to Alonzo: the genesis supplies the Plutus parameters; the Shelley set carries over", epochForkAlonzo()),
		pu("epoch_hard_fork_babbage", "Alonzo to Babbage with a V2 cost model already present: coins per word become per byte; d, entropy and min UTxO value go", epochForkBabbage(6, true)),
		pu("epoch_hard_fork_conway", "Babbage to Conway: the genesis supplies governance parameters and the V3 cost model", epochForkConway()),
		pu("epoch_hard_fork_not_consecutive", "the fork must enter the era after `from`: UNSUPPORTED on both", epochForkCall(4, 6, 4)),
		mir("epoch_mir_negative_unsupported", "negative delta is outside the reference Coin map: UNSUPPORTED on both", 6, 10, 10,
			[]*v1.MirEntry{me(R, key(1), -3)}, []*v1.Credential{key(1)}, 0, 0),
	}
}
