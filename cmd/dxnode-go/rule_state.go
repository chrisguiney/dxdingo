package main

import (
	"encoding/hex"
	"errors"
	"fmt"
	"math/big"
	"time"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// ruleState is a gouroboros common.LedgerState backed by the request's
// ledger tables. It only answers what the validation rules ask.
type ruleState struct {
	ctx      *v1.LedgerContext
	globals  *v1.Globals
	state    *v1.LedgerState
	utxo     map[string]common.Utxo
	accounts map[string]*v1.Account
	pools    map[common.Blake2b224]*v1.Pool
	dreps    map[string]*v1.DRepRecord
}

var _ common.LedgerState = (*ruleState)(nil)

func credKey(c *v1.Credential) string {
	return fmt.Sprintf("%d:%x", c.GetKind(), c.GetHash())
}

func commonCredKey(c common.Credential) string {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if c.CredType == common.CredentialTypeScriptHash {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return fmt.Sprintf("%d:%x", kind, c.Credential.Bytes())
}

func utxoKey(id common.Blake2b256, idx uint32) string {
	return fmt.Sprintf("%x#%d", id.Bytes(), idx)
}

func newRuleState(ctx *v1.LedgerContext, g *v1.Globals, st *v1.LedgerState) (*ruleState, error) {
	rs := &ruleState{
		ctx: ctx, globals: g, state: st,
		utxo:     map[string]common.Utxo{},
		accounts: map[string]*v1.Account{},
		pools:    map[common.Blake2b224]*v1.Pool{},
		dreps:    map[string]*v1.DRepRecord{},
	}
	for _, e := range st.GetUtxo() {
		out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo txout: %w", err))
		}
		in, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(e.GetTxin().GetTxId()), int(e.GetTxin().GetIndex()))
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo txin: %w", err))
		}
		rs.utxo[utxoKey(in.Id(), in.Index())] = common.Utxo{Id: in, Output: out}
	}
	for _, a := range st.GetAccounts() {
		rs.accounts[credKey(a.GetCredential())] = a
	}
	for _, p := range st.GetPools() {
		if p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE {
			continue
		}
		var h common.Blake2b224
		copy(h[:], p.GetPoolId())
		rs.pools[h] = p
	}
	for _, d := range st.GetDreps() {
		rs.dreps[credKey(d.GetCredential())] = d
	}
	return rs, nil
}

func (s *ruleState) UtxoById(in common.TransactionInput) (common.Utxo, error) {
	if u, ok := s.utxo[utxoKey(in.Id(), in.Index())]; ok {
		return u, nil
	}
	return common.Utxo{}, common.ErrUtxoNotFound
}

func (s *ruleState) StakeRegistration(key []byte) ([]common.StakeRegistrationCertificate, error) {
	for _, a := range s.accounts {
		if string(a.GetCredential().GetHash()) == string(key) {
			return []common.StakeRegistrationCertificate{{}}, nil
		}
	}
	return nil, nil
}

func (s *ruleState) IsStakeCredentialRegistered(c common.Credential) bool {
	_, ok := s.accounts[commonCredKey(c)]
	return ok
}

func (s *ruleState) StakeCredentialDeposit(c common.Credential) (*uint64, error) {
	a, ok := s.accounts[commonCredKey(c)]
	if !ok {
		return nil, nil
	}
	d := a.GetDeposit()
	return &d, nil
}

func (s *ruleState) systemStart() time.Time {
	return time.Unix(s.globals.GetSystemStartUnix(), 0).UTC()
}

func (s *ruleState) slotLength() (*big.Rat, error) {
	r, err := convert.Rat(s.globals.GetSlotLength())
	if err != nil || r.Sign() <= 0 {
		return nil, errors.New("globals: slot_length must be positive")
	}
	return r, nil
}

func (s *ruleState) SlotToTime(slot uint64) (time.Time, error) {
	l, err := s.slotLength()
	if err != nil {
		return time.Time{}, err
	}
	ns := new(big.Rat).Mul(new(big.Rat).SetUint64(slot), l)
	ns.Mul(ns, big.NewRat(1_000_000_000, 1))
	n := new(big.Int).Quo(ns.Num(), ns.Denom())
	return s.systemStart().Add(time.Duration(n.Int64())), nil
}

func (s *ruleState) TimeToSlot(t time.Time) (uint64, error) {
	l, err := s.slotLength()
	if err != nil {
		return 0, err
	}
	d := t.Sub(s.systemStart())
	if d < 0 {
		return 0, errors.New("time precedes system start")
	}
	q := new(big.Rat).Quo(big.NewRat(int64(d), 1_000_000_000), l)
	return new(big.Int).Quo(q.Num(), q.Denom()).Uint64(), nil
}

func (s *ruleState) EpochForSlot(slot uint64) (uint64, error) {
	if s.globals.GetEpochLength() == 0 {
		return 0, errors.New("globals: epoch_length is zero")
	}
	return slot / s.globals.GetEpochLength(), nil
}

func (s *ruleState) PoolCurrentState(h common.PoolKeyHash) (*common.PoolRegistrationCertificate, *uint64, error) {
	p, ok := s.pools[h]
	if !ok {
		return nil, nil, nil
	}
	cert := &common.PoolRegistrationCertificate{Operator: h, Pledge: p.GetPledge(), Cost: p.GetCost()}
	copy(cert.VrfKeyHash[:], p.GetVrfKeyHash())
	var ret *uint64
	if p.RetiringEpoch != nil {
		e := p.GetRetiringEpoch()
		ret = &e
	}
	return cert, ret, nil
}

func (s *ruleState) IsPoolRegistered(h common.PoolKeyHash) bool {
	_, ok := s.pools[h]
	return ok
}

func (s *ruleState) IsVrfKeyInUse(vrf common.Blake2b256) (bool, common.PoolKeyHash, error) {
	for h, p := range s.pools {
		if string(p.GetVrfKeyHash()) == string(vrf.Bytes()) {
			return true, h, nil
		}
	}
	return false, common.PoolKeyHash{}, nil
}

func (s *ruleState) CalculateRewards(common.AdaPots, common.RewardSnapshot, common.RewardParameters) (*common.RewardCalculationResult, error) {
	return nil, errors.New("rewards are not part of transaction rules")
}

func (s *ruleState) GetAdaPots() common.AdaPots {
	p := s.state.GetPots()
	return common.AdaPots{Reserves: p.GetReserves(), Treasury: p.GetTreasury(), Rewards: 0}
}

func (s *ruleState) UpdateAdaPots(common.AdaPots) error { return nil }

func (s *ruleState) GetRewardSnapshot(uint64) (common.RewardSnapshot, error) {
	return common.RewardSnapshot{}, errors.New("no reward snapshot")
}

func (s *ruleState) IsRewardAccountRegistered(c common.Credential) bool {
	return s.IsStakeCredentialRegistered(c)
}

func (s *ruleState) RewardAccountBalance(c common.Credential) (*uint64, error) {
	a, ok := s.accounts[commonCredKey(c)]
	if !ok {
		return nil, nil
	}
	b := a.GetBalance()
	return &b, nil
}

func (s *ruleState) NetworkId() uint { return uint(s.ctx.GetNetworkId()) }

func (s *ruleState) CostModels() map[common.PlutusLanguage]common.CostModel {
	return map[common.PlutusLanguage]common.CostModel{}
}

func (s *ruleState) CommitteeMember(cold common.Blake2b224) (*common.CommitteeMember, error) {
	for _, m := range s.state.GetCommittee().GetMembers() {
		if string(m.GetCold().GetHash()) == string(cold.Bytes()) {
			return toCommitteeMember(m), nil
		}
	}
	return nil, nil
}

func toCommitteeMember(m *v1.CommitteeMember) *common.CommitteeMember {
	out := &common.CommitteeMember{ExpiryEpoch: m.GetExpiry(), Resigned: m.GetResigned()}
	copy(out.ColdKey[:], m.GetCold().GetHash())
	if m.GetHot() != nil {
		var hot common.Blake2b224
		copy(hot[:], m.GetHot().GetHash())
		out.HotKey = &hot
	}
	return out
}

func (s *ruleState) CommitteeMembers() ([]common.CommitteeMember, error) {
	var out []common.CommitteeMember
	for _, m := range s.state.GetCommittee().GetMembers() {
		out = append(out, *toCommitteeMember(m))
	}
	return out, nil
}

func (s *ruleState) DRepRegistration(c common.Credential) (*common.DRepRegistration, error) {
	d, ok := s.dreps[commonCredKey(c)]
	if !ok {
		return nil, nil
	}
	dep := d.GetDeposit()
	return &common.DRepRegistration{Credential: c, Deposit: &dep}, nil
}

func (s *ruleState) DRepRegistrations() ([]common.DRepRegistration, error) {
	var out []common.DRepRegistration
	for _, d := range s.state.GetDreps() {
		var c common.Credential
		c.CredType = common.CredentialTypeAddrKeyHash
		if d.GetCredential().GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
			c.CredType = common.CredentialTypeScriptHash
		}
		copy(c.Credential[:], d.GetCredential().GetHash())
		dep := d.GetDeposit()
		out = append(out, common.DRepRegistration{Credential: c, Deposit: &dep})
	}
	return out, nil
}

func (s *ruleState) Constitution() (*common.Constitution, error) {
	c := s.state.GetEnactState().GetConstitution()
	if c == nil {
		return nil, nil
	}
	out := &common.Constitution{ScriptHash: c.GetGuardrailScriptHash()}
	out.Anchor.Url = c.GetAnchorUrl()
	copy(out.Anchor.DataHash[:], c.GetAnchorHash())
	return out, nil
}

func (s *ruleState) TreasuryValue() (uint64, error) { return s.state.GetPots().GetTreasury(), nil }

func (s *ruleState) GovActionById(id common.GovActionId) (*common.GovActionState, error) {
	for _, p := range s.state.GetProposals() {
		if string(p.GetId().GetTxId()) == string(id.TransactionId[:]) && p.GetId().GetIndex() == id.GovActionIdx {
			return &common.GovActionState{ActionId: id, ActionType: common.GovActionType(p.GetActionType())}, nil
		}
	}
	return nil, nil
}

func (s *ruleState) GovActionExists(id common.GovActionId) bool {
	a, _ := s.GovActionById(id)
	return a != nil
}

// ruleProtocolParameters converts the request tables into the era's
// gouroboros parameter struct.
func ruleProtocolParameters(era v1.Era, p *v1.ProtocolParameters) (common.ProtocolParameters, error) {
	rat := func(name string, r *v1.Rational) (*cbor.Rat, error) {
		x, err := ratArg(name, r)
		if err != nil {
			return nil, err
		}
		return &cbor.Rat{Rat: x}, nil
	}
	a0, err := rat("a0", p.GetA0())
	if err != nil {
		return nil, err
	}
	rho, err := rat("rho", p.GetRho())
	if err != nil {
		return nil, err
	}
	tau, err := rat("tau", p.GetTau())
	if err != nil {
		return nil, err
	}
	pv := common.ProtocolParametersProtocolVersion{Major: uint(p.GetProtocolVersion().GetMajor()), Minor: uint(p.GetProtocolVersion().GetMinor())}
	models := map[uint][]int64{}
	if m := p.GetCostModelPlutusV1(); len(m) > 0 {
		models[0] = m
	}
	if m := p.GetCostModelPlutusV2(); len(m) > 0 {
		models[1] = m
	}
	if m := p.GetCostModelPlutusV3(); len(m) > 0 {
		models[2] = m
	}
	var prices common.ExUnitPrice
	if p.GetPriceMem() != nil {
		if prices.MemPrice, err = rat("price_mem", p.GetPriceMem()); err != nil {
			return nil, err
		}
		if prices.StepPrice, err = rat("price_steps", p.GetPriceSteps()); err != nil {
			return nil, err
		}
	}
	maxTx := common.ExUnits{Memory: int64(p.GetMaxTxExMem()), Steps: int64(p.GetMaxTxExSteps())}
	maxBlock := common.ExUnits{Memory: int64(p.GetMaxBlockExMem()), Steps: int64(p.GetMaxBlockExSteps())}
	switch era {
	case v1.Era_ERA_BABBAGE:
		return &babbage.BabbageProtocolParameters{
			MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
			MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
			MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
			PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
			A0: a0, Rho: rho, Tau: tau, ProtocolMajor: pv.Major, ProtocolMinor: pv.Minor,
			MinPoolCost: p.GetMinPoolCost(), AdaPerUtxoByte: p.GetCoinsPerUtxoByte(), CostModels: models,
			ExecutionCosts: prices, MaxTxExUnits: maxTx, MaxBlockExUnits: maxBlock,
			MaxValueSize: uint(p.GetMaxValueSize()), CollateralPercentage: uint(p.GetCollateralPercentage()),
			MaxCollateralInputs: uint(p.GetMaxCollateralInputs()),
		}, nil
	case v1.Era_ERA_CONWAY:
		pool := p.GetPoolVotingThresholds()
		drep := p.GetDrepVotingThresholds()
		var pt conway.PoolVotingThresholds
		var dt conway.DRepVotingThresholds
		for _, f := range []struct {
			dst *cbor.Rat
			src *v1.Rational
		}{
			{&pt.MotionNoConfidence, pool.GetMotionNoConfidence()}, {&pt.CommitteeNormal, pool.GetCommitteeNormal()},
			{&pt.CommitteeNoConfidence, pool.GetCommitteeNoConfidence()}, {&pt.HardForkInitiation, pool.GetHardForkInitiation()},
			{&pt.PpSecurityGroup, pool.GetPpSecurityGroup()},
			{&dt.MotionNoConfidence, drep.GetMotionNoConfidence()}, {&dt.CommitteeNormal, drep.GetCommitteeNormal()},
			{&dt.CommitteeNoConfidence, drep.GetCommitteeNoConfidence()}, {&dt.UpdateToConstitution, drep.GetUpdateToConstitution()},
			{&dt.HardForkInitiation, drep.GetHardForkInitiation()}, {&dt.PpNetworkGroup, drep.GetPpNetworkGroup()},
			{&dt.PpEconomicGroup, drep.GetPpEconomicGroup()}, {&dt.PpTechnicalGroup, drep.GetPpTechnicalGroup()},
			{&dt.PpGovGroup, drep.GetPpGovGroup()}, {&dt.TreasuryWithdrawal, drep.GetTreasuryWithdrawal()},
		} {
			if f.src == nil {
				f.dst.Rat = new(big.Rat)
				continue
			}
			x, err := ratArg("voting threshold", f.src)
			if err != nil {
				return nil, err
			}
			f.dst.Rat = x
		}
		refCost := &cbor.Rat{Rat: new(big.Rat)}
		if p.GetMinFeeRefScriptCostPerByte() != nil {
			if refCost, err = rat("min_fee_ref_script_cost_per_byte", p.GetMinFeeRefScriptCostPerByte()); err != nil {
				return nil, err
			}
		}
		return &conway.ConwayProtocolParameters{
			MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
			MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
			MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
			PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
			A0: a0, Rho: rho, Tau: tau, ProtocolVersion: pv,
			MinPoolCost: p.GetMinPoolCost(), AdaPerUtxoByte: p.GetCoinsPerUtxoByte(), CostModels: models,
			ExecutionCosts: prices, MaxTxExUnits: maxTx, MaxBlockExUnits: maxBlock,
			MaxValueSize: uint(p.GetMaxValueSize()), CollateralPercentage: uint(p.GetCollateralPercentage()),
			MaxCollateralInputs:  uint(p.GetMaxCollateralInputs()),
			PoolVotingThresholds: pt, DRepVotingThresholds: dt,
			MinCommitteeSize: uint(p.GetCommitteeMinSize()), CommitteeTermLimit: p.GetCommitteeMaxTermLength(),
			GovActionValidityPeriod: p.GetGovActionLifetime(), GovActionDeposit: p.GetGovActionDeposit(),
			DRepDeposit: p.GetDrepDeposit(), DRepInactivityPeriod: p.GetDrepActivity(),
			MinFeeRefScriptCostPerByte: refCost,
		}, nil
	case v1.Era_ERA_ALONZO:
		d, err := rat("d", p.GetD())
		if err != nil {
			return nil, err
		}
		return &alonzo.AlonzoProtocolParameters{
			MinFeeA: uint(p.GetMinFeeA()), MinFeeB: uint(p.GetMinFeeB()),
			MaxBlockBodySize: uint(p.GetMaxBlockBodySize()), MaxTxSize: uint(p.GetMaxTxSize()),
			MaxBlockHeaderSize: uint(p.GetMaxBlockHeaderSize()), KeyDeposit: uint(p.GetKeyDeposit()),
			PoolDeposit: uint(p.GetPoolDeposit()), MaxEpoch: uint(p.GetMaxEpoch()), NOpt: uint(p.GetNOpt()),
			A0: a0, Rho: rho, Tau: tau, Decentralization: d, ProtocolMajor: pv.Major, ProtocolMinor: pv.Minor,
			MinUtxoValue: uint(p.GetMinUtxoValue()), MinPoolCost: p.GetMinPoolCost(),
			AdaPerUtxoByte: p.GetCoinsPerUtxoWord() / 8, CostModels: models,
			ExecutionCosts: prices, MaxTxExUnits: maxTx, MaxBlockExUnits: maxBlock,
			MaxValueSize: uint(p.GetMaxValueSize()), CollateralPercentage: uint(p.GetCollateralPercentage()),
			MaxCollateralInputs: uint(p.GetMaxCollateralInputs()),
		}, nil
	}
	return nil, dispatch.Unsupported(fmt.Errorf("rule functions are modelled for Alonzo, Babbage and Conway, not %v", era))
}
