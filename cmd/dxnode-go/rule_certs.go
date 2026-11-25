package main

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"sync"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func registerRuleCerts(r *dispatch.Registry) {
	dispatch.Register(r, ruleDeleg)
	dispatch.Register(r, rulePool)
	dispatch.Register(r, ruleGovCert)
	dispatch.Register(r, ruleCerts)
}

// certOracle is the process-wide ledger database the certificate rules run
// against. Opening it creates a Badger store and the LedgerDB set, so it is
// opened on first use.
var certOracle struct {
	once sync.Once
	o    *dxoracle.CertOracle
	err  error
}

func theCertOracle() (*dxoracle.CertOracle, error) {
	certOracle.once.Do(func() { certOracle.o, certOracle.err = dxoracle.OpenCertOracle() })
	return certOracle.o, certOracle.err
}

// certCategory is the set of certificates one STS rule consumes.
type certCategory int

const (
	catDeleg certCategory = iota
	catPool
	catGov
)

func (c certCategory) rule() string {
	return [...]string{"DELEG", "POOL", "GOVCERT"}[c]
}

func (c certCategory) family() common.UtxoValidationFamily {
	return [...]common.UtxoValidationFamily{common.UtxoFamilyDeleg, common.UtxoFamilyPool, common.UtxoFamilyGovCert}[c]
}

// categoryOf places a certificate in the STS rule that handles it in Conway.
func categoryOf(c common.Certificate) (certCategory, bool) {
	switch c.(type) {
	case *common.StakeRegistrationCertificate, *common.StakeDeregistrationCertificate,
		*common.StakeDelegationCertificate, *common.RegistrationCertificate,
		*common.DeregistrationCertificate, *common.VoteDelegationCertificate,
		*common.StakeVoteDelegationCertificate, *common.StakeRegistrationDelegationCertificate,
		*common.VoteRegistrationDelegationCertificate, *common.StakeVoteRegistrationDelegationCertificate:
		return catDeleg, true
	case *common.PoolRegistrationCertificate, *common.PoolRetirementCertificate:
		return catPool, true
	case *common.RegistrationDrepCertificate, *common.DeregistrationDrepCertificate,
		*common.UpdateDrepCertificate, *common.AuthCommitteeHotCertificate,
		*common.ResignCommitteeColdCertificate:
		return catGov, true
	}
	return 0, false
}

func ruleDeleg(ctx context.Context, req *v1.RuleDelegRequest) (*v1.RuleDelegResponse, error) {
	rej, delta, err := runCertCategory(ctx, catDeleg, req.GetContext(), req.GetPparams(), req.GetState(), req.GetTx(), req.CertIndex)
	return &v1.RuleDelegResponse{Rejection: rej, Delta: delta}, err
}

func rulePool(ctx context.Context, req *v1.RulePoolRequest) (*v1.RulePoolResponse, error) {
	rej, delta, err := runCertCategory(ctx, catPool, req.GetContext(), req.GetPparams(), req.GetState(), req.GetTx(), req.CertIndex)
	return &v1.RulePoolResponse{Rejection: rej, Delta: delta}, err
}

func ruleGovCert(ctx context.Context, req *v1.RuleGovCertRequest) (*v1.RuleGovCertResponse, error) {
	rej, delta, err := runCertCategory(ctx, catGov, req.GetContext(), req.GetPparams(), req.GetState(), req.GetTx(), req.CertIndex)
	return &v1.RuleGovCertResponse{Rejection: rej, Delta: delta}, err
}

// ruleCerts runs the whole certificate sequence and the withdrawals.
func ruleCerts(ctx context.Context, req *v1.RuleCertsRequest) (*v1.RuleCertsResponse, error) {
	in, err := newCertRun(req.GetContext(), req.GetPparams(), req.GetState(), req.GetTx())
	if err != nil {
		return nil, err
	}
	for _, c := range in.tx.Certificates() {
		if _, ok := in.categoryOf(c); !ok {
			return nil, dispatch.Unsupported(fmt.Errorf("certificate type %d does not exist in %v", c.Type(), in.era))
		}
	}
	in.run.Certs = in.tx.Certificates()
	// From protocol version 11 withdrawals are drained by LEDGER, not CERTS;
	// the Shelley-family DELEGS rule never runs them.
	in.run.Withdrawals = !in.legacy && in.pv < 11
	in.run.Families = []common.UtxoValidationFamily{
		common.UtxoFamilyDeleg, common.UtxoFamilyPool, common.UtxoFamilyGovCert, common.UtxoFamilyCerts,
	}
	rej, delta, err := in.execute(ctx, "")
	return &v1.RuleCertsResponse{Rejection: rej, Delta: delta}, err
}

func runCertCategory(
	ctx context.Context, cat certCategory,
	lc *v1.LedgerContext, pp *v1.ProtocolParameters, st *v1.LedgerState, txCbor []byte, certIndex *uint32,
) (*v1.Rejection, *v1.LedgerDelta, error) {
	in, err := newCertRun(lc, pp, st, txCbor)
	if err != nil {
		return nil, nil, err
	}
	all := in.tx.Certificates()
	if certIndex != nil {
		if int(*certIndex) >= len(all) {
			return nil, nil, dispatch.BadRequest(fmt.Errorf("cert_index %d out of range", *certIndex))
		}
		c := all[*certIndex]
		if got, ok := in.categoryOf(c); !ok || got != cat {
			return nil, nil, dispatch.Unsupported(errors.New("the certificate at cert_index is not handled by this rule"))
		}
		in.run.Certs = []common.Certificate{c}
	} else {
		for _, c := range all {
			if got, ok := in.categoryOf(c); ok && got == cat {
				in.run.Certs = append(in.run.Certs, c)
			}
		}
	}
	in.run.Families = []common.UtxoValidationFamily{cat.family(), common.UtxoFamilyPool, common.UtxoFamilyDeleg}
	return in.execute(ctx, cat.rule())
}

type certRun struct {
	run   dxoracle.CertRunInput
	tx    common.Transaction
	state *v1.LedgerState
	pv    uint
	era   v1.Era
	// legacy marks a Shelley-to-Babbage run, which uses the era's own rule
	// list and the names of its rules.
	legacy bool
}

// categoryOf places a certificate in the STS rule that handles it in the era.
func (c *certRun) categoryOf(cert common.Certificate) (certCategory, bool) {
	if !c.legacy {
		return categoryOf(cert)
	}
	switch cert.(type) {
	case *common.StakeRegistrationCertificate, *common.StakeDeregistrationCertificate,
		*common.StakeDelegationCertificate, *common.GenesisKeyDelegationCertificate,
		*common.MoveInstantaneousRewardsCertificate:
		return catDeleg, true
	case *common.PoolRegistrationCertificate, *common.PoolRetirementCertificate:
		return catPool, true
	}
	return 0, false
}

// failureName names a failure of the run's era; the second result is false
// for failures the reference reports in another rule or not at all.
func (c *certRun) failureName(f common.UtxoValidationFailure) (ruleName, bool) {
	if c.legacy {
		return legacyCertFailureName(f)
	}
	return certFailureName(f, c.pv)
}

func newCertRun(lc *v1.LedgerContext, pp *v1.ProtocolParameters, st *v1.LedgerState, txCbor []byte) (*certRun, error) {
	era := lc.GetEra()
	legacy := era != v1.Era_ERA_CONWAY
	if legacy {
		if _, ok := legacyEraID[era]; !ok {
			return nil, dispatch.Unsupported(fmt.Errorf("certificate rules are modelled for Shelley through Conway, not %v", era))
		}
	}
	if len(st.GetProposals()) > 0 {
		return nil, dispatch.Unsupported(errors.New("committee proposals are not modelled"))
	}
	params, err := ruleProtocolParameters(era, pp)
	if err != nil {
		return nil, err
	}
	txType, err := txTypeOf(era)
	if err != nil {
		return nil, err
	}
	if !legacy {
		if _, ok := params.(*conway.ConwayProtocolParameters); !ok {
			return nil, dispatch.Unsupported(errors.New("conway parameters"))
		}
	}
	tx, err := ledger.NewTransactionFromCbor(txType, txCbor)
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	tables, err := certTablesOf(st)
	if err != nil {
		return nil, err
	}
	run := &certRun{
		run: dxoracle.CertRunInput{
			Slot: lc.GetSlot(), Epoch: lc.GetEpoch(), NetworkID: uint(lc.GetNetworkId()), Params: params, State: *tables, Tx: tx,
		},
		tx: tx, state: st, pv: uint(pp.GetProtocolVersion().GetMajor()), era: era, legacy: legacy,
	}
	if legacy {
		id := legacyEraID[era]
		run.run.RuleFailures = func(
			tx common.Transaction, slot uint64, ls common.LedgerState, pp common.ProtocolParameters, f ...common.UtxoValidationFamily,
		) []common.UtxoValidationFailure {
			out, _ := eras.LegacyRuleFailures(id, tx, slot, ls, pp, f...)
			return out
		}
	}
	return run, nil
}

// legacyEraID maps the Shelley-family eras to the era ids of their rule lists.
var legacyEraID = map[v1.Era]uint{
	v1.Era_ERA_SHELLEY: shelley.EraIdShelley, v1.Era_ERA_ALLEGRA: allegra.EraIdAllegra, v1.Era_ERA_MARY: mary.EraIdMary,
	v1.Era_ERA_ALONZO: alonzo.EraIdAlonzo, v1.Era_ERA_BABBAGE: babbage.EraIdBabbage,
}

// legacyCertFailureName names a failure of a Shelley-to-Babbage certificate
// rule by the constructor the reference reports and the rule that owns it:
// DELEG or POOL. The Shelley-family DELEGS rule has no predicates of its own
// here: the pinned ledger checks withdrawals in LEDGER.
func legacyCertFailureName(f common.UtxoValidationFailure) (ruleName, bool) {
	as := func(target any) bool { return errors.As(f.Err, target) }
	switch {
	case as(new(shelley.StakeKeyAlreadyRegisteredError)):
		return ruleName{"DELEG", "StakeKeyAlreadyRegisteredDELEG"}, true
	case as(new(shelley.StakeKeyNotRegisteredError)):
		return ruleName{"DELEG", "StakeKeyNotRegisteredDELEG"}, true
	case as(new(shelley.StakeKeyNonZeroAccountBalanceError)):
		return ruleName{"DELEG", "StakeKeyNonZeroAccountBalanceDELEG"}, true
	case as(new(shelley.DelegateUnregisteredStakeCredentialError)):
		return ruleName{"DELEG", "StakeDelegationImpossibleDELEG"}, true
	case as(new(shelley.DelegateToUnregisteredPoolError)):
		return ruleName{"DELEG", "DelegateeNotRegisteredDELEG"}, true
	case as(new(shelley.GenesisKeyNotInMappingError)):
		return ruleName{"DELEG", "GenesisKeyNotInMappingDELEG"}, true
	case as(new(shelley.DuplicateGenesisDelegateError)):
		return ruleName{"DELEG", "DuplicateGenesisDelegateDELEG"}, true
	case as(new(shelley.DuplicateGenesisVRFError)):
		return ruleName{"DELEG", "DuplicateGenesisVRFDELEG"}, true
	case as(new(shelley.MIRCertificateTooLateinEpochError)):
		return ruleName{"DELEG", "MIRCertificateTooLateinEpochDELEG"}, true
	case as(new(shelley.InsufficientForInstantaneousRewardsError)):
		return ruleName{"DELEG", "InsufficientForInstantaneousRewardsDELEG"}, true
	case as(new(shelley.InsufficientForTransferError)):
		return ruleName{"DELEG", "InsufficientForTransferDELEG"}, true
	case as(new(shelley.MIRNegativesNotCurrentlyAllowedError)):
		return ruleName{"DELEG", "MIRNegativesNotCurrentlyAllowed"}, true
	case as(new(shelley.MIRTransferNotCurrentlyAllowedError)):
		return ruleName{"DELEG", "MIRTransferNotCurrentlyAllowed"}, true
	case as(new(shelley.MIRProducesNegativeUpdateError)):
		return ruleName{"DELEG", "MIRProducesNegativeUpdate"}, true
	case as(new(shelley.StakePoolNotRegisteredOnKeyError)):
		return ruleName{"POOL", "StakePoolNotRegisteredOnKeyPOOL"}, true
	case as(new(shelley.StakePoolRetirementWrongEpochError)):
		return ruleName{"POOL", "StakePoolRetirementWrongEpochPOOL"}, true
	case as(new(shelley.StakePoolCostTooLowError)):
		return ruleName{"POOL", "StakePoolCostTooLowPOOL"}, true
	case as(new(shelley.WrongNetworkPoolError)):
		return ruleName{"POOL", "WrongNetworkPOOL"}, true
	case as(new(shelley.PoolMetadataHashTooBigError)):
		return ruleName{"POOL", "PoolMedataHashTooBig"}, true
	case as(new(shelley.VrfKeyHashAlreadyRegisteredError)):
		return ruleName{"POOL", "VRFKeyHashAlreadyRegistered"}, true
	}
	return ruleName{string(f.Family), "GoRule:" + string(f.RuleId) + ":" + fmt.Sprintf("%T", f.Err)}, true
}

// execute runs the oracle and keeps the failures that belong to the named
// rule; with no rule it keeps every certificate-family failure.
func (c *certRun) execute(ctx context.Context, rule string) (*v1.Rejection, *v1.LedgerDelta, error) {
	o, err := theCertOracle()
	if err != nil {
		return nil, nil, err
	}
	c.run.Ignore = func(f common.UtxoValidationFailure) bool {
		n, ok := c.failureName(f)
		return !ok || (rule != "" && n.rule != rule)
	}
	res, err := o.Run(ctx, c.run)
	if err != nil {
		return nil, nil, err
	}
	if len(res.Failures) > 0 {
		var out []*v1.Failure
		seen := map[ruleName]bool{}
		for _, f := range res.Failures {
			n, ok := c.failureName(f)
			if !ok || (rule != "" && n.rule != rule) || seen[n] {
				continue
			}
			seen[n] = true
			out = append(out, &v1.Failure{Rule: n.rule, Name: n.name, Detail: f.Err.Error()})
		}
		if len(out) > 0 {
			sort.Slice(out, func(i, j int) bool {
				if out[i].Rule != out[j].Rule {
					return out[i].Rule < out[j].Rule
				}
				return out[i].Name < out[j].Name
			})
			return &v1.Rejection{Failures: out}, nil, nil
		}
	}
	return nil, certDelta(c.state, res.After), nil
}

// certFailureName names a gouroboros failure by the constructor the
// reference reports and the STS rule that owns it. The second result is
// false for failures the reference reports in another rule or not at all.
func certFailureName(f common.UtxoValidationFailure, pv uint) (ruleName, bool) {
	as := func(target any) bool { return errors.As(f.Err, target) }
	deposit := func(t common.CertificateType) bool {
		return t == common.CertificateTypeRegistrationDrep || t == common.CertificateTypeDeregistrationDrep
	}
	{
		var e conway.CertificateDepositIncorrectError
		if as(&e) {
			if deposit(e.CertificateType) {
				return ruleName{"GOVCERT", "ConwayDRepIncorrectDeposit"}, true
			}
			if pv > 10 {
				return ruleName{"DELEG", "DepositIncorrectDELEG"}, true
			}
			return ruleName{"DELEG", "IncorrectDepositDELEG"}, true
		}
	}
	{
		var e conway.CertificateRefundIncorrectError
		if as(&e) {
			if deposit(e.CertificateType) {
				return ruleName{"GOVCERT", "ConwayDRepIncorrectRefund"}, true
			}
			if pv > 10 {
				return ruleName{"DELEG", "RefundIncorrectDELEG"}, true
			}
			return ruleName{"DELEG", "IncorrectDepositDELEG"}, true
		}
	}
	switch {
	case as(new(conway.StakeCredentialAlreadyRegisteredError)):
		return ruleName{"DELEG", "StakeKeyRegisteredDELEG"}, true
	case as(new(conway.StakeCredentialNotRegisteredError)), as(new(shelley.DelegateUnregisteredStakeCredentialError)):
		return ruleName{"DELEG", "StakeKeyNotRegisteredDELEG"}, true
	case as(new(conway.StakeCredentialNonZeroRewardBalanceError)):
		return ruleName{"DELEG", "StakeKeyHasNonZeroAccountBalanceDELEG"}, true
	case as(new(shelley.DelegateToUnregisteredPoolError)):
		return ruleName{"DELEG", "DelegateeStakePoolNotRegisteredDELEG"}, true
	case as(new(conway.DelegateVoteToUnregisteredDRepError)):
		return ruleName{"DELEG", "DelegateeDRepNotRegisteredDELEG"}, true
	case as(new(conway.DRepAlreadyRegisteredError)):
		return ruleName{"GOVCERT", "ConwayDRepAlreadyRegistered"}, true
	case as(new(conway.DRepNotRegisteredError)):
		return ruleName{"GOVCERT", "ConwayDRepNotRegistered"}, true
	case as(new(conway.NotCommitteeMemberError)):
		return ruleName{"GOVCERT", "ConwayCommitteeIsUnknown"}, true
	case as(new(conway.ResignedCommitteeMemberError)), as(new(conway.ResignedCommitteeMemberHotKeyError)):
		return ruleName{"GOVCERT", "ConwayCommitteeHasPreviouslyResigned"}, true
	case as(new(shelley.StakePoolNotRegisteredOnKeyError)):
		return ruleName{"POOL", "StakePoolNotRegisteredOnKeyPOOL"}, true
	case as(new(shelley.StakePoolRetirementWrongEpochError)):
		return ruleName{"POOL", "StakePoolRetirementWrongEpochPOOL"}, true
	case as(new(shelley.StakePoolCostTooLowError)):
		return ruleName{"POOL", "StakePoolCostTooLowPOOL"}, true
	case as(new(shelley.WrongNetworkPoolError)):
		return ruleName{"POOL", "WrongNetworkPOOL"}, true
	case as(new(shelley.PoolMetadataHashTooBigError)):
		return ruleName{"POOL", "PoolMedataHashTooBig"}, true
	case as(new(shelley.VrfKeyHashAlreadyRegisteredError)), as(new(conway.DuplicateVrfKeyError)):
		return ruleName{"POOL", "VRFKeyHashAlreadyRegistered"}, true
	case as(new(shelley.WithdrawalFromUnregisteredRewardAccountError)), as(new(conway.WithdrawalFromUnregisteredRewardAccountError)),
		as(new(shelley.IncorrectWithdrawalAmountError)):
		// Withdrawals are checked by CERTS before protocol version 11 and by
		// LEDGER from then on, which this function does not run.
		if pv < 11 {
			return ruleName{"CERTS", "WithdrawalsNotInRewardsCERTS"}, true
		}
		return ruleName{}, false
	case as(new(conway.WithdrawalNotDelegatedToDRepError)):
		return ruleName{}, false
	}
	return ruleName{string(f.Family), "GoRule:" + string(f.RuleId) + ":" + fmt.Sprintf("%T", f.Err)}, true
}

// ---------------------------------------------------------------------
// Tables

func certCredential(c *v1.Credential) (common.Credential, error) {
	var h common.Blake2b224
	if len(c.GetHash()) != 28 {
		return common.Credential{}, dispatch.BadRequest(errors.New("credential: want 28 bytes"))
	}
	copy(h[:], c.GetHash())
	switch c.GetKind() {
	case v1.CredentialKind_CREDENTIAL_KIND_KEY:
		return common.Credential{CredType: common.CredentialTypeAddrKeyHash, Credential: h}, nil
	case v1.CredentialKind_CREDENTIAL_KIND_SCRIPT:
		return common.Credential{CredType: common.CredentialTypeScriptHash, Credential: h}, nil
	}
	return common.Credential{}, dispatch.BadRequest(errors.New("credential kind"))
}

func credentialProto(c common.Credential) *v1.Credential {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if c.CredType == common.CredentialTypeScriptHash {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.Credential{Kind: kind, Hash: c.Credential.Bytes()}
}

func drepOf(d *v1.DRep) (*common.Drep, error) {
	switch t := d.GetTarget().(type) {
	case *v1.DRep_AlwaysAbstain:
		return &common.Drep{Type: common.DrepTypeAbstain}, nil
	case *v1.DRep_AlwaysNoConfidence:
		return &common.Drep{Type: common.DrepTypeNoConfidence}, nil
	case *v1.DRep_Credential:
		c, err := certCredential(t.Credential)
		if err != nil {
			return nil, err
		}
		typ := common.DrepTypeAddrKeyHash
		if c.CredType == common.CredentialTypeScriptHash {
			typ = common.DrepTypeScriptHash
		}
		return &common.Drep{Type: typ, Credential: c.Credential.Bytes()}, nil
	}
	return nil, dispatch.BadRequest(errors.New("drep target unset"))
}

func drepProtoOf(d *common.Drep) *v1.DRep {
	switch d.Type {
	case common.DrepTypeAbstain:
		return &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	case common.DrepTypeNoConfidence:
		return &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
	}
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if d.Type == common.DrepTypeScriptHash {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.DRep{Target: &v1.DRep_Credential{Credential: &v1.Credential{Kind: kind, Hash: d.Credential}}}
}

func certTablesOf(st *v1.LedgerState) (*dxoracle.CertTables, error) {
	t := &dxoracle.CertTables{}
	for _, a := range st.GetAccounts() {
		cred, err := certCredential(a.GetCredential())
		if err != nil {
			return nil, err
		}
		row := dxoracle.CertAccount{Credential: cred, Deposit: a.GetDeposit(), Balance: a.GetBalance()}
		if len(a.GetPoolId()) > 0 {
			if len(a.GetPoolId()) != 28 {
				return nil, dispatch.BadRequest(errors.New("accounts pool_id: want 28 bytes"))
			}
			row.Pool = a.GetPoolId()
		}
		if a.GetDrep() != nil {
			if row.Drep, err = drepOf(a.GetDrep()); err != nil {
				return nil, err
			}
		}
		t.Accounts = append(t.Accounts, row)
	}
	for _, p := range st.GetPools() {
		if len(p.GetPoolId()) != 28 {
			return nil, dispatch.BadRequest(errors.New("pools pool_id: want 28 bytes"))
		}
		if len(p.GetVrfKeyHash()) != 32 {
			return nil, dispatch.BadRequest(errors.New("pools vrf_key_hash: want 32 bytes"))
		}
		margin, err := ratArg("pools margin", p.GetMargin())
		if err != nil {
			return nil, err
		}
		if err := unitInterval("pools margin", margin); err != nil {
			return nil, err
		}
		ra, err := certCredential(p.GetRewardAccount())
		if err != nil {
			return nil, err
		}
		row := dxoracle.CertPool{
			PoolKeyHash: p.GetPoolId(), VrfKeyHash: p.GetVrfKeyHash(), Pledge: p.GetPledge(), Cost: p.GetCost(),
			Deposit: p.GetDeposit(), Margin: margin, RewardAccount: ra, Owners: p.GetOwners(),
			Future: p.GetStage() == v1.PoolStage_POOL_STAGE_FUTURE,
		}
		if p.RetiringEpoch != nil {
			e := p.GetRetiringEpoch()
			row.RetiringEpoch = &e
		}
		t.Pools = append(t.Pools, row)
	}
	for _, d := range st.GetDreps() {
		cred, err := certCredential(d.GetCredential())
		if err != nil {
			return nil, err
		}
		t.DReps = append(t.DReps, dxoracle.CertDRep{
			Credential: cred, Deposit: d.GetDeposit(), Expiry: d.GetExpiry(),
			AnchorURL: d.GetAnchorUrl(), AnchorHash: d.GetAnchorHash(),
		})
	}
	if c := st.GetCommittee(); c != nil {
		t.HasCommittee = true
		thr, err := ratArg("committee threshold", c.GetThreshold())
		if err != nil {
			return nil, err
		}
		if err := unitInterval("committee threshold", thr); err != nil {
			return nil, err
		}
		t.CommitteeThreshold = thr
		for _, m := range c.GetMembers() {
			cold, err := certCredential(m.GetCold())
			if err != nil {
				return nil, err
			}
			row := dxoracle.CertCommitteeMember{Cold: cold, Expiry: m.GetExpiry(), Resigned: m.GetResigned()}
			if m.GetHot() != nil {
				hot, err := certCredential(m.GetHot())
				if err != nil {
					return nil, err
				}
				row.Hot = &hot
			}
			t.Committee = append(t.Committee, row)
		}
	}
	return t, nil
}

// ---------------------------------------------------------------------
// Delta

func accountRowProto(a dxoracle.CertAccount) *v1.Account {
	row := &v1.Account{Credential: credentialProto(a.Credential), Deposit: a.Deposit, Balance: a.Balance, PoolId: a.Pool}
	if a.Drep != nil {
		row.Drep = drepProtoOf(a.Drep)
	}
	return row
}

func poolRowProto(p dxoracle.CertPool) *v1.Pool {
	stage := v1.PoolStage_POOL_STAGE_CURRENT
	if p.Future {
		stage = v1.PoolStage_POOL_STAGE_FUTURE
	}
	owners := append([][]byte(nil), p.Owners...)
	sort.Slice(owners, func(i, j int) bool { return string(owners[i]) < string(owners[j]) })
	return &v1.Pool{
		PoolId: p.PoolKeyHash, Stage: stage, VrfKeyHash: p.VrfKeyHash, Pledge: p.Pledge, Cost: p.Cost,
		Margin: convert.RatProto(p.Margin), RewardAccount: credentialProto(p.RewardAccount),
		Owners: owners, Deposit: p.Deposit, RetiringEpoch: p.RetiringEpoch,
	}
}

func drepRowProto(d dxoracle.CertDRep) *v1.DRepRecord {
	return &v1.DRepRecord{
		Credential: credentialProto(d.Credential), Deposit: d.Deposit, Expiry: d.Expiry,
		AnchorUrl: d.AnchorURL, AnchorHash: d.AnchorHash,
	}
}

func credSortKey(c *v1.Credential) string { return fmt.Sprintf("%d:%x", c.GetKind(), c.GetHash()) }

// certDelta diffs the request tables against the rows the oracle read back.
// Rows the oracle did not read cannot have changed.
func certDelta(before *v1.LedgerState, after *dxoracle.CertTables) *v1.LedgerDelta {
	d := &v1.LedgerDelta{}
	beforeAccounts := map[string]*v1.Account{}
	for _, a := range before.GetAccounts() {
		beforeAccounts[credSortKey(a.GetCredential())] = a
	}
	afterAccounts := map[string]*v1.Account{}
	for _, a := range after.Accounts {
		row := accountRowProto(a)
		afterAccounts[credSortKey(row.GetCredential())] = row
	}
	for k, row := range afterAccounts {
		b, ok := beforeAccounts[k]
		switch {
		case !ok:
			d.AccountsAdded = append(d.AccountsAdded, row)
		case !proto.Equal(b, row):
			d.AccountsChanged = append(d.AccountsChanged, row)
		}
	}
	for k, b := range beforeAccounts {
		if _, ok := afterAccounts[k]; !ok {
			d.AccountsRemoved = append(d.AccountsRemoved, b.GetCredential())
		}
	}
	sort.Slice(d.AccountsAdded, func(i, j int) bool {
		return credSortKey(d.AccountsAdded[i].GetCredential()) < credSortKey(d.AccountsAdded[j].GetCredential())
	})
	sort.Slice(d.AccountsChanged, func(i, j int) bool {
		return credSortKey(d.AccountsChanged[i].GetCredential()) < credSortKey(d.AccountsChanged[j].GetCredential())
	})
	sort.Slice(d.AccountsRemoved, func(i, j int) bool {
		return credSortKey(d.AccountsRemoved[i]) < credSortKey(d.AccountsRemoved[j])
	})

	poolKey := func(p *v1.Pool) string { return fmt.Sprintf("%x/%d", p.GetPoolId(), p.GetStage()) }
	beforePools := map[string]*v1.Pool{}
	for _, p := range before.GetPools() {
		row := proto.Clone(p).(*v1.Pool)
		if row.Stage == v1.PoolStage_POOL_STAGE_UNSPECIFIED {
			row.Stage = v1.PoolStage_POOL_STAGE_CURRENT
		}
		beforePools[poolKey(row)] = row
	}
	afterPools := map[string]*v1.Pool{}
	for _, p := range after.Pools {
		row := poolRowProto(p)
		afterPools[poolKey(row)] = row
	}
	for k, row := range afterPools {
		if b, ok := beforePools[k]; !ok || !proto.Equal(b, row) {
			d.PoolsChanged = append(d.PoolsChanged, row)
		}
	}
	for k, b := range beforePools {
		if _, ok := afterPools[k]; !ok && b.GetStage() == v1.PoolStage_POOL_STAGE_CURRENT {
			d.PoolsRemoved = append(d.PoolsRemoved, b.GetPoolId())
		}
	}
	sort.Slice(d.PoolsChanged, func(i, j int) bool { return poolKey(d.PoolsChanged[i]) < poolKey(d.PoolsChanged[j]) })
	sort.Slice(d.PoolsRemoved, func(i, j int) bool { return string(d.PoolsRemoved[i]) < string(d.PoolsRemoved[j]) })

	beforeDReps := map[string]*v1.DRepRecord{}
	for _, r := range before.GetDreps() {
		beforeDReps[credSortKey(r.GetCredential())] = r
	}
	afterDReps := map[string]*v1.DRepRecord{}
	for _, r := range after.DReps {
		row := drepRowProto(r)
		afterDReps[credSortKey(row.GetCredential())] = row
	}
	for k, row := range afterDReps {
		if b, ok := beforeDReps[k]; !ok || !proto.Equal(b, row) {
			d.DrepsChanged = append(d.DrepsChanged, row)
		}
	}
	for k, b := range beforeDReps {
		if _, ok := afterDReps[k]; !ok {
			d.DrepsRemoved = append(d.DrepsRemoved, b.GetCredential())
		}
	}
	sort.Slice(d.DrepsChanged, func(i, j int) bool {
		return credSortKey(d.DrepsChanged[i].GetCredential()) < credSortKey(d.DrepsChanged[j].GetCredential())
	})
	sort.Slice(d.DrepsRemoved, func(i, j int) bool { return credSortKey(d.DrepsRemoved[i]) < credSortKey(d.DrepsRemoved[j]) })

	d.CommitteeAfter = committeeAfter(before.GetCommittee(), after)
	return d
}

// committeeAfter reports the committee when an authorization changed. Members
// are the request's, then credentials only the committee state holds.
func committeeAfter(before *v1.Committee, after *dxoracle.CertTables) *v1.Committee {
	type auth struct {
		hot      string
		resigned bool
	}
	authOf := func(hot *v1.Credential, resigned bool) auth {
		if resigned {
			return auth{resigned: true}
		}
		if hot != nil {
			return auth{hot: credSortKey(hot)}
		}
		return auth{}
	}
	beforeAuth := map[string]auth{}
	for _, m := range before.GetMembers() {
		if a := authOf(m.GetHot(), m.GetResigned()); a != (auth{}) {
			beforeAuth[credSortKey(m.GetCold())] = a
		}
	}
	afterAuth := map[string]auth{}
	members := map[string]*v1.CommitteeMember{}
	for _, m := range before.GetMembers() {
		members[credSortKey(m.GetCold())] = &v1.CommitteeMember{Cold: m.GetCold(), Expiry: m.GetExpiry()}
	}
	for _, m := range after.Committee {
		row := &v1.CommitteeMember{Cold: credentialProto(m.Cold), Expiry: m.Expiry, Resigned: m.Resigned}
		if m.Hot != nil {
			row.Hot = credentialProto(*m.Hot)
		}
		k := credSortKey(row.Cold)
		if prior, ok := members[k]; ok {
			row.Expiry = prior.Expiry
		}
		members[k] = row
		if a := authOf(row.Hot, row.Resigned); a != (auth{}) {
			afterAuth[k] = a
		}
	}
	if len(beforeAuth) == len(afterAuth) {
		same := true
		for k, a := range beforeAuth {
			if afterAuth[k] != a {
				same = false
			}
		}
		if same {
			return nil
		}
	}
	out := &v1.Committee{}
	if before != nil {
		out.Threshold = before.GetThreshold()
	}
	keys := make([]string, 0, len(members))
	for k := range members {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		out.Members = append(out.Members, members[k])
	}
	return out
}
