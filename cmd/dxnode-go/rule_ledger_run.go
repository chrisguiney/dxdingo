package main

import (
	"context"
	"encoding/hex"
	"errors"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/gouroboros/ledger"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// ledgerRule selects the rule a whole-transaction run answers for.
type ledgerRule int

const (
	ruleGovOnly ledgerRule = iota
	ruleLedgerFull
)

// ruleGov maps to GOV: the proposals and votes of a transaction.
func ruleGov(ctx context.Context, req *v1.RuleGovRequest) (*v1.RuleGovResponse, error) {
	res, err := runLedgerRule(ctx, ruleGovOnly, req.GetContext(), nil, req.GetPparams(), req.GetState(), req.GetTx())
	if err != nil {
		return nil, err
	}
	return &v1.RuleGovResponse{Rejection: res.rej, Delta: res.delta}, nil
}

type ledgerOutcome struct {
	rej          *v1.Rejection
	delta        *v1.LedgerDelta
	scriptsValid bool
}

// ruleLedger maps to LEDGER: UTXOW, CERTS and GOV composed over one transaction.
func ruleLedger(ctx context.Context, req *v1.RuleLedgerRequest) (*v1.RuleLedgerResponse, error) {
	res, err := runLedgerRule(ctx, ruleLedgerFull, req.GetContext(), req.GetGlobals(), req.GetPparams(), req.GetState(), req.GetTx())
	if err != nil {
		return nil, err
	}
	return &v1.RuleLedgerResponse{Rejection: res.rej, Delta: res.delta, ScriptsValid: res.scriptsValid}, nil
}

// runLedgerRule validates a transaction against the tables with the rule
// families the selected rule owns, applies it when accepted and diffs the rows
// the production writer left into a delta. Conway runs Dingo's Conway rule
// list; Shelley to Babbage run the era's own through eras.LegacyRuleFailures.
func runLedgerRule(
	ctx context.Context, which ledgerRule,
	lc *v1.LedgerContext, g *v1.Globals, pp *v1.ProtocolParameters, st *v1.LedgerState, txCbor []byte,
) (*ledgerOutcome, error) {
	legacyID, legacy := legacyEraID[lc.GetEra()]
	if lc.GetEra() != v1.Era_ERA_CONWAY && !legacy {
		return nil, dispatch.Unsupported(fmt.Errorf("this rule is modelled for Shelley through Conway, not %v", lc.GetEra()))
	}
	if legacy && which == ruleGovOnly {
		return nil, dispatch.Unsupported(fmt.Errorf("the GOV rule exists from Conway, not in %v", lc.GetEra()))
	}
	params, err := ruleProtocolParameters(lc.GetEra(), pp)
	if err != nil {
		return nil, err
	}
	txType, err := txTypeOf(lc.GetEra())
	if err != nil {
		return nil, err
	}
	tx, err := ledger.NewTransactionFromCbor(txType, txCbor)
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	tables, err := ledgerTablesOf(lc, st)
	if err != nil {
		return nil, err
	}
	pv := uint(pp.GetProtocolVersion().GetMajor())
	in := dxoracle.LedgerRunInput{
		Slot: lc.GetSlot(), Epoch: lc.GetEpoch(), NetworkID: uint(lc.GetNetworkId()),
		Params: params, State: *tables, Tx: tx,
	}
	switch {
	case which == ruleGovOnly:
		in.Families = []lcommon.UtxoValidationFamily{lcommon.UtxoFamilyGov}
	case legacy:
		in.LegacyEraID = legacyID
		in.Families = []lcommon.UtxoValidationFamily{lcommon.UtxoFamilyUtxo, lcommon.UtxoFamilyUtxow, lcommon.UtxoFamilyUtxos}
		in.CertFamilies = []lcommon.UtxoValidationFamily{lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyCerts}
		in.Withdrawals = true
	default:
		in.Families = []lcommon.UtxoValidationFamily{
			lcommon.UtxoFamilyUtxo, lcommon.UtxoFamilyUtxow, lcommon.UtxoFamilyUtxos, lcommon.UtxoFamilyGov, lcommon.UtxoFamilyLedger,
		}
		in.CertFamilies = []lcommon.UtxoValidationFamily{
			lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyGovCert, lcommon.UtxoFamilyCerts,
		}
		in.Withdrawals = true
	}
	owned := func(f lcommon.UtxoValidationFailure) []ruleName {
		var out []ruleName
		for _, n := range ledgerFailureNames(lc.GetEra(), f, pv, tx) {
			if which == ruleLedgerFull || n.rule == "GOV" {
				out = append(out, n)
			}
		}
		return out
	}
	in.Ignore = func(f lcommon.UtxoValidationFailure) bool { return len(owned(f)) == 0 }
	o, err := theCertOracle()
	if err != nil {
		return nil, err
	}
	res, err := o.RunLedger(ctx, in)
	if err != nil {
		return nil, err
	}
	if len(res.Failures) > 0 {
		seen := map[ruleName]string{}
		for _, f := range res.Failures {
			for _, n := range owned(f) {
				if _, dup := seen[n]; !dup {
					seen[n] = f.Err.Error()
				}
			}
		}
		if len(seen) > 0 {
			var out []*v1.Failure
			for n, d := range seen {
				out = append(out, &v1.Failure{Rule: n.rule, Name: n.name, Detail: d})
			}
			sort.Slice(out, func(i, j int) bool {
				if out[i].Rule != out[j].Rule {
					return out[i].Rule < out[j].Rule
				}
				return out[i].Name < out[j].Name
			})
			mismatch := false
			for _, f := range out {
				mismatch = mismatch || (f.Rule == "UTXOS" && f.Name == "ValidationTagMismatch")
			}
			return &ledgerOutcome{rej: &v1.Rejection{Failures: out}, scriptsValid: mismatch && !tx.IsValid()}, nil
		}
	}
	d := ledgerDelta(lc, st, []lcommon.Transaction{tx}, res)
	if which == ruleGovOnly {
		// GOV changes the proposal and vote tables only.
		d = &v1.LedgerDelta{ProposalsAdded: d.ProposalsAdded, VotesAdded: d.VotesAdded}
	}
	return &ledgerOutcome{delta: d, scriptsValid: tx.IsValid()}, nil
}

// ledgerFailureNames is ledgerFailureName, with the failures gouroboros
// reports in one error split into the reference's separate constructors: a
// wrong-network proposal error covers return accounts and treasury withdrawal
// targets, which the reference reports apart.
func ledgerFailureNames(era v1.Era, f lcommon.UtxoValidationFailure, pv uint, tx lcommon.Transaction) []ruleName {
	var wrong conway.WrongNetworkProposalAddressError
	if errors.As(f.Err, &wrong) {
		bad := map[string]bool{}
		for _, a := range wrong.Addrs {
			bad[a.String()] = true
		}
		var out []ruleName
		var ret, withdrawal bool
		for _, p := range tx.ProposalProcedures() {
			if a := p.RewardAccount(); bad[a.String()] {
				ret = true
			}
			if tw, ok := p.GovAction().(*lcommon.TreasuryWithdrawalGovAction); ok && tw != nil {
				for a := range tw.Withdrawals {
					if bad[a.String()] {
						withdrawal = true
					}
				}
			}
		}
		if ret {
			out = append(out, ruleName{"GOV", "ProposalProcedureNetworkIdMismatch"})
		}
		if withdrawal {
			out = append(out, ruleName{"GOV", "TreasuryWithdrawalsNetworkIdMismatch"})
		}
		return out
	}
	if n, ok := ledgerFailureName(era, f, pv); ok {
		return []ruleName{n}
	}
	return nil
}

// ledgerFailureName names a gouroboros failure by the Haskell constructor and
// the STS rule that owns it. The second result is false for failures the
// reference reports in another rule or not at all.
func ledgerFailureName(era v1.Era, f lcommon.UtxoValidationFailure, pv uint) (ruleName, bool) {
	if era != v1.Era_ERA_CONWAY {
		return legacyLedgerFailureName(era, f)
	}
	switch f.Family {
	case lcommon.UtxoFamilyGov:
		return govFailureName(f), true
	case lcommon.UtxoFamilyLedger:
		return ledgerOwnName(f)
	case lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyGovCert, lcommon.UtxoFamilyCerts:
		as := func(target any) bool { return errors.As(f.Err, target) }
		if pv >= 11 {
			switch {
			case as(new(shelley.WithdrawalFromUnregisteredRewardAccountError)), as(new(conway.WithdrawalFromUnregisteredRewardAccountError)):
				return ruleName{"LEDGER", "ConwayWithdrawalsMissingAccounts"}, true
			case as(new(shelley.IncorrectWithdrawalAmountError)):
				return ruleName{"LEDGER", "ConwayIncompleteWithdrawals"}, true
			case as(new(conway.WithdrawalNotDelegatedToDRepError)):
				return ruleName{"LEDGER", "ConwayWdrlNotDelegatedToDRep"}, true
			}
		} else if as(new(conway.WithdrawalNotDelegatedToDRepError)) {
			return ruleName{}, false
		}
		return certFailureName(f, pv)
	}
	return haskellFailure(era, f), true
}

// legacyLedgerFailureName names a failure of a Shelley-to-Babbage LEDGER run.
// The reference checks withdrawals in LEDGER itself, ahead of DELEGS.
func legacyLedgerFailureName(era v1.Era, f lcommon.UtxoValidationFailure) (ruleName, bool) {
	switch f.Family {
	case lcommon.UtxoFamilyDeleg, lcommon.UtxoFamilyPool, lcommon.UtxoFamilyCerts:
		as := func(target any) bool { return errors.As(f.Err, target) }
		switch {
		case as(new(shelley.WithdrawalFromUnregisteredRewardAccountError)):
			return ruleName{"LEDGER", "ShelleyWithdrawalsMissingAccounts"}, true
		case as(new(shelley.IncorrectWithdrawalAmountError)):
			return ruleName{"LEDGER", "ShelleyIncompleteWithdrawals"}, true
		}
		return legacyCertFailureName(f)
	}
	return haskellFailure(era, f), true
}

// ledgerOwnName names the failures of the LEDGER rule's own predicates.
func ledgerOwnName(f lcommon.UtxoValidationFailure) (ruleName, bool) {
	switch f.RuleId {
	case lcommon.UtxoValidationRuleCurrentTreasuryValue:
		return ruleName{"LEDGER", "ConwayTreasuryValueMismatch"}, true
	case lcommon.UtxoValidationRuleRefScriptSizePerTx:
		return ruleName{"LEDGER", "ConwayTxRefScriptsSizeTooBig"}, true
	}
	return ruleName{"LEDGER", "GoRule:" + string(f.RuleId)}, true
}

// govFailureName maps the error each GOV rule reports to the reference's
// constructor.
func govFailureName(f lcommon.UtxoValidationFailure) ruleName {
	as := func(target any) bool { return errors.As(f.Err, target) }
	name := func(n string) ruleName { return ruleName{"GOV", n} }
	switch {
	case as(new(conway.ConflictingCommitteeUpdateError)):
		return name("ConflictingCommitteeUpdate")
	case as(new(conway.MalformedGovActionError)):
		return name("MalformedProposal")
	case as(new(conway.CommitteeMemberAlreadyExpiredError)):
		return name("ExpirationEpochTooSmall")
	case as(new(conway.BadHardForkProtocolVersionError)):
		return name("ProposalCantFollow")
	case as(new(conway.InvalidGovActionAncestorError)):
		return name("InvalidPrevGovActionId")
	case as(new(conway.ProposalDepositIncorrectError)):
		return name("ProposalDepositIncorrect")
	case as(new(conway.WrongNetworkProposalAddressError)):
		return name("ProposalProcedureNetworkIdMismatch")
	case as(new(conway.ProposalReturnAccountDoesNotExistError)):
		return name("ProposalReturnAccountDoesNotExist")
	case as(new(conway.TreasuryWithdrawalReturnAccountsDoNotExistError)):
		return name("TreasuryWithdrawalReturnAccountsDoNotExist")
	case as(new(conway.EmptyTreasuryWithdrawalsError)), as(new(conway.ZeroTreasuryWithdrawalAmountError)):
		return name("ZeroTreasuryWithdrawals")
	case as(new(conway.BootstrapDisallowedGovActionError)):
		return name("DisallowedProposalDuringBootstrap")
	case as(new(conway.UnknownGovActionIdError)):
		return name("GovActionsDoNotExist")
	case as(new(conway.UnknownVoterError)):
		return name("VotersDoNotExist")
	case as(new(conway.VotingOnExpiredGovActionError)):
		return name("VotingOnExpiredGovAction")
	case as(new(conway.BootstrapVotingRestrictionError)):
		return name("DisallowedVotesDuringBootstrap")
	case as(new(conway.StakePoolVotingRestrictionError)), as(new(conway.CCVotingRestrictionError)):
		return name("DisallowedVoters")
	case as(new(conway.UnelectedCommitteeVoterError)):
		return name("UnelectedCommitteeVoters")
	}
	return name("GoRule:" + string(f.RuleId) + fmt.Sprintf(":%T", f.Err))
}

// ---------------------------------------------------------------------
// Tables

func ledgerTablesOf(lc *v1.LedgerContext, st *v1.LedgerState) (*dxoracle.LedgerTables, error) {
	certs, err := certTablesOf(st)
	if err != nil {
		return nil, err
	}
	t := &dxoracle.LedgerTables{CertTables: *certs, Treasury: st.GetPots().GetTreasury()}
	if c := st.GetEnactState().GetConstitution(); c != nil {
		t.Constitution = &dxoracle.LedgerConstitution{
			AnchorURL: c.GetAnchorUrl(), AnchorHash: c.GetAnchorHash(), PolicyHash: c.GetGuardrailScriptHash(),
		}
	}
	for _, e := range st.GetUtxo() {
		out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo txout: %w", err))
		}
		if len(e.GetTxin().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("utxo txin: tx_id must be 32 bytes"))
		}
		t.Utxos = append(t.Utxos, dxoracle.LedgerUtxo{TxHash: e.GetTxin().GetTxId(), Index: e.GetTxin().GetIndex(), Output: out})
	}
	votesOf := map[string]map[string]uint8{}
	for _, v := range st.GetVotes() {
		key, err := voterKey(v.GetVoter())
		if err != nil {
			return nil, err
		}
		val, err := voteValue(v.GetVote())
		if err != nil {
			return nil, err
		}
		id := idKey(v.GetActionId())
		if votesOf[id] == nil {
			votesOf[id] = map[string]uint8{}
		}
		votesOf[id][key] = val
	}
	for _, p := range st.GetProposals() {
		if len(p.GetId().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("proposals id: tx_id must be 32 bytes"))
		}
		prop := dxoracle.LedgerProposal{
			ID: idKey(p.GetId()), ActionType: lcommon.GovActionType(p.GetActionType() - 1),
			ExpiresAfter: p.GetExpiresEpoch(), SubmittedEpoch: p.GetProposedEpoch(), Deposit: p.GetDeposit(),
			Votes: votesOf[idKey(p.GetId())],
		}
		if p.GetReturnAccount() != nil {
			cred, err := certCredential(p.GetReturnAccount())
			if err != nil {
				return nil, err
			}
			prop.ReturnAccount = &cred
		}
		if p.GetParent() != nil {
			s := idKey(p.GetParent())
			prop.Parent = &s
		}
		t.Proposals = append(t.Proposals, prop)
	}
	return t, nil
}

// voterKey is the key dingo files a vote under: the voter type and hash.
func voterKey(v *v1.Voter) (string, error) {
	switch v.GetRole() {
	case v1.VoterRole_VOTER_ROLE_STAKE_POOL:
		return fmt.Sprintf("%d:%x", lcommon.VoterTypeStakingPoolKeyHash, v.GetPoolId()), nil
	case v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, v1.VoterRole_VOTER_ROLE_DREP:
		tag, h, err := govCredential("voter credential", v.GetCredential())
		if err != nil {
			return "", err
		}
		typ := lcommon.VoterTypeConstitutionalCommitteeHotKeyHash
		if v.GetRole() == v1.VoterRole_VOTER_ROLE_DREP {
			typ = lcommon.VoterTypeDRepKeyHash
		}
		return fmt.Sprintf("%d:%x", typ+tag, h), nil
	}
	return "", dispatch.BadRequest(errors.New("voter role"))
}

// ---------------------------------------------------------------------
// Delta

func parseOutRef(s string) *v1.TxIn {
	hash, idx, _ := strings.Cut(s, "#")
	h, _ := hex.DecodeString(hash)
	n, _ := strconv.ParseUint(idx, 10, 32)
	return &v1.TxIn{TxId: h, Index: uint32(n)}
}

func ledgerDelta(lc *v1.LedgerContext, before *v1.LedgerState, txs []lcommon.Transaction, res *dxoracle.LedgerRunResult) *v1.LedgerDelta {
	d := certDelta(before, res.After)
	for _, u := range res.UtxoAdded {
		d.UtxoAdded = append(d.UtxoAdded, &v1.UtxoEntry{
			Txin:  &v1.TxIn{TxId: u.TxHash, Index: u.Index},
			Txout: u.Output.Cbor(),
		})
	}
	for _, id := range res.UtxoRemoved {
		d.UtxoRemoved = append(d.UtxoRemoved, parseOutRef(id))
	}
	known := map[string]bool{}
	for _, p := range before.GetProposals() {
		known[idKey(p.GetId())] = true
	}
	for _, p := range res.ProposalsAfter {
		if known[fmt.Sprintf("%x#%d", p.TxHash, p.ActionIndex)] {
			continue
		}
		d.ProposalsAdded = append(d.ProposalsAdded, proposalRow(p))
	}
	beforeVotes := map[string]bool{}
	for _, v := range before.GetVotes() {
		k, _ := voterKey(v.GetVoter())
		beforeVotes[idKey(v.GetActionId())+"/"+k] = true
	}
	for _, v := range res.VotesAfter {
		row := voteRow(v)
		k, _ := voterKey(row.GetVoter())
		if beforeVotes[v.ActionID+"/"+k] {
			continue
		}
		d.VotesAdded = append(d.VotesAdded, row)
	}
	sort.Slice(d.ProposalsAdded, func(i, j int) bool { return idKey(d.ProposalsAdded[i].Id) < idKey(d.ProposalsAdded[j].Id) })
	sort.Slice(d.VotesAdded, func(i, j int) bool {
		return idKey(d.VotesAdded[i].ActionId)+voterSort(d.VotesAdded[i].Voter) < idKey(d.VotesAdded[j].ActionId)+voterSort(d.VotesAdded[j].Voter)
	})
	var proposed int64
	for _, tx := range txs {
		for _, p := range tx.ProposalProcedures() {
			proposed += int64(p.Deposit())
		}
	}
	d.DepositsDelta = obligations(res, 0) - beforeObligations(before) + proposed
	d.DonationsDelta = int64(res.Donations)
	for _, tx := range txs {
		if tx.IsValid() {
			d.FeesDelta += int64(tx.Fee().Uint64())
		}
	}
	return d
}

func voterSort(v *v1.Voter) string {
	return fmt.Sprintf("%d/%x/%x", v.GetRole(), v.GetCredential().GetHash(), v.GetPoolId())
}

func obligations(res *dxoracle.LedgerRunResult, proposals int64) int64 {
	n := proposals
	for _, a := range res.After.Accounts {
		n += int64(a.Deposit)
	}
	for _, p := range res.After.Pools {
		if !p.Future {
			n += int64(p.Deposit)
		}
	}
	for _, r := range res.After.DReps {
		n += int64(r.Deposit)
	}
	return n
}

func beforeObligations(st *v1.LedgerState) int64 {
	var n int64
	for _, a := range st.GetAccounts() {
		n += int64(a.GetDeposit())
	}
	for _, p := range st.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_FUTURE {
			n += int64(p.GetDeposit())
		}
	}
	for _, r := range st.GetDreps() {
		n += int64(r.GetDeposit())
	}
	return n
}

func proposalRow(p models.GovernanceProposal) *v1.Proposal {
	row := &v1.Proposal{
		Id:            &v1.GovActionId{TxId: p.TxHash, Index: p.ActionIndex},
		ActionType:    v1.GovActionType(p.ActionType + 1),
		Action:        p.GovActionCbor,
		ProposedEpoch: p.ProposedEpoch,
		ExpiresEpoch:  p.ExpiresEpoch,
		Deposit:       p.Deposit,
	}
	if len(p.ParentTxHash) > 0 && p.ParentActionIdx != nil {
		row.Parent = &v1.GovActionId{TxId: p.ParentTxHash, Index: *p.ParentActionIdx}
	}
	if len(p.ReturnAddress) == 29 {
		kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
		if p.ReturnAddress[0]&0x10 != 0 {
			kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
		}
		row.ReturnAccount = &v1.Credential{Kind: kind, Hash: p.ReturnAddress[1:]}
	}
	return row
}

// voteRow converts a stored vote. The database files a voter as 0 committee,
// 1 DRep or 2 stake pool, with the credential type apart in CredTag.
func voteRow(v dxoracle.LedgerVote) *v1.VoteRecord {
	id, idx, _ := strings.Cut(v.ActionID, "#")
	h, _ := hex.DecodeString(id)
	n, _ := strconv.ParseUint(idx, 10, 32)
	voter := &v1.Voter{}
	if v.VoterType == 2 {
		voter.Role = v1.VoterRole_VOTER_ROLE_STAKE_POOL
		voter.PoolId = v.Credential
	} else {
		kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
		if v.CredTag == 1 {
			kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
		}
		voter.Credential = &v1.Credential{Kind: kind, Hash: v.Credential}
		voter.Role = v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE
		if v.VoterType == 1 {
			voter.Role = v1.VoterRole_VOTER_ROLE_DREP
		}
	}
	vote := [...]v1.Vote{v1.Vote_VOTE_NO, v1.Vote_VOTE_YES, v1.Vote_VOTE_ABSTAIN}[v.Vote%3]
	return &v1.VoteRecord{ActionId: &v1.GovActionId{TxId: h, Index: uint32(n)}, Voter: voter, Vote: vote}
}
