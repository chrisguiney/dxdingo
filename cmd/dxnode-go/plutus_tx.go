package main

import (
	"bytes"
	"encoding/hex"
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/blinklabs-io/dingo/ledger/hardfork"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/common/script"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"github.com/blinklabs-io/plutigo/data"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// plutusEra is what dxnode-go knows of an era that has Plutus: the
// transaction type, the highest language and its name in error messages.
type plutusEra struct {
	txType      uint
	maxLanguage uint
	name        string
	strictUpper bool
}

func plutusEraOf(era v1.Era) (plutusEra, error) {
	switch era {
	case v1.Era_ERA_ALONZO:
		return plutusEra{ledger.TxTypeAlonzo, 1, "Alonzo", false}, nil
	case v1.Era_ERA_BABBAGE:
		return plutusEra{ledger.TxTypeBabbage, 2, "Babbage", false}, nil
	case v1.Era_ERA_CONWAY:
		return plutusEra{ledger.TxTypeConway, 3, "Conway", true}, nil
	}
	return plutusEra{}, dispatch.Unsupported(fmt.Errorf("no Plutus context for %v", era))
}

// supportsLanguage reports whether the era has the language, as the
// reference's per-era EraPlutusTxInfo instances do.
func (e plutusEra) supportsLanguage(v v1.PlutusVersion) bool {
	return v >= v1.PlutusVersion_PLUTUS_VERSION_V1 && uint(v) <= e.maxLanguage
}

// pureLedger is the ledger state the Plutus functions read: the resolved
// UTxOs and the slot-to-time conversion. Anything else is a bug in the
// adapter and panics through the nil embedded interface.
type pureLedger struct {
	common.LedgerState
	utxos   common.ResolvedUtxos
	summary *hardfork.Summary
}

func (l pureLedger) UtxoById(in common.TransactionInput) (common.Utxo, error) {
	return l.utxos.UtxoById(in)
}

func (l pureLedger) SlotToTime(slot uint64) (time.Time, error) {
	return l.summary.SlotToTime(slot)
}

func (l pureLedger) TimeToSlot(t time.Time) (uint64, error) {
	return l.summary.TimeToSlot(t)
}

func decodePlutusTx(e plutusEra, raw []byte) (common.Transaction, error) {
	tx, err := ledger.NewTransactionFromCbor(e.txType, raw)
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("tx: %w", err))
	}
	return tx, nil
}

func decodeUtxos(entries []*v1.UtxoEntry) (common.ResolvedUtxos, error) {
	out := make(common.ResolvedUtxos, 0, len(entries))
	for _, e := range entries {
		id := e.GetTxin()
		if len(id.GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("tx_id must be 32 bytes"))
		}
		if id.GetIndex() > 0xffff {
			return nil, dispatch.Unsupported(errors.New("input index exceeds Word16"))
		}
		in, err := shelley.NewShelleyTransactionInput(fmt.Sprintf("%x", id.GetTxId()), int(id.GetIndex()))
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		o, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("txout: %w", err))
		}
		out = append(out, common.Utxo{Id: in, Output: o})
	}
	return out, nil
}

// historySummary maps the request's era history onto dingo's hardfork
// summary, the type its slot-to-time conversion lives on.

// purposeProto converts a script purpose to the message the reference's
// PlutusPurpose AsIxItem maps to.
func purposeProto(key common.RedeemerKey, p script.ScriptPurpose) *v1.ScriptPurpose {
	out := &v1.ScriptPurpose{Index: key.Index}
	switch p := p.(type) {
	case script.ScriptPurposeSpending:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_SPEND
		out.SpendInput = &v1.TxIn{TxId: p.Input.Id.Id().Bytes(), Index: p.Input.Id.Index()}
	case script.ScriptPurposeMinting:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT
		out.MintPolicyId = p.PolicyId.Bytes()
	case script.ScriptPurposeCertifying:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_CERT
		out.Certificate = p.Certificate.Cbor()
	case script.ScriptPurposeRewarding:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_REWARD
		out.RewardAccount = &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: p.StakeCredential.Credential.Bytes()}
	case script.ScriptPurposeVoting:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE
		out.Voter = voterProto(p.Voter)
	case script.ScriptPurposeProposing:
		out.Tag = v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_PROPOSE
		b, _ := cbor.Encode(p.ProposalProcedure)
		out.Proposal = b
	}
	return out
}

func voterProto(v common.Voter) *v1.Voter {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	switch v.Type {
	case common.VoterTypeConstitutionalCommitteeHotScriptHash, common.VoterTypeDRepScriptHash:
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	switch v.Type {
	case common.VoterTypeConstitutionalCommitteeHotKeyHash, common.VoterTypeConstitutionalCommitteeHotScriptHash:
		return &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, Credential: &v1.Credential{Kind: kind, Hash: bytes.Clone(v.Hash[:])}}
	case common.VoterTypeDRepKeyHash, common.VoterTypeDRepScriptHash:
		return &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_DREP, Credential: &v1.Credential{Kind: kind, Hash: bytes.Clone(v.Hash[:])}}
	}
	return &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_STAKE_POOL, PoolId: bytes.Clone(v.Hash[:])}
}

func sortPurposes(ps []*v1.ScriptPurpose) {
	slices.SortStableFunc(ps, func(a, b *v1.ScriptPurpose) int {
		if a.GetTag() != b.GetTag() {
			return int(a.GetTag()) - int(b.GetTag())
		}
		return int(a.GetIndex()) - int(b.GetIndex())
	})
}

// purposeFromProto builds the script purpose a ScriptPurpose message names, in
// the form NewScriptContextV1V2 and NewScriptContextV3 take. datum is the
// spending datum a V3 ScriptInfo carries.
func purposeFromProto(e plutusEra, major uint, p *v1.ScriptPurpose, datum data.PlutusData) (script.ScriptPurpose, error) {
	switch p.GetTag() {
	case v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_SPEND:
		if len(p.GetSpendInput().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("spend_input tx_id must be 32 bytes"))
		}
		in, err := shelley.NewShelleyTransactionInput(hex.EncodeToString(p.GetSpendInput().GetTxId()), int(p.GetSpendInput().GetIndex()))
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		return script.ScriptPurposeSpending{Input: common.Utxo{Id: in}, Datum: datum}, nil
	case v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_MINT:
		if len(p.GetMintPolicyId()) != 28 {
			return nil, dispatch.BadRequest(errors.New("mint_policy_id must be 28 bytes"))
		}
		return script.ScriptPurposeMinting{PolicyId: common.NewBlake2b224(p.GetMintPolicyId())}, nil
	case v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_CERT:
		var cert common.CertificateWrapper
		if _, err := cbor.Decode(p.GetCertificate(), &cert); err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("certificate: %w", err))
		}
		return script.ScriptPurposeCertifying{Index: p.GetIndex(), Certificate: cert.Certificate, ProtocolVersionMajor: major}, nil
	case v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_REWARD:
		if len(p.GetRewardAccount().GetHash()) != 28 {
			return nil, dispatch.BadRequest(errors.New("reward_account hash must be 28 bytes"))
		}
		var kind uint = common.CredentialTypeAddrKeyHash
		if p.GetRewardAccount().GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
			kind = common.CredentialTypeScriptHash
		}
		return script.ScriptPurposeRewarding{StakeCredential: common.Credential{CredType: kind, Credential: common.NewBlake2b224(p.GetRewardAccount().GetHash())}}, nil
	case v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE, v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_PROPOSE:
		if e.txType != ledger.TxTypeConway {
			return nil, dispatch.Unsupported(fmt.Errorf("no %v purpose in %s", p.GetTag(), e.name))
		}
		if p.GetTag() == v1.ScriptPurposeTag_SCRIPT_PURPOSE_TAG_VOTE {
			return votePurpose(p)
		}
		var proposal conway.ConwayProposalProcedure
		if _, err := cbor.Decode(p.GetProposal(), &proposal); err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("proposal: %w", err))
		}
		return script.ScriptPurposeProposing{Index: p.GetIndex(), ProposalProcedure: &proposal}, nil
	}
	return nil, dispatch.BadRequest(fmt.Errorf("purpose tag %v", p.GetTag()))
}

func votePurpose(p *v1.ScriptPurpose) (script.ScriptPurpose, error) {
	v := p.GetVoter()
	var voter common.Voter
	hash := v.GetCredential().GetHash()
	isScript := v.GetCredential().GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	switch v.GetRole() {
	case v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE:
		voter.Type = common.VoterTypeConstitutionalCommitteeHotKeyHash
		if isScript {
			voter.Type = common.VoterTypeConstitutionalCommitteeHotScriptHash
		}
	case v1.VoterRole_VOTER_ROLE_DREP:
		voter.Type = common.VoterTypeDRepKeyHash
		if isScript {
			voter.Type = common.VoterTypeDRepScriptHash
		}
	case v1.VoterRole_VOTER_ROLE_STAKE_POOL:
		voter.Type = common.VoterTypeStakingPoolKeyHash
		hash = v.GetPoolId()
	default:
		return nil, dispatch.BadRequest(errors.New("voter role"))
	}
	if len(hash) != 28 {
		return nil, dispatch.BadRequest(errors.New("voter hash must be 28 bytes"))
	}
	copy(voter.Hash[:], hash)
	return script.ScriptPurposeVoting{Voter: voter}, nil
}
