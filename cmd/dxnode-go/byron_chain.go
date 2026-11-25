package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/ledger"
	"github.com/blinklabs-io/dingo/ledger/byronupdate"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/cbor"
	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"
	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/byron"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// byronPortionDenominator is the denominator of a LovelacePortion.
const byronPortionDenominator = 1_000_000_000_000_000

// portionArg reads a LovelacePortion given as a rational in [0, 1] that is a
// multiple of 1e-15; an absent value is zero, and anything else is
// UNSUPPORTED on both nodes.
func portionArg(name string, r *v1.Rational) (uint64, error) {
	if r == nil {
		return 0, nil
	}
	x, err := ratArg(name, r)
	if err != nil {
		return 0, err
	}
	scaled := new(big.Rat).Mul(x, big.NewRat(byronPortionDenominator, 1))
	if x.Sign() < 0 || x.Cmp(big.NewRat(1, 1)) > 0 || !scaled.IsInt() {
		return 0, dispatch.Unsupported(fmt.Errorf("%s is not a LovelacePortion", name))
	}
	return scaled.Num().Uint64(), nil
}

func portionProto(p uint64) *v1.Rational {
	return convert.RatProto(big.NewRat(0, 1).SetFrac(
		new(big.Int).SetUint64(p), big.NewInt(byronPortionDenominator),
	))
}

// byronFullParams reads every Byron protocol parameter; absent thresholds are
// zero. The adoption threshold is the softfork rule's minimum.
func byronFullParams(pp *v1.ByronProtocolParameters) (*eras.ByronProtocolParameters, error) {
	params, err := byronParams(pp)
	if err != nil {
		return nil, err
	}
	if pp.GetScriptVersion() > 0xffff {
		return nil, dispatch.Unsupported(errors.New("script_version does not fit Word16"))
	}
	params.ScriptVersion = uint16(pp.GetScriptVersion()) //nolint:gosec // bounded above
	params.SlotDuration = new(big.Int).SetUint64(pp.GetSlotDuration())
	params.MaxProposalSize = new(big.Int).SetUint64(pp.GetMaxProposalSize())
	params.UpdateProposalTTL = pp.GetUpdateProposalTtl()
	params.UnlockStakeEpoch = pp.GetUnlockStakeEpoch()
	for _, f := range []struct {
		dst  *uint64
		name string
		src  *v1.Rational
	}{
		{&params.MpcThd, "mpc_threshold", pp.GetMpcThreshold()},
		{&params.HeavyDelThd, "heavy_delegation_threshold", pp.GetHeavyDelegationThreshold()},
		{&params.UpdateVoteThd, "update_vote_threshold", pp.GetUpdateVoteThreshold()},
		{&params.UpdateProposalThd, "update_proposal_threshold", pp.GetUpdateProposalThreshold()},
		{&params.SoftforkRule.InitThd, "softfork_init_threshold", pp.GetSoftforkInitThreshold()},
		{&params.SoftforkRule.MinThd, "update_adoption_threshold", pp.GetUpdateAdoptionThreshold()},
		{&params.SoftforkRule.ThdDecrement, "softfork_decrement", pp.GetSoftforkDecrement()},
	} {
		if *f.dst, err = portionArg(f.name, f.src); err != nil {
			return nil, err
		}
	}
	return params, nil
}

func bigUint64(x *big.Int) uint64 {
	if x == nil || !x.IsUint64() {
		return 0
	}
	return x.Uint64()
}

// byronParamsProto is the inverse of byronFullParams.
func byronParamsProto(p *eras.ByronProtocolParameters) *v1.ByronProtocolParameters {
	return &v1.ByronProtocolParameters{
		TxFeeA: convert.RatProto(new(big.Rat).SetInt64(0).SetFrac(
			new(big.Int).SetUint64(p.TxFeeSummand), big.NewInt(1))),
		TxFeeB: convert.RatProto(new(big.Rat).SetFrac(
			p.TxFeeMultiplierNano, big.NewInt(byronNano))),
		MaxTxSize:                bigUint64(p.MaxTxSize),
		MaxBlockSize:             bigUint64(p.MaxBlockSize),
		MaxHeaderSize:            bigUint64(p.MaxHeaderSize),
		UpdateAdoptionThreshold:  portionProto(p.SoftforkRule.MinThd),
		UpdateProposalTtl:        p.UpdateProposalTTL,
		UpdateVoteThreshold:      portionProto(p.UpdateVoteThd),
		ScriptVersion:            uint32(p.ScriptVersion),
		SlotDuration:             bigUint64(p.SlotDuration),
		MaxProposalSize:          bigUint64(p.MaxProposalSize),
		MpcThreshold:             portionProto(p.MpcThd),
		HeavyDelegationThreshold: portionProto(p.HeavyDelThd),
		UpdateProposalThreshold:  portionProto(p.UpdateProposalThd),
		SoftforkInitThreshold:    portionProto(p.SoftforkRule.InitThd),
		SoftforkDecrement:        portionProto(p.SoftforkRule.ThdDecrement),
		UnlockStakeEpoch:         p.UnlockStakeEpoch,
	}
}

func keyHashArg(name string, b []byte) (lcommon.Blake2b224, error) {
	var h lcommon.Blake2b224
	if len(b) != len(h) {
		return h, dispatch.BadRequest(fmt.Errorf("%s is not 28 bytes", name))
	}
	copy(h[:], b)
	return h, nil
}

func byronVersionArg(name string, v *v1.ProtocolVersion) (byronupdate.ProtocolVersion, error) {
	if v.GetMajor() > 0xffff || v.GetMinor() > 0xffff || v.GetAlt() > 0xff {
		return byronupdate.ProtocolVersion{}, dispatch.Unsupported(
			fmt.Errorf("%s does not fit a Byron ProtocolVersion", name))
	}
	return byronupdate.ProtocolVersion{
		Major: uint16(v.GetMajor()), //nolint:gosec // bounded above
		Minor: uint16(v.GetMinor()), //nolint:gosec // bounded above
		Alt:   uint8(v.GetAlt()),    //nolint:gosec // bounded above
	}, nil
}

func byronVersionProto(v byronupdate.ProtocolVersion) *v1.ProtocolVersion {
	return &v1.ProtocolVersion{Major: uint32(v.Major), Minor: uint32(v.Minor), Alt: uint32(v.Alt)}
}

// decodeByronProposal decodes a proposal from its CBOR, which must be the
// whole input.
func decodeByronProposal(raw []byte) (*byron.ByronUpdateProposal, error) {
	var p byron.ByronUpdateProposal
	n, err := cbor.Decode(raw, &p)
	if err != nil {
		return nil, err
	}
	if n != len(raw) {
		return nil, errors.New("trailing bytes after the proposal")
	}
	return &p, nil
}

// byronUpdateParts reads an update state; fallback supplies the adopted
// parameters when the state carries none.
func byronUpdateParts(
	st *v1.ByronUpdateState,
	fallback *v1.ByronProtocolParameters,
) (byronupdate.StateParts, error) {
	var zero byronupdate.StateParts
	ppMsg := st.GetPparams()
	if ppMsg == nil {
		ppMsg = fallback
	}
	adopted, err := byronFullParams(ppMsg)
	if err != nil {
		return zero, err
	}
	parts := byronupdate.StateParts{
		AdoptedParams:     adopted,
		AppVersions:       map[string]byronupdate.ApplicationVersion{},
		ProtocolProposals: map[byronupdate.UpId]byronupdate.ProtocolProposal{},
		SoftwareProposals: map[byronupdate.UpId]byronupdate.SoftwareProposal{},
		Confirmed:         map[byronupdate.UpId]uint64{},
		Votes:             map[byronupdate.UpId][]byronupdate.KeyHash{},
		RegistrationSlot:  map[byronupdate.UpId]uint64{},
	}
	if st == nil {
		return parts, nil
	}
	if parts.AdoptedVersion, err = byronVersionArg("adopted", st.GetAdopted()); err != nil {
		return zero, err
	}
	for i, p := range st.GetProposals() {
		prop, err := decodeByronProposal(p.GetProposal())
		if err != nil {
			return zero, dispatch.BadRequest(fmt.Errorf("proposals[%d]: %w", i, err))
		}
		id := lcommon.Blake2b256Hash(prop.Cbor())
		if !bytes.Equal(id[:], p.GetProposalId()) {
			return zero, dispatch.BadRequest(fmt.Errorf("proposals[%d]: proposal_id is not the hash of the proposal", i))
		}
		if !p.GetProtocol() && !p.GetSoftware() {
			return zero, dispatch.BadRequest(fmt.Errorf("proposals[%d]: neither protocol nor software", i))
		}
		if _, dup := parts.RegistrationSlot[id]; dup {
			return zero, dispatch.BadRequest(fmt.Errorf("proposals[%d]: repeated proposal", i))
		}
		if p.GetProtocol() {
			params, err := adopted.ApplyUpdate(prop.BlockVersionMod)
			if err != nil {
				return zero, dispatch.BadRequest(fmt.Errorf("proposals[%d]: %w", i, err))
			}
			parts.ProtocolProposals[id] = byronupdate.ProtocolProposal{
				Version: byronupdate.ProtocolVersion{
					Major: prop.BlockVersion.Major,
					Minor: prop.BlockVersion.Minor,
					Alt:   prop.BlockVersion.Unknown,
				},
				Params: params,
			}
		}
		if p.GetSoftware() {
			parts.SoftwareProposals[id] = byronupdate.SoftwareProposal{
				AppName: prop.SoftwareVersion.Name,
				Version: prop.SoftwareVersion.Version,
			}
		}
		parts.RegistrationSlot[id] = p.GetRegisteredSlot()
		if p.ConfirmedSlot != nil {
			parts.Confirmed[id] = p.GetConfirmedSlot()
		}
	}
	for i, v := range st.GetVotes() {
		var id byronupdate.UpId
		if len(v.GetProposalId()) != len(id) {
			return zero, dispatch.BadRequest(fmt.Errorf("votes[%d]: proposal_id is not 32 bytes", i))
		}
		copy(id[:], v.GetProposalId())
		if _, ok := parts.RegistrationSlot[id]; !ok {
			return zero, dispatch.BadRequest(fmt.Errorf("votes[%d]: proposal is not in the state", i))
		}
		voter, err := keyHashArg(fmt.Sprintf("votes[%d].voter_key_hash", i), v.GetVoterKeyHash())
		if err != nil {
			return zero, err
		}
		parts.Votes[id] = append(parts.Votes[id], voter)
	}
	for i, e := range st.GetEndorsements() {
		version, err := byronVersionArg("endorsements", e.GetProtocolVersion())
		if err != nil {
			return zero, err
		}
		kh, err := keyHashArg(fmt.Sprintf("endorsements[%d].key_hash", i), e.GetKeyHash())
		if err != nil {
			return zero, err
		}
		parts.Endorsements = append(parts.Endorsements, byronupdate.Endorsement{Version: version, KeyHash: kh})
	}
	for i, c := range st.GetCandidates() {
		version, err := byronVersionArg("candidates", c.GetProtocolVersion())
		if err != nil {
			return zero, err
		}
		params, err := byronFullParams(c.GetPparams())
		if err != nil {
			return zero, fmt.Errorf("candidates[%d]: %w", i, err)
		}
		parts.Candidates = append(parts.Candidates, byronupdate.Candidate{
			Slot: c.GetSlot(), Version: version, Params: params,
		})
	}
	for _, a := range st.GetAppVersions() {
		parts.AppVersions[a.GetName()] = byronupdate.ApplicationVersion{Version: a.GetVersion(), Slot: a.GetSlot()}
	}
	return parts, nil
}

// byronUpdateProto is the canonical response form of an update state:
// everything sorted, proposals identified by id alone.
func byronUpdateProto(parts byronupdate.StateParts) *v1.ByronUpdateState {
	out := &v1.ByronUpdateState{
		Adopted: byronVersionProto(parts.AdoptedVersion),
		Pparams: byronParamsProto(parts.AdoptedParams),
	}
	ids := map[byronupdate.UpId]*v1.ByronUpdateProposal{}
	for id := range parts.ProtocolProposals {
		ids[id] = &v1.ByronUpdateProposal{ProposalId: id[:], Protocol: true}
	}
	for id := range parts.SoftwareProposals {
		if p, ok := ids[id]; ok {
			p.Software = true
		} else {
			ids[id] = &v1.ByronUpdateProposal{ProposalId: id[:], Software: true}
		}
	}
	for id, p := range ids {
		p.RegisteredSlot = parts.RegistrationSlot[id]
		if slot, ok := parts.Confirmed[id]; ok {
			p.ConfirmedSlot = &slot
		}
		out.Proposals = append(out.Proposals, p)
	}
	sort.Slice(out.Proposals, func(i, j int) bool {
		return bytes.Compare(out.Proposals[i].ProposalId, out.Proposals[j].ProposalId) < 0
	})
	for id, voters := range parts.Votes {
		for _, v := range voters {
			out.Votes = append(out.Votes, &v1.ByronUpdateVote{ProposalId: id[:], VoterKeyHash: v[:], Accept: true})
		}
	}
	sort.Slice(out.Votes, func(i, j int) bool {
		if c := bytes.Compare(out.Votes[i].ProposalId, out.Votes[j].ProposalId); c != 0 {
			return c < 0
		}
		return bytes.Compare(out.Votes[i].VoterKeyHash, out.Votes[j].VoterKeyHash) < 0
	})
	for _, e := range parts.Endorsements {
		out.Endorsements = append(out.Endorsements, &v1.ByronEndorsement{
			ProtocolVersion: byronVersionProto(e.Version), KeyHash: e.KeyHash[:],
		})
	}
	sort.Slice(out.Endorsements, func(i, j int) bool {
		a, b := out.Endorsements[i], out.Endorsements[j]
		if a.ProtocolVersion.Major != b.ProtocolVersion.Major {
			return a.ProtocolVersion.Major < b.ProtocolVersion.Major
		}
		if a.ProtocolVersion.Minor != b.ProtocolVersion.Minor {
			return a.ProtocolVersion.Minor < b.ProtocolVersion.Minor
		}
		if a.ProtocolVersion.Alt != b.ProtocolVersion.Alt {
			return a.ProtocolVersion.Alt < b.ProtocolVersion.Alt
		}
		return bytes.Compare(a.KeyHash, b.KeyHash) < 0
	})
	for _, c := range parts.Candidates {
		out.Candidates = append(out.Candidates, &v1.ByronCandidate{
			Slot: c.Slot, ProtocolVersion: byronVersionProto(c.Version), Pparams: byronParamsProto(c.Params),
		})
	}
	for name, a := range parts.AppVersions {
		out.AppVersions = append(out.AppVersions, &v1.ByronAppVersion{Name: name, Version: a.Version, Slot: a.Slot})
	}
	sort.Slice(out.AppVersions, func(i, j int) bool { return out.AppVersions[i].Name < out.AppVersions[j].Name })
	return out
}

// byronUpdateRejection reports an update-interface failure as the reference
// names it: the interface error, its sub-rule and the sub-rule's constructor.
func byronUpdateRejection(err error) []*v1.Failure {
	var (
		dupProto  byronupdate.ProposalDuplicateProtocolVersionError
		dupSoft   byronupdate.ProposalDuplicateSoftwareVersionError
		proposer  byronupdate.ProposalInvalidProposerError
		protoVer  byronupdate.ProposalInvalidProtocolVersionError
		scriptVer byronupdate.ProposalInvalidScriptVersionError
		propSig   byronupdate.ProposalInvalidSignatureError
		softVer   byronupdate.ProposalInvalidSoftwareVersionError
		maxBlock  byronupdate.ProposalMaxBlockSizeTooLargeError
		maxTx     byronupdate.ProposalMaxTxSizeTooLargeError
		tooLarge  byronupdate.ProposalTooLargeError
		null      byronupdate.ProposalNullUpdateError
		appName   byronupdate.ProposalInvalidApplicationNameError
		sysTag    byronupdate.ProposalInvalidSystemTagError
		voteSig   byronupdate.VoteInvalidSignatureError
		voteReg   byronupdate.VoteProposalNotRegisteredError
		voteDlg   byronupdate.VoteVoterNotDelegateError
		voteDup   byronupdate.VoteAlreadyCastError
		multi     byronupdate.EndorsementMultipleProposalsError
	)
	kind, name := "", ""
	switch {
	case errors.As(err, &dupProto):
		kind, name = "Registration", "DuplicateProtocolVersion"
	case errors.As(err, &dupSoft):
		kind, name = "Registration", "DuplicateSoftwareVersion"
	case errors.As(err, &proposer):
		kind, name = "Registration", "InvalidProposer"
	case errors.As(err, &protoVer):
		kind, name = "Registration", "InvalidProtocolVersion"
	case errors.As(err, &scriptVer):
		kind, name = "Registration", "InvalidScriptVersion"
	case errors.As(err, &propSig):
		kind, name = "Registration", "InvalidSignature"
	case errors.As(err, &softVer):
		kind, name = "Registration", "InvalidSoftwareVersion"
	case errors.As(err, &maxBlock):
		kind, name = "Registration", "MaxBlockSizeTooLarge"
	case errors.As(err, &maxTx):
		kind, name = "Registration", "MaxTxSizeTooLarge"
	case errors.As(err, &tooLarge):
		kind, name = "Registration", "ProposalTooLarge"
	case errors.As(err, &null):
		kind, name = "Registration", "NullUpdateProposal"
	case errors.As(err, &appName):
		kind, name = "Registration", "SoftwareVersionError"
	case errors.As(err, &sysTag):
		kind, name = "Registration", "SystemTagError"
	case errors.As(err, &voteSig):
		kind, name = "Voting", "VotingInvalidSignature"
	case errors.As(err, &voteReg):
		kind, name = "Voting", "VotingProposalNotRegistered"
	case errors.As(err, &voteDlg):
		kind, name = "Voting", "VotingVoterNotDelegate"
	case errors.As(err, &voteDup):
		kind, name = "Voting", "VotingVoteAlreadyCast"
	case errors.As(err, &multi):
		kind, name = "Endorsement", "MultipleProposalsForProtocolVersion"
	default:
		return []*v1.Failure{{Rule: "Byron.UPI", Name: "DingoError", Detail: err.Error()}}
	}
	return []*v1.Failure{
		{Rule: "Byron.UPI", Name: kind, Detail: err.Error()},
		{Rule: "Byron.UPI." + kind, Name: name, Detail: err.Error()},
	}
}

// byronChainConfig builds gouroboros' configuration from the delegations. It is
// byronPBFTConfig's table of issuers without the window and previous-slot
// restriction, which only the header check has.
func byronChainDelegations(
	magic uint32,
	k uint64,
	delegations []*v1.ByronDelegation,
) (byronconsensus.ByronConfig, error) {
	var zero byronconsensus.ByronConfig
	if k == 0 {
		return zero, dispatch.Unsupported(errors.New("security_param must be nonzero"))
	}
	if len(delegations) == 0 {
		return zero, dispatch.Unsupported(errors.New("no delegations"))
	}
	cfg := byronconsensus.ByronConfig{
		ProtocolMagic:      magic,
		SlotsPerEpoch:      10 * k,
		SecurityParam:      k,
		NumGenesisKeys:     len(delegations),
		GenesisDelegations: map[lcommon.Blake2b224]lcommon.Blake2b224{},
	}
	delegates := map[lcommon.Blake2b224]bool{}
	for i, d := range delegations {
		issuer, err := byronconsensus.PBFTVerificationKeyHash(d.GetIssuerVkey())
		if err != nil {
			return zero, dispatch.BadRequest(fmt.Errorf("delegations[%d].issuer_vkey: %w", i, err))
		}
		delegate, err := byronconsensus.PBFTVerificationKeyHash(d.GetDelegateVkey())
		if err != nil {
			return zero, dispatch.BadRequest(fmt.Errorf("delegations[%d].delegate_vkey: %w", i, err))
		}
		if _, dup := cfg.GenesisDelegations[issuer]; dup || delegates[delegate] {
			return zero, dispatch.Unsupported(fmt.Errorf("delegations[%d] repeats an issuer or delegate", i))
		}
		delegates[delegate] = true
		cfg.GenesisKeyHashes = append(cfg.GenesisKeyHashes, issuer.Bytes())
		cfg.GenesisDelegations[issuer] = delegate
	}
	return cfg, nil
}

// byronUpdateEnv is the environment of registerUpdate: the chain constants and
// the delegation map read right to left.
func byronUpdateEnv(
	magic uint32,
	k uint64,
	numGenesisKeys int,
	delegations []*v1.ByronDelegation,
) (byronupdate.Environment, error) {
	env := byronupdate.Environment{
		Config: byronupdate.Config{
			ProtocolMagic: magic, K: k, NumGenesisKeys: numGenesisKeys,
		},
		DelegateToGenesis: map[byronupdate.KeyHash]byronupdate.KeyHash{},
	}
	seenIssuer := map[lcommon.Blake2b224]bool{}
	for i, d := range delegations {
		issuer, err := byronconsensus.PBFTVerificationKeyHash(d.GetIssuerVkey())
		if err != nil {
			return env, dispatch.BadRequest(fmt.Errorf("delegations[%d].issuer_vkey: %w", i, err))
		}
		delegate, err := byronconsensus.PBFTVerificationKeyHash(d.GetDelegateVkey())
		if err != nil {
			return env, dispatch.BadRequest(fmt.Errorf("delegations[%d].delegate_vkey: %w", i, err))
		}
		// The reference's Delegation.Map is a Bimap: a repeated issuer or
		// delegate is not a map it can hold.
		if _, dup := env.DelegateToGenesis[delegate]; dup || seenIssuer[issuer] {
			return env, dispatch.Unsupported(fmt.Errorf("delegations[%d] repeats an issuer or delegate", i))
		}
		seenIssuer[issuer] = true
		env.DelegateToGenesis[delegate] = issuer
	}
	return env, nil
}

// decodeByronUpdatePayload splits a block's update payload: at most one
// proposal and the votes. Any malformed payload is a deserialisation failure.
func decodeByronUpdatePayload(raw []byte) (*byron.ByronUpdateProposal, []*byron.UpdateVote, error) {
	var fields []cbor.RawMessage
	if n, err := cbor.Decode(raw, &fields); err != nil || n != len(raw) {
		return nil, nil, fmt.Errorf("update payload: %v", err)
	}
	if len(fields) != 2 {
		return nil, nil, fmt.Errorf("update payload has %d fields, expected 2", len(fields))
	}
	var rawProposals, rawVotes []cbor.RawMessage
	if _, err := cbor.Decode(fields[0], &rawProposals); err != nil {
		return nil, nil, err
	}
	if _, err := cbor.Decode(fields[1], &rawVotes); err != nil {
		return nil, nil, err
	}
	var proposal *byron.ByronUpdateProposal
	switch len(rawProposals) {
	case 0:
	case 1:
		p, err := decodeByronProposal(rawProposals[0])
		if err != nil {
			return nil, nil, err
		}
		proposal = p
	default:
		return nil, nil, fmt.Errorf("update payload carries %d proposals", len(rawProposals))
	}
	votes := make([]*byron.UpdateVote, 0, len(rawVotes))
	for i, rv := range rawVotes {
		v, err := byron.ParseUpdateVote(rv)
		if err != nil {
			return nil, nil, fmt.Errorf("update vote %d: %w", i, err)
		}
		votes = append(votes, v)
	}
	return proposal, votes, nil
}

// byronUpdateProposal maps to byronupdate.State.Apply, registerUpdate: the
// block's proposal, its votes in order, then the issuer's endorsement.
func byronUpdateProposal(_ context.Context, req *v1.ByronUpdateProposalRequest) (*v1.ByronUpdateProposalResponse, error) {
	if req.GetState() == nil {
		return nil, dispatch.BadRequest(errors.New("state missing"))
	}
	if req.GetSecurityParam() == 0 {
		return nil, dispatch.Unsupported(errors.New("security_param must be nonzero"))
	}
	parts, err := byronUpdateParts(req.GetState(), nil)
	if err != nil {
		return nil, err
	}
	if req.GetNumGenesisKeys() > 255 {
		return nil, dispatch.Unsupported(errors.New("num_genesis_keys does not fit Word8"))
	}
	env, err := byronUpdateEnv(req.GetProtocolMagic(), req.GetSecurityParam(), int(req.GetNumGenesisKeys()), req.GetDelegations())
	if err != nil {
		return nil, err
	}
	version, err := byronVersionArg("block_version", req.GetBlockVersion())
	if err != nil {
		return nil, err
	}
	issuer, err := byronconsensus.PBFTVerificationKeyHash(req.GetIssuerVkey())
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("issuer_vkey: %w", err))
	}
	proposal, votes, err := decodeByronUpdatePayload(req.GetUpdatePayload())
	if err != nil {
		return &v1.ByronUpdateProposalResponse{Rejection: byronRejection("Byron.UPI", "DeserialiseFailure", err)}, nil
	}
	next, err := byronupdate.NewStateFromParts(parts).Apply(env, byronupdate.Block{
		Slot: req.GetSlot(), Proposal: proposal, Votes: votes, Version: version, IssuerKeyHash: issuer,
	})
	if err != nil {
		return &v1.ByronUpdateProposalResponse{Rejection: &v1.Rejection{Failures: byronUpdateRejection(err)}}, nil
	}
	return &v1.ByronUpdateProposalResponse{State: byronUpdateProto(next.Parts())}, nil
}

// byronDecodeBlock decodes the reference's ABlockOrBoundary encoding.
func byronDecodeBlock(raw []byte) (gledger.Block, error) {
	var wrapped struct {
		cbor.StructAsArray
		Tag   uint
		Block cbor.RawMessage
	}
	if n, err := cbor.Decode(raw, &wrapped); err != nil || n != len(raw) {
		return nil, fmt.Errorf("block: %v", err)
	}
	switch wrapped.Tag {
	case byron.BlockTypeByronEbb, byron.BlockTypeByronMain:
		return gledger.NewBlockFromCbor(wrapped.Tag, wrapped.Block)
	}
	return nil, fmt.Errorf("block tag %d is neither 0 nor 1", wrapped.Tag)
}

// byronChainFailures names a failure of ApplyByronBlock. A boundary block's
// previous hash is the reference's Left (genesis hash) in epoch 0 and Right
// (header hash) after, and the constructor of a mismatch depends on which side
// the state and the block are on; genesisBlock says the block is in epoch 0.
func byronChainFailures(err error, genesisBlock bool) []*v1.Failure {
	chain := func(name string) *v1.Failure {
		return &v1.Failure{Rule: "Byron.Chain", Name: name, Detail: err.Error()}
	}
	var (
		magic ledger.ByronProtocolMagicMismatchError
		size  ledger.ByronBlockSizeError
		proof ledger.ByronBodyProofError
		dlg   ledger.ByronDelegationPayloadError
		upd   ledger.ByronUpdatePayloadError
		txErr *ledger.ByronBlockTxError
	)
	switch {
	case errors.As(err, &magic):
		return []*v1.Failure{chain("ChainValidationProtocolMagicMismatch")}
	case errors.Is(err, ledger.ErrStaleChainIterator):
		// The state has a previous header hash, so a genesis boundary block's
		// genesis hash is the wrong kind of hash.
		if genesisBlock {
			return []*v1.Failure{chain("ChainValidationExpectedHeaderHash")}
		}
		return []*v1.Failure{chain("ChainValidationInvalidHash")}
	case errors.Is(err, ledger.ErrByronChainOpen):
		return []*v1.Failure{chain("ChainValidationExpectedGenesisHash")}
	case errors.As(err, &size):
		switch size.What {
		case "header":
			return []*v1.Failure{chain("ChainValidationHeaderTooLarge")}
		case "block":
			return []*v1.Failure{chain("ChainValidationBlockTooLarge")}
		}
		return []*v1.Failure{chain("ChainValidationBoundaryTooLarge")}
	case errors.As(err, &proof):
		return []*v1.Failure{chain("ChainValidationProofValidationError")}
	case errors.As(err, &dlg):
		return []*v1.Failure{chain("ChainValidationDelegationSchedulingError")}
	case errors.As(err, &upd):
		return append([]*v1.Failure{chain("ChainValidationUpdateError")}, byronUpdateRejection(upd.Err)...)
	case errors.As(err, &txErr):
		return append([]*v1.Failure{chain("ChainValidationUTxOValidationError")}, byronFailures("Byron.Tx", txErr.Err).Failures...)
	}
	return []*v1.Failure{chain("DingoError")}
}

// byronChainUpdate maps to ledger.ApplyByronBlock without the PBFT header
// checks, the ledger-level checks of updateChainBlockOrBoundary.
func byronChainUpdate(_ context.Context, req *v1.ByronChainUpdateRequest) (*v1.ByronChainUpdateResponse, error) {
	st := req.GetChainState()
	if st == nil {
		return nil, dispatch.BadRequest(errors.New("chain_state missing"))
	}
	cfg, err := byronChainDelegations(req.GetProtocolMagic(), req.GetSecurityParam(), st.GetDelegations())
	if err != nil {
		return nil, err
	}
	if len(st.GetDelegations()) > 255 {
		return nil, dispatch.Unsupported(errors.New("more than 255 genesis keys"))
	}
	if len(st.GetRecentSigners()) > 0 || len(st.GetSigners()) > 0 {
		// The reference's updateChainBlockOrBoundary has no PBFT window.
		return nil, dispatch.Unsupported(errors.New("a PBFT signature window is not part of the ledger-level update"))
	}
	parts, err := byronUpdateParts(st.GetUpdate(), req.GetPparams())
	if err != nil {
		return nil, err
	}
	parts.LastSlot = st.GetPrevSlot()
	pbft, err := ledger.NewByronPBFTState(cfg, parts.AdoptedParams)
	if err != nil {
		return nil, err
	}
	pbft = pbft.WithUpdateState(byronupdate.NewStateFromParts(parts))
	utxo := ledger.ByronUtxo{}
	for i, e := range req.GetUtxo() {
		if len(e.GetTxin().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: tx_id is not 32 bytes", i))
		}
		out, err := byron.NewByronTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: %w", i, err))
		}
		utxo.Add(lcommon.NewBlake2b256(e.GetTxin().GetTxId()), e.GetTxin().GetIndex(), out)
	}
	if n := len(st.GetPrevHash()); n != 0 && n != 32 {
		return nil, dispatch.BadRequest(errors.New("prev_hash is not 32 bytes"))
	}
	block, err := byronDecodeBlock(req.GetBlock())
	if errors.Is(err, byron.ErrBodyProofMismatch) {
		// gouroboros checks the proofs while decoding; the reference decodes
		// first and reports the mismatch from updateBlock.
		return &v1.ByronChainUpdateResponse{Rejection: byronRejection("Byron.Chain", "ChainValidationProofValidationError", err)}, nil
	}
	if err != nil {
		return &v1.ByronChainUpdateResponse{Rejection: byronRejection("Byron.Chain", "DeserialiseFailure", err)}, nil
	}
	state := ledger.ByronChainState{PBFT: pbft, PrevHash: st.GetPrevHash(), Utxo: utxo}
	next, err := ledger.ApplyByronBlock(cfg, state, block, false)
	if err != nil {
		ebb, isEbb := block.Header().(*byron.ByronEpochBoundaryBlockHeader)
		genesisBlock := isEbb && ebb != nil && ebb.ConsensusData.Epoch == 0
		return &v1.ByronChainUpdateResponse{Rejection: &v1.Rejection{Failures: byronChainFailures(err, genesisBlock)}}, nil
	}
	resp := &v1.ByronChainUpdateResponse{}
	nextParts := next.PBFT.UpdateState().Parts()
	slot := nextParts.LastSlot
	resp.ChainState = &v1.ByronChainState{
		PrevHash: next.PrevHash,
		PrevSlot: &slot,
		Update:   byronUpdateProto(nextParts),
	}
	dlg := next.PBFT.DelegationState()
	for issuer, delegate := range dlg.ActiveDelegations() {
		resp.ChainState.Delegations = append(resp.ChainState.Delegations, &v1.ByronDelegation{
			IssuerKeyHash: issuer[:], DelegateKeyHash: delegate[:],
		})
	}
	sort.Slice(resp.ChainState.Delegations, func(i, j int) bool {
		return bytes.Compare(resp.ChainState.Delegations[i].IssuerKeyHash, resp.ChainState.Delegations[j].IssuerKeyHash) < 0
	})
	for key := range utxo {
		if _, ok := next.Utxo[key]; !ok {
			resp.UtxoRemoved = append(resp.UtxoRemoved, &v1.TxIn{TxId: key.TxId[:], Index: key.Index})
		}
	}
	for key, u := range next.Utxo {
		if _, ok := utxo[key]; !ok {
			resp.UtxoAdded = append(resp.UtxoAdded, &v1.UtxoEntry{
				Txin: &v1.TxIn{TxId: key.TxId[:], Index: key.Index}, Txout: u.Output.Cbor(),
			})
		}
	}
	byronSortTxIns(resp.UtxoRemoved)
	byronSortEntries(resp.UtxoAdded)
	return resp, nil
}
