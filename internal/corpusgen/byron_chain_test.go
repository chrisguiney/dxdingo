package corpusgen

import (
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
)

// preprodChainParams are the preprod Byron genesis protocol parameters.
func preprodChainParams() *v1.ByronProtocolParameters {
	portion := func(n string) *v1.Rational { return rat(n, "1000000000000000") }
	return &v1.ByronProtocolParameters{
		TxFeeA: rat("155381", "1"), TxFeeB: rat("43946", "1000000"),
		MaxTxSize: 4096, MaxBlockSize: 2_000_000, MaxHeaderSize: 2_000_000,
		SlotDuration: 20_000, MaxProposalSize: 700,
		MpcThreshold: portion("20000000000000"), HeavyDelegationThreshold: portion("300000000000"),
		UpdateVoteThreshold: portion("1000000000000"), UpdateProposalThreshold: portion("100000000000000"),
		UpdateProposalTtl: 10_000, SoftforkInitThreshold: portion("900000000000000"),
		UpdateAdoptionThreshold: portion("600000000000000"), SoftforkDecrement: portion("50000000000000"),
		UnlockStakeEpoch: 18446744073709551615,
	}
}

func chainState(c *byronfixture.Chain, dlg []*v1.ByronDelegation, i int, adopted uint32) *v1.ByronChainState {
	slot := c.PrevSlot(i)
	return &v1.ByronChainState{
		PrevHash: c.PrevHash(i), PrevSlot: &slot, Delegations: dlg,
		Update: &v1.ByronUpdateState{Adopted: &v1.ProtocolVersion{Major: adopted}},
	}
}

func preprodFixtures(t *testing.T) (*byronfixture.Chain, []*v1.ByronDelegation) {
	t.Helper()
	dir, err := byronfixture.ModuleDir("github.com/blinklabs-io/dingo")
	if err != nil {
		t.Fatal(err)
	}
	c, err := byronfixture.LoadPreprodChain(dir)
	if err != nil {
		t.Fatal(err)
	}
	dlgs, err := byronfixture.PreprodDelegations(dir)
	if err != nil {
		t.Fatal(err)
	}
	var out []*v1.ByronDelegation
	for _, d := range dlgs {
		out = append(out, &v1.ByronDelegation{IssuerVkey: d.Issuer, DelegateVkey: d.Delegate})
	}
	return c, out
}

func byronChainCases(t *testing.T) []entry {
	t.Helper()
	c, dlg := preprodFixtures(t)
	req := func(name, why string, i int, adopted uint32, mod func(*v1.ByronChainUpdateRequest)) entry {
		r := &v1.ByronChainUpdateRequest{
			ProtocolMagic: preprodMagic, SecurityParam: 2160, Pparams: preprodChainParams(),
			ChainState: chainState(c, dlg, i, adopted), Block: c.Wrapped(i),
		}
		if mod != nil {
			mod(r)
		}
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronChainUpdate{ByronChainUpdate: r}}}
	}
	var f2, f4 []cbor.RawMessage
	if _, err := cbor.Decode(c.Raw[2].CBOR, &f2); err != nil {
		t.Fatal(err)
	}
	if _, err := cbor.Decode(c.Raw[4].CBOR, &f4); err != nil {
		t.Fatal(err)
	}
	spliced := byronfixture.Array(byronfixture.Uint(1), byronfixture.Array(f2[0], f4[1], f2[2]))
	last := len(c.Blocks) - 1
	return []entry{
		req("byron_chain_update_preprod_genesis_boundary", "the genesis epoch boundary block opens the chain: no previous hash", 0, 0, nil),
		req("byron_chain_update_preprod_first_main", "first main block after the genesis boundary", 1, 0, nil),
		req("byron_chain_update_preprod_with_update", "block 2 carries the first update proposal and seven confirming votes", 2, 0, nil),
		req("byron_chain_update_preprod_plain", "main block with empty payloads", 3, 0, nil),
		req("byron_chain_update_preprod_mid_chain", "mid-chain main block", 12, 0, nil),
		req("byron_chain_update_preprod_epoch_change", "block 23 is the first in a new epoch since the last main block, so the epoch transition runs; it proposes 2.0.0 over adopted 1.0.0", 23, 1, nil),
		req("byron_chain_update_preprod_last", "last Byron block on preprod, slot 84242", last, 1, nil),
		req("byron_chain_update_preprod_version_cannot_follow", "block 23 proposes 2.0.0 while 0.0.0 is adopted: not the next version", 23, 0, nil),
		req("byron_chain_update_wrong_magic", "protocol magic of another network: the first check", 3, 0, func(r *v1.ByronChainUpdateRequest) { r.ProtocolMagic = 2 }),
		req("byron_chain_update_block_too_large", "max_block_size one below the block's size", 3, 0, func(r *v1.ByronChainUpdateRequest) { r.Pparams.MaxBlockSize = uint64(len(c.Raw[3].CBOR)) - 1 }),
		req("byron_chain_update_block_size_at_limit", "max_block_size equal to the block's size is accepted", 3, 0, func(r *v1.ByronChainUpdateRequest) { r.Pparams.MaxBlockSize = uint64(len(c.Raw[3].CBOR)) }),
		req("byron_chain_update_header_too_large", "max_header_size one below the header's size", 3, 0, func(r *v1.ByronChainUpdateRequest) {
			r.Pparams.MaxHeaderSize = uint64(len(c.Blocks[3].Header().Cbor())) - 1
		}),
		req("byron_chain_update_body_proof", "block 4's body under block 2's header: the proofs do not match", 2, 0, func(r *v1.ByronChainUpdateRequest) { r.Block = spliced }),
		req("byron_chain_update_boundary_wrong_prev", "genesis boundary block offered after a block: a header hash was expected", 0, 0, func(r *v1.ByronChainUpdateRequest) { r.ChainState.PrevHash = rep(0x42, 32) }),
		req("byron_chain_update_garbage", "bytes that are not a block", 3, 0, func(r *v1.ByronChainUpdateRequest) { r.Block = []byte{0xff} }),
		req("byron_chain_update_zero_k", "security_param 0: UNSUPPORTED on both", 3, 0, func(r *v1.ByronChainUpdateRequest) { r.SecurityParam = 0 }),
		req("byron_chain_update_signature_window", "a PBFT window is not part of the ledger-level update: UNSUPPORTED on both", 3, 0, func(r *v1.ByronChainUpdateRequest) {
			r.ChainState.Signers = []*v1.ByronSigner{{Key: rep(1, 28), Slot: 1}}
		}),
	}
}

const updateK = 2160

// hand-built update cases run over a synthetic chain of seven genesis keys,
// each with a delegate, so proposals and votes can be signed.
type updateFixture struct {
	genesis, delegate []byronfixture.Key
	dlg               []*v1.ByronDelegation
}

func newUpdateFixture() *updateFixture {
	f := &updateFixture{}
	for i := 0; i < 7; i++ {
		g, d := byronfixture.NewKey(byte(0x20+i)), byronfixture.NewKey(byte(0x10+i))
		f.genesis, f.delegate = append(f.genesis, g), append(f.delegate, d)
		f.dlg = append(f.dlg, &v1.ByronDelegation{IssuerVkey: g.XPub, DelegateVkey: d.XPub})
	}
	return f
}

func keyHash(xpub []byte) []byte {
	h, err := byronconsensus.PBFTVerificationKeyHash(xpub)
	if err != nil {
		panic(err)
	}
	return h[:]
}

func (f *updateFixture) req(slot uint64, issuer int, version *v1.ProtocolVersion, st *v1.ByronUpdateState, payload []byte, mod func(*v1.ByronUpdateProposalRequest)) *v1.ByronUpdateProposalRequest {
	if st == nil {
		st = &v1.ByronUpdateState{}
	}
	st.Pparams = preprodChainParams()
	st.Pparams.MaxBlockSize, st.Pparams.MaxTxSize = 2_000_000, 4096
	r := &v1.ByronUpdateProposalRequest{
		Slot: slot, State: st, UpdatePayload: payload, IssuerVkey: f.delegate[issuer].XPub,
		ProtocolMagic: preprodMagic, SecurityParam: updateK, Delegations: f.dlg,
		BlockVersion: version, NumGenesisKeys: 7,
	}
	if mod != nil {
		mod(r)
	}
	return r
}

func byronUpdateCases(t *testing.T) []entry {
	t.Helper()
	c, dlg := preprodFixtures(t)
	f := newUpdateFixture()
	mk := func(name, why string, r *v1.ByronUpdateProposalRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_ByronUpdateProposal{ByronUpdateProposal: r}}}
	}
	// real: preprod blocks 2 and 23 against the genesis delegations.
	real := func(i int, adopted uint32, mod func(*v1.ByronUpdateProposalRequest)) *v1.ByronUpdateProposalRequest {
		m := c.Main(i)
		bv := m.BlockHeader.ExtraData.BlockVersion
		r := &v1.ByronUpdateProposalRequest{
			Slot: m.SlotNumber(), State: &v1.ByronUpdateState{Pparams: preprodChainParams(), Adopted: &v1.ProtocolVersion{Major: adopted}},
			UpdatePayload: m.Body.UpdPayloadCbor(), IssuerVkey: c.DelegateVkey(i),
			ProtocolMagic: preprodMagic, SecurityParam: updateK, Delegations: dlg,
			BlockVersion:   &v1.ProtocolVersion{Major: uint32(bv.Major), Minor: uint32(bv.Minor), Alt: uint32(bv.Unknown)},
			NumGenesisKeys: uint32(len(dlg)),
		}
		if mod != nil {
			mod(r)
		}
		return r
	}
	var p2, votes2 []cbor.RawMessage
	var fields []cbor.RawMessage
	payload2 := c.Main(2).Body.UpdPayloadCbor()
	if _, err := cbor.Decode(payload2, &fields); err != nil {
		t.Fatal(err)
	}
	if _, err := cbor.Decode(fields[0], &p2); err != nil {
		t.Fatal(err)
	}
	if _, err := cbor.Decode(fields[1], &votes2); err != nil {
		t.Fatal(err)
	}
	proposal2 := []byte(p2[0])
	id2 := byronfixture.ProposalID(proposal2)
	proposalOnly := byronfixture.UpdatePayload(proposal2)
	votesOnly := byronfixture.Array(byronfixture.Array(), fields[1])
	stateWith2 := func(votes int, confirmed *uint64, endorsers int) *v1.ByronUpdateState {
		st := &v1.ByronUpdateState{
			Pparams: preprodChainParams(),
			Adopted: &v1.ProtocolVersion{},
			Proposals: []*v1.ByronUpdateProposal{{
				ProposalId: id2[:], Proposal: proposal2, RegisteredSlot: 2163, ConfirmedSlot: confirmed, Protocol: true, Software: true,
			}},
		}
		for i := 0; i < votes; i++ {
			st.Votes = append(st.Votes, &v1.ByronUpdateVote{ProposalId: id2[:], VoterKeyHash: keyHash(dlg[i].IssuerVkey), Accept: true})
		}
		for i := 0; i < endorsers; i++ {
			st.Endorsements = append(st.Endorsements, &v1.ByronEndorsement{ProtocolVersion: &v1.ProtocolVersion{Major: 1}, KeyHash: keyHash(dlg[i].IssuerVkey)})
		}
		return st
	}
	slot := func(s uint64) *uint64 { return &s }
	_ = slot
	v100 := &v1.ProtocolVersion{Major: 1}
	zero := &v1.ProtocolVersion{}

	// hand-built proposals signed by delegate 0.
	spec := func(s byronfixture.ProposalSpec) []byte { return f.delegate[0].Proposal(preprodMagic, s, false) }
	u64 := func(x uint64) *uint64 { return &x }
	pay := func(p []byte, v ...[]byte) []byte { return byronfixture.UpdatePayload(p, v...) }
	valid := spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1})
	validID := byronfixture.ProposalID(valid)
	v01 := &v1.ProtocolVersion{Minor: 1}
	voteBy := func(i int, id [32]byte) []byte { return f.genesis[i].Vote(preprodMagic, id, true) }
	// votes are signed by the delegate key, which is what signs for the genesis key.
	delVote := func(i int, id [32]byte, ok bool) []byte { return f.delegate[i].Vote(preprodMagic, id, ok) }
	_ = voteBy
	stateWithValid := func(votes, endorsers int, confirmed *uint64) *v1.ByronUpdateState {
		st := &v1.ByronUpdateState{Pparams: preprodChainParams(), Proposals: []*v1.ByronUpdateProposal{{
			ProposalId: validID[:], Proposal: valid, RegisteredSlot: 100, ConfirmedSlot: confirmed, Protocol: true, Software: true,
		}}}
		for i := 0; i < votes; i++ {
			st.Votes = append(st.Votes, &v1.ByronUpdateVote{ProposalId: validID[:], VoterKeyHash: keyHash(f.genesis[i].XPub), Accept: true})
		}
		for i := 0; i < endorsers; i++ {
			st.Endorsements = append(st.Endorsements, &v1.ByronEndorsement{ProtocolVersion: v01, KeyHash: keyHash(f.genesis[i].XPub)})
		}
		return st
	}
	four := func(from int, id [32]byte) [][]byte {
		var vs [][]byte
		for i := from; i < from+4; i++ {
			vs = append(vs, delVote(i, id, true))
		}
		return vs
	}

	return []entry{
		mk("byron_update_proposal_preprod_block2", "real preprod block 2: the first proposal and seven votes confirm it at the four-key threshold", real(2, 0, nil)),
		mk("byron_update_proposal_preprod_block2_proposal_only", "block 2's proposal without its votes: registered, unconfirmed", real(2, 0, func(r *v1.ByronUpdateProposalRequest) { r.UpdatePayload = proposalOnly })),
		mk("byron_update_proposal_preprod_block2_votes_only", "block 2's votes without the proposal: the proposal is not registered", real(2, 0, func(r *v1.ByronUpdateProposalRequest) { r.UpdatePayload = votesOnly })),
		mk("byron_update_proposal_preprod_block2_voter_not_delegate", "the first genesis key's delegation removed: its delegate's vote is from a non-delegate", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.Delegations, r.NumGenesisKeys = r.Delegations[1:], 6
		})),
		mk("byron_update_proposal_preprod_block2_again", "block 2's proposal offered when already registered at version 1.0.0", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.State = stateWith2(0, nil, 0)
			r.UpdatePayload = proposalOnly
		})),
		mk("byron_update_proposal_preprod_block2_vote_again", "a vote from a genesis key that already voted", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.State = stateWith2(7, slot(2163), 0)
			r.UpdatePayload = votesOnly
		})),
		mk("byron_update_proposal_preprod_block23", "real preprod block 23: 2.0.0 follows adopted 1.0.0", real(23, 1, nil)),
		mk("byron_update_proposal_preprod_block23_cannot_follow", "block 23's proposal of 2.0.0 over adopted 0.0.0", real(23, 0, nil)),
		mk("byron_update_proposal_endorsement_candidate", "the fourth genesis key endorses 1.0.0 two k slots after confirmation: the version becomes a candidate", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.State = stateWith2(7, slot(2163), 3)
			r.UpdatePayload = byronfixture.UpdatePayload(nil)
			r.Slot = 2163 + 2*updateK
			r.BlockVersion = v100
			r.IssuerVkey = c.DelegateVkey(2)
		})),
		mk("byron_update_proposal_endorsement_too_early", "the same endorsement one slot before 2k have passed since confirmation: nothing registers", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.State = stateWith2(7, slot(2163), 3)
			r.UpdatePayload = byronfixture.UpdatePayload(nil)
			r.Slot = 2163 + 2*updateK - 1
			r.BlockVersion = v100
			r.IssuerVkey = c.DelegateVkey(2)
		})),
		mk("byron_update_proposal_proposal_expires", "an unconfirmed proposal past its time to live is dropped with its votes", real(2, 0, func(r *v1.ByronUpdateProposalRequest) {
			r.State = stateWith2(2, nil, 0)
			r.UpdatePayload = byronfixture.UpdatePayload(nil)
			r.Slot = 2163 + 10_001
		})),
		mk("byron_update_proposal_valid", "a signed proposal for 0.1.0 and software version 1", f.req(100, 0, zero, nil, pay(valid), nil)),
		mk("byron_update_proposal_valid_confirmed_by_votes", "four votes confirm the proposal in the same block", f.req(100, 0, zero, nil, pay(valid, four(0, validID)...), nil)),
		mk("byron_update_proposal_three_votes_not_confirmed", "three votes stay below the four-key threshold", f.req(100, 0, zero, nil, pay(valid, four(0, validID)[:3]...), nil)),
		mk("byron_update_proposal_vote_already_cast", "a repeated vote by the same delegate", f.req(101, 0, zero, stateWithValid(1, 0, nil), pay(nil, delVote(0, validID, true)), nil)),
		mk("byron_update_proposal_vote_bad_signature", "a vote whose signature covers another decision: rejected", f.req(101, 0, zero, stateWithValid(0, 0, nil), pay(nil, f.delegate[1].Vote(preprodMagic+1, validID, true)), nil)),
		mk("byron_update_proposal_vote_unknown_proposal", "a vote for a proposal that is not registered", f.req(101, 0, zero, nil, pay(nil, delVote(0, validID, true)), nil)),
		mk("byron_update_proposal_vote_not_approving", "a signed vote that rejects the proposal", f.req(101, 0, zero, stateWithValid(0, 0, nil), pay(nil, delVote(1, validID, false)), nil)),
		mk("byron_update_proposal_bad_signature", "proposal with a corrupted signature", f.req(100, 0, zero, nil, pay(f.delegate[0].Proposal(preprodMagic, byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1}, true)), nil)),
		mk("byron_update_proposal_not_delegate", "the proposer is not an active delegate", f.req(100, 0, zero, nil, pay(valid), func(r *v1.ByronUpdateProposalRequest) { r.Delegations = r.Delegations[1:]; r.NumGenesisKeys = 6 })),
		mk("byron_update_proposal_null_update", "same protocol version and parameters, software version already current", f.req(100, 0, zero, &v1.ByronUpdateState{AppVersions: []*v1.ByronAppVersion{{Name: "csl-daedalus", Version: 1}}}, pay(spec(byronfixture.ProposalSpec{AppVer: 1})), nil)),
		mk("byron_update_proposal_version_skips", "0.2.0 does not follow 0.0.0", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 2, 0}, AppVer: 1})), nil)),
		mk("byron_update_proposal_block_size_doubled", "max block size exactly doubled is accepted", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, MaxBlock: u64(4_000_000)})), nil)),
		mk("byron_update_proposal_block_size_too_large", "max block size more than doubled", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, MaxBlock: u64(4_000_001)})), nil)),
		mk("byron_update_proposal_tx_size_not_below_block", "max tx size equal to max block size", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, MaxTx: u64(2_000_000)})), nil)),
		mk("byron_update_proposal_script_version_jump", "script version raised by two", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, Script: u64(2)})), nil)),
		mk("byron_update_proposal_script_version_plus_one", "script version raised by one", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, Script: u64(1)})), nil)),
		mk("byron_update_proposal_software_version_skips", "software version 2 for an unknown application", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 2})), nil)),
		mk("byron_update_proposal_app_name_too_long", "application name longer than 12 characters", f.req(100, 0, zero, nil, pay(spec(byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, AppName: "thirteen-char"})), nil)),
		mk("byron_update_proposal_too_large", "max_proposal_size one below the proposal's size", f.req(100, 0, zero, nil, pay(valid), func(r *v1.ByronUpdateProposalRequest) { r.State.Pparams.MaxProposalSize = uint64(len(valid)) - 1 })),
		mk("byron_update_proposal_too_large_at_limit", "max_proposal_size equal to the proposal's size", f.req(100, 0, zero, nil, pay(valid), func(r *v1.ByronUpdateProposalRequest) { r.State.Pparams.MaxProposalSize = uint64(len(valid)) })),
		mk("byron_update_proposal_second_proposal_same_version", "a second proposal for a version already proposed", f.req(101, 1, zero, stateWithValid(0, 0, nil), pay(f.delegate[1].Proposal(preprodMagic, byronfixture.ProposalSpec{Version: [3]uint64{0, 1, 0}, AppVer: 1, AppName: "other"}, false)), nil)),
		mk("byron_update_proposal_garbage_payload", "bytes that are not an update payload", f.req(100, 0, zero, nil, []byte{0xff}, nil)),
		mk("byron_update_proposal_zero_k", "security_param 0: UNSUPPORTED on both", f.req(100, 0, zero, nil, pay(valid), func(r *v1.ByronUpdateProposalRequest) { r.SecurityParam = 0 })),
	}
}

// byronChainDivergences are confirmed mismatches; see
// divergence_byron_chain_test.go and the "divergence:" checks in Test/Byron.hs.
func byronChainDivergences(t *testing.T) []entry {
	t.Helper()
	c, dlg := preprodFixtures(t)
	return []entry{{
		"byron_chain_update_main_wrong_prev_hash",
		"a main block whose previous hash is not the tip: dingo rejects it in block application, updateChainBlockOrBoundary never compares a main block's previous hash (the consensus layer does)",
		&v1.Call{Request: &v1.Call_ByronChainUpdate{ByronChainUpdate: &v1.ByronChainUpdateRequest{
			ProtocolMagic: preprodMagic, SecurityParam: 2160, Pparams: preprodChainParams(),
			ChainState: func() *v1.ByronChainState {
				st := chainState(c, dlg, 3, 0)
				st.PrevHash = flip(st.PrevHash, 0)
				return st
			}(),
			Block: c.Wrapped(3),
		}}},
	}}
}
