package main

import (
	"bytes"
	"context"
	"sync"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

const chainMagic = 1

var (
	chainOnce    sync.Once
	chainFixture *byronfixture.Chain
	chainDlg     []*v1.ByronDelegation
)

func loadChain(t *testing.T) (*byronfixture.Chain, []*v1.ByronDelegation) {
	t.Helper()
	chainOnce.Do(func() {
		dir, err := byronfixture.ModuleDir("github.com/blinklabs-io/dingo")
		if err != nil {
			t.Fatal(err)
		}
		if chainFixture, err = byronfixture.LoadPreprodChain(dir); err != nil {
			t.Fatal(err)
		}
		dlgs, err := byronfixture.PreprodDelegations(dir)
		if err != nil {
			t.Fatal(err)
		}
		for _, d := range dlgs {
			chainDlg = append(chainDlg, &v1.ByronDelegation{IssuerVkey: d.Issuer, DelegateVkey: d.Delegate})
		}
	})
	if chainFixture == nil {
		t.Fatal("preprod chain did not load")
	}
	return chainFixture, chainDlg
}

// chainParams are the preprod Byron genesis protocol parameters.
func chainParams() *v1.ByronProtocolParameters {
	return &v1.ByronProtocolParameters{
		TxFeeA: byronRat("155381", "1"), TxFeeB: byronRat("43946", "1000000"),
		MaxTxSize: 4096, MaxBlockSize: 2_000_000, MaxHeaderSize: 2_000_000,
		SlotDuration: 20_000, MaxProposalSize: 700,
		MpcThreshold:             byronRat("20000000000000", "1000000000000000"),
		HeavyDelegationThreshold: byronRat("300000000000", "1000000000000000"),
		UpdateVoteThreshold:      byronRat("1000000000000", "1000000000000000"),
		UpdateProposalThreshold:  byronRat("100000000000000", "1000000000000000"),
		UpdateProposalTtl:        10_000,
		SoftforkInitThreshold:    byronRat("900000000000000", "1000000000000000"),
		UpdateAdoptionThreshold:  byronRat("600000000000000", "1000000000000000"),
		SoftforkDecrement:        byronRat("50000000000000", "1000000000000000"),
		UnlockStakeEpoch:         18446744073709551615,
	}
}

func chainReq(c *byronfixture.Chain, dlg []*v1.ByronDelegation, i int) *v1.ByronChainUpdateRequest {
	slot := c.PrevSlot(i)
	// Block 2 proposes and adopts version 1.0.0, which block 23's proposal of
	// 2.0.0 follows; the chain state starts from that adopted version.
	adopted := &v1.ProtocolVersion{}
	if i >= 23 {
		adopted.Major = 1
	}
	return &v1.ByronChainUpdateRequest{
		ProtocolMagic: chainMagic, SecurityParam: 2160, Pparams: chainParams(),
		ChainState: &v1.ByronChainState{
			PrevHash: c.PrevHash(i), PrevSlot: &slot, Delegations: dlg,
			Update: &v1.ByronUpdateState{Adopted: adopted},
		},
		Block: c.Wrapped(i),
	}
}

func chainRejection(t *testing.T, req *v1.ByronChainUpdateRequest) []string {
	t.Helper()
	got, err := byronChainUpdate(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() == nil {
		t.Fatalf("accepted, want a rejection")
	}

	return byronNames(got.GetRejection())
}

// Every block of the preprod Byron chain, which cardano-node accepted, is a
// valid update of the state its predecessor leaves.
func TestByronChainUpdatePreprodChain(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	for i := range c.Blocks {
		got, err := byronChainUpdate(context.Background(), chainReq(c, dlg, i))
		if err != nil {
			t.Fatal(err)
		}
		if got.GetRejection() != nil {
			t.Fatalf("block %d (slot %d): %v", i, c.Blocks[i].SlotNumber(), got.GetRejection())
		}
		want := c.Blocks[i].Hash()
		if !bytes.Equal(got.GetChainState().GetPrevHash(), want[:]) {
			t.Fatalf("block %d: prev_hash %x, want %x", i, got.GetChainState().GetPrevHash(), want)
		}
		if len(got.GetChainState().GetDelegations()) != len(dlg) {
			t.Fatalf("block %d: %d active delegations, want %d", i, len(got.GetChainState().GetDelegations()), len(dlg))
		}
	}
}

func TestByronChainUpdateRejects(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	// splice replaces block 2's body with block 4's at the wire level, so the
	// header's proofs describe another body.
	var f2, f4 []cbor.RawMessage
	if _, err := cbor.Decode(c.Raw[2].CBOR, &f2); err != nil {
		t.Fatal(err)
	}
	if _, err := cbor.Decode(c.Raw[4].CBOR, &f4); err != nil {
		t.Fatal(err)
	}
	spliced := byronfixture.Array(byronfixture.Uint(1),
		byronfixture.Array(f2[0], f4[1], f2[2]))

	for name, tc := range map[string]struct {
		mod  func(r *v1.ByronChainUpdateRequest)
		want string
	}{
		"protocol magic": {func(r *v1.ByronChainUpdateRequest) { r.ProtocolMagic = 2 }, "ChainValidationProtocolMagicMismatch"},
		"previous hash":  {func(r *v1.ByronChainUpdateRequest) { r.ChainState.PrevHash[0] ^= 1 }, "ChainValidationInvalidHash"},
		"block size":     {func(r *v1.ByronChainUpdateRequest) { r.Pparams.MaxBlockSize = uint64(len(c.Raw[3].CBOR)) - 1 }, "ChainValidationBlockTooLarge"},
		"header size": {func(r *v1.ByronChainUpdateRequest) {
			r.Pparams.MaxHeaderSize = uint64(len(c.Blocks[3].Header().Cbor())) - 1
		}, "ChainValidationHeaderTooLarge"},
		"body proof": {func(r *v1.ByronChainUpdateRequest) {
			r.Block = spliced
		}, "ChainValidationProofValidationError"},
		"garbage": {func(r *v1.ByronChainUpdateRequest) { r.Block = []byte{0xff} }, "DeserialiseFailure"},
		"update payload": {func(r *v1.ByronChainUpdateRequest) {
			r.ChainState.Update.Adopted = &v1.ProtocolVersion{}
		}, "InvalidProtocolVersion"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			i := 3
			switch name {
			case "body proof":
				i = 2
			case "update payload":
				i = 23
			}
			r := chainReq(c, dlg, i)
			tc.mod(r)
			names := chainRejection(t, r)
			if got := names[len(names)-1]; got != tc.want {
				t.Fatalf("rejected as %v, want %s", names, tc.want)
			}
		})
	}
}

func TestByronChainUpdateDomain(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	for name, tc := range map[string]struct {
		mod  func(r *v1.ByronChainUpdateRequest)
		want v1.HarnessErrorCode
	}{
		"zero k":                {func(r *v1.ByronChainUpdateRequest) { r.SecurityParam = 0 }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"no delegations":        {func(r *v1.ByronChainUpdateRequest) { r.ChainState.Delegations = nil }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"signature window":      {func(r *v1.ByronChainUpdateRequest) { r.ChainState.Signers = []*v1.ByronSigner{{Key: make([]byte, 28)}} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"threshold above 1":     {func(r *v1.ByronChainUpdateRequest) { r.Pparams.UpdateAdoptionThreshold = byronRat("3", "2") }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"threshold below 1e-15": {func(r *v1.ByronChainUpdateRequest) { r.Pparams.MpcThreshold = byronRat("1", "3") }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"missing chain state":   {func(r *v1.ByronChainUpdateRequest) { r.ChainState = nil }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"bad utxo tx id": {func(r *v1.ByronChainUpdateRequest) {
			r.Utxo = []*v1.UtxoEntry{{Txin: &v1.TxIn{TxId: []byte{1}}, Txout: []byte{0x80}}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			r := chainReq(c, dlg, 3)
			tc.mod(r)
			_, err := byronChainUpdate(context.Background(), r)
			if dispatch.Code(err) != tc.want {
				t.Fatalf("code = %v (%v), want %v", dispatch.Code(err), err, tc.want)
			}
		})
	}
}

func updateReq(c *byronfixture.Chain, dlg []*v1.ByronDelegation, i int) *v1.ByronUpdateProposalRequest {
	m := c.Main(i)
	bv := m.BlockHeader.ExtraData.BlockVersion
	return &v1.ByronUpdateProposalRequest{
		Slot: m.SlotNumber(), State: &v1.ByronUpdateState{Pparams: chainParams(), Adopted: &v1.ProtocolVersion{}},
		UpdatePayload: m.Body.UpdPayloadCbor(), IssuerVkey: c.DelegateVkey(i),
		ProtocolMagic: chainMagic, SecurityParam: 2160, Delegations: dlg,
		BlockVersion:   &v1.ProtocolVersion{Major: uint32(bv.Major), Minor: uint32(bv.Minor), Alt: uint32(bv.Unknown)},
		NumGenesisKeys: uint32(len(dlg)),
	}
}

// Block 2 of the preprod chain carries the first update proposal and seven
// votes for it, which confirm it at the four-key adoption threshold.
func TestByronUpdateProposalPreprodBlock(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	got, err := byronUpdateProposal(context.Background(), updateReq(c, dlg, 2))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	st := got.GetState()
	if len(st.GetProposals()) == 0 || len(st.GetVotes()) != 7 {
		t.Fatalf("state after the block: %d proposals, %d votes", len(st.GetProposals()), len(st.GetVotes()))
	}
	if st.GetProposals()[0].ConfirmedSlot == nil {
		t.Fatalf("seven votes did not confirm the proposal")
	}
}

func TestByronUpdateProposalRejects(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	for name, tc := range map[string]struct {
		mod  func(r *v1.ByronUpdateProposalRequest)
		want []string
	}{
		"issuer not a delegate": {func(r *v1.ByronUpdateProposalRequest) {
			r.Delegations = r.Delegations[1:]
			r.NumGenesisKeys = 6
		}, []string{"Voting", "VotingVoterNotDelegate"}},
		"vote without proposal": {func(r *v1.ByronUpdateProposalRequest) {
			// Only the votes: the payload's proposal is dropped.
			var f []cbor.RawMessage
			if _, err := cbor.Decode(r.UpdatePayload, &f); err != nil {
				t.Fatal(err)
			}
			r.UpdatePayload = byronfixture.Array(byronfixture.Array(), f[1])
		}, []string{"Voting", "VotingProposalNotRegistered"}},
		"garbage payload": {func(r *v1.ByronUpdateProposalRequest) { r.UpdatePayload = []byte{0xff} }, []string{"DeserialiseFailure"}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			r := updateReq(c, dlg, 2)
			tc.mod(r)
			got, err := byronUpdateProposal(context.Background(), r)
			if err != nil {
				t.Fatal(err)
			}
			names := byronNames(got.GetRejection())
			// The first failure is the interface sub-rule, the second its constructor.
			if len(names) == 0 || names[len(names)-1] != tc.want[len(tc.want)-1] {
				t.Fatalf("rejected as %v, want %v", names, tc.want)
			}
		})
	}
}

func TestByronUpdateProposalDomain(t *testing.T) {
	t.Parallel()
	c, dlg := loadChain(t)
	for name, tc := range map[string]struct {
		mod  func(r *v1.ByronUpdateProposalRequest)
		want v1.HarnessErrorCode
	}{
		"zero k":               {func(r *v1.ByronUpdateProposalRequest) { r.SecurityParam = 0 }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"missing state":        {func(r *v1.ByronUpdateProposalRequest) { r.State = nil }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"version above Word16": {func(r *v1.ByronUpdateProposalRequest) { r.BlockVersion.Major = 70000 }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"bad issuer key":       {func(r *v1.ByronUpdateProposalRequest) { r.IssuerVkey = []byte{1} }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"repeated delegate":    {func(r *v1.ByronUpdateProposalRequest) { r.Delegations = append(r.Delegations, r.Delegations[0]) }, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"proposal not hash of id": {func(r *v1.ByronUpdateProposalRequest) {
			r.State.Proposals = []*v1.ByronUpdateProposal{{ProposalId: make([]byte, 32), Proposal: []byte{0x80}, Protocol: true}}
		}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			r := updateReq(c, dlg, 2)
			tc.mod(r)
			_, err := byronUpdateProposal(context.Background(), r)
			if dispatch.Code(err) != tc.want {
				t.Fatalf("code = %v (%v), want %v", dispatch.Code(err), err, tc.want)
			}
		})
	}
}
