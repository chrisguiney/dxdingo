package main

import (
	"bytes"
	"errors"
	"fmt"
	"math"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/chain"
	"github.com/blinklabs-io/dingo/ledger"
	gledger "github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"
	"github.com/blinklabs-io/gouroboros/vrf"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// failure is one predicate failure of a rule. TPraos rules accumulate their
// failures where Praos stops at the first.
type failure struct{ rule, name, detail string }

func failuresRejection(fs []failure) *v1.Rejection {
	if len(fs) == 0 {
		return nil
	}
	r := &v1.Rejection{}
	for _, f := range fs {
		r.Failures = append(r.Failures, &v1.Failure{Rule: f.rule, Name: f.name, Detail: f.detail})
	}
	return r
}

const (
	ruleOCERT   = "OCERT"
	ruleOVERLAY = "OVERLAY"
)

// tpraosEraOf is the TPraos era whose header format a protocol major version
// uses: Shelley 2, Allegra 3, Mary 4, Alonzo 5 and 6.
func tpraosEraOf(major uint32) (v1.Era, bool) {
	switch major {
	case 2:
		return v1.Era_ERA_SHELLEY, true
	case 3:
		return v1.Era_ERA_ALLEGRA, true
	case 4:
		return v1.Era_ERA_MARY, true
	case 5, 6:
		return v1.Era_ERA_ALONZO, true
	}
	return 0, false
}

// tpraosOcert runs the checks of the reference's OCERT rule, which records
// every failure: the KES period window, the certificate's cold signature, the
// KES signature, then the counter. recorded gives the issuer's recorded
// counter for the header's own counter, and false when none is recorded and
// the issuer is neither a registered pool nor a genesis delegate.
func tpraosOcert(h *praosHeader, slotsPerKesPeriod, maxKesEvolutions uint64, recorded func(headerCounter uint64) (uint64, bool), eraID uint8) (fs []failure, evolution uint64, err error) {
	if slotsPerKesPeriod == 0 || maxKesEvolutions == 0 {
		return nil, 0, dispatch.Unsupported(errors.New("slots_per_kes_period and max_kes_evolutions must be nonzero"))
	}
	oc, err := gledger.ExtractOpCertFromHeader(h.hdr)
	if err != nil {
		return nil, 0, err
	}
	evolution, err = gledger.ValidateKesPeriod(oc.KesPeriod, h.slot, slotsPerKesPeriod, maxKesEvolutions)
	if err != nil {
		name := "KESAfterEndOCERT"
		if h.slot/slotsPerKesPeriod < oc.KesPeriod {
			name = "KESBeforeStartOCERT"
		}
		fs = append(fs, failure{ruleOCERT, name, err.Error()})
	}
	if err := gledger.VerifyOpCertSignature(oc, h.issuer); err != nil {
		fs = append(fs, failure{ruleOCERT, "InvalidSignatureOCERT", err.Error()})
	}
	if ok, err := gledger.VerifyKes(h.hdr, slotsPerKesPeriod); err != nil || !ok {
		fs = append(fs, failure{ruleOCERT, "InvalidKesSignatureOCERT", fmt.Sprintf("verify: ok=%v err=%v", ok, err)})
	}
	if last, found := recorded(oc.IssueNumber); !found {
		fs = append(fs, failure{ruleOCERT, "NoCounterForKeyHashOCERT", "no counter for the issuer"})
	} else if res := ledger.CheckOpCertCounter(last, true, oc.IssueNumber, eraID); !res.Ok {
		fs = append(fs, failure{ruleOCERT, "CounterTooSmallOCERT", res.Err.Error()})
	}
	return fs, evolution, nil
}

// tpraosEraID is the gouroboros era id of a TPraos era.
func tpraosEraID(era v1.Era) uint8 {
	switch era {
	case v1.Era_ERA_SHELLEY:
		return shelley.EraIdShelley
	case v1.Era_ERA_ALLEGRA:
		return allegra.EraIdAllegra
	case v1.Era_ERA_MARY:
		return mary.EraIdMary
	}
	return alonzo.EraIdAlonzo
}

// overlayEnv is the request's overlay environment: no overlay slot (d = 0)
// and an active slot coefficient of one when it is unset.
type overlayEnv struct {
	d, f        *big.Rat
	epochLength uint64
	// delegations are sorted by genesis key hash, as the reference's Set of
	// genesis keys orders them.
	delegations []*v1.GenesisDelegation
}

func overlayEnvOf(o *v1.TpraosOverlayEnv) (*overlayEnv, error) {
	if o == nil {
		return &overlayEnv{d: new(big.Rat), f: big.NewRat(1, 1), epochLength: 1}, nil
	}
	d, err := ratArg("overlay.d", o.GetD())
	if err != nil {
		return nil, err
	}
	if err := unitInterval("overlay.d", d); err != nil {
		return nil, err
	}
	f, err := ratArg("overlay.active_slot_coeff", o.GetActiveSlotCoeff())
	if err != nil {
		return nil, err
	}
	if err := positiveUnitInterval("overlay.active_slot_coeff", f); err != nil {
		return nil, err
	}
	if err := nonzero("overlay.epoch_length", o.GetEpochLength()); err != nil {
		return nil, err
	}
	seen := map[string]bool{}
	ds := append([]*v1.GenesisDelegation(nil), o.GetGenesisDelegations()...)
	for _, g := range ds {
		if err := sizedHash("genesis_key_hash", g.GetGenesisKeyHash(), 28); err != nil {
			return nil, err
		}
		if err := sizedHash("delegate_key_hash", g.GetDelegateKeyHash(), 28); err != nil {
			return nil, err
		}
		if err := sizedHash("vrf_key_hash", g.GetVrfKeyHash(), 32); err != nil {
			return nil, err
		}
		if seen[string(g.GetGenesisKeyHash())] {
			return nil, dispatch.Unsupported(errors.New("duplicate genesis key"))
		}
		seen[string(g.GetGenesisKeyHash())] = true
	}
	sort.Slice(ds, func(i, j int) bool { return bytes.Compare(ds[i].GetGenesisKeyHash(), ds[j].GetGenesisKeyHash()) < 0 })
	if d.Sign() > 0 && len(ds) == 0 {
		return nil, dispatch.Unsupported(errors.New("no genesis keys"))
	}
	return &overlayEnv{d: d, f: f, epochLength: o.GetEpochLength(), delegations: ds}, nil
}

// tpraosVrfChecks runs the VRF part of the reference's OVERLAY rule on a
// TPraos header, with registeredKeyHash as the key hash registered for the
// issuer outside the overlay schedule. In an overlay slot the genesis
// delegation of the slot decides instead; a slot nobody produces is a
// failure by itself. The leader threshold (VRFLeaderValueTooBig) is not
// checked: outside the overlay schedule it needs a coefficient of one.
func tpraosVrfChecks(h *praosHeader, eta0 []byte, registeredKeyHash []byte, env *overlayEnv) ([]failure, error) {
	rel := h.slot % env.epochLength
	index, status := ledger.ClassifyGenesisOverlaySlot(rel, env.d, env.f, uint64(len(env.delegations)))
	proofs := func() *failure {
		for _, p := range []struct {
			name, seed    string
			proof, output []byte
		}{
			{"VRFKeyBadNonce", "eta", h.nonceProof, h.nonceOutput},
			{"VRFKeyBadLeaderValue", "l", h.vrfProof, h.vrfOutput},
		} {
			seed := vrf.SeedL()
			if p.seed == "eta" {
				seed = vrf.SeedEta()
			}
			input, err := vrf.MkSeedTPraos(int64(h.slot), eta0, seed) //nolint:gosec // bounded by decodeOrReject
			if err != nil {
				return &failure{ruleOVERLAY, p.name, err.Error()}
			}
			if ok, err := vrf.Verify(h.vrfKey, p.proof, p.output, input); err != nil || !ok {
				return &failure{ruleOVERLAY, p.name, fmt.Sprintf("verify: ok=%v err=%v", ok, err)}
			}
		}
		return nil
	}
	switch status {
	case ledger.GenesisOverlayNonActive:
		return []failure{{ruleOVERLAY, "NotActiveSlotOVERLAY", fmt.Sprintf("slot %d", h.slot)}}, nil
	case ledger.GenesisOverlayActive:
		var fs []failure
		g := env.delegations[index]
		if !bytes.Equal(b224(h.issuer), g.GetDelegateKeyHash()) {
			fs = append(fs, failure{ruleOVERLAY, "WrongGenesisColdKeyOVERLAY", "issuer is not the slot's genesis delegate"})
		}
		if err := ledger.CheckRegisteredVrfKey(h.slot, g.GetDelegateKeyHash(), h.vrfKey, g.GetVrfKeyHash()); err != nil {
			return append(fs, failure{ruleOVERLAY, "WrongGenesisVRFKeyOVERLAY", err.Error()}), nil
		}
		if f := proofs(); f != nil {
			fs = append(fs, *f)
		}
		return fs, nil
	}
	if env.f.Cmp(big.NewRat(1, 1)) != 0 {
		return nil, dispatch.Unsupported(errors.New("a slot outside the overlay schedule needs active_slot_coeff 1: the leader threshold is not part of the request"))
	}
	if err := ledger.CheckRegisteredVrfKey(h.slot, h.poolID(), h.vrfKey, registeredKeyHash); err != nil {
		return []failure{{ruleOVERLAY, "VRFKeyWrongVRFKey", err.Error()}}, nil
	}
	if f := proofs(); f != nil {
		return []failure{*f}, nil
	}
	return nil, nil
}

// b224 is the Blake2b-224 hash of a key, the key hash of the reference.
func b224(key []byte) []byte {
	var vk lcommon.IssuerVkey
	copy(vk[:], key)
	return vk.Hash().Bytes()
}

// tpraosNonceValue is the nonce a TPraos block contributes: the Blake2b-256
// hash of its nonce VRF output.
func tpraosNonceValue(nonceOutput []byte) []byte {
	return lcommon.Blake2b256Hash(nonceOutput).Bytes()
}

// checkParent validates the request's parent tip: a 32-byte hash and a block
// number and slot below the largest Word64, whose successor the reference
// cannot represent.
func checkParent(parent *v1.EnvelopeTip) error {
	if parent == nil {
		return nil
	}
	if err := sizedHash("parent.hash", parent.GetHash(), 32); err != nil {
		return err
	}
	if parent.GetBlockNo() == math.MaxUint64 || parent.GetSlot() == math.MaxUint64 {
		return dispatch.Unsupported(errors.New("parent block number or slot is the largest Word64: its successor is not representable"))
	}
	return nil
}

// envelopePosition applies the position checks of the reference's
// validateEnvelope in its order: block number, slot, then the previous hash.
// Dingo checks block number and slot in ledger.CheckBlockOrder and the previous
// hash in chain.FitsTip. An empty chain has no parent to check against, and
// dingo applies none of them there.
func envelopePosition(h *praosHeader, parent *v1.EnvelopeTip) *v1.Rejection {
	if parent == nil {
		return nil
	}
	order := ledger.BlockOrder{
		BlockNumber: h.blockNo, Slot: h.slot,
		ParentBlockNumber: parent.GetBlockNo(), ParentSlot: parent.GetSlot(),
	}
	if err := ledger.CheckBlockOrderNumber(order); err != nil {
		return envelopeRejection("UnexpectedBlockNo", err)
	}
	if err := ledger.CheckBlockOrderSlot(order); err != nil {
		return envelopeRejection("UnexpectedSlotNo", err)
	}
	if !chain.FitsTip(h.prevHash, parent.GetHash()) {
		return envelopeRejection("UnexpectedPrevHash", fmt.Errorf("previous hash %x does not extend tip %x", h.prevHash, parent.GetHash()))
	}
	return nil
}
