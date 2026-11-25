package main

import (
	"context"
	"crypto/ed25519"
	"errors"
	"fmt"

	"github.com/blinklabs-io/gouroboros/kes"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/vrf"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// vkeyWitness maps to gouroboros VerifyVKeySignature over the body hash.
// The key hash is reported only for a well-formed 32-byte key.
func vkeyWitness(_ context.Context, req *v1.CryptoVkeyWitnessRequest) (*v1.CryptoVkeyWitnessResponse, error) {
	resp := &v1.CryptoVkeyWitnessResponse{
		Valid: lcommon.VerifyVKeySignature(req.GetVkey(), req.GetSignature(), req.GetBodyHash()) == nil,
	}
	if len(req.GetVkey()) == ed25519.PublicKeySize {
		resp.KeyHash = lcommon.Blake2b224Hash(req.GetVkey()).Bytes()
	}
	return resp, nil
}

// vrfVerify maps to gouroboros vrf.VerifyAndHash (ECVRF-ED25519-SHA512-
// Elligator2, draft 03).
func vrfVerify(_ context.Context, req *v1.CryptoVrfVerifyRequest) (*v1.CryptoVrfVerifyResponse, error) {
	out, err := vrf.VerifyAndHash(req.GetVkey(), req.GetProof(), req.GetAlpha())
	if err != nil {
		return &v1.CryptoVrfVerifyResponse{}, nil
	}
	return &v1.CryptoVrfVerifyResponse{Valid: true, Output: out}, nil
}

// kesVerify maps to gouroboros kes.VerifySignedKES, which is Sum6KES only.
func kesVerify(_ context.Context, req *v1.CryptoKesVerifyRequest) (*v1.CryptoKesVerifyResponse, error) {
	if req.GetDepth() != kes.CardanoKesDepth {
		return nil, dispatch.Unsupported(fmt.Errorf("KES depth %d; only %d is modelled", req.GetDepth(), kes.CardanoKesDepth))
	}
	return &v1.CryptoKesVerifyResponse{
		Valid: kes.VerifySignedKES(req.GetVkey(), req.GetPeriod(), req.GetMessage(), req.GetSignature()),
	}, nil
}

// addressDecode maps to gouroboros NewAddressFromBytes plus the pointer range
// check, matching the reference's strict decodeAddr. A decode failure is the
// rejection "DeserialiseFailure" on both nodes.
func addressDecode(_ context.Context, req *v1.CryptoAddressDecodeRequest) (*v1.CryptoAddressDecodeResponse, error) {
	raw := req.GetRaw()
	if raw == nil {
		return nil, dispatch.Unsupported(errors.New("only raw address bytes are modelled"))
	}
	reject := func(err error) *v1.CryptoAddressDecodeResponse {
		return &v1.CryptoAddressDecodeResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
			Rule: "address", Name: "DeserialiseFailure", Detail: err.Error(),
		}}}}
	}
	addr, err := lcommon.NewAddressFromBytes(raw)
	if err != nil {
		return reject(err), nil
	}
	if err := lcommon.CheckAddressPointerInRange(addr); err != nil {
		return reject(err), nil
	}
	enc, err := addr.Bytes()
	if err != nil {
		return reject(err), nil
	}
	resp := &v1.CryptoAddressDecodeResponse{Raw: enc}
	switch addr.Type() {
	case lcommon.AddressTypeByron:
		resp.Kind = v1.AddressKind_ADDRESS_KIND_BOOTSTRAP
		return resp, nil
	case lcommon.AddressTypeKeyKey, lcommon.AddressTypeScriptKey, lcommon.AddressTypeKeyScript, lcommon.AddressTypeScriptScript:
		resp.Kind = v1.AddressKind_ADDRESS_KIND_BASE
	case lcommon.AddressTypeKeyPointer, lcommon.AddressTypeScriptPointer:
		resp.Kind = v1.AddressKind_ADDRESS_KIND_POINTER
	case lcommon.AddressTypeKeyNone, lcommon.AddressTypeScriptNone:
		resp.Kind = v1.AddressKind_ADDRESS_KIND_ENTERPRISE
	case lcommon.AddressTypeNoneKey, lcommon.AddressTypeNoneScript:
		resp.Kind = v1.AddressKind_ADDRESS_KIND_REWARD
	default:
		return reject(fmt.Errorf("address type %d", addr.Type())), nil
	}
	resp.NetworkId = uint32(addr.NetworkId()) //nolint:gosec // a 4-bit header field
	resp.Payment = payloadCredential(addr.PayloadPayload())
	switch p := addr.StakingPayload().(type) {
	case lcommon.AddressPayloadPointer:
		resp.Pointer = &v1.Pointer{Slot: p.Slot, TxIndex: p.TxIndex, CertIndex: p.CertIndex}
	default:
		resp.Stake = payloadCredential(p)
	}
	return resp, nil
}

func payloadCredential(p lcommon.AddressPayload) *v1.Credential {
	switch c := p.(type) {
	case lcommon.AddressPayloadKeyHash:
		return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: c.Hash.Bytes()}
	case lcommon.AddressPayloadScriptHash:
		return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: c.Hash.Bytes()}
	}
	return nil
}

// scriptHash maps to gouroboros Script.Hash: Blake2b-224 over the language tag
// and the script bytes. Native scripts are decoded first, as the reference
// decodes them, so malformed CBOR is the rejection "DeserialiseFailure".
// Plutus bytes are the PlutusBinary, hashed as given.
func scriptHash(_ context.Context, req *v1.CryptoScriptHashRequest) (*v1.CryptoScriptHashResponse, error) {
	var script lcommon.Script
	switch req.GetLanguage() {
	case v1.ScriptLanguage_SCRIPT_LANGUAGE_NATIVE:
		var ns lcommon.NativeScript
		if err := ns.UnmarshalCBOR(req.GetScript()); err != nil {
			return &v1.CryptoScriptHashResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
				Rule: "script", Name: "DeserialiseFailure", Detail: err.Error(),
			}}}}, nil
		}
		script = ns
	case v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V1:
		script = lcommon.PlutusV1Script(req.GetScript())
	case v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V2:
		script = lcommon.PlutusV2Script(req.GetScript())
	case v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V3:
		script = lcommon.PlutusV3Script(req.GetScript())
	case v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V4:
		script = lcommon.PlutusV4Script(req.GetScript())
	default:
		return nil, dispatch.BadRequest(fmt.Errorf("script language %v", req.GetLanguage()))
	}
	return &v1.CryptoScriptHashResponse{Hash: script.Hash().Bytes()}, nil
}

// bootstrapWitness maps to gouroboros BootstrapWitness.KeyHash and
// VerifyVKeySignature (the check ValidateBootstrapWitnesses applies), mirroring
// bootstrapWitKeyHash and verifyBootstrapWit. A key or signature of the wrong
// width cannot be built in the reference and is reported as an invalid
// witness. matches_address compares the witness key hash with the address
// root of a Byron address; an empty address does not match, and an address
// that does not decode is the rejection "DeserialiseFailure". A body hash that
// is not 32 bytes cannot be a reference Hash and is UNSUPPORTED.
func bootstrapWitness(_ context.Context, req *v1.CryptoBootstrapWitnessRequest) (*v1.CryptoBootstrapWitnessResponse, error) {
	if len(req.GetBodyHash()) != 32 {
		return nil, dispatch.Unsupported(errors.New("body hash is not 32 bytes"))
	}
	resp := &v1.CryptoBootstrapWitnessResponse{}
	var addrRoot *lcommon.Blake2b224
	if raw := req.GetAddress(); len(raw) > 0 {
		addr, err := lcommon.NewAddressFromBytes(raw)
		if err != nil {
			return &v1.CryptoBootstrapWitnessResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
				Rule: "address", Name: "DeserialiseFailure", Detail: err.Error(),
			}}}}, nil
		}
		if err := lcommon.CheckAddressPointerInRange(addr); err != nil {
			return &v1.CryptoBootstrapWitnessResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
				Rule: "address", Name: "DeserialiseFailure", Detail: err.Error(),
			}}}}, nil
		}
		if kh, ok := addr.PayloadPayload().(lcommon.AddressPayloadKeyHash); ok && addr.Type() == lcommon.AddressTypeByron {
			h := lcommon.Blake2b224(kh.Hash)
			addrRoot = &h
		}
	}
	if len(req.GetVkey()) != ed25519.PublicKeySize || len(req.GetSignature()) != ed25519.SignatureSize {
		return resp, nil
	}
	w := lcommon.BootstrapWitness{
		PublicKey:  req.GetVkey(),
		Signature:  req.GetSignature(),
		ChainCode:  req.GetChainCode(),
		Attributes: req.GetAttributes(),
	}
	resp.Valid = lcommon.VerifyVKeySignature(w.PublicKey, w.Signature, req.GetBodyHash()) == nil
	if kh, err := w.KeyHash(); err == nil && addrRoot != nil {
		resp.MatchesAddress = kh == *addrRoot
	}
	return resp, nil
}

// vrfProve maps to gouroboros vrf.Prove. The secret key is the 32-byte seed
// (the reference's keypairFromSeed input); any other width is UNSUPPORTED.
func vrfProve(_ context.Context, req *v1.CryptoVrfProveRequest) (*v1.CryptoVrfProveResponse, error) {
	if len(req.GetSkey()) != vrf.SeedSize {
		return nil, dispatch.Unsupported(fmt.Errorf("VRF seed of %d bytes; only %d is modelled", len(req.GetSkey()), vrf.SeedSize))
	}
	proof, out, err := vrf.Prove(req.GetSkey(), req.GetAlpha())
	if err != nil {
		return &v1.CryptoVrfProveResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
			Rule: "vrf", Name: "DingoError", Detail: err.Error(),
		}}}}, nil
	}
	return &v1.CryptoVrfProveResponse{Proof: proof, Output: out}, nil
}

func registerCrypto(r *dispatch.Registry) {
	dispatch.Register(r, scriptHash)
	dispatch.Register(r, bootstrapWitness)
	dispatch.Register(r, vrfProve)
	dispatch.Register(r, vkeyWitness)
	dispatch.Register(r, vrfVerify)
	dispatch.Register(r, kesVerify)
	dispatch.Register(r, addressDecode)
}
