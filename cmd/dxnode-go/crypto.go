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

func registerCrypto(r *dispatch.Registry) {
	dispatch.Register(r, vkeyWitness)
	dispatch.Register(r, vrfVerify)
	dispatch.Register(r, kesVerify)
	dispatch.Register(r, addressDecode)
}
