package main

import (
	"bytes"
	"context"
	"crypto/ed25519"
	"testing"

	"github.com/blinklabs-io/gouroboros/kes"
	"github.com/blinklabs-io/gouroboros/vrf"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func flip(b []byte) []byte {
	c := bytes.Clone(b)
	c[0] ^= 1
	return c
}

func TestVkeyWitness(t *testing.T) {
	sk := ed25519.NewKeyFromSeed(bytes.Repeat([]byte{7}, 32))
	vk := sk.Public().(ed25519.PublicKey)
	body := bytes.Repeat([]byte{0xbb}, 32)
	sig := ed25519.Sign(sk, body)
	h, _ := blake2b.New(28, nil)
	h.Write(vk)
	wantHash := h.Sum(nil)

	ok, err := vkeyWitness(context.Background(), &v1.CryptoVkeyWitnessRequest{BodyHash: body, Vkey: vk, Signature: sig})
	if err != nil {
		t.Fatal(err)
	}
	if !ok.GetValid() || !bytes.Equal(ok.GetKeyHash(), wantHash) {
		t.Fatalf("valid witness: %v, want valid and key hash %x", ok, wantHash)
	}
	bad, err := vkeyWitness(context.Background(), &v1.CryptoVkeyWitnessRequest{BodyHash: body, Vkey: vk, Signature: flip(sig)})
	if err != nil {
		t.Fatal(err)
	}
	if bad.GetValid() {
		t.Fatal("tampered signature verified")
	}
	short, err := vkeyWitness(context.Background(), &v1.CryptoVkeyWitnessRequest{BodyHash: body, Vkey: vk[:31], Signature: sig})
	if err != nil {
		t.Fatal(err)
	}
	if short.GetValid() || len(short.GetKeyHash()) != 0 {
		t.Fatalf("short key: %v, want invalid with no key hash", short)
	}
}

func TestVrfVerify(t *testing.T) {
	vk, sk, err := vrf.KeyGen(bytes.Repeat([]byte{3}, 32))
	if err != nil {
		t.Fatal(err)
	}
	alpha := []byte("dxdingo alpha")
	proof, out, err := vrf.Prove(sk, alpha)
	if err != nil {
		t.Fatal(err)
	}
	got, err := vrfVerify(context.Background(), &v1.CryptoVrfVerifyRequest{Vkey: vk, Proof: proof, Alpha: alpha})
	if err != nil {
		t.Fatal(err)
	}
	if !got.GetValid() || !bytes.Equal(got.GetOutput(), out) {
		t.Fatalf("valid proof: %v, want output %x", got, out)
	}
	bad, err := vrfVerify(context.Background(), &v1.CryptoVrfVerifyRequest{Vkey: vk, Proof: proof, Alpha: []byte("other")})
	if err != nil {
		t.Fatal(err)
	}
	if bad.GetValid() || len(bad.GetOutput()) != 0 {
		t.Fatalf("wrong alpha: %v, want invalid with no output", bad)
	}
}

func TestKesVerify(t *testing.T) {
	sk, vk, err := kes.KeyGen(6, bytes.Repeat([]byte{9}, 32))
	if err != nil {
		t.Fatal(err)
	}
	msg := []byte("dxdingo header body")
	sig, err := kes.Sign(sk, 0, msg)
	if err != nil {
		t.Fatal(err)
	}
	for name, tc := range map[string]struct {
		req  *v1.CryptoKesVerifyRequest
		want bool
	}{
		"valid":        {&v1.CryptoKesVerifyRequest{Depth: 6, Vkey: vk, Period: 0, Message: msg, Signature: sig}, true},
		"wrong period": {&v1.CryptoKesVerifyRequest{Depth: 6, Vkey: vk, Period: 1, Message: msg, Signature: sig}, false},
		"wrong msg":    {&v1.CryptoKesVerifyRequest{Depth: 6, Vkey: vk, Period: 0, Message: []byte("x"), Signature: sig}, false},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := kesVerify(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetValid() != tc.want {
				t.Fatalf("valid = %v, want %v", got.GetValid(), tc.want)
			}
		})
	}
	if _, err := kesVerify(context.Background(), &v1.CryptoKesVerifyRequest{Depth: 5}); err == nil {
		t.Fatal("depth 5 accepted; only Sum6KES is modelled")
	}
}

func TestAddressDecode(t *testing.T) {
	pay := bytes.Repeat([]byte{0x11}, 28)
	stake := bytes.Repeat([]byte{0x22}, 28)
	keyCred := func(h []byte) *v1.Credential {
		return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: h}
	}
	scriptCred := func(h []byte) *v1.Credential {
		return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: h}
	}
	for name, tc := range map[string]struct {
		raw     []byte
		kind    v1.AddressKind
		network uint32
		payment *v1.Credential
		stake   *v1.Credential
		pointer *v1.Pointer
	}{
		"base key/key mainnet":    {append(append([]byte{0x01}, pay...), stake...), v1.AddressKind_ADDRESS_KIND_BASE, 1, keyCred(pay), keyCred(stake), nil},
		"base script/key testnet": {append(append([]byte{0x10}, pay...), stake...), v1.AddressKind_ADDRESS_KIND_BASE, 0, scriptCred(pay), keyCred(stake), nil},
		"enterprise":              {append([]byte{0x61}, pay...), v1.AddressKind_ADDRESS_KIND_ENTERPRISE, 1, keyCred(pay), nil, nil},
		"reward":                  {append([]byte{0xe1}, stake...), v1.AddressKind_ADDRESS_KIND_REWARD, 1, nil, keyCred(stake), nil},
		"pointer":                 {append(append([]byte{0x41}, pay...), 0x81, 0x00, 0x02, 0x03), v1.AddressKind_ADDRESS_KIND_POINTER, 1, keyCred(pay), nil, &v1.Pointer{Slot: 128, TxIndex: 2, CertIndex: 3}},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := addressDecode(context.Background(), &v1.CryptoAddressDecodeRequest{Input: &v1.CryptoAddressDecodeRequest_Raw{Raw: tc.raw}})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil {
				t.Fatalf("rejected: %v", got.GetRejection())
			}
			if got.GetKind() != tc.kind || got.GetNetworkId() != tc.network {
				t.Fatalf("kind/network = %v/%d, want %v/%d", got.GetKind(), got.GetNetworkId(), tc.kind, tc.network)
			}
			if !credEqual(got.GetPayment(), tc.payment) || !credEqual(got.GetStake(), tc.stake) {
				t.Fatalf("payment/stake = %v/%v, want %v/%v", got.GetPayment(), got.GetStake(), tc.payment, tc.stake)
			}
			if tc.pointer != nil && (got.GetPointer().GetSlot() != tc.pointer.Slot ||
				got.GetPointer().GetTxIndex() != tc.pointer.TxIndex || got.GetPointer().GetCertIndex() != tc.pointer.CertIndex) {
				t.Fatalf("pointer = %v, want %v", got.GetPointer(), tc.pointer)
			}
			if !bytes.Equal(got.GetRaw(), tc.raw) {
				t.Fatalf("re-encoding = %x, want %x", got.GetRaw(), tc.raw)
			}
		})
	}
	got, err := addressDecode(context.Background(), &v1.CryptoAddressDecodeRequest{Input: &v1.CryptoAddressDecodeRequest_Raw{Raw: []byte{0x01, 0x02}}})
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() == nil {
		t.Fatalf("truncated address accepted: %v", got)
	}
}

func credEqual(a, b *v1.Credential) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	return a.GetKind() == b.GetKind() && bytes.Equal(a.GetHash(), b.GetHash())
}
