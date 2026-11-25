package main

import (
	"bytes"
	"context"
	"crypto/ed25519"
	"encoding/hex"
	"testing"

	"github.com/blinklabs-io/gouroboros/kes"
	"github.com/blinklabs-io/gouroboros/vrf"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/sha3"
	"google.golang.org/protobuf/proto"

	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
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

func hexb(t *testing.T, s string) []byte {
	t.Helper()
	b, err := hex.DecodeString(s)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func blake224(parts ...[]byte) []byte {
	h, _ := blake2b.New(28, nil)
	for _, p := range parts {
		h.Write(p)
	}
	return h.Sum(nil)
}

// The preimage is the language tag byte followed by the script bytes, hashed
// with Blake2b-224 (hashScript, scriptPrefixTag).
func TestScriptHash(t *testing.T) {
	t.Parallel()
	keyHash := bytes.Repeat([]byte{0x11}, 28)
	native := append([]byte{0x82, 0x00, 0x58, 0x1c}, keyHash...) // [0, h'11..']
	plutus := []byte{0x01, 0x02, 0x03}
	for name, tc := range map[string]struct {
		lang   v1.ScriptLanguage
		script []byte
		want   []byte
	}{
		"native": {v1.ScriptLanguage_SCRIPT_LANGUAGE_NATIVE, native, blake224([]byte{0}, native)},
		"v1":     {v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V1, plutus, blake224([]byte{1}, plutus)},
		"v2":     {v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V2, plutus, blake224([]byte{2}, plutus)},
		"v3":     {v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V3, plutus, blake224([]byte{3}, plutus)},
		"v4":     {v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V4, plutus, blake224([]byte{4}, plutus)},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := scriptHash(context.Background(), &v1.CryptoScriptHashRequest{Language: tc.lang, Script: tc.script})
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(got.GetHash(), tc.want) {
				t.Fatalf("hash = %x, want %x", got.GetHash(), tc.want)
			}
		})
	}
	bad, err := scriptHash(context.Background(), &v1.CryptoScriptHashRequest{Language: v1.ScriptLanguage_SCRIPT_LANGUAGE_NATIVE, Script: []byte{0xff}})
	if err != nil {
		t.Fatal(err)
	}
	if bad.GetRejection().GetFailures()[0].GetName() != "DeserialiseFailure" || len(bad.GetHash()) != 0 {
		t.Fatalf("malformed native script: %v, want DeserialiseFailure", bad)
	}
	if _, err := scriptHash(context.Background(), &v1.CryptoScriptHashRequest{}); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("unspecified language: %v, want BAD_REQUEST", err)
	}
}

// bootstrapAddress builds a Byron address whose root is the witness key hash,
// hand-derived from bootstrapWitKeyHash: Blake2b-224(SHA3-256(prefix || vkey ||
// chain code || attributes)) with the constant prefix 83 00 82 00 58 40.
func bootstrapFixture(t *testing.T) (req *v1.CryptoBootstrapWitnessRequest, root []byte) {
	t.Helper()
	sk := ed25519.NewKeyFromSeed(bytes.Repeat([]byte{5}, 32))
	vk := []byte(sk.Public().(ed25519.PublicKey))
	cc := bytes.Repeat([]byte{0x33}, 32)
	attrs := []byte{0xa0}
	body := bytes.Repeat([]byte{0xcc}, 32)
	pre := sha3.Sum256(bytes.Join([][]byte{{0x83, 0x00, 0x82, 0x00, 0x58, 0x40}, vk, cc, attrs}, nil))
	root = blake224(pre[:])
	addr, err := lcommon.NewByronAddressFromParts(lcommon.ByronAddressTypePubkey, root, lcommon.ByronAddressAttributes{})
	if err != nil {
		t.Fatal(err)
	}
	raw, err := addr.Bytes()
	if err != nil {
		t.Fatal(err)
	}
	return &v1.CryptoBootstrapWitnessRequest{
		BodyHash: body, Vkey: vk, ChainCode: cc, Attributes: attrs,
		Signature: ed25519.Sign(sk, body), Address: raw,
	}, root
}

func TestBootstrapWitness(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	req, _ := bootstrapFixture(t)
	got, err := bootstrapWitness(ctx, req)
	if err != nil {
		t.Fatal(err)
	}
	if !got.GetValid() || !got.GetMatchesAddress() {
		t.Fatalf("valid witness: %v, want valid and matching", got)
	}
	for name, mut := range map[string]func(r *v1.CryptoBootstrapWitnessRequest){
		"tampered signature": func(r *v1.CryptoBootstrapWitnessRequest) { r.Signature = flip(r.Signature) },
		"wrong body":         func(r *v1.CryptoBootstrapWitnessRequest) { r.BodyHash = flip(r.BodyHash) },
	} {
		r := proto.Clone(req).(*v1.CryptoBootstrapWitnessRequest)
		mut(r)
		got, err := bootstrapWitness(ctx, r)
		if err != nil {
			t.Fatal(err)
		}
		if got.GetValid() || !got.GetMatchesAddress() {
			t.Fatalf("%s: %v, want invalid but still matching the address", name, got)
		}
	}
	for name, mut := range map[string]func(r *v1.CryptoBootstrapWitnessRequest){
		"other chain code": func(r *v1.CryptoBootstrapWitnessRequest) { r.ChainCode = flip(r.ChainCode) },
		"other attributes": func(r *v1.CryptoBootstrapWitnessRequest) { r.Attributes = []byte{0xa1, 0x01, 0x40} },
		"no address":       func(r *v1.CryptoBootstrapWitnessRequest) { r.Address = nil },
	} {
		r := proto.Clone(req).(*v1.CryptoBootstrapWitnessRequest)
		mut(r)
		got, err := bootstrapWitness(ctx, r)
		if err != nil {
			t.Fatal(err)
		}
		if !got.GetValid() || got.GetMatchesAddress() {
			t.Fatalf("%s: %v, want valid but not matching", name, got)
		}
	}
	short := proto.Clone(req).(*v1.CryptoBootstrapWitnessRequest)
	short.BodyHash = short.BodyHash[:31]
	if _, err := bootstrapWitness(ctx, short); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("short body hash: %v, want UNSUPPORTED", err)
	}
	r := proto.Clone(req).(*v1.CryptoBootstrapWitnessRequest)
	r.Address = []byte{0x01, 0x02}
	got, err = bootstrapWitness(ctx, r)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection().GetFailures()[0].GetName() != "DeserialiseFailure" {
		t.Fatalf("bad address: %v, want DeserialiseFailure", got)
	}
}

// Vectors vrf_ver03_standard_10 and vrf_ver03_generated_1 from cardano-base
// cardano-crypto-praos/test_vectors.
func TestVrfProve(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct{ sk, alpha, pi, beta string }{
		"standard_10": {
			"9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60", "",
			"b6b4699f87d56126c9117a7da55bd0085246f4c56dbc95d20172612e9d38e8d7ca65e573a126ed88d4e30a46f80a666854d675cf3ba81de0de043c3774f061560f55edc256a787afe701677c0f602900",
			"5b49b554d05c0cd5a5325376b3387de59d924fd1e13ded44648ab33c21349a603f25b84ec5ed887995b33da5e3bfcb87cd2f64521c4c62cf825cffabbe5d31cc",
		},
		"generated_1": {
			"0000000000000000000000000000000000000000000000000000000000000000", "00",
			"000f006e64c91f84212919fe0899970cd341206fc081fe599339c8492e2cea3299ae9de4b6ce21cda0a975f65f45b70f82b3952ba6d0dbe11a06716e67aca233c0d78f115a655aa1952ada9f3d692a0a",
			"9930b5dddc0938f01cf6f9746eded569ee676bd6ff3b4f19233d74b903ec53a45c5728116088b7c622b6d6c354f7125c7d09870b56ec6f1e4bf4970f607e04b2",
		},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := vrfProve(context.Background(), &v1.CryptoVrfProveRequest{Skey: hexb(t, tc.sk), Alpha: hexb(t, tc.alpha)})
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(got.GetProof(), hexb(t, tc.pi)) || !bytes.Equal(got.GetOutput(), hexb(t, tc.beta)) {
				t.Fatalf("proof %x output %x, want %s / %s", got.GetProof(), got.GetOutput(), tc.pi, tc.beta)
			}
		})
	}
	if _, err := vrfProve(context.Background(), &v1.CryptoVrfProveRequest{Skey: []byte{1, 2, 3}}); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("short seed: %v, want UNSUPPORTED", err)
	}
}
