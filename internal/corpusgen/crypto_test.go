package corpusgen

import (
	"bytes"
	"crypto/ed25519"
	"encoding/hex"
	"testing"

	"github.com/blinklabs-io/gouroboros/kes"
	"github.com/blinklabs-io/gouroboros/vrf"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/byronfixture"
)

func TestGenerateCryptoCorpus(t *testing.T) {
	writeCorpus(t, "", append(cryptoCases(t), cryptoWitnessCases(t)...))
	writeCorpus(t, "divergences", cryptoDivergences())
}

// cryptoDivergences are confirmed mismatches; see
// TestDivergenceNativeScriptTrailingByte and the "divergence:" check in
// Test/Crypto.hs.
func cryptoDivergences() []entry {
	script := append([]byte{0x82, 0x00, 0x58, 0x1c}, rep(1, 28)...)
	return []entry{{
		"crypto_script_hash_native_trailing",
		"valid native script plus one trailing byte: the reference rejects (full decode), gouroboros hashes it",
		&v1.Call{Request: &v1.Call_CryptoScriptHash{CryptoScriptHash: &v1.CryptoScriptHashRequest{
			Language: v1.ScriptLanguage_SCRIPT_LANGUAGE_NATIVE, Script: append(script, 0x00),
		}}},
	}}
}

func cryptoCases(t *testing.T) []entry {
	t.Helper()
	sk := ed25519.NewKeyFromSeed(rep(7, 32))
	vk := []byte(sk.Public().(ed25519.PublicKey))
	body := rep(0xbb, 32)
	sig := ed25519.Sign(sk, body)
	vkey := func(name, why string, b, k, s []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoVkeyWitness{CryptoVkeyWitness: &v1.CryptoVkeyWitnessRequest{
			BodyHash: b, Vkey: k, Signature: s,
		}}}}
	}

	vrfVK, vrfSK, err := vrf.KeyGen(rep(3, 32))
	if err != nil {
		t.Fatal(err)
	}
	alpha := []byte("dxdingo alpha")
	proof, _, err := vrf.Prove(vrfSK, alpha)
	if err != nil {
		t.Fatal(err)
	}
	vrfCase := func(name, why string, k, p, a []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoVrfVerify{CryptoVrfVerify: &v1.CryptoVrfVerifyRequest{
			Vkey: k, Proof: p, Alpha: a,
		}}}}
	}

	kesSK, kesVK, err := kes.KeyGen(6, rep(9, 32))
	if err != nil {
		t.Fatal(err)
	}
	msg := []byte("dxdingo header body")
	sig0, err := kes.Sign(kesSK, 0, msg)
	if err != nil {
		t.Fatal(err)
	}
	evolved := kesSK
	for range 5 {
		if evolved, err = kes.Update(evolved); err != nil {
			t.Fatal(err)
		}
	}
	sig5, err := kes.Sign(evolved, 5, msg)
	if err != nil {
		t.Fatal(err)
	}
	kesCase := func(name, why string, period uint64, m, s []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoKesVerify{CryptoKesVerify: &v1.CryptoKesVerifyRequest{
			Depth: 6, Vkey: kesVK, Period: period, Message: m, Signature: s,
		}}}}
	}

	pay, stake := rep(0x11, 28), rep(0x22, 28)
	addr := func(name, why string, raw []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoAddressDecode{CryptoAddressDecode: &v1.CryptoAddressDecodeRequest{
			Input: &v1.CryptoAddressDecodeRequest_Raw{Raw: raw},
		}}}}
	}
	cat := func(parts ...[]byte) []byte { return bytes.Join(parts, nil) }

	h, _ := blake2b.New(28, nil)
	h.Write(vk)
	return []entry{
		vkey("crypto_vkey_witness_valid", "Ed25519 signature over the body hash", body, vk, sig),
		vkey("crypto_vkey_witness_tampered_sig", "one bit flipped in R", body, vk, flip(sig, 0)),
		vkey("crypto_vkey_witness_tampered_s", "one bit flipped in S", body, vk, flip(sig, 40)),
		vkey("crypto_vkey_witness_wrong_body", "signature over a different body", rep(0xcc, 32), vk, sig),
		vkey("crypto_vkey_witness_short_key", "31-byte key: no key hash, invalid", body, vk[:31], sig),
		vrfCase("crypto_vrf_verify_valid", "ECVRF draft-03 proof", vrfVK, proof, alpha),
		vrfCase("crypto_vrf_verify_wrong_alpha", "proof for another input", vrfVK, proof, []byte("other")),
		vrfCase("crypto_vrf_verify_tampered_proof", "one bit flipped in the proof scalar", vrfVK, flip(proof, 50), alpha),
		kesCase("crypto_kes_verify_period0", "Sum6KES signature at period 0", 0, msg, sig0),
		kesCase("crypto_kes_verify_period5", "Sum6KES signature after five evolutions", 5, msg, sig5),
		kesCase("crypto_kes_verify_wrong_period", "period-0 signature checked at period 1", 1, msg, sig0),
		kesCase("crypto_kes_verify_wrong_msg", "signature over another message", 0, []byte("x"), sig0),
		addr("crypto_address_decode_base", "base address, key/key, mainnet", cat([]byte{0x01}, pay, stake)),
		addr("crypto_address_decode_base_script", "base address, script/key, testnet", cat([]byte{0x10}, pay, stake)),
		addr("crypto_address_decode_enterprise", "enterprise address", cat([]byte{0x61}, pay)),
		addr("crypto_address_decode_reward", "reward (account) address", cat([]byte{0xe1}, stake)),
		addr("crypto_address_decode_pointer", "pointer address, slot 128 tx 2 cert 3", cat([]byte{0x41}, pay, []byte{0x81, 0x00, 0x02, 0x03})),
		addr("crypto_address_decode_truncated", "two bytes: both reject", []byte{0x01, 0x02}),
		addr("crypto_address_decode_trailing_byte", "enterprise address plus one trailing byte", cat([]byte{0x61}, pay, []byte{0x00})),
	}
}

func mustHex(t *testing.T, s string) []byte {
	t.Helper()
	b, err := hex.DecodeString(s)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// cryptoWitnessCases covers script hashing, Byron bootstrap witnesses and VRF
// proving.
func cryptoWitnessCases(t *testing.T) []entry {
	t.Helper()
	script := func(name, why string, lang v1.ScriptLanguage, b []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoScriptHash{CryptoScriptHash: &v1.CryptoScriptHashRequest{
			Language: lang, Script: b,
		}}}}
	}
	native, v1l, v2l, v3l, v4l := v1.ScriptLanguage_SCRIPT_LANGUAGE_NATIVE, v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V1,
		v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V2, v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V3, v1.ScriptLanguage_SCRIPT_LANGUAGE_PLUTUS_V4
	kh := func(b byte) []byte { return byronfixture.Array(byronfixture.Uint(0), byronfixture.Bytes(rep(b, 28))) }
	list := func(items ...[]byte) []byte { return byronfixture.Array(items...) }
	allOf := byronfixture.Array(byronfixture.Uint(1), list(kh(1), kh(2)))
	anyOf := byronfixture.Array(byronfixture.Uint(2), list(kh(1), byronfixture.Array(byronfixture.Uint(4), byronfixture.Uint(100))))
	nOfK := byronfixture.Array(byronfixture.Uint(3), byronfixture.Uint(2), list(kh(1), kh(2), kh(3)))
	timelock := byronfixture.Array(byronfixture.Uint(1), list(kh(7), byronfixture.Array(byronfixture.Uint(5), byronfixture.Uint(5_000_000))))
	plutus := []byte{0x59, 0x01, 0x00, 0x01, 0x00, 0x00, 0x33, 0x22, 0x22, 0x32, 0x32}

	bw := func(name, why string, mut func(r *v1.CryptoBootstrapWitnessRequest)) entry {
		k := byronfixture.NewKey(5)
		body := rep(0xcc, 32)
		r := &v1.CryptoBootstrapWitnessRequest{
			BodyHash: body, Vkey: k.XPub[:32], ChainCode: k.XPub[32:], Attributes: byronfixture.EmptyMap,
			Signature: k.Sign(body), Address: k.Address(nil),
		}
		if mut != nil {
			mut(r)
		}
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoBootstrapWitness{CryptoBootstrapWitness: r}}}
	}
	other := byronfixture.NewKey(6)
	testnet := uint32(1)
	pre := byronfixture.NewKey(5)

	prove := func(name, why string, sk, alpha []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_CryptoVrfProve{CryptoVrfProve: &v1.CryptoVrfProveRequest{Skey: sk, Alpha: alpha}}}}
	}
	return []entry{
		script("crypto_script_hash_native_sig", "native script requiring one key hash", native, kh(1)),
		script("crypto_script_hash_native_all", "native allOf two key hashes", native, allOf),
		script("crypto_script_hash_native_any_before", "native anyOf with an invalidBefore guard", native, anyOf),
		script("crypto_script_hash_native_nofk", "native 2-of-3", native, nOfK),
		script("crypto_script_hash_native_timelock", "native all of a key and invalidHereafter", native, timelock),
		script("crypto_script_hash_native_empty", "native script bytes empty: both reject", native, nil),
		script("crypto_script_hash_native_garbage", "native script bytes not CBOR: both reject", native, []byte{0xff, 0x00}),
		script("crypto_script_hash_plutus_v1", "PlutusV1 binary, tag 1", v1l, plutus),
		script("crypto_script_hash_plutus_v2", "PlutusV2 binary, tag 2", v2l, plutus),
		script("crypto_script_hash_plutus_v3", "PlutusV3 binary, tag 3", v3l, plutus),
		script("crypto_script_hash_plutus_v4", "PlutusV4 binary, tag 4", v4l, plutus),
		script("crypto_script_hash_plutus_empty", "empty Plutus binary: the preimage is the tag alone", v2l, nil),
		script("crypto_script_hash_plutus_large", "20 KiB Plutus binary", v3l, rep(0xab, 20<<10)),

		bw("crypto_bootstrap_witness_valid", "mainnet Byron address, valid witness", nil),
		bw("crypto_bootstrap_witness_tampered_sig", "signature bit flipped, address still matches", func(r *v1.CryptoBootstrapWitnessRequest) { r.Signature = flip(r.Signature, 3) }),
		bw("crypto_bootstrap_witness_wrong_body", "signature over another body", func(r *v1.CryptoBootstrapWitnessRequest) { r.BodyHash = rep(0xdd, 32) }),
		bw("crypto_bootstrap_witness_other_chain_code", "chain code changes the address root", func(r *v1.CryptoBootstrapWitnessRequest) { r.ChainCode = flip(r.ChainCode, 0) }),
		bw("crypto_bootstrap_witness_other_attributes", "attributes change the address root", func(r *v1.CryptoBootstrapWitnessRequest) {
			r.Attributes = byronfixture.NetworkAttrs(&testnet)
		}),
		bw("crypto_bootstrap_witness_testnet_address", "testnet address built from the same key and attributes", func(r *v1.CryptoBootstrapWitnessRequest) {
			r.Attributes = byronfixture.NetworkAttrs(&testnet)
			r.Address = pre.Address(&testnet)
		}),
		bw("crypto_bootstrap_witness_other_key_address", "address of another key", func(r *v1.CryptoBootstrapWitnessRequest) { r.Address = other.Address(nil) }),
		bw("crypto_bootstrap_witness_no_address", "no address: valid, no match", func(r *v1.CryptoBootstrapWitnessRequest) { r.Address = nil }),
		bw("crypto_bootstrap_witness_shelley_address", "enterprise address is not a Byron address: no match", func(r *v1.CryptoBootstrapWitnessRequest) {
			r.Address = append([]byte{0x61}, rep(0x11, 28)...)
		}),
		bw("crypto_bootstrap_witness_bad_address", "address that does not decode: both reject", func(r *v1.CryptoBootstrapWitnessRequest) { r.Address = []byte{0x01, 0x02} }),
		bw("crypto_bootstrap_witness_short_key", "31-byte key cannot be a witness: invalid", func(r *v1.CryptoBootstrapWitnessRequest) { r.Vkey = r.Vkey[:31] }),
		bw("crypto_bootstrap_witness_short_chain_code", "short chain code is hashed as given", func(r *v1.CryptoBootstrapWitnessRequest) { r.ChainCode = r.ChainCode[:16] }),
		bw("crypto_bootstrap_witness_short_body", "31-byte body hash: UNSUPPORTED on both", func(r *v1.CryptoBootstrapWitnessRequest) { r.BodyHash = r.BodyHash[:31] }),

		prove("crypto_vrf_prove_standard_10", "cardano-base vrf_ver03_standard_10: empty alpha", mustHex(t, "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"), nil),
		prove("crypto_vrf_prove_generated_1", "cardano-base vrf_ver03_generated_1: zero seed, alpha 00", rep(0, 32), []byte{0}),
		prove("crypto_vrf_prove_ff_seed", "all-ones seed", rep(0xff, 32), []byte("dxdingo alpha")),
		prove("crypto_vrf_prove_long_alpha", "4 KiB alpha", rep(7, 32), rep(0x5a, 4096)),
		prove("crypto_vrf_prove_short_seed", "16-byte seed: UNSUPPORTED on both", rep(1, 16), []byte("x")),
	}
}
