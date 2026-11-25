package corpusgen

import (
	"bytes"
	"crypto/ed25519"
	"testing"

	"github.com/blinklabs-io/gouroboros/kes"
	"github.com/blinklabs-io/gouroboros/vrf"
	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateCryptoCorpus(t *testing.T) { writeCorpus(t, "", cryptoCases(t)) }

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
