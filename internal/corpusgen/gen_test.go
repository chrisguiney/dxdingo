// Package corpusgen writes the seed corpus. The crypto cases need real keys,
// proofs and signatures, so they are generated here from fixed seeds rather
// than written by hand. Regenerate with:
//
//	DXDINGO_GEN_CORPUS=1 go test ./internal/corpusgen/
package corpusgen

import (
	"bytes"
	"crypto/ed25519"
	"math/big"
	"os"
	"path/filepath"
	"testing"

	"github.com/blinklabs-io/gouroboros/kes"
	"github.com/blinklabs-io/gouroboros/vrf"
	"golang.org/x/crypto/blake2b"
	"google.golang.org/protobuf/encoding/prototext"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func rat(n, d string) *v1.Rational { return &v1.Rational{Numerator: n, Denominator: d} }

func rep(b byte, n int) []byte { return bytes.Repeat([]byte{b}, n) }

func flip(b []byte, i int) []byte {
	c := bytes.Clone(b)
	c[i] ^= 1
	return c
}

// fraction returns floor(num/den * 2^(8*size)) as size big-endian bytes.
func fraction(num, den int64, size int) []byte {
	v := new(big.Int).Lsh(big.NewInt(num), uint(8*size))
	v.Quo(v, big.NewInt(den))
	return v.FillBytes(make([]byte, size))
}

type entry struct {
	name, comment string
	call          *v1.Call
}

func TestGenerateCorpus(t *testing.T) {
	if os.Getenv("DXDINGO_GEN_CORPUS") == "" {
		t.Skip("set DXDINGO_GEN_CORPUS=1 to regenerate corpus/")
	}
	root := filepath.Join("..", "..", "corpus")
	write := func(dir string, entries []entry) {
		for _, e := range entries {
			b, err := prototext.MarshalOptions{Multiline: true}.Marshal(e.call)
			if err != nil {
				t.Fatal(err)
			}
			out := append([]byte("# "+e.comment+"\n"), b...)
			if err := os.WriteFile(filepath.Join(dir, e.name+".txtpb"), out, 0o644); err != nil {
				t.Fatal(err)
			}
		}
	}
	write(root, rewardsCases())
	write(root, cryptoCases(t))
	write(root, praosCases())
	if err := os.MkdirAll(filepath.Join(root, "divergences"), 0o755); err != nil {
		t.Fatal(err)
	}
	write(filepath.Join(root, "divergences"), divergenceCases())
}

func rewardsCases() []entry {
	leader := func(name, why string, f, c uint64, m *v1.Rational, owner, pool, total uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsLeaderReward{RewardsLeaderReward: &v1.RewardsLeaderRewardRequest{
			PoolReward: f, Cost: c, Margin: m, OwnerStake: owner, PoolStake: pool, TotalStake: total,
		}}}}
	}
	maxp := func(name, why string, a0 *v1.Rational, k uint32, r, total, pool, pledge uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsMaxPool{RewardsMaxPool: &v1.RewardsMaxPoolRequest{
			Pparams: &v1.ProtocolParameters{A0: a0, NOpt: &k}, PoolPot: r, TotalStake: total, PoolStake: pool, Pledge: pledge,
		}}}}
	}
	perf := func(name, why string, d *v1.Rational, pool, total, n, nbar uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_RewardsApparentPerformance{RewardsApparentPerformance: &v1.RewardsApparentPerformanceRequest{
			D: d, PoolStake: pool, TotalActiveStake: total, Blocks: n, BlocksTotal: nbar,
		}}}}
	}
	return []entry{
		leader("rewards_leader_reward_owner_fifth", "340 + floor(660 * (0.01 + 0.99 * 0.2)) ADA", 1_000_000_000, 340_000_000, rat("1", "100"), 10, 50, 1000),
		leader("rewards_leader_reward_below_cost", "f <= cost: the operator takes f", 300_000_000, 340_000_000, rat("0", "1"), 10, 50, 1000),
		leader("rewards_leader_reward_odd_split", "unreduced margin, uneven owner share", 987_654_321, 1234, rat("50", "200"), 3, 13, 1_000_000),
		maxp("rewards_max_pool_basic", "a0 3/10, k 500, sigma 1/1000, pledge 1/2000: 812500", rat("3", "10"), 500, 1_000_000_000, 1_000_000, 1000, 500),
		maxp("rewards_max_pool_saturated", "sigma and pledge above z0 are capped at 1/k", rat("3", "10"), 500, 1_000_000_000, 1_000_000, 10_000, 9_000),
		maxp("rewards_max_pool_mainnet_like", "mainnet-sized pot and supply", rat("3", "10"), 500, 7_000_000_000_000, 34_000_000_000_000_000, 70_000_000_000_000, 1_000_000_000_000),
		perf("rewards_apparent_performance_decentralized", "beta / sigma = (3/10) / (1/4)", rat("0", "1"), 1, 4, 3, 10),
		perf("rewards_apparent_performance_federated", "d >= 0.8: performance is 1", rat("9", "10"), 1, 4, 3, 10),
		perf("rewards_apparent_performance_no_blocks_total", "beta divides by max 1 blocks_total", rat("1", "2"), 7, 100, 0, 0),
	}
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

func praosCases() []entry {
	leader := func(name, why string, era v1.Era, v []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PraosLeaderCheck{PraosLeaderCheck: &v1.PraosLeaderCheckRequest{
			Era: era, LeaderValue: v, PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
		}}}}
	}
	nonce := func(name, why string, c, l, e []byte) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_PraosNonceEpoch{PraosNonceEpoch: &v1.PraosNonceEpochRequest{
			Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{Hash: c}, PreviousLab: &v1.Nonce{Hash: l}, ExtraEntropy: &v1.Nonce{Hash: e},
		}}}}
	}
	var keys []*v1.GenesisDelegation
	for i := byte(1); i <= 7; i++ {
		keys = append(keys, &v1.GenesisDelegation{GenesisKeyHash: rep(i, 28)})
	}
	overlay := func(name, why string, slot uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TpraosOverlaySchedule{TpraosOverlaySchedule: &v1.TpraosOverlayScheduleRequest{
			Slot: slot, EpochFirstSlot: 1000, D: rat("1", "2"), ActiveSlotCoeff: rat("1", "20"), GenesisDelegations: keys,
		}}}}
	}
	a, b, c := rep(0xaa, 32), rep(0xbb, 32), rep(0xcc, 32)
	// Leader threshold for sigma 1/2, f 1/20: 1 - 0.95^0.5 = 0.02532.
	return []entry{
		leader("praos_leader_check_praos_min", "Praos: value 0 always leads", v1.Era_ERA_BABBAGE, make([]byte, 32)),
		leader("praos_leader_check_praos_max", "Praos: the largest value never leads", v1.Era_ERA_BABBAGE, rep(0xff, 32)),
		leader("praos_leader_check_praos_below_threshold", "Praos: p = 0.0250 < 0.02532", v1.Era_ERA_CONWAY, fraction(250, 10000, 32)),
		leader("praos_leader_check_praos_above_threshold", "Praos: p = 0.0255 > 0.02532", v1.Era_ERA_CONWAY, fraction(255, 10000, 32)),
		leader("praos_leader_check_tpraos_below_threshold", "TPraos: 64-byte value, p = 0.0250", v1.Era_ERA_SHELLEY, fraction(250, 10000, 64)),
		leader("praos_leader_check_tpraos_above_threshold", "TPraos: 64-byte value, p = 0.0255", v1.Era_ERA_ALONZO, fraction(255, 10000, 64)),
		nonce("praos_nonce_epoch_candidate_lab", "candidate ⭒ lab", a, b, nil),
		nonce("praos_nonce_epoch_candidate_only", "neutral lab and extra entropy: the candidate", a, nil, nil),
		nonce("praos_nonce_epoch_extra_entropy", "candidate ⭒ extra entropy", a, nil, c),
		nonce("praos_nonce_epoch_all", "(candidate ⭒ lab) ⭒ extra entropy", a, b, c),
		overlay("tpraos_overlay_schedule_active_first", "relative slot 0: active, genesis key 0", 1000),
		overlay("tpraos_overlay_schedule_praos", "relative slot 1: not an overlay slot", 1001),
		overlay("tpraos_overlay_schedule_non_active", "relative slot 2: overlay slot nobody produces", 1002),
		overlay("tpraos_overlay_schedule_active_second_key", "relative slot 40: active, genesis key 1", 1040),
	}
}

// divergenceCases are known mismatches, kept out of corpus/ so the default
// replay stays green; each has a direct confirmation test in dxnode-go.
func divergenceCases() []entry {
	return []entry{{
		"praos_nonce_epoch_neutral_candidate",
		"reference: NeutralNonce ⭒ lab = lab; dingo hashes its 32-zero neutral candidate (TestDivergenceNeutralCandidateIsHashed)",
		&v1.Call{Request: &v1.Call_PraosNonceEpoch{PraosNonceEpoch: &v1.PraosNonceEpochRequest{
			Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{}, PreviousLab: &v1.Nonce{Hash: rep(0xbb, 32)}, ExtraEntropy: &v1.Nonce{},
		}}},
	}}
}
