package main

import (
	"bytes"
	"context"
	"testing"

	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// checkLeaderNatValue: leader iff p < 1 - (1 - f)^sigma, p = value / 2^(8n).
func TestLeaderCheck(t *testing.T) {
	zero32 := make([]byte, 32)
	max32 := bytes.Repeat([]byte{0xff}, 32)
	for name, tc := range map[string]struct {
		req  *v1.PraosLeaderCheckRequest
		want bool
	}{
		"praos lowest value leads": {&v1.PraosLeaderCheckRequest{
			Era: v1.Era_ERA_BABBAGE, LeaderValue: zero32, PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
		}, true},
		"praos highest value does not": {&v1.PraosLeaderCheckRequest{
			Era: v1.Era_ERA_BABBAGE, LeaderValue: max32, PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
		}, false},
		"tpraos lowest value leads": {&v1.PraosLeaderCheckRequest{
			Era: v1.Era_ERA_SHELLEY, LeaderValue: make([]byte, 64), PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
		}, true},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := leaderCheck(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetIsLeader() != tc.want {
				t.Fatalf("is_leader = %v, want %v", got.GetIsLeader(), tc.want)
			}
		})
	}
	_, err := leaderCheck(context.Background(), &v1.PraosLeaderCheckRequest{
		Era: v1.Era_ERA_BABBAGE, LeaderValue: make([]byte, 64), PoolStake: 1, TotalStake: 2, ActiveSlotCoeff: rat("1", "20"),
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("64-byte praos value: err = %v, want UNSUPPORTED", err)
	}
}

// (candidate ⭒ lab) ⭒ extra; NeutralNonce is the identity, and
// Nonce a ⭒ Nonce b = Nonce (blake2b_256 (a <> b)).
func TestNonceEpoch(t *testing.T) {
	a := bytes.Repeat([]byte{0xaa}, 32)
	b := bytes.Repeat([]byte{0xbb}, 32)
	ab := blake2b.Sum256(append(bytes.Clone(a), b...))
	// Divergence: dingo stores the neutral candidate as 32 zero bytes and,
	// with a lab present, hashes it as a real nonce. The reference returns
	// the lab unchanged (NeutralNonce ⭒ x = x). corpus/divergences holds the
	// replayable case.
	zb := blake2b.Sum256(append(make([]byte, 32), b...))
	for name, tc := range map[string]struct {
		candidate, lab, extra, want []byte
	}{
		"candidate and lab":   {a, b, nil, ab[:]},
		"candidate only":      {a, nil, nil, a},
		"neutral candidate":   {nil, b, nil, zb[:]},
		"extra entropy mixes": {a, nil, b, ab[:]},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := nonceEpoch(context.Background(), &v1.PraosNonceEpochRequest{
				Era: v1.Era_ERA_SHELLEY, Candidate: &v1.Nonce{Hash: tc.candidate},
				PreviousLab: &v1.Nonce{Hash: tc.lab}, ExtraEntropy: &v1.Nonce{Hash: tc.extra},
			})
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(got.GetEpochNonce().GetHash(), tc.want) {
				t.Fatalf("epoch nonce = %x, want %x", got.GetEpochNonce().GetHash(), tc.want)
			}
		})
	}
}

// classifyOverlaySlot with d = 1/2, f = 1/20: relative slot 0 is active
// (genesis key 0), 1 is a Praos slot, 2 is an overlay slot nobody produces.
func TestOverlaySchedule(t *testing.T) {
	var keys []*v1.GenesisDelegation
	for i := byte(1); i <= 7; i++ {
		keys = append(keys, &v1.GenesisDelegation{GenesisKeyHash: bytes.Repeat([]byte{i}, 28)})
	}
	for name, tc := range map[string]struct {
		slot uint64
		kind v1.OverlaySlotKind
		key  []byte
	}{
		"active":     {1000, v1.OverlaySlotKind_OVERLAY_SLOT_KIND_ACTIVE, keys[0].GenesisKeyHash},
		"praos":      {1001, v1.OverlaySlotKind_OVERLAY_SLOT_KIND_PRAOS, nil},
		"non-active": {1002, v1.OverlaySlotKind_OVERLAY_SLOT_KIND_NON_ACTIVE, nil},
	} {
		t.Run(name, func(t *testing.T) {
			got, err := overlaySchedule(context.Background(), &v1.TpraosOverlayScheduleRequest{
				Slot: tc.slot, EpochFirstSlot: 1000, D: rat("1", "2"), ActiveSlotCoeff: rat("1", "20"),
				GenesisDelegations: keys,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetKind() != tc.kind || !bytes.Equal(got.GetGenesisKeyHash(), tc.key) {
				t.Fatalf("got %v, want %v %x", got, tc.kind, tc.key)
			}
		})
	}
	_, err := overlaySchedule(context.Background(), &v1.TpraosOverlayScheduleRequest{
		Slot: 999, EpochFirstSlot: 1000, D: rat("1", "2"), ActiveSlotCoeff: rat("1", "20"), GenesisDelegations: keys,
	})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("slot before epoch start: err = %v, want UNSUPPORTED", err)
	}
}
