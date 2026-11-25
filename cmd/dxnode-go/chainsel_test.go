package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func vrfOut(b byte) []byte { return bytes.Repeat([]byte{b}, 64) }

func tip(blockNo, slot uint64, issuer byte, issueNo uint64, vrfByte byte) *v1.ChainTip {
	return &v1.ChainTip{
		BlockNo: blockNo, Slot: slot, IssuerVkey: bytes.Repeat([]byte{issuer}, 32),
		IssueNo: issueNo, TiebreakVrf: vrfOut(vrfByte),
	}
}

func win(n uint64) *uint64 { return &n }

// comparePraos / preferCandidate: block number first, then the opcert issue
// number for the same issuer in the same slot, then the lower VRF output;
// the restricted flavour arms the VRF only within the slot window.
func TestChainselCompare(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		window      *uint64
		a, b        *v1.ChainTip
		want        v1.Ordering
		wantPreferB bool
	}{
		"higher block number wins":                       {nil, tip(10, 5, 1, 0, 9), tip(9, 5, 2, 0, 1), v1.Ordering_ORDERING_GT, false},
		"lower block number loses":                       {nil, tip(9, 5, 1, 0, 1), tip(10, 5, 2, 0, 9), v1.Ordering_ORDERING_LT, true},
		"same issuer and slot, higher issue number wins": {nil, tip(7, 5, 1, 1, 1), tip(7, 5, 1, 2, 9), v1.Ordering_ORDERING_LT, true},
		"same issuer and slot, lower issue number loses": {nil, tip(7, 5, 1, 3, 9), tip(7, 5, 1, 2, 1), v1.Ordering_ORDERING_GT, false},
		"issue number outranks the VRF":                  {nil, tip(7, 5, 1, 2, 9), tip(7, 5, 1, 1, 1), v1.Ordering_ORDERING_GT, false},
		"different issuers ignore issue numbers":         {nil, tip(7, 5, 1, 9, 9), tip(7, 5, 2, 0, 1), v1.Ordering_ORDERING_LT, true},
		"equal issue numbers fall to the VRF":            {nil, tip(7, 5, 1, 2, 9), tip(7, 5, 1, 2, 1), v1.Ordering_ORDERING_LT, true},
		"lower VRF wins, unrestricted":                   {nil, tip(7, 5, 1, 0, 1), tip(7, 500, 2, 0, 2), v1.Ordering_ORDERING_GT, false},
		"candidate with lower VRF wins, unrestricted":    {nil, tip(7, 5, 1, 0, 2), tip(7, 500, 2, 0, 1), v1.Ordering_ORDERING_LT, true},
		"equal VRF is a tie":                             {nil, tip(7, 5, 1, 0, 3), tip(7, 6, 2, 0, 3), v1.Ordering_ORDERING_EQ, false},
		"restricted: distance equal to window arms VRF":  {win(5), tip(7, 10, 1, 0, 2), tip(7, 15, 2, 0, 1), v1.Ordering_ORDERING_LT, true},
		"restricted: distance past window disarms VRF":   {win(5), tip(7, 10, 1, 0, 2), tip(7, 16, 2, 0, 1), v1.Ordering_ORDERING_EQ, false},
		"restricted: distance is symmetric":              {win(5), tip(7, 16, 1, 0, 2), tip(7, 10, 2, 0, 1), v1.Ordering_ORDERING_EQ, false},
		"restricted: window zero needs the same slot":    {win(0), tip(7, 10, 1, 0, 2), tip(7, 11, 2, 0, 1), v1.Ordering_ORDERING_EQ, false},
		"restricted: issue number still arms outside":    {win(0), tip(7, 10, 1, 1, 2), tip(7, 10, 1, 2, 9), v1.Ordering_ORDERING_LT, true},
		"identical tips tie":                             {nil, tip(7, 5, 1, 2, 3), tip(7, 5, 1, 2, 3), v1.Ordering_ORDERING_EQ, false},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := chainselCompare(context.Background(), &v1.ChainselCompareRequest{
				Era: v1.Era_ERA_CONWAY, RestrictedVrfWindow: tc.window, A: tc.a, B: tc.b,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetOrder() != tc.want || got.GetPreferB() != tc.wantPreferB {
				t.Fatalf("order=%v prefer_b=%v, want %v %v", got.GetOrder(), got.GetPreferB(), tc.want, tc.wantPreferB)
			}
		})
	}
}

func TestChainselVrfTiebreak(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		window *uint64
		a, b   *v1.ChainTip
		want   v1.Ordering
	}{
		"block numbers are ignored":     {nil, tip(1, 5, 1, 0, 1), tip(99, 5, 2, 0, 2), v1.Ordering_ORDERING_GT},
		"candidate lower VRF":           {nil, tip(1, 5, 1, 0, 2), tip(1, 6, 2, 0, 1), v1.Ordering_ORDERING_LT},
		"restricted window disarms":     {win(1), tip(1, 5, 1, 0, 2), tip(1, 7, 2, 0, 1), v1.Ordering_ORDERING_EQ},
		"issue number beats VRF":        {nil, tip(1, 5, 1, 2, 9), tip(1, 5, 1, 1, 1), v1.Ordering_ORDERING_GT},
		"same issuer, equal everything": {nil, tip(1, 5, 1, 2, 3), tip(1, 5, 1, 2, 3), v1.Ordering_ORDERING_EQ},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := chainselVrfTiebreak(context.Background(), &v1.ChainselVrfTiebreakRequest{
				RestrictedVrfWindow: tc.window, A: tc.a, B: tc.b,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetOrder() != tc.want {
				t.Fatalf("order = %v, want %v", got.GetOrder(), tc.want)
			}
		})
	}
}

func TestChainselDomain(t *testing.T) {
	t.Parallel()
	short := tip(1, 1, 1, 0, 1)
	short.TiebreakVrf = short.TiebreakVrf[:63]
	badIssuer := tip(1, 1, 1, 0, 1)
	badIssuer.IssuerVkey = badIssuer.IssuerVkey[:31]
	for name, req := range map[string]*v1.ChainselCompareRequest{
		"byron has no Praos tiebreaker": {Era: v1.Era_ERA_BYRON, A: tip(1, 1, 1, 0, 1), B: tip(1, 1, 2, 0, 1)},
		"unspecified era":               {A: tip(1, 1, 1, 0, 1), B: tip(1, 1, 2, 0, 1)},
		"short VRF output":              {Era: v1.Era_ERA_CONWAY, A: short, B: tip(1, 1, 2, 0, 1)},
		"short issuer key":              {Era: v1.Era_ERA_CONWAY, A: tip(1, 1, 1, 0, 1), B: badIssuer},
		"missing tip":                   {Era: v1.Era_ERA_CONWAY, A: tip(1, 1, 1, 0, 1)},
	} {
		_, err := chainselCompare(context.Background(), req)
		if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED && code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
			t.Errorf("%s: err = %v, want UNSUPPORTED or BAD_REQUEST", name, err)
		}
	}
	_, err := chainselVrfTiebreak(context.Background(), &v1.ChainselVrfTiebreakRequest{A: short, B: tip(1, 1, 2, 0, 1)})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Errorf("vrf-tiebreak short VRF: err = %v, want UNSUPPORTED", err)
	}
}

func cand(peer string, idling bool, tipSlot uint64, slots ...uint64) *v1.DensityCandidate {
	return &v1.DensityCandidate{Peer: peer, Slots: slots, TipSlot: tipSlot, Idling: idling}
}

// densityDisconnect, window slots 101..110 after the intersection at slot 100.
// An open window leaves b every slot after its head, so b can still reach
// lb + (110 - head) headers.
func TestChainselGenesisDensity(t *testing.T) {
	t.Parallel()
	full := []uint64{101, 102, 103, 104, 105, 106, 107, 108, 109, 110}
	for name, tc := range map[string]struct {
		k     uint64
		peers []*v1.DensityCandidate
		want  []string
	}{
		// b reaches at most 1 + 5 = 6 headers; a has 10 and offers more than k.
		"denser peer beats a sparser one": {2, []*v1.DensityCandidate{cand("a", false, 110, full...), cand("b", false, 105, 105)}, []string{"b"}},
		// b is idle, so it stays at 1 header; a's 4 > k = 2.
		"idling peer is compared at its delivered density": {2, []*v1.DensityCandidate{cand("a", false, 104, 101, 102, 103, 104), cand("b", true, 105, 105)}, []string{"b"}},
		// a's 4 headers do not exceed k = 20 and b's window is open.
		"a rival with k or fewer headers does not qualify": {20, []*v1.DensityCandidate{cand("a", false, 104, 101, 102, 103, 104), cand("b", true, 105, 105)}, nil},
		"a peer with no headers is not compared":           {2, []*v1.DensityCandidate{cand("a", false, 110, full...), cand("b", false, 100)}, nil},
		"a lone candidate is never disconnected":           {2, []*v1.DensityCandidate{cand("a", false, 105, 105)}, nil},
		// a reaches at most 3 + 7 = 10 headers and b has 10: meeting the bound
		// exactly is enough, so a loses.
		"a rival meeting the bound exactly dominates": {2, []*v1.DensityCandidate{cand("a", false, 103, 101, 102, 103), cand("b", false, 110, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110)}, []string{"a"}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := chainselGenesisDensity(context.Background(), &v1.ChainselGenesisDensityRequest{
				GenesisWindow: 10, SecurityParam: tc.k, LoeSlot: 100, Candidates: tc.peers,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || !slicesEqual(got.GetDisconnect(), tc.want) {
				t.Fatalf("disconnect = %v, want %v", got.GetDisconnect(), tc.want)
			}
		})
	}
}

func slicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestChainselGenesisDensityDomain(t *testing.T) {
	t.Parallel()
	base := func() *v1.ChainselGenesisDensityRequest {
		return &v1.ChainselGenesisDensityRequest{GenesisWindow: 10, SecurityParam: 2, LoeSlot: 100,
			Candidates: []*v1.DensityCandidate{cand("a", false, 105, 101, 105), cand("b", false, 103, 103)}}
	}
	for name, mod := range map[string]func(*v1.ChainselGenesisDensityRequest){
		"zero window":                 func(r *v1.ChainselGenesisDensityRequest) { r.GenesisWindow = 0 },
		"zero security parameter":     func(r *v1.ChainselGenesisDensityRequest) { r.SecurityParam = 0 },
		"slots at or before the loe":  func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[0].Slots = []uint64{100, 105} },
		"slots out of order":          func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[0].Slots = []uint64{105, 101} },
		"repeated slot":               func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[0].Slots = []uint64{101, 101} },
		"tip slot behind the headers": func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[0].TipSlot = 104 },
		"duplicate peer":              func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[1].Peer = "a" },
		"empty peer name":             func(r *v1.ChainselGenesisDensityRequest) { r.Candidates[1].Peer = "" },
		"window past the slot range":  func(r *v1.ChainselGenesisDensityRequest) { r.GenesisWindow = 1 << 63 },
	} {
		req := base()
		mod(req)
		_, err := chainselGenesisDensity(context.Background(), req)
		if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Errorf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
}
