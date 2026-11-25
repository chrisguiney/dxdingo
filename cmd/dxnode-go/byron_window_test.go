package main

import (
	"context"
	"testing"

	byronconsensus "github.com/blinklabs-io/gouroboros/consensus/byron"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// windowSigners builds a signature window of n entries in slots 1..n, the
// signer at index i being genesis(i). threshold for k = 2160 is
// floor(0.22 * 2160) = 475 signatures per key.
func windowSigners(n int, genesis func(i int) []byte) []*v1.ByronSigner {
	out := make([]*v1.ByronSigner, n)
	for i := range out {
		out[i] = &v1.ByronSigner{Key: genesis(i), Slot: uint64(i + 1)}
	}
	return out
}

func headerWindowReq(t *testing.T, signers []*v1.ByronSigner) (*v1.ByronHeaderValidateRequest, []byte) {
	t.Helper()
	c, dlg := loadChain(t)
	issuer, err := byronconsensus.PBFTIssuerFromHeader(c.Main(3).BlockHeader)
	if err != nil {
		t.Fatal(err)
	}
	return &v1.ByronHeaderValidateRequest{
		ProtocolMagic: chainMagic, SecurityParam: 2160, Pparams: chainParams(),
		ChainState: &v1.ByronChainState{Delegations: dlg, Signers: signers},
		Header:     c.Blocks[3].Header().Cbor(),
	}, issuer.GenesisKeyHash[:]
}

func headerWindowNames(t *testing.T, req *v1.ByronHeaderValidateRequest) []string {
	t.Helper()
	got, err := byronHeaderValidate(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	return byronNames(got.GetRejection())
}

func filler(i int) []byte {
	k := make([]byte, 28)
	k[0] = byte(1 + i%7)
	k[1] = 0xee
	return k
}

// The PBFT window of the request is the reference's PBftState: at most
// floor(0.22 k) signatures per genesis key, counted after the new one is
// appended and the oldest dropped from a full window.
func TestByronHeaderValidateWindow(t *testing.T) {
	t.Parallel()
	_, g := headerWindowReq(t, nil)
	withG := func(n, firstG int) func(int) []byte {
		// G signs entries firstG..firstG+n-1.
		return func(i int) []byte {
			if i >= firstG && i < firstG+n {
				return g
			}
			return filler(i)
		}
	}
	for name, tc := range map[string]struct {
		signers []*v1.ByronSigner
		want    []string
	}{
		"empty window":                                        {nil, nil},
		"474 signatures plus this one":                        {windowSigners(474, withG(474, 0)), nil},
		"475 signatures plus this one":                        {windowSigners(475, withG(475, 0)), []string{"PBftExceededSignThreshold"}},
		"full window, oldest is the issuer's: it leaves":      {windowSigners(2160, withG(475, 0)), nil},
		"full window, oldest is another's: 476 in the window": {windowSigners(2160, withG(475, 1)), []string{"PBftExceededSignThreshold"}},
		"slot before the last signer":                         {[]*v1.ByronSigner{{Key: filler(0), Slot: 4326}}, []string{"PBftInvalidSlot"}},
		"slot equal to the last signer":                       {[]*v1.ByronSigner{{Key: filler(0), Slot: 4325}}, nil},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req, _ := headerWindowReq(t, tc.signers)
			got := headerWindowNames(t, req)
			if len(got) != len(tc.want) || (len(got) == 1 && got[0] != tc.want[0]) {
				t.Fatalf("rejected as %v, want %v", got, tc.want)
			}
		})
	}
}

func TestByronHeaderValidateWindowDomain(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		signers []*v1.ByronSigner
		want    v1.HarnessErrorCode
	}{
		"more signers than k": {windowSigners(2161, filler), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"slots decrease":      {[]*v1.ByronSigner{{Key: filler(0), Slot: 5}, {Key: filler(1), Slot: 4}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"short key":           {[]*v1.ByronSigner{{Key: []byte{1}, Slot: 1}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			req, _ := headerWindowReq(t, tc.signers)
			_, err := byronHeaderValidate(context.Background(), req)
			if dispatch.Code(err) != tc.want {
				t.Fatalf("code = %v (%v), want %v", dispatch.Code(err), err, tc.want)
			}
		})
	}
}
