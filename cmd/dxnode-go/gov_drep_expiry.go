package main

import (
	"context"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// govDRepExpiry runs dingo's SQL dormancy bump, once per dormant epoch,
// against a scratch database holding the request's DReps. The request's
// epoch is not an input to dingo's bump.
func govDRepExpiry(ctx context.Context, req *v1.GovDRepExpiryRequest) (*v1.GovDRepExpiryResponse, error) {
	in := dxoracle.DRepDormancyInput{DormantEpochs: req.GetDormantEpochs()}
	for _, d := range req.GetDreps() {
		tag, hash, err := govCredential("dreps credential", d.GetCredential())
		if err != nil {
			return nil, err
		}
		in.Dreps = append(in.Dreps, dxoracle.DRep{Tag: tag, Key: hash, ExpiryEpoch: d.GetExpiry(), Active: true})
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	after, err := oracle.DRepDormancy(ctx, in)
	if err != nil {
		return &v1.GovDRepExpiryResponse{Rejection: dingoRejection("EPOCH", err)}, nil
	}
	resp := &v1.GovDRepExpiryResponse{}
	for i, d := range req.GetDreps() {
		if after[i].ExpiryEpoch == d.GetExpiry() {
			continue
		}
		changed := proto.Clone(d).(*v1.DRepRecord)
		changed.Expiry = after[i].ExpiryEpoch
		resp.DrepsChanged = append(resp.DrepsChanged, changed)
	}
	sort.Slice(resp.DrepsChanged, func(i, j int) bool {
		a, b := resp.DrepsChanged[i].GetCredential(), resp.DrepsChanged[j].GetCredential()
		if a.GetKind() != b.GetKind() {
			return a.GetKind() < b.GetKind()
		}
		return string(a.GetHash()) < string(b.GetHash())
	})
	return resp, nil
}
