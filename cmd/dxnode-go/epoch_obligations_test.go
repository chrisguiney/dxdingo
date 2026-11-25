package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Same case as dxnode-hs test/Test/Epoch.hs "obligations".
func obligationsRequest() *v1.EpochObligationsRequest {
	return &v1.EpochObligationsRequest{
		Accounts: []*v1.Account{
			{Credential: rotKey(1), Deposit: 2_000_000},
			{Credential: rotKey(2), Deposit: 2_000_000},
		},
		Pools: []*v1.Pool{{
			PoolId: bytes.Repeat([]byte{0xa}, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, Deposit: 500_000_000,
			RewardAccount: rotKey(5), Margin: &v1.Rational{Numerator: "1", Denominator: "100"},
		}},
		Dreps: []*v1.DRepRecord{{Credential: rotKey(3), Deposit: 500_000_000}},
		Proposals: []*v1.Proposal{{
			Id:      &v1.GovActionId{TxId: bytes.Repeat([]byte{7}, 32)},
			Deposit: 100_000_000_000, ReturnAccount: rotKey(1), ExpiresEpoch: 10,
		}},
	}
}

func TestEpochObligations(t *testing.T) {
	t.Parallel()
	got, err := epochObligations(context.Background(), obligationsRequest())
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejection %v", got.GetRejection())
	}
	if got.GetStakeDeposits() != 4_000_000 || got.GetPoolDeposits() != 500_000_000 ||
		got.GetDrepDeposits() != 500_000_000 || got.GetProposalDeposits() != 100_000_000_000 ||
		got.GetTotal() != 101_004_000_000 {
		t.Fatalf("got %v", got)
	}
}

func TestEpochObligationsEmpty(t *testing.T) {
	t.Parallel()
	got, err := epochObligations(context.Background(), &v1.EpochObligationsRequest{})
	if err != nil || got.GetTotal() != 0 || got.GetRejection() != nil {
		t.Fatalf("got %v, %v", got, err)
	}
}

func TestEpochObligationsUnsupported(t *testing.T) {
	t.Parallel()
	for name, mutate := range map[string]func(*v1.EpochObligationsRequest){
		"future pool":       func(r *v1.EpochObligationsRequest) { r.Pools[0].Stage = v1.PoolStage_POOL_STAGE_FUTURE },
		"repeated account":  func(r *v1.EpochObligationsRequest) { r.Accounts = append(r.Accounts, r.Accounts[0]) },
		"repeated proposal": func(r *v1.EpochObligationsRequest) { r.Proposals = append(r.Proposals, r.Proposals[0]) },
		"deposit overflow":  func(r *v1.EpochObligationsRequest) { r.Accounts[0].Deposit = ^uint64(0) },
	} {
		req := obligationsRequest()
		mutate(req)
		if _, err := epochObligations(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Errorf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
}

func TestEpochObligationsShortCredentialIsBadRequest(t *testing.T) {
	t.Parallel()
	req := obligationsRequest()
	req.Dreps[0].Credential.Hash = []byte{1}
	if _, err := epochObligations(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("err = %v, want BAD_REQUEST", err)
	}
}
