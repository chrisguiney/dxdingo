package main

import (
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Expected values follow Rules/Certs.hs updateDormantDRepExpiry: the dormant
// epoch count is added to each expiry unless the sum is below the current
// epoch, in which case the expiry stays.
func expiryRequest(epoch, dormant uint64, dreps ...*v1.DRepRecord) *v1.GovDRepExpiryRequest {
	return &v1.GovDRepExpiryRequest{Epoch: epoch, Dreps: dreps, DormantEpochs: dormant}
}

func drepRecord(b byte, expiry uint64) *v1.DRepRecord {
	return &v1.DRepRecord{Credential: cred(b), Deposit: 500, Expiry: expiry, AnchorUrl: "https://x"}
}

func TestGovDRepExpiry(t *testing.T) {
	t.Parallel()
	got, err := govDRepExpiry(context.Background(), expiryRequest(20, 3,
		drepRecord(1, 30), // 33
		drepRecord(3, 17), // 20, exactly the current epoch
		drepRecord(4, 0),  // 3 is below epoch 20: stays 0
	))
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil || len(got.GetDrepsChanged()) != 2 {
		t.Fatalf("got %v", got)
	}
	for i, want := range []struct {
		b      byte
		expiry uint64
	}{{1, 33}, {3, 20}} {
		d := got.GetDrepsChanged()[i]
		if d.GetCredential().GetHash()[0] != want.b || d.GetExpiry() != want.expiry || d.GetDeposit() != 500 || d.GetAnchorUrl() != "https://x" {
			t.Errorf("changed[%d] = %v, want %d expiry %d with its deposit and anchor", i, d, want.b, want.expiry)
		}
	}
}

func TestGovDRepExpiryNoDormantEpochs(t *testing.T) {
	t.Parallel()
	got, err := govDRepExpiry(context.Background(), expiryRequest(20, 0, drepRecord(1, 30)))
	if err != nil || got.GetRejection() != nil || len(got.GetDrepsChanged()) != 0 {
		t.Fatalf("got %v, %v", got, err)
	}
}
