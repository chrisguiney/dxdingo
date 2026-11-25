package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func keyCred(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: bytes.Repeat([]byte{b}, 28)}
}

func drepCred(c *v1.Credential) *v1.DRep {
	return &v1.DRep{Target: &v1.DRep_Credential{Credential: c}}
}

// Expected values follow computeDRepDistr: a DRep's stake is the UTxO stake
// plus reward balance of every account delegated to it, plus the deposits of
// active proposals whose return account delegates to it.
func TestDRepDistributionRunsDingoSQL(t *testing.T) {
	drepA := keyCred(0xa0)
	acct1, acct2, acct3 := keyCred(0x01), keyCred(0x02), keyCred(0x03)
	req := &v1.StakeDRepDistributionRequest{
		Epoch: 10,
		InstantStake: []*v1.StakeEntry{
			{Credential: acct1, Amount: 10},
			{Credential: acct2, Amount: 7},
			{Credential: acct3, Amount: 1000},
		},
		Accounts: []*v1.Account{
			{Credential: acct1, Balance: 5, Drep: drepCred(drepA)},
			{Credential: acct2, Drep: &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}},
			// Not delegated: its stake counts for no DRep.
			{Credential: acct3, Balance: 1},
		},
		Dreps: []*v1.DRepRecord{{Credential: drepA, Expiry: 20}},
		Proposals: []*v1.Proposal{{
			Id: &v1.GovActionId{TxId: bytes.Repeat([]byte{0xee}, 32)}, Deposit: 100,
			ReturnAccount: acct1, ProposedEpoch: 9, ExpiresEpoch: 15,
		}},
	}

	got, err := drepDistribution(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	want := map[string]uint64{"drepA": 115, "abstain": 7}
	if len(got.GetDreps()) != len(want) {
		t.Fatalf("dreps = %v, want %d entries", got.GetDreps(), len(want))
	}
	for _, d := range got.GetDreps() {
		switch {
		case bytes.Equal(d.GetDrep().GetCredential().GetHash(), drepA.GetHash()):
			if d.GetStake() != want["drepA"] {
				t.Errorf("drepA stake = %d, want %d", d.GetStake(), want["drepA"])
			}
		case d.GetDrep().GetAlwaysAbstain():
			if d.GetStake() != want["abstain"] {
				t.Errorf("abstain stake = %d, want %d", d.GetStake(), want["abstain"])
			}
		default:
			t.Errorf("unexpected entry %v", d)
		}
	}
}

// The scratch database is shared by every call; a second call must not see
// the first call's rows.
func TestDRepDistributionCallsAreIsolated(t *testing.T) {
	drepA, acct := keyCred(0xb0), keyCred(0x11)
	first := &v1.StakeDRepDistributionRequest{
		Epoch:        10,
		InstantStake: []*v1.StakeEntry{{Credential: acct, Amount: 50}},
		Accounts:     []*v1.Account{{Credential: acct, Drep: drepCred(drepA)}},
		Dreps:        []*v1.DRepRecord{{Credential: drepA, Expiry: 20}},
	}
	if _, err := drepDistribution(context.Background(), first); err != nil {
		t.Fatal(err)
	}
	second := &v1.StakeDRepDistributionRequest{
		Epoch: 10,
		Dreps: []*v1.DRepRecord{{Credential: drepA, Expiry: 20}},
	}
	got, err := drepDistribution(context.Background(), second)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.GetDreps()) != 0 {
		t.Fatalf("second call saw first call's stake: %v", got.GetDreps())
	}
}

func TestDRepDistributionRejectsUnspecifiedCredentialKind(t *testing.T) {
	_, err := drepDistribution(context.Background(), &v1.StakeDRepDistributionRequest{
		Dreps: []*v1.DRepRecord{{Credential: &v1.Credential{Hash: []byte{1}}}},
	})
	if err == nil {
		t.Fatal("accepted a credential with no kind")
	}
}
