package main

import (
	"bytes"
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func poolID(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

func retiring(e uint64) *uint64 { return &e }

// Haskell: casTreasury <>~ donations (Conway EPOCH).
func TestEpochDonations(t *testing.T) {
	t.Parallel()
	got, err := epochDonations(context.Background(), &v1.EpochDonationsRequest{Treasury: 1000, Donations: 234})
	if err != nil {
		t.Fatal(err)
	}
	if got.GetTreasury() != 1234 {
		t.Fatalf("treasury = %d, want 1234", got.GetTreasury())
	}
	// A Haskell Coin is unbounded but the response is a Word64.
	_, err = epochDonations(context.Background(), &v1.EpochDonationsRequest{Treasury: ^uint64(0), Donations: 1})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("overflow: err = %v, want UNSUPPORTED", err)
	}
}

// POOLREAP: pool 1 and 2 retire at epoch 5 and share reward account A; pool 3
// retires with an unregistered account B; pool 4 retires at epoch 6.
func TestEpochPoolReap(t *testing.T) {
	t.Parallel()
	req := &v1.EpochPoolReapRequest{
		Epoch: 5,
		Pools: []*v1.Pool{
			{PoolId: poolID(1), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(0xa), Deposit: 500, RetiringEpoch: retiring(5)},
			{PoolId: poolID(2), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(0xa), Deposit: 300, RetiringEpoch: retiring(5)},
			{PoolId: poolID(3), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(0xb), Deposit: 200, RetiringEpoch: retiring(5)},
			{PoolId: poolID(4), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(0xa), Deposit: 900, RetiringEpoch: retiring(6)},
		},
		Accounts: []*v1.Account{
			{Credential: keyCred(0xa)},
			{Credential: keyCred(0xc), PoolId: poolID(1)},
			{Credential: keyCred(0xd), PoolId: poolID(4)},
		},
	}
	got, err := epochPoolReap(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.GetRetired()) != 3 || !bytes.Equal(got.Retired[0], poolID(1)) || !bytes.Equal(got.Retired[2], poolID(3)) {
		t.Fatalf("retired = %x", got.GetRetired())
	}
	// Refunds are merged per reward account: 500 + 300.
	if len(got.GetRefunds()) != 1 || got.Refunds[0].GetAmount() != 800 || !bytes.Equal(got.Refunds[0].Credential.Hash, keyCred(0xa).Hash) {
		t.Fatalf("refunds = %v", got.GetRefunds())
	}
	if got.GetUnclaimedToTreasury() != 200 {
		t.Fatalf("unclaimed = %d, want 200", got.GetUnclaimedToTreasury())
	}
	if len(got.GetUndelegated()) != 1 || !bytes.Equal(got.Undelegated[0].Hash, keyCred(0xc).Hash) {
		t.Fatalf("undelegated = %v", got.GetUndelegated())
	}
}

func TestEpochPoolReapUnsupported(t *testing.T) {
	t.Parallel()
	for name, req := range map[string]*v1.EpochPoolReapRequest{
		"future stage": {Epoch: 1, Pools: []*v1.Pool{{PoolId: poolID(1), Stage: v1.PoolStage_POOL_STAGE_FUTURE, RewardAccount: keyCred(1), RetiringEpoch: retiring(1)}}},
		"deposit sum overflow": {Epoch: 1, Pools: []*v1.Pool{
			{PoolId: poolID(1), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(1), Deposit: ^uint64(0), RetiringEpoch: retiring(1)},
			{PoolId: poolID(2), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(2), Deposit: 1, RetiringEpoch: retiring(1)},
		}},
	} {
		if _, err := epochPoolReap(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
}

func mirEntry(src v1.MirSource, b byte, amt int64) *v1.MirEntry {
	return &v1.MirEntry{Source: src, Credential: keyCred(b), Amount: amt}
}

// MIR: credits fit, then a pot-to-pot transfer is applied before the check.
func TestEpochMir(t *testing.T) {
	t.Parallel()
	const R, T = v1.MirSource_MIR_SOURCE_RESERVES, v1.MirSource_MIR_SOURCE_TREASURY
	got, err := epochMir(context.Background(), &v1.EpochMirRequest{
		ProtocolVersion: &v1.ProtocolVersion{Major: 6},
		Reserves:        1000, Treasury: 500,
		Mir:        []*v1.MirEntry{mirEntry(R, 1, 100), mirEntry(T, 1, 50), mirEntry(R, 2, 200), mirEntry(R, 3, 999)},
		Registered: []*v1.Credential{keyCred(1), keyCred(2)},
		// Credential 3 is unregistered and skipped.
		ReservesToTreasury: 100,
	})
	if err != nil {
		t.Fatal(err)
	}
	// reserves 1000 - 100 transfer - 300 credits; treasury 500 + 100 - 50.
	if !got.GetApplied() || got.GetReserves() != 600 || got.GetTreasury() != 550 {
		t.Fatalf("applied=%v reserves=%d treasury=%d", got.GetApplied(), got.GetReserves(), got.GetTreasury())
	}
	if len(got.GetCredited()) != 2 || got.Credited[0].Amount != 150 || got.Credited[1].Amount != 200 {
		t.Fatalf("credited = %v", got.GetCredited())
	}
	// Over budget: nothing moves, not even the transfer.
	got, err = epochMir(context.Background(), &v1.EpochMirRequest{
		ProtocolVersion: &v1.ProtocolVersion{Major: 6},
		Reserves:        100, Treasury: 0,
		Mir:                []*v1.MirEntry{mirEntry(R, 1, 150)},
		Registered:         []*v1.Credential{keyCred(1)},
		ReservesToTreasury: 10,
	})
	if err != nil {
		t.Fatal(err)
	}
	if got.GetApplied() || got.GetReserves() != 100 || got.GetTreasury() != 0 || len(got.GetCredited()) != 0 {
		t.Fatalf("over budget: %v", got)
	}
}

func TestEpochMirUnsupported(t *testing.T) {
	t.Parallel()
	const R = v1.MirSource_MIR_SOURCE_RESERVES
	for name, req := range map[string]*v1.EpochMirRequest{
		"negative delta": {ProtocolVersion: &v1.ProtocolVersion{Major: 6}, Mir: []*v1.MirEntry{mirEntry(R, 1, -5)}, Registered: []*v1.Credential{keyCred(1)}},
		"repeated entry": {ProtocolVersion: &v1.ProtocolVersion{Major: 6}, Mir: []*v1.MirEntry{mirEntry(R, 1, 5), mirEntry(R, 1, 6)}, Registered: []*v1.Credential{keyCred(1)}},
		"pots overflow":  {ProtocolVersion: &v1.ProtocolVersion{Major: 6}, Reserves: ^uint64(0), Treasury: 1},
		"unknown source": {ProtocolVersion: &v1.ProtocolVersion{Major: 6}, Mir: []*v1.MirEntry{{Credential: keyCred(1), Amount: 1}}},
	} {
		if _, err := epochMir(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
}

// Domain shared with dxnode-hs: duplicate rows and non-28-byte hashes.
func TestEpochPoolReapDomain(t *testing.T) {
	t.Parallel()
	pool := func(id byte) *v1.Pool {
		return &v1.Pool{PoolId: poolID(id), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(1), RetiringEpoch: retiring(1)}
	}
	for name, tc := range map[string]struct {
		req  *v1.EpochPoolReapRequest
		code v1.HarnessErrorCode
	}{
		"duplicate pool":    {&v1.EpochPoolReapRequest{Epoch: 1, Pools: []*v1.Pool{pool(1), pool(1)}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"duplicate account": {&v1.EpochPoolReapRequest{Epoch: 1, Accounts: []*v1.Account{{Credential: keyCred(1)}, {Credential: keyCred(1)}}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
		"short account":     {&v1.EpochPoolReapRequest{Epoch: 1, Accounts: []*v1.Account{{Credential: &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: []byte{1}}}}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"short pool id":     {&v1.EpochPoolReapRequest{Epoch: 1, Pools: []*v1.Pool{{PoolId: []byte{1}, Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: keyCred(1)}}}, v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
	} {
		if _, err := epochPoolReap(context.Background(), tc.req); dispatch.Code(err) != tc.code {
			t.Fatalf("%s: err = %v, want %v", name, err, tc.code)
		}
	}
}

func TestEpochMirProtocolVersion(t *testing.T) {
	t.Parallel()
	for _, major := range []uint32{0, 1, 9, 10} {
		_, err := epochMir(context.Background(), &v1.EpochMirRequest{ProtocolVersion: &v1.ProtocolVersion{Major: major}})
		if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("major %d: err = %v, want UNSUPPORTED", major, err)
		}
	}
}
