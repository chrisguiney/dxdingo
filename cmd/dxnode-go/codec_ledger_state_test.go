package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/lsfixture"
)

// The fixtures are Cardano Blueprint ledger vectors converted to the pinned
// ledger's encoding by lsfixture. Expected values are read from the vector's
// bytes: the pool, its registration and the accounts in pool_and_drep.json,
// the three proposals in proposals.json.

const lsPparams = "testdata/ledger_state/pparams"

func lsRequest(format v1.LedgerStateFormat, b []byte) *v1.CodecLedgerStateRequest {
	return &v1.CodecLedgerStateRequest{Era: v1.Era_ERA_CONWAY, Format: format, Cbor: b}
}

func newEpochState(t *testing.T, vector string, opt lsfixture.Options) []byte {
	t.Helper()
	b, err := lsfixture.FromBlueprint("testdata/ledger_state/"+vector, lsPparams, "newLedgerState", opt)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func decodeNES(t *testing.T, b []byte) *v1.CodecLedgerStateResponse {
	t.Helper()
	got, err := codecLedgerState(context.Background(), lsRequest(v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, b))
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	return got
}

func hexOf(b []byte) string { return hex.EncodeToString(b) }

func TestCodecNewEpochStatePoolAndDRep(t *testing.T) {
	t.Parallel()
	pool := bytes.Repeat([]byte{0x77}, 28)
	got := decodeNES(t, newEpochState(t, "pool_and_drep.json", lsfixture.Options{
		Epoch: 7, Treasury: 1000, Reserves: 2000,
		Prev: lsfixture.Blocks{string(pool): 3}, Cur: lsfixture.Blocks{string(pool): 5},
		Mark: []lsfixture.Stake{{Hash: bytes.Repeat([]byte{0x01}, 28), Amount: 40, Pool: pool}},
	}))
	st := got.GetState()
	if got.GetEpoch() != 7 || st.GetPots().GetTreasury() != 1000 || st.GetPots().GetReserves() != 2000 || st.GetPots().GetFees() != 194490 {
		t.Fatalf("epoch %d pots %v", got.GetEpoch(), st.GetPots())
	}
	if len(st.GetAccounts()) != 3 {
		t.Fatalf("%d accounts, want 3", len(st.GetAccounts()))
	}
	a := st.GetAccounts()[0]
	if hexOf(a.GetCredential().GetHash()) != "1601bb34497f96e8dc7abfbffafdc4ec8b71b87472593c41403097e1" || a.GetBalance() != 500000000 || a.GetDeposit() != 2000000 {
		t.Fatalf("first account %v", a)
	}
	d := st.GetAccounts()[2]
	if hexOf(d.GetPoolId()) != "6a04213acc095f128dc4d90bd88b34a3b0c5c7dc2accd1e97ab9fe96" ||
		hexOf(d.GetDrep().GetCredential().GetHash()) != "ab0a0fd54097b56f33e1ad7e3204ac57fd80e99db43bb98734b875ca" {
		t.Fatalf("delegating account %v", d)
	}
	if len(st.GetPools()) != 2 {
		t.Fatalf("%d pool rows, want the current and the future registration", len(st.GetPools()))
	}
	cur, fut := st.GetPools()[0], st.GetPools()[1]
	if cur.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT || fut.GetStage() != v1.PoolStage_POOL_STAGE_FUTURE ||
		hexOf(cur.GetVrfKeyHash()) != "95f688dae2d137bd97cd47326225f73e643c705d526b146d08a0a80fed70c080" ||
		cur.GetPledge() != 71343432 || cur.GetCost() != 400249652 || cur.GetDeposit() != 500000000 ||
		hexOf(fut.GetVrfKeyHash()) != "e2f2beb2f08c53ec688bebf83d6712461f2cfb695eba187c9e91277029559225" ||
		fut.GetDeposit() != 500000000 {
		t.Fatalf("pools %v", st.GetPools())
	}
	if len(st.GetDreps()) != 1 || st.GetDreps()[0].GetDeposit() != 70000000 {
		t.Fatalf("dreps %v", st.GetDreps())
	}
	if len(st.GetCommittee().GetMembers()) != 2 || st.GetCommittee().GetThreshold().GetNumerator() != "1" {
		t.Fatalf("committee %v", st.GetCommittee())
	}
	if got.GetPparams().GetMinFeeA() != 44 || got.GetPparams().GetKeyDeposit() != 2000000 || got.GetPparams().GetProtocolVersion().GetMajor() != 10 {
		t.Fatalf("pparams %v", got.GetPparams())
	}
	if len(got.GetBlocksCurrent()) != 1 || got.GetBlocksCurrent()[0].GetCount() != 5 || got.GetBlocksPrevious()[0].GetCount() != 3 {
		t.Fatalf("blocks %v %v", got.GetBlocksCurrent(), got.GetBlocksPrevious())
	}
	if len(got.GetSnapshots()) != 1 || got.GetSnapshots()[0].GetSnapshot() != v1.SnapshotKind_SNAPSHOT_KIND_MARK ||
		got.GetSnapshots()[0].GetAmount() != 40 || !bytes.Equal(got.GetSnapshots()[0].GetPoolId(), pool) {
		t.Fatalf("snapshots %v", got.GetSnapshots())
	}
}

func TestCodecNewEpochStateProposals(t *testing.T) {
	t.Parallel()
	got := decodeNES(t, newEpochState(t, "proposals.json", lsfixture.Options{Epoch: 900}))
	st := got.GetState()
	if len(st.GetProposals()) != 3 || len(st.GetAccounts()) != 5 {
		t.Fatalf("%d proposals %d accounts", len(st.GetProposals()), len(st.GetAccounts()))
	}
	first := st.GetProposals()[0]
	if hexOf(first.GetId().GetTxId()) != "c1ccf2025a174f6fa56fbef00275e359fb9b4202a72a46c0d392fcee3b13a27e" ||
		first.GetActionType() != v1.GovActionType_GOV_ACTION_TYPE_PARAMETER_CHANGE ||
		first.GetProposedEpoch() != 899 || first.GetExpiresEpoch() != 909 || first.GetDeposit() != 123 ||
		hexOf(first.GetReturnAccount().GetHash()) != "0785de77ae7c7fcb35d2d0f05d27f4782469d48495fbdd22624f1e2a" || first.GetParent() != nil {
		t.Fatalf("first proposal %v", first)
	}
	if hot := st.GetCommittee().GetMembers()[0].GetHot(); hexOf(hot.GetHash()) != "c74dcd423bf991e50eb9956086a0496ffc7d44c5079f9df0f5931cff" {
		t.Fatalf("hot key %v", hot)
	}
}

// Every set leaves in canonical order whatever order the encoding gave it.
func TestCodecNewEpochStateIsSorted(t *testing.T) {
	t.Parallel()
	got := decodeNES(t, newEpochState(t, "proposals.json", lsfixture.Options{Epoch: 900}))
	st := got.GetState()
	for i := 1; i < len(st.GetAccounts()); i++ {
		if bytes.Compare(st.GetAccounts()[i-1].GetCredential().GetHash(), st.GetAccounts()[i].GetCredential().GetHash()) >= 0 {
			t.Fatalf("accounts %d and %d out of order", i-1, i)
		}
	}
	for i := 1; i < len(st.GetProposals()); i++ {
		if bytes.Compare(st.GetProposals()[i-1].GetId().GetTxId(), st.GetProposals()[i].GetId().GetTxId()) >= 0 {
			t.Fatalf("proposals %d and %d out of order", i-1, i)
		}
	}
}

func TestCodecNewEpochStateRejections(t *testing.T) {
	t.Parallel()
	good := newEpochState(t, "minimal.json", lsfixture.Options{Epoch: 1})
	for name, b := range map[string][]byte{
		"empty":         nil,
		"not an array":  {0x01},
		"truncated":     good[:len(good)-1],
		"short array":   append([]byte{0x83}, good[1:]...),
		"half the body": good[:len(good)/2],
	} {
		got, err := codecLedgerState(context.Background(), lsRequest(v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE, b))
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustReject(t, got.GetRejection(), name)
	}
}

func TestCodecLedgerStateArguments(t *testing.T) {
	t.Parallel()
	for name, req := range map[string]*v1.CodecLedgerStateRequest{
		"byron state":   {Era: v1.Era_ERA_BYRON, Format: v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE},
		"byron mempack": {Era: v1.Era_ERA_BYRON, Format: v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT},
	} {
		if _, err := codecLedgerState(context.Background(), req); err == nil {
			t.Errorf("%s: want UNSUPPORTED", name)
		}
	}
	if _, err := codecLedgerState(context.Background(), &v1.CodecLedgerStateRequest{Era: v1.Era_ERA_CONWAY}); err == nil {
		t.Error("unspecified format: want BAD_REQUEST")
	}
}

// Tag 0 of a MemPack TxOut is CompactAddr + Value: the address as a length
// prefixed byte string, then an ada-only value, tag 0 and the coin as a
// MemPack varint (1000000 is 0xBD 0x84 0x40: groups of seven bits, most
// significant first). The output is checked by its content: dingo writes the
// map format where the reference writes the two-element array (see
// TestDivergenceMempackTxOutIsMapFormat).
func TestCodecMempackTxOut(t *testing.T) {
	t.Parallel()
	addr := append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...)
	b := append([]byte{0x00, 0x1d}, addr...)
	b = append(b, 0x00, 0xbd, 0x84, 0x40)
	got, err := codecLedgerState(context.Background(), lsRequest(v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT, b))
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	out, err := ledger.NewBabbageTransactionOutputFromCbor(got.GetCbor())
	if err != nil {
		t.Fatal(err)
	}
	gotAddr, err := out.Address().Bytes()
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(gotAddr, addr) || out.Amount().Uint64() != 1000000 || out.Assets() != nil {
		t.Fatalf("address %x amount %v assets %v", gotAddr, out.Amount(), out.Assets())
	}
}

func TestCodecMempackTxOutRejections(t *testing.T) {
	t.Parallel()
	addr := append([]byte{0x61}, bytes.Repeat([]byte{0x11}, 28)...)
	good := append(append([]byte{0x00, 0x1d}, addr...), 0x00, 0xbd, 0x84, 0x40)
	for name, b := range map[string][]byte{
		"empty":     nil,
		"truncated": good[:len(good)-1],
		"trailing":  append(bytes.Clone(good), 0x00),
		"bad tag":   append([]byte{0x09}, good[1:]...),
	} {
		got, err := codecLedgerState(context.Background(), lsRequest(v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT, b))
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustReject(t, got.GetRejection(), name)
	}
}
