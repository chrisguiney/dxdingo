package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"testing"

	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Dijkstra is protocol version 12 only. A transaction is [body, witnesses,
// auxiliary data], the is-valid flag moved to the block body, and a block body
// is [invalid transactions, transactions, leios, peras].

func TestCodecDijkstraTx(t *testing.T) {
	t.Parallel()
	body := unhex(t, "a3 00 80 01 80 02 00")
	id := blake2b.Sum256(body)
	b := unhex(t, "83 a3 00 80 01 80 02 00 a0 f6")
	got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: b})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetTxId(), id[:]) || got.GetSize() != 10 || !got.GetIsValid() || !bytes.Equal(got.GetCbor(), b) {
		t.Fatalf("tx_id %x size %d valid %v cbor %x", got.GetTxId(), got.GetSize(), got.GetIsValid(), got.GetCbor())
	}
	for name, bad := range map[string][]byte{
		"trailing byte": append(bytes.Clone(b), 0),
		"truncated":     b[:9],
		"not an array":  {0x01},
	} {
		got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: bad})
		if err != nil {
			t.Fatal(err)
		}
		mustReject(t, got.GetRejection(), name)
	}
}

func TestCodecDijkstraProtocolVersion(t *testing.T) {
	t.Parallel()
	b := unhex(t, "83 a3 00 80 01 80 02 00 a0 f6")
	for major, want := range map[uint32]bool{11: false, 12: true, 13: false} {
		_, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_DIJKSTRA, ProtocolVersion: &v1.ProtocolVersion{Major: major}, Cbor: b})
		if got := err == nil; got != want {
			t.Fatalf("version %d: accepted %v, want %v (err %v)", major, got, want, err)
		}
		if err != nil && dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("version %d: err = %v, want UNSUPPORTED", major, err)
		}
	}
	// Conway stops at 11: a Dijkstra version is not Conway's.
	_, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_CONWAY, ProtocolVersion: &v1.ProtocolVersion{Major: 12}, Cbor: unhex(t, minimalConwayTx)})
	if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("Conway at version 12: err = %v, want UNSUPPORTED", err)
	}
}

func TestCodecDijkstraTxOutput(t *testing.T) {
	t.Parallel()
	dh := bytes.Repeat([]byte{0x33}, 32)
	hashed := append([]byte{0xa3, 0x00, 0x58, 0x1d}, testAddr()...)
	hashed = append(hashed, 0x01, 0x1a, 0x00, 0x16, 0xe3, 0x60, 0x02, 0x82, 0x00, 0x58, 0x20)
	hashed = append(hashed, dh...)
	got, err := codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: hashed})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetDatumHash(), dh) || got.GetCoin() != 1_500_000 || !bytes.Equal(got.GetAddress(), testAddr()) {
		t.Fatalf("datum hash %x coin %d address %x", got.GetDatumHash(), got.GetCoin(), got.GetAddress())
	}
	got, _ = codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: []byte{0x01}})
	mustReject(t, got.GetRejection(), "integer")
}

func TestCodecDijkstraCertificate(t *testing.T) {
	t.Parallel()
	key := append([]byte{0x82, 0x00, 0x58, 0x1c}, h28...)
	cert := func(prefix ...byte) []byte { return append(prefix, key...) }
	for name, tc := range map[string]struct {
		cbor []byte
		kind string
	}{
		"registration deposit":   {append(cert(0x83, 0x07), 0x1a, 0x00, 0x1e, 0x84, 0x80), "RegDepositTxCert"},
		"deregistration deposit": {append(cert(0x83, 0x08), 0x1a, 0x00, 0x1e, 0x84, 0x80), "UnRegDepositTxCert"},
		"vote delegation":        {append(cert(0x83, 0x09), 0x81, 0x02), "DelegTxCert"},
	} {
		got, err := codecCertificate(context.Background(), &v1.CodecCertificateRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: tc.cbor})
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustAccept(t, got.GetRejection())
		if got.GetCertKind() != tc.kind || !bytes.Equal(got.GetCbor(), tc.cbor) {
			t.Fatalf("%s: kind %q cbor %x, want %q", name, got.GetCertKind(), got.GetCbor(), tc.kind)
		}
	}
}

func TestCodecDijkstraGovernance(t *testing.T) {
	t.Parallel()
	reward := append([]byte{0x58, 0x1d, 0xe0}, h28...)
	anchor := append([]byte{0x82, 0x75}, []byte("https://example.org/p")...)
	anchor = append(anchor, 0x58, 0x20)
	anchor = append(anchor, bytes.Repeat([]byte{0x33}, 32)...)
	b := append([]byte{0x84, 0x1a, 0x3b, 0x9a, 0xca, 0x00}, reward...)
	b = append(append(b, 0x81, 0x06), anchor...)
	got, err := codecGovernance(context.Background(), &v1.CodecGovernanceRequest{Era: v1.Era_ERA_DIJKSTRA, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: b})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if got.GetActionType() != v1.GovActionType_GOV_ACTION_TYPE_INFO || !bytes.Equal(got.GetCbor(), b) {
		t.Fatalf("type %v cbor %x", got.GetActionType(), got.GetCbor())
	}
	voter := append([]byte{0x82, 0x02, 0x58, 0x1c}, h28...)
	id := append([]byte{0x82, 0x58, 0x20}, bytes.Repeat([]byte{0x33}, 32)...)
	votes := append(append(append([]byte{0xa1}, voter...), 0xa1), id...)
	votes = append(votes, 0x00, 0x82, 0x01, 0xf6)
	got, _ = codecGovernance(context.Background(), &v1.CodecGovernanceRequest{Era: v1.Era_ERA_DIJKSTRA, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: votes})
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetCbor(), votes) {
		t.Fatalf("voting procedures re-encoded as %x", got.GetCbor())
	}
	got, _ = codecGovernance(context.Background(), &v1.CodecGovernanceRequest{Era: v1.Era_ERA_DIJKSTRA, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: append(votes, 0)})
	mustReject(t, got.GetRejection(), "trailing byte")
}

func TestCodecDijkstraBlockHeader(t *testing.T) {
	t.Parallel()
	raw := dijkstraHeader(t)
	got, err := codecBlockHeader(context.Background(), &v1.CodecBlockHeaderRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: raw})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if hex.EncodeToString(got.GetHeaderHash()) != "1a68d6908bdfec726530a7da46bdda2a459e12ea386679e3807dff01d2e1ecb3" ||
		got.GetSlot() != 9 || got.GetBlockNo() != 3 || !bytes.Equal(got.GetCbor(), raw) {
		t.Fatalf("hash %x slot %d block %d", got.GetHeaderHash(), got.GetSlot(), got.GetBlockNo())
	}
	for what, b := range map[string][]byte{"truncated": raw[:len(raw)-1], "trailing": append(bytes.Clone(raw), 0)} {
		got, _ := codecBlockHeader(context.Background(), &v1.CodecBlockHeaderRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: b})
		mustReject(t, got.GetRejection(), what)
	}
}

func TestCodecDijkstraBlockBody(t *testing.T) {
	t.Parallel()
	got, err := codecBlockBody(context.Background(), &v1.CodecBlockBodyRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: dijkstraBlock(t)})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if got.GetTxCount() != 0 || len(got.GetBodyHash()) != 32 {
		t.Fatalf("tx count %d body hash %x", got.GetTxCount(), got.GetBodyHash())
	}
	got, _ = codecBlockBody(context.Background(), &v1.CodecBlockBodyRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: dijkstraBlock(t)[:len(dijkstraBlock(t))-1]})
	mustReject(t, got.GetRejection(), "truncated")
}

func TestCodecDijkstraPParamsUpdate(t *testing.T) {
	t.Parallel()
	run := func(hexStr string) *v1.CodecPParamsUpdateResponse {
		t.Helper()
		got, err := codecPParamsUpdate(context.Background(), &v1.CodecPParamsUpdateRequest{Era: v1.Era_ERA_DIJKSTRA, Cbor: unhex(t, hexStr)})
		if err != nil {
			t.Fatal(err)
		}
		return got
	}
	got := run("a1 03 19 4000")
	mustAccept(t, got.GetRejection())
	if got.GetUpdate().GetMaxTxSize() != 16384 || !bytes.Equal(got.GetCbor(), unhex(t, "a1 03 19 4000")) {
		t.Fatalf("update = %v", got.GetUpdate())
	}
	if u := run("a2 18 1b 07 18 1d 06").GetUpdate(); u.GetCommitteeMinSize() != 7 || u.GetGovActionLifetime() != 6 {
		t.Fatalf("governance update = %v", u)
	}
	mustReject(t, run("a1 03 19 4000 00").GetRejection(), "trailing byte")
}
