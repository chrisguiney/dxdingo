package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"os"
	"strings"
	"testing"

	"golang.org/x/crypto/blake2b"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func fixture(t *testing.T, name string) []byte {
	t.Helper()
	s, err := os.ReadFile("testdata/" + name)
	if err != nil {
		t.Fatal(err)
	}
	b, err := hex.DecodeString(strings.TrimSpace(string(s)))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func unhex(t *testing.T, s string) []byte {
	t.Helper()
	b, err := hex.DecodeString(strings.ReplaceAll(s, " ", ""))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func mustAccept(t *testing.T, r *v1.Rejection) {
	t.Helper()
	if r != nil {
		t.Fatalf("rejected: %v", r)
	}
}

func mustReject(t *testing.T, r *v1.Rejection, what string) {
	t.Helper()
	f := r.GetFailures()
	if len(f) != 1 || f[0].GetRule() != "codec" || f[0].GetName() != "DeserialiseFailure" {
		t.Fatalf("%s: rejection = %v, want codec/DeserialiseFailure", what, r)
	}
}

// A Conway transaction [body, wits, true, aux] with an empty body
// {0: [], 1: [], 2: 0}. Its id is blake2b-256 of the body bytes; the
// fee-relevant size is body + witnesses + aux + the array header, which
// leaves out the IsValid byte (Cardano.Ledger.Core.sizeTxForFeeCalculation).
var minimalConwayTx = "84 a3 00 80 01 80 02 00 a0 f5 f6"

func TestCodecTx(t *testing.T) {
	t.Parallel()
	body := unhex(t, "a3 00 80 01 80 02 00")
	id := blake2b.Sum256(body)
	got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_CONWAY, Cbor: unhex(t, minimalConwayTx)})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetTxId(), id[:]) || got.GetSize() != 10 || !got.GetIsValid() {
		t.Fatalf("tx_id %x size %d valid %v, want %x 10 true", got.GetTxId(), got.GetSize(), got.GetIsValid(), id)
	}
	if !bytes.Equal(got.GetCbor(), unhex(t, minimalConwayTx)) {
		t.Fatalf("cbor = %x", got.GetCbor())
	}

	invalid := unhex(t, "84 a3 00 80 01 80 02 00 a0 f4 f6")
	got, _ = codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_CONWAY, Cbor: invalid})
	mustAccept(t, got.GetRejection())
	if got.GetIsValid() {
		t.Fatal("IsValid false decoded as valid")
	}

	for name, b := range map[string][]byte{
		"trailing byte": append(unhex(t, minimalConwayTx), 0),
		"truncated":     unhex(t, minimalConwayTx)[:10],
		"not an array":  {0x01},
	} {
		got, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_CONWAY, Cbor: b})
		if err != nil {
			t.Fatal(err)
		}
		mustReject(t, got.GetRejection(), name)
	}
	for name, req := range map[string]*v1.CodecTxRequest{
		"byron":                {Era: v1.Era_ERA_BYRON, Cbor: unhex(t, minimalConwayTx)},
		"version outside era":  {Era: v1.Era_ERA_CONWAY, ProtocolVersion: &v1.ProtocolVersion{Major: 5}, Cbor: unhex(t, minimalConwayTx)},
		"version inside era":   nil,
		"version above Conway": {Era: v1.Era_ERA_CONWAY, ProtocolVersion: &v1.ProtocolVersion{Major: 12}, Cbor: unhex(t, minimalConwayTx)},
	} {
		if req == nil {
			continue
		}
		if _, err := codecTx(context.Background(), req); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Fatalf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
	if _, err := codecTx(context.Background(), &v1.CodecTxRequest{Era: v1.Era_ERA_CONWAY, ProtocolVersion: &v1.ProtocolVersion{Major: 10}, Cbor: unhex(t, minimalConwayTx)}); err != nil {
		t.Fatalf("version 10 is inside Conway: %v", err)
	}
}

var h28 = bytes.Repeat([]byte{0x11}, 28)

// enterprise mainnet key address: header 0x61 then the key hash.
func testAddr() []byte { return append([]byte{0x61}, h28...) }

func TestCodecTxOutput(t *testing.T) {
	t.Parallel()
	// Shelley [address, coin].
	out := append([]byte{0x82, 0x58, 0x1d}, testAddr()...)
	out = append(out, 0x1a, 0x00, 0x0f, 0x42, 0x40)
	got, err := codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_SHELLEY, Cbor: out})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if got.GetCoin() != 1_000_000 || !bytes.Equal(got.GetAddress(), testAddr()) || len(got.GetAssets()) != 0 {
		t.Fatalf("coin %d address %x assets %v", got.GetCoin(), got.GetAddress(), got.GetAssets())
	}

	// Mary [address, [coin, {policy: {"b": 7, "aa": 5}}]]: assets come back
	// sorted by name bytes, "aa" before "b".
	mary := append([]byte{0x82, 0x58, 0x1d}, testAddr()...)
	mary = append(mary, 0x82, 0x1a, 0x00, 0x1e, 0x84, 0x80, 0xa1, 0x58, 0x1c)
	mary = append(mary, h28...)
	mary = append(mary, 0xa2, 0x41, 'b', 0x07, 0x42, 'a', 'a', 0x05)
	got, _ = codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_MARY, Cbor: mary})
	mustAccept(t, got.GetRejection())
	a := got.GetAssets()
	if got.GetCoin() != 2_000_000 || len(a) != 2 ||
		string(a[0].GetAssetName()) != "aa" || a[0].GetQuantity() != 5 ||
		string(a[1].GetAssetName()) != "b" || a[1].GetQuantity() != 7 {
		t.Fatalf("coin %d assets %v", got.GetCoin(), a)
	}

	// Babbage map output with an inline datum, d87980 (Constr 0 []).
	inline := []byte{0xa3, 0x00, 0x58, 0x1d}
	inline = append(inline, testAddr()...)
	inline = append(inline, 0x01, 0x1a, 0x00, 0x16, 0xe3, 0x60, 0x02, 0x82, 0x01, 0xd8, 0x18, 0x43, 0xd8, 0x79, 0x80)
	got, _ = codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_BABBAGE, Cbor: inline})
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetInlineDatum(), unhex(t, "d87980")) || len(got.GetDatumHash()) != 0 {
		t.Fatalf("inline datum %x, datum hash %x", got.GetInlineDatum(), got.GetDatumHash())
	}

	// Babbage map output with a datum hash.
	dh := bytes.Repeat([]byte{0x33}, 32)
	hashed := []byte{0xa3, 0x00, 0x58, 0x1d}
	hashed = append(hashed, testAddr()...)
	hashed = append(hashed, 0x01, 0x1a, 0x00, 0x16, 0xe3, 0x60, 0x02, 0x82, 0x00, 0x58, 0x20)
	hashed = append(hashed, dh...)
	got, _ = codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_BABBAGE, Cbor: hashed})
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetDatumHash(), dh) || len(got.GetInlineDatum()) != 0 {
		t.Fatalf("datum hash %x, inline %x", got.GetDatumHash(), got.GetInlineDatum())
	}

	got, _ = codecTxOutput(context.Background(), &v1.CodecTxOutputRequest{Era: v1.Era_ERA_CONWAY, Cbor: []byte{0x01}})
	mustReject(t, got.GetRejection(), "integer")
}

func TestCodecCertificate(t *testing.T) {
	t.Parallel()
	key := append([]byte{0x82, 0x00, 0x58, 0x1c}, h28...)
	cert := func(prefix ...byte) []byte { return append(prefix, key...) }
	for name, tc := range map[string]struct {
		era  v1.Era
		cbor []byte
		kind string
	}{
		"stake registration":      {v1.Era_ERA_SHELLEY, cert(0x82, 0x00), "RegTxCert"},
		"stake deregistration":    {v1.Era_ERA_SHELLEY, cert(0x82, 0x01), "UnRegTxCert"},
		"registration deposit":    {v1.Era_ERA_CONWAY, append(cert(0x83, 0x07), 0x1a, 0x00, 0x1e, 0x84, 0x80), "RegDepositTxCert"},
		"registration no deposit": {v1.Era_ERA_CONWAY, cert(0x82, 0x00), "RegTxCert"},
		"vote delegation":         {v1.Era_ERA_CONWAY, append(cert(0x83, 0x09), 0x81, 0x02), "DelegTxCert"},
	} {
		got, err := codecCertificate(context.Background(), &v1.CodecCertificateRequest{Era: tc.era, Cbor: tc.cbor})
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustAccept(t, got.GetRejection())
		if got.GetCertKind() != tc.kind || !bytes.Equal(got.GetCbor(), tc.cbor) {
			t.Fatalf("%s: kind %q cbor %x, want %q", name, got.GetCertKind(), got.GetCbor(), tc.kind)
		}
	}
	// Tag 7 is a Conway constructor.
	got, _ := codecCertificate(context.Background(), &v1.CodecCertificateRequest{
		Era: v1.Era_ERA_BABBAGE, Cbor: append(cert(0x83, 0x07), 0x1a, 0x00, 0x1e, 0x84, 0x80),
	})
	mustReject(t, got.GetRejection(), "Conway certificate in Babbage")
}

func TestCodecGovernance(t *testing.T) {
	t.Parallel()
	reward := append([]byte{0x58, 0x1d, 0xe0}, h28...)
	anchor := append([]byte{0x82, 0x75}, []byte("https://example.org/p")...)
	anchor = append(anchor, 0x58, 0x20)
	anchor = append(anchor, bytes.Repeat([]byte{0x33}, 32)...)
	proposal := func(action ...byte) []byte {
		b := []byte{0x84, 0x1a, 0x3b, 0x9a, 0xca, 0x00}
		b = append(b, reward...)
		b = append(b, action...)
		return append(b, anchor...)
	}
	for name, tc := range map[string]struct {
		action []byte
		want   v1.GovActionType
	}{
		"info":          {[]byte{0x81, 0x06}, v1.GovActionType_GOV_ACTION_TYPE_INFO},
		"no confidence": {[]byte{0x82, 0x03, 0xf6}, v1.GovActionType_GOV_ACTION_TYPE_NO_CONFIDENCE},
		"hard fork":     {[]byte{0x83, 0x01, 0xf6, 0x82, 0x0b, 0x00}, v1.GovActionType_GOV_ACTION_TYPE_HARD_FORK_INITIATION},
	} {
		req := &v1.CodecGovernanceRequest{Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: proposal(tc.action...)}
		got, err := codecGovernance(context.Background(), req)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		mustAccept(t, got.GetRejection())
		if got.GetActionType() != tc.want || !bytes.Equal(got.GetCbor(), req.GetCbor()) {
			t.Fatalf("%s: type %v cbor %x", name, got.GetActionType(), got.GetCbor())
		}
	}
	// Voting procedures: {voter: {action_id: [vote, null]}}.
	voter := append([]byte{0x82, 0x02, 0x58, 0x1c}, h28...)
	id := append([]byte{0x82, 0x58, 0x20}, bytes.Repeat([]byte{0x33}, 32)...)
	id = append(id, 0x00)
	votes := append([]byte{0xa1}, voter...)
	votes = append(votes, 0xa1)
	votes = append(votes, id...)
	votes = append(votes, 0x82, 0x01, 0xf6)
	got, _ := codecGovernance(context.Background(), &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: votes,
	})
	mustAccept(t, got.GetRejection())
	if !bytes.Equal(got.GetCbor(), votes) {
		t.Fatalf("voting procedures re-encoded as %x", got.GetCbor())
	}
	got, _ = codecGovernance(context.Background(), &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: append(votes, 0),
	})
	mustReject(t, got.GetRejection(), "trailing byte")
	if _, err := codecGovernance(context.Background(), &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_BABBAGE, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: proposal(0x81, 0x06),
	}); dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("Babbage proposal: err = %v, want UNSUPPORTED", err)
	}
}

// A datum keeps its wire bytes: Data decoded from an indefinite list
// re-encodes as that indefinite list (Cardano.Ledger.Plutus.Data keeps the
// original bytes of an annotated value).
func TestCodecPlutusData(t *testing.T) {
	t.Parallel()
	for name, hexStr := range map[string]string{
		"constr":           "d87980",
		"indefinite list":  "9f 01 02 ff",
		"indefinite bytes": "5f 41 01 41 02 ff",
		"general constr 9": "d8 66 82 09 80",
		"integer":          "1a 000f4240",
		"map":              "a1 01 02",
	} {
		b := unhex(t, hexStr)
		got, err := codecPlutusData(context.Background(), &v1.CodecPlutusDataRequest{Cbor: b})
		if err != nil {
			t.Fatal(err)
		}
		mustAccept(t, got.GetRejection())
		if !bytes.Equal(got.GetCbor(), b) {
			t.Fatalf("%s: re-encoded %x, want %x", name, got.GetCbor(), b)
		}
	}
	for name, hexStr := range map[string]string{"empty": "", "break": "ff", "trailing": "d87980 00", "text": "6161"} {
		got, _ := codecPlutusData(context.Background(), &v1.CodecPlutusDataRequest{Cbor: unhex(t, hexStr)})
		mustReject(t, got.GetRejection(), name)
	}
}

// The oracle is the explorer: the mainnet Shelley block at slot 16156972
// has header hash 2308cdd4...5c2b, and the Conway block at slot 159835207
// has 27807a70...bf47.
func TestCodecBlockHeader(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		era  v1.Era
		file string
		hash string
		slot uint64
		pv   uint32
	}{
		"shelley": {v1.Era_ERA_SHELLEY, "shelley_header.hex", "2308cdd4c0bf8b8bf92523bdd1dd31640c0f42ff079d985fcc07c36cbf915c2b", 16156972, 2},
		"conway":  {v1.Era_ERA_CONWAY, "conway_header.hex", "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47", 159835207, 10},
	} {
		raw := fixture(t, tc.file)
		got, err := codecBlockHeader(context.Background(), &v1.CodecBlockHeaderRequest{Era: tc.era, Cbor: raw})
		if err != nil {
			t.Fatal(err)
		}
		mustAccept(t, got.GetRejection())
		if hex.EncodeToString(got.GetHeaderHash()) != tc.hash || got.GetSlot() != tc.slot ||
			got.GetProtocolVersion().GetMajor() != tc.pv || !bytes.Equal(got.GetCbor(), raw) ||
			len(got.GetIssuerVkey()) != 32 || len(got.GetVrfVkey()) != 32 || len(got.GetBodyHash()) != 32 {
			t.Fatalf("%s: hash %x slot %d version %v", name, got.GetHeaderHash(), got.GetSlot(), got.GetProtocolVersion())
		}
		for what, b := range map[string][]byte{"truncated": raw[:len(raw)-1], "trailing": append(bytes.Clone(raw), 0)} {
			got, _ := codecBlockHeader(context.Background(), &v1.CodecBlockHeaderRequest{Era: tc.era, Cbor: b})
			mustReject(t, got.GetRejection(), name+" "+what)
		}
	}
	// A Shelley header is not a Conway one: the header body is shorter.
	got, _ := codecBlockHeader(context.Background(), &v1.CodecBlockHeaderRequest{Era: v1.Era_ERA_CONWAY, Cbor: fixture(t, "shelley_header.hex")})
	mustReject(t, got.GetRejection(), "Shelley header as Conway")
}

// The mainnet Byron main block at slot 4471207 (epoch 207, slot 7 under
// 21600 slots per epoch) has header hash 1451a0db...a2d8.
func TestCodecByronBlock(t *testing.T) {
	t.Parallel()
	wrapped := append([]byte{0x82, 0x01}, fixture(t, "byron_block.hex")...)
	got, err := codecByronBlock(context.Background(), &v1.CodecByronBlockRequest{ProtocolMagic: 764824073, Cbor: wrapped})
	if err != nil {
		t.Fatal(err)
	}
	mustAccept(t, got.GetRejection())
	if hex.EncodeToString(got.GetHeaderHash()) != "1451a0dbf16cfeddf4991a838961df1b08a68f43a19c0eb3b36cc4029c77a2d8" ||
		got.GetEpoch() != 207 || got.GetSlot() != 7 || got.GetIsBoundary() {
		t.Fatalf("hash %x epoch %d slot %d boundary %v", got.GetHeaderHash(), got.GetEpoch(), got.GetSlot(), got.GetIsBoundary())
	}
	for what, b := range map[string][]byte{
		"truncated": wrapped[:len(wrapped)-1],
		"trailing":  append(bytes.Clone(wrapped), 0),
		"bad tag":   append([]byte{0x82, 0x02}, fixture(t, "byron_block.hex")...),
	} {
		got, _ := codecByronBlock(context.Background(), &v1.CodecByronBlockRequest{Cbor: b})
		mustReject(t, got.GetRejection(), what)
	}
}
