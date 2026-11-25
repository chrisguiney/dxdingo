package main

import (
	"bytes"
	"context"
	"encoding/binary"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// netCbor encodes a CBOR term. The cases below are hand-built from the CDDL
// in ouroboros-network, not produced by the code under test.
func netCbor(t *testing.T, term any) []byte {
	t.Helper()
	b, err := cbor.Encode(term)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func tPoint(slot uint64, fill byte) []any { return []any{slot, bytes.Repeat([]byte{fill}, 32)} }

func tTip(slot uint64, fill byte, block uint64) []any {
	return []any{tPoint(slot, fill), block}
}

func netRejectedAs(rej *v1.Rejection, name string) bool {
	return len(rej.GetFailures()) == 1 && rej.GetFailures()[0].GetName() == name &&
		rej.GetFailures()[0].GetRule() == "net"
}

func TestNetCodecChainSync(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	call := func(state string, version uint32, msg []byte) *v1.NetCodecChainSyncResponse {
		resp, err := netCodecChainSync(ctx, &v1.NetCodecChainSyncRequest{Codec: &v1.NetCodecRequest{Version: version, State: state, Message: msg}})
		if err != nil {
			t.Fatal(err)
		}
		return resp
	}

	req := call("StIdle", 14, []byte{0x81, 0x00})
	if req.GetMessageType() != "MsgRequestNext" || !bytes.Equal(req.GetCbor(), []byte{0x81, 0x00}) {
		t.Fatalf("RequestNext: %v", req)
	}

	back := call("StCanAwait", 14, netCbor(t, []any{3, tPoint(5, 0xaa), tTip(10, 0xbb, 7)}))
	if back.GetMessageType() != "MsgRollBackward" || back.GetPointSlot() != 5 || !bytes.Equal(back.GetPointHash(), bytes.Repeat([]byte{0xaa}, 32)) ||
		back.GetTipSlot() != 10 || back.GetTipBlockNo() != 7 {
		t.Fatalf("RollBackward: %v", back)
	}

	origin := call("StIntersect", 14, netCbor(t, []any{5, []any{}, []any{[]any{}, 0}}))
	if origin.GetMessageType() != "MsgIntersectFound" || origin.PointSlot != nil || origin.TipSlot != nil || origin.TipBlockNo != nil {
		t.Fatalf("IntersectFound at origin: %v, want unset slots", origin)
	}

	// MsgRequestNext is the client's message; it is not decodable in StMustReply.
	wrongState := call("StMustReply", 14, []byte{0x81, 0x00})
	if !netRejectedAs(wrongState.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("wrong state: %v, want DeserialiseFailure", wrongState)
	}
	done := call("StDone", 14, []byte{0x81, 0x07})
	if !netRejectedAs(done.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("StDone has no agency: %v, want DeserialiseFailure", done)
	}
	trailing := call("StIdle", 14, []byte{0x81, 0x00, 0x00})
	if !netRejectedAs(trailing.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("trailing byte: %v, want DeserialiseFailure", trailing)
	}
}

func TestNetCodecDomain(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	for name, tc := range map[string]struct {
		state   string
		version uint32
	}{
		"unknown state":         {"StNowhere", 14},
		"n2n version too old":   {"StIdle", 13},
		"n2c version in n2n":    {"StIdle", 32784 + 100},
		"zero version":          {"StIdle", 0},
		"block fetch state":     {"BFIdle", 14},
		"n2c version, n2n only": {"StClient", 32784},
	} {
		_, err := netCodecKeepAlive(ctx, &v1.NetCodecKeepAliveRequest{Codec: &v1.NetCodecRequest{Version: tc.version, State: tc.state, Message: []byte{0x81, 0x02}}})
		if err == nil {
			t.Errorf("%s: want a harness error", name)
		}
	}
}

func TestNetCodecKeepAlive(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	call := func(state string, msg []byte) *v1.NetCodecKeepAliveResponse {
		resp, err := netCodecKeepAlive(ctx, &v1.NetCodecKeepAliveRequest{Codec: &v1.NetCodecRequest{Version: 14, State: state, Message: msg}})
		if err != nil {
			t.Fatal(err)
		}
		return resp
	}
	ka := call("StClient", []byte{0x82, 0x00, 0x18, 0x2a})
	if ka.GetMessageType() != "MsgKeepAlive" || ka.GetCookie() != 42 || !bytes.Equal(ka.GetCbor(), []byte{0x82, 0x00, 0x18, 0x2a}) {
		t.Fatalf("MsgKeepAlive: %v", ka)
	}
	resp := call("StServer", []byte{0x82, 0x01, 0x19, 0x01, 0x00})
	if resp.GetMessageType() != "MsgKeepAliveResponse" || resp.GetCookie() != 256 {
		t.Fatalf("MsgKeepAliveResponse: %v", resp)
	}
	if rej := call("StServer", []byte{0x82, 0x00, 0x01}); !netRejectedAs(rej.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("MsgKeepAlive in StServer: %v", rej)
	}
	// The cookie is a Word16.
	if rej := call("StClient", netCbor(t, []any{0, 65536})); !netRejectedAs(rej.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("cookie 65536: %v", rej)
	}
}

func TestNetCodecMuxSdu(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	header := func(ts uint32, proto, length uint16) []byte {
		b := make([]byte, 8)
		binary.BigEndian.PutUint32(b, ts)
		binary.BigEndian.PutUint16(b[4:], proto)
		binary.BigEndian.PutUint16(b[6:], length)
		return b
	}
	resp, err := netCodecMuxSdu(ctx, &v1.NetCodecMuxSduRequest{Header: header(0x01020304, 2, 513)})
	if err != nil {
		t.Fatal(err)
	}
	if resp.GetTimestamp() != 0x01020304 || resp.GetMode() != v1.MuxMode_MUX_MODE_INITIATOR || resp.GetProtocol() != 2 || resp.GetLength() != 513 {
		t.Fatalf("initiator SDU: %v", resp)
	}
	resp, err = netCodecMuxSdu(ctx, &v1.NetCodecMuxSduRequest{Header: header(7, 0x8000|10, 1)})
	if err != nil {
		t.Fatal(err)
	}
	if resp.GetMode() != v1.MuxMode_MUX_MODE_RESPONDER || resp.GetProtocol() != 10 || resp.GetLength() != 1 {
		t.Fatalf("responder SDU: %v", resp)
	}
	// The reference decoder refuses an SDU announcing no payload.
	resp, err = netCodecMuxSdu(ctx, &v1.NetCodecMuxSduRequest{Header: header(7, 2, 0)})
	if err != nil {
		t.Fatal(err)
	}
	if !netRejectedAs(resp.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("zero-length SDU: %v", resp)
	}
	resp, err = netCodecMuxSdu(ctx, &v1.NetCodecMuxSduRequest{Header: []byte{1, 2, 3}})
	if err != nil {
		t.Fatal(err)
	}
	if !netRejectedAs(resp.GetRejection(), "DeserialiseFailure") {
		t.Fatalf("short header: %v", resp)
	}
}

func TestNetAgency(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	call := func(p v1.MiniProtocol, state, msg string) *v1.NetAgencyResponse {
		resp, err := netAgency(ctx, &v1.NetAgencyRequest{Protocol: p, State: state, MessageType: msg})
		if err != nil {
			t.Fatal(err)
		}
		return resp
	}
	r := call(v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC, "StIdle", "MsgRequestNext")
	if r.GetNextState() != "StCanAwait" || r.GetAgency() != v1.Agency_AGENCY_SERVER {
		t.Fatalf("RequestNext: %v", r)
	}
	r = call(v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC, "StIdle", "MsgDone")
	if r.GetNextState() != "StDone" || r.GetAgency() != v1.Agency_AGENCY_NOBODY {
		t.Fatalf("Done: %v", r)
	}
	r = call(v1.MiniProtocol_MINI_PROTOCOL_KEEP_ALIVE, "StClient", "MsgKeepAlive")
	if r.GetNextState() != "StServer" || r.GetAgency() != v1.Agency_AGENCY_SERVER {
		t.Fatalf("KeepAlive: %v", r)
	}
	r = call(v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC, "StIdle", "MsgAwaitReply")
	if !netRejectedAs(r.GetRejection(), "IllegalTransition") {
		t.Fatalf("MsgAwaitReply in StIdle: %v, want IllegalTransition", r)
	}
	r = call(v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC, "StDone", "MsgDone")
	if !netRejectedAs(r.GetRejection(), "IllegalTransition") {
		t.Fatalf("MsgDone in StDone: %v, want IllegalTransition", r)
	}
}
