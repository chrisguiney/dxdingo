package main

import (
	"bytes"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/blockfetch"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"github.com/blinklabs-io/gouroboros/protocol/handshake"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
	"github.com/blinklabs-io/gouroboros/protocol/peersharing"
	"github.com/blinklabs-io/gouroboros/protocol/txsubmission"
)

// Direct checks of gouroboros, bypassing the adapters, for the cases in
// corpus/divergences. The matching "divergence:" checks in
// haskell/dxnode-hs/test/Test/Net.hs call the reference libraries on the
// same bytes.

func divCbor(t *testing.T, term any) []byte {
	t.Helper()
	b, err := cbor.Encode(term)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// The reference writes a non-empty list indefinite (0x9f ... 0xff);
// gouroboros writes it definite. Both decoders accept either form.
func TestDivergenceListEncodingIsDefinite(t *testing.T) {
	t.Parallel()
	point := pcommon.NewPoint(9, bytes.Repeat([]byte{1}, 32))
	got, err := cbor.Encode(chainsync.NewMsgFindIntersect([]pcommon.Point{point}))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.HasPrefix(got, []byte{0x82, 0x04, 0x81}) {
		t.Fatalf("FindIntersect = %x, want a definite one-element list after 82 04", got)
	}
	peers, err := cbor.Encode(peersharing.NewMsgSharePeers([]peersharing.PeerAddress{{IP: []byte{192, 0, 2, 7}, Port: 3001}}))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.HasPrefix(peers, []byte{0x82, 0x01, 0x81}) {
		t.Fatalf("SharePeers = %x, want a definite one-element list after 82 01", peers)
	}
}

// The reference requires ascending version numbers in a handshake proposal.
func TestDivergenceHandshakeAcceptsUnorderedVersions(t *testing.T) {
	t.Parallel()
	data := []byte{0x82, 0x00, 0xa2, 0x0f, 0x84, 0x01, 0xf4, 0x00, 0xf4, 0x0e, 0x84, 0x01, 0xf4, 0x00, 0xf4}
	if _, err := handshake.NewMsgFromCbor(handshake.MessageTypeProposeVersions, data); err != nil {
		t.Fatalf("versions 15 then 14: %v, want accepted", err)
	}
}

// The reference accepts peer sharing 0 or 1 in version 13 and later.
func TestDivergencePeerSharingValueTwoIsAccepted(t *testing.T) {
	t.Parallel()
	data := divCbor(t, []any{1, false, 2, false})
	if _, err := protocol.GetProtocolVersion(14).NewVersionDataFromCborFunc(data); err != nil {
		t.Fatalf("peer sharing 2: %v, want accepted", err)
	}
}

// The reference knows MsgReplyVersions (tag 0 in the confirm state);
// gouroboros has no such message.
func TestDivergenceHandshakeHasNoReplyVersions(t *testing.T) {
	t.Parallel()
	sm := handshake.StateMapNtN
	var confirm protocol.State
	for s := range sm {
		if s.Name == "Confirm" {
			confirm = s
		}
	}
	_, _, err := protocol.NextState(sm, nil, confirm, handshake.MessageTypeProposeVersions, nil)
	if err == nil {
		t.Fatal("tag 0 transitions out of Confirm")
	}
}

// The reference rejects acquire-failure codes other than 0 and 1.
func TestDivergenceAcquireFailureCodeTwoIsAccepted(t *testing.T) {
	t.Parallel()
	if _, err := localstatequery.NewMsgFromCbor(localstatequery.MessageTypeFailure, []byte{0x82, 0x02, 0x02}); err != nil {
		t.Fatalf("failure code 2: %v, want accepted", err)
	}
}

// The reference has the DebugLedgerConfig query, tag 4; gouroboros does not.
func TestDivergenceDebugLedgerConfigIsUnknown(t *testing.T) {
	t.Parallel()
	if _, err := localstatequery.NewMsgFromCbor(localstatequery.MessageTypeQuery, []byte{0x82, 0x03, 0x81, 0x04}); err == nil {
		t.Fatal("DebugLedgerConfig decoded")
	}
}

// The reference decodes the transaction-submission lists only when they are
// indefinite, and refuses an empty blocking reply; gouroboros accepts both.
func TestDivergenceTxSubmissionAcceptsDefiniteAndEmptyBlockingLists(t *testing.T) {
	t.Parallel()
	txid := []any{6, bytes.Repeat([]byte{7}, 32)}
	if _, err := txsubmission.NewMsgFromCbor(txsubmission.MessageTypeReplyTxIds, divCbor(t, []any{1, []any{[]any{txid, 100}}})); err != nil {
		t.Fatalf("definite reply: %v, want accepted", err)
	}
	empty := []byte{0x82, 0x01, 0x9f, 0xff}
	msg, err := txsubmission.NewMsgFromCbor(txsubmission.MessageTypeReplyTxIds, empty)
	if err != nil {
		t.Fatalf("empty reply: %v, want accepted", err)
	}
	var blocking protocol.State
	for s := range txsubmission.StateMap {
		if s.Name == "TxIdsBlocking" {
			blocking = s
		}
	}
	if _, _, err := protocol.NextState(txsubmission.StateMap, nil, blocking, msg.Type(), msg); err != nil {
		t.Fatalf("empty reply in the blocking state: %v, want accepted", err)
	}
}

// The reference has MsgGetMeasures (tag 11) and its reply (tag 12);
// gouroboros stops at tag 10.
func TestDivergenceLocalTxMonitorHasNoMeasures(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		tag  uint
		data []byte
	}{{11, []byte{0x81, 0x0b}}, {12, []byte{0x83, 0x0c, 0x00, 0xa0}}} {
		if _, err := localtxmonitor.NewMsgFromCbor(tc.tag, tc.data); err == nil {
			t.Fatalf("tag %d decoded", tc.tag)
		}
	}
}

// A Byron-era transaction id is [0, [0, hash]]; gouroboros expects [era, hash].
func TestDivergenceByronTxIdIsRefused(t *testing.T) {
	t.Parallel()
	data := divCbor(t, []any{7, []any{0, []any{0, bytes.Repeat([]byte{9}, 32)}}})
	if _, err := localtxmonitor.NewMsgFromCbor(localtxmonitor.MessageTypeHasTx, data); err == nil {
		t.Fatal("Byron transaction id decoded")
	}
}

// gouroboros keeps the header and block bytes of a wrapper opaque; the
// reference decodes them.
func TestDivergenceBlockAndHeaderPayloadsAreOpaque(t *testing.T) {
	t.Parallel()
	block := divCbor(t, []any{4, cbor.Tag{Number: 24, Content: []byte{0x82, 0x07, 0x01}}})
	if _, err := blockfetch.NewMsgFromCbor(blockfetch.MessageTypeBlock, block); err != nil {
		t.Fatalf("garbage block: %v, want accepted", err)
	}
	tip := []any{[]any{uint64(1), bytes.Repeat([]byte{2}, 32)}, uint64(1)}
	header := divCbor(t, []any{2, []any{6, cbor.Tag{Number: 24, Content: []byte{0x01}}}, tip})
	if _, err := chainsync.NewMsgFromCbor(protocol.ProtocolModeNodeToNode, chainsync.MessageTypeRollForward, header); err != nil {
		t.Fatalf("garbage header: %v, want accepted", err)
	}
}
