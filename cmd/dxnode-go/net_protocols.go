package main

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/blockfetch"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"github.com/blinklabs-io/gouroboros/protocol/keepalive"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
	"github.com/blinklabs-io/gouroboros/protocol/localtxsubmission"
	"github.com/blinklabs-io/gouroboros/protocol/peersharing"
	"github.com/blinklabs-io/gouroboros/protocol/txsubmission"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

var netChainSync = &netProtocol{
	ntn: true, ntc: true,
	states: map[string]string{
		"StIdle": "Idle", "StCanAwait": "CanAwait", "StMustReply": "MustReply",
		"StIntersect": "Intersect", "StDone": "Done",
	},
	stateMap: func(ntc bool) protocol.StateMap {
		if ntc {
			return chainsync.StateMapNtC
		}
		return chainsync.StateMapNtN
	},
	decode: func(ntc bool, t uint, d []byte) (protocol.Message, error) {
		mode := protocol.ProtocolModeNodeToNode
		if ntc {
			mode = protocol.ProtocolModeNodeToClient
		}
		return chainsync.NewMsgFromCbor(mode, t, d)
	},
	names: map[uint8]string{
		0: "MsgRequestNext", 1: "MsgAwaitReply", 2: "MsgRollForward", 3: "MsgRollBackward",
		4: "MsgFindIntersect", 5: "MsgIntersectFound", 6: "MsgIntersectNotFound", 7: "MsgDone",
	},
}

var netBlockFetch = &netProtocol{
	ntn: true,
	states: map[string]string{
		"BFIdle": "Idle", "BFBusy": "Busy", "BFStreaming": "Streaming", "BFDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return blockfetch.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return blockfetch.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{
		0: "MsgRequestRange", 1: "MsgClientDone", 2: "MsgStartBatch", 3: "MsgNoBlocks",
		4: "MsgBlock", 5: "MsgBatchDone",
	},
}

var netTxSubmission2 = &netProtocol{
	ntn: true, matchMsg: true,
	states: map[string]string{
		"StInit": "Init", "StIdle": "Idle", "StTxIdsBlocking": "TxIdsBlocking",
		"StTxIdsNonBlocking": "TxIdsNonBlocking", "StTxs": "Txs", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return txsubmission.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return txsubmission.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{
		0: "MsgRequestTxIds", 1: "MsgReplyTxIds", 2: "MsgRequestTxs", 3: "MsgReplyTxs",
		4: "MsgDone", 6: "MsgInit",
	},
}

var netLocalStateQuery = &netProtocol{
	ntc: true,
	states: map[string]string{
		"StIdle": "Idle", "StAcquiring": "Acquiring", "StAcquired": "Acquired",
		"StQuerying": "Querying", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return localstatequery.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return localstatequery.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{
		0: "MsgAcquire", 1: "MsgAcquired", 2: "MsgFailure", 3: "MsgQuery", 4: "MsgResult",
		5: "MsgRelease", 6: "MsgReAcquire", 7: "MsgDone", 8: "MsgAcquire", 9: "MsgReAcquire",
		10: "MsgAcquire", 11: "MsgReAcquire",
	},
}

var netLocalTxSubmission = &netProtocol{
	ntc: true,
	states: map[string]string{
		"StIdle": "Idle", "StBusy": "Busy", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return localtxsubmission.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return localtxsubmission.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{0: "MsgSubmitTx", 1: "MsgAcceptTx", 2: "MsgRejectTx", 3: "MsgDone"},
}

// The reference splits Busy by the query in flight; gouroboros keeps one
// Busy state and tracks the query in a private per-connection context.
var netLocalTxMonitor = &netProtocol{
	ntc: true,
	states: map[string]string{
		"StIdle": "Idle", "StAcquiring": "Acquiring", "StAcquired": "Acquired",
		"StBusyNextTx": "Busy", "StBusyHasTx": "Busy", "StBusyGetSizes": "Busy",
		"StBusyGetMeasures": "Busy", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return localtxmonitor.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return localtxmonitor.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{
		0: "MsgDone", 1: "MsgAcquire", 2: "MsgAcquired", 3: "MsgRelease", 5: "MsgNextTx",
		6: "MsgReplyNextTx", 7: "MsgHasTx", 8: "MsgReplyHasTx", 9: "MsgGetSizes", 10: "MsgReplyGetSizes",
	},
}

var netKeepAlive = &netProtocol{
	ntn: true,
	states: map[string]string{
		"StClient": "Client", "StServer": "Server", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return keepalive.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return keepalive.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{0: "MsgKeepAlive", 1: "MsgKeepAliveResponse", 2: "MsgDone"},
}

var netPeerSharing = &netProtocol{
	ntn: true,
	states: map[string]string{
		"StIdle": "Idle", "StBusy": "Busy", "StDone": "Done",
	},
	stateMap: func(bool) protocol.StateMap { return peersharing.StateMap },
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return peersharing.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{0: "MsgShareRequest", 1: "MsgSharePeers", 2: "MsgDone"},
}

// netPoint reports a point as an optional slot and a hash; Origin has
// neither.
func netPoint(p pcommon.Point) (*uint64, []byte) {
	if p.Slot == 0 && p.Hash == nil {
		return nil, nil
	}
	return &p.Slot, p.Hash
}

func netEncoded(v any) []byte {
	b, err := cbor.Encode(v)
	if err != nil {
		return nil
	}
	return b
}

// netRun is the shared body of the codec handlers: decode, re-encode, then
// let fill add the message-specific fields.
func netRun(p *netProtocol, c *v1.NetCodecRequest) (*netDecoded, []byte, *v1.Rejection, error) {
	d, rej, err := netDecode(p, c)
	if err != nil || rej != nil {
		return nil, nil, rej, err
	}
	enc, err := netReencode(d.msg)
	if err != nil {
		return nil, nil, netRejection("DingoError", err), nil
	}
	return d, enc, nil, nil
}

func netCodecChainSync(_ context.Context, req *v1.NetCodecChainSyncRequest) (*v1.NetCodecChainSyncResponse, error) {
	d, enc, rej, err := netRun(netChainSync, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecChainSyncResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecChainSyncResponse{MessageType: d.name, Cbor: enc}
	setTip := func(t pcommon.Tip) {
		if t.Point.Slot == 0 && t.Point.Hash == nil {
			return
		}
		resp.TipSlot = &t.Point.Slot
		resp.TipBlockNo = &t.BlockNumber
	}
	setPoint := func(p pcommon.Point) { resp.PointSlot, resp.PointHash = netPoint(p) }
	switch m := d.msg.(type) {
	case *chainsync.MsgRollForwardNtN:
		resp.Payload = netEncoded(&m.WrappedHeader)
		setTip(m.Tip)
	case *chainsync.MsgRollForwardNtC:
		resp.Payload = netEncoded(m.WrappedBlock)
		setTip(m.Tip)
	case *chainsync.MsgRollBackward:
		setPoint(m.Point)
		setTip(m.Tip)
	case *chainsync.MsgIntersectFound:
		setPoint(m.Point)
		setTip(m.Tip)
	case *chainsync.MsgIntersectNotFound:
		setTip(m.Tip)
	}
	return resp, nil
}

func netCodecBlockFetch(_ context.Context, req *v1.NetCodecBlockFetchRequest) (*v1.NetCodecBlockFetchResponse, error) {
	d, enc, rej, err := netRun(netBlockFetch, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecBlockFetchResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecBlockFetchResponse{MessageType: d.name, Cbor: enc}
	switch m := d.msg.(type) {
	case *blockfetch.MsgRequestRange:
		resp.FromSlot, _ = netPoint(m.Start)
		resp.ToSlot, _ = netPoint(m.End)
	case *blockfetch.MsgBlock:
		resp.Payload = netEncoded(cbor.Tag{Number: cbor.CborTagCbor, Content: m.WrappedBlock})
	}
	return resp, nil
}

func netCodecTxSubmission2(_ context.Context, req *v1.NetCodecTxSubmission2Request) (*v1.NetCodecTxSubmission2Response, error) {
	d, enc, rej, err := netRun(netTxSubmission2, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecTxSubmission2Response{Rejection: rej}, err
	}
	resp := &v1.NetCodecTxSubmission2Response{MessageType: d.name, Cbor: enc}
	switch m := d.msg.(type) {
	case *txsubmission.MsgRequestTxIds:
		ack, req, blocking := uint32(m.Ack), uint32(m.Req), m.Blocking
		resp.Ack, resp.Req, resp.Blocking = &ack, &req, &blocking
	case *txsubmission.MsgReplyTxIds:
		for _, id := range m.TxIds {
			resp.TxIds = append(resp.TxIds, netEncoded(id.TxId))
		}
	case *txsubmission.MsgRequestTxs:
		for _, id := range m.TxIds {
			resp.TxIds = append(resp.TxIds, netEncoded(id))
		}
	case *txsubmission.MsgReplyTxs:
		for i := range m.Txs {
			resp.Txs = append(resp.Txs, netEncoded(&m.Txs[i]))
		}
	}
	return resp, nil
}

func netCodecLocalStateQuery(_ context.Context, req *v1.NetCodecLocalStateQueryRequest) (*v1.NetCodecLocalStateQueryResponse, error) {
	// The reference decodes a MsgResult by the result type of the query in
	// flight, so StQuerying needs that query, and it must itself decode.
	if req.GetCodec().GetState() == "StQuerying" {
		if len(req.GetQuery()) == 0 {
			return nil, dispatch.BadRequest(errors.New("StQuerying needs the query in flight"))
		}
		var q localstatequery.QueryWrapper
		if _, err := cbor.Decode(req.GetQuery(), &q); err != nil {
			return &v1.NetCodecLocalStateQueryResponse{Rejection: netRejection(netTagKey, err)}, nil
		}
	}
	d, enc, rej, err := netRun(netLocalStateQuery, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecLocalStateQueryResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecLocalStateQueryResponse{MessageType: d.name, Cbor: enc}
	switch m := d.msg.(type) {
	case *localstatequery.MsgQuery:
		resp.Query = netQueryPath(m.Query.Query)
	case *localstatequery.MsgResult:
		resp.Result = m.Result
	}
	return resp, nil
}

func netCodecLocalTxSubmission(_ context.Context, req *v1.NetCodecLocalTxSubmissionRequest) (*v1.NetCodecLocalTxSubmissionResponse, error) {
	d, enc, rej, err := netRun(netLocalTxSubmission, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecLocalTxSubmissionResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecLocalTxSubmissionResponse{MessageType: d.name, Cbor: enc}
	if m, ok := d.msg.(*localtxsubmission.MsgSubmitTx); ok {
		resp.Tx = netEncoded(&m.Transaction)
	}
	return resp, nil
}

func netCodecLocalTxMonitor(_ context.Context, req *v1.NetCodecLocalTxMonitorRequest) (*v1.NetCodecLocalTxMonitorResponse, error) {
	d, enc, rej, err := netRun(netLocalTxMonitor, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecLocalTxMonitorResponse{Rejection: rej}, err
	}
	name := d.name
	if name == "MsgAcquire" && req.GetCodec().GetState() == "StAcquired" {
		name = "MsgAwaitAcquire"
	}
	resp := &v1.NetCodecLocalTxMonitorResponse{MessageType: name, Cbor: enc}
	switch m := d.msg.(type) {
	case *localtxmonitor.MsgReplyNextTx:
		if len(m.Transaction.Tx) > 0 {
			resp.Payload = netEncoded([]any{m.Transaction.EraId, cbor.Tag{Number: cbor.CborTagCbor, Content: m.Transaction.Tx}})
		}
	case *localtxmonitor.MsgHasTx:
		resp.Payload = netEncoded([]any{m.EraId, m.TxId})
	}
	return resp, nil
}

func netCodecKeepAlive(_ context.Context, req *v1.NetCodecKeepAliveRequest) (*v1.NetCodecKeepAliveResponse, error) {
	d, enc, rej, err := netRun(netKeepAlive, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecKeepAliveResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecKeepAliveResponse{MessageType: d.name, Cbor: enc}
	var cookie uint32
	switch m := d.msg.(type) {
	case *keepalive.MsgKeepAlive:
		cookie = uint32(m.Cookie)
		resp.Cookie = &cookie
	case *keepalive.MsgKeepAliveResponse:
		cookie = uint32(m.Cookie)
		resp.Cookie = &cookie
	}
	return resp, nil
}

// netPeerAddress renders an address the way dxnode-hs does: dotted quad, or
// eight lowercase hex groups in brackets, then the port.
func netPeerAddress(a peersharing.PeerAddress) (string, error) {
	if v4 := a.IP.To4(); v4 != nil {
		return fmt.Sprintf("%s:%d", v4, a.Port), nil
	}
	v6 := a.IP.To16()
	if v6 == nil {
		return "", errors.New("peer address is not an IP")
	}
	groups := make([]string, 8)
	for i := range groups {
		groups[i] = fmt.Sprintf("%x", uint16(v6[2*i])<<8|uint16(v6[2*i+1]))
	}
	return fmt.Sprintf("[%s]:%d", strings.Join(groups, ":"), a.Port), nil
}

func netCodecPeerSharing(_ context.Context, req *v1.NetCodecPeerSharingRequest) (*v1.NetCodecPeerSharingResponse, error) {
	d, enc, rej, err := netRun(netPeerSharing, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecPeerSharingResponse{Rejection: rej}, err
	}
	resp := &v1.NetCodecPeerSharingResponse{MessageType: d.name, Cbor: enc}
	switch m := d.msg.(type) {
	case *peersharing.MsgShareRequest:
		amount := uint32(m.Amount)
		resp.Amount = &amount
	case *peersharing.MsgSharePeers:
		for _, a := range m.PeerAddresses {
			s, err := netPeerAddress(a)
			if err != nil {
				return &v1.NetCodecPeerSharingResponse{Rejection: netRejection(netTagKey, err)}, nil
			}
			resp.Peers = append(resp.Peers, s)
		}
	}
	return resp, nil
}
