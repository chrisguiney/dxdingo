package corpusgen

import (
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// The net cases are hand-assembled from the message layouts in the
// ouroboros-network CDDL specs. Real headers, blocks, transactions, queries
// and results are the consensus golden files, which are the reference's own
// serialisation at the negotiated version. Point DXDINGO_THIRD_PARTY at the
// directory holding the submodule checkouts (default ../../third_party).

func TestGenerateNetCorpus(t *testing.T) {
	writeCorpus(t, "", netCases(t))
	writeCorpus(t, "divergences", netDivergenceCases(t))
}

const (
	ntnV14, ntnV16 = 14, 16
	// Node-to-client versions in wire form: 0x8000 | version.
	ntcV16, ntcV19, ntcV23 = 0x8000 | 16, 0x8000 | 19, 0x8000 | 23
)

func goldenDir(t *testing.T, sub string) string {
	t.Helper()
	root := os.Getenv("DXDINGO_THIRD_PARTY")
	if root == "" {
		root = filepath.Join("..", "..", "third_party")
	}
	return filepath.Join(root, "ouroboros-consensus", "ouroboros-consensus-cardano", "golden", "cardano", sub)
}

// golden reads one golden file of a version directory.
func golden(t *testing.T, dir, name string) []byte {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(goldenDir(t, dir), name))
	if err != nil {
		t.Skipf("golden file unavailable: %v", err)
	}
	return b
}

const (
	goldenN2N = "CardanoNodeToNodeVersion2"
	goldenN2C = "QueryVersion3/CardanoNodeToClientVersion19"
)

func netEnc(t *testing.T, term any) []byte {
	t.Helper()
	b, err := cbor.Encode(term)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func netPt(slot uint64, fill byte) []any { return []any{slot, rep(fill, 32)} }

func netTipTerm(slot uint64, fill byte, block uint64) []any {
	return []any{netPt(slot, fill), block}
}

var netOrigin = []any{}

func netEntry(name, why string, call *v1.Call) entry { return entry{"net_" + name, why, call} }

func netCodecReq(version uint32, state string, msg []byte) *v1.NetCodecRequest {
	return &v1.NetCodecRequest{Version: version, State: state, Message: msg}
}

func netCS(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecChainSync{NetCodecChainSync: &v1.NetCodecChainSyncRequest{Codec: r}}}
}

func netBF(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecBlockFetch{NetCodecBlockFetch: &v1.NetCodecBlockFetchRequest{Codec: r}}}
}

func netTX(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecTxSubmission2{NetCodecTxSubmission2: &v1.NetCodecTxSubmission2Request{Codec: r}}}
}

func netLTS(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecLocalTxSubmission{NetCodecLocalTxSubmission: &v1.NetCodecLocalTxSubmissionRequest{Codec: r}}}
}

func netLTM(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecLocalTxMonitor{NetCodecLocalTxMonitor: &v1.NetCodecLocalTxMonitorRequest{Codec: r}}}
}

func netHS(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecHandshake{NetCodecHandshake: &v1.NetCodecHandshakeRequest{Codec: r}}}
}

func netKA(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecKeepAlive{NetCodecKeepAlive: &v1.NetCodecKeepAliveRequest{Codec: r}}}
}

func netPS(r *v1.NetCodecRequest) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecPeerSharing{NetCodecPeerSharing: &v1.NetCodecPeerSharingRequest{Codec: r}}}
}

func netLSQ(r *v1.NetCodecRequest, query []byte) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecLocalStateQuery{NetCodecLocalStateQuery: &v1.NetCodecLocalStateQueryRequest{Codec: r, Query: query}}}
}

func netMux(h []byte) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetCodecMuxSdu{NetCodecMuxSdu: &v1.NetCodecMuxSduRequest{Header: h}}}
}

func netAg(p v1.MiniProtocol, state, msg string) *v1.Call {
	return &v1.Call{Request: &v1.Call_NetAgency{NetAgency: &v1.NetAgencyRequest{Protocol: p, State: state, MessageType: msg}}}
}

// netMsg builds a message array whose payload items may be raw CBOR.
func netMsg(t *testing.T, tag uint64, items ...any) []byte {
	t.Helper()
	return netEnc(t, append([]any{tag}, items...))
}

func raw(b []byte) cbor.RawMessage { return cbor.RawMessage(b) }

var netEras = []string{"Byron_regular", "Byron_EBB", "Shelley", "Allegra", "Mary", "Alonzo", "Babbage", "Conway", "Dijkstra"}

// netDivergent names the cases where dxnode-go and dxnode-hs disagree. Each
// is confirmed by direct tests in cmd/dxnode-go/divergence_net_test.go and
// the "divergence:" checks in haskell/dxnode-hs/test/Test/Net.hs, and is
// written to corpus/divergences instead of corpus/.
var netDivergent = map[string]bool{
	"net_codec_chain_sync_find_intersect_points":                           true,
	"net_codec_chain_sync_find_intersect_indefinite":                       true,
	"net_codec_peer_sharing_share_peers_v4":                                true,
	"net_codec_peer_sharing_share_peers_v6":                                true,
	"net_codec_peer_sharing_share_peers_mixed":                             true,
	"net_codec_handshake_reject_bad_peer_sharing":                          true,
	"net_codec_handshake_reject_unordered_versions":                        true,
	"net_codec_local_state_query_reject_failure_code":                      true,
	"net_codec_local_tx_monitor_has_tx_Byron":                              true,
	"net_codec_local_tx_monitor_reply_next_tx_Byron":                       true,
	"net_codec_local_tx_submission_submit_Byron":                           true,
	"net_codec_block_fetch_block_Dijkstra":                                 true,
	"net_codec_tx_submission2_reject_reply_tx_ids_blocking_empty":          true,
	"net_codec_tx_submission2_reply_tx_ids_definite_list":                  true,
	"net_codec_tx_submission2_request_txs_definite_list":                   true,
	"net_codec_tx_submission2_reply_txs_definite_list":                     true,
	"net_codec_tx_submission2_reject_reply_tx_ids_blocking_empty_definite": true,
	"net_codec_local_tx_monitor_get_measures":                              true,
	"net_codec_local_tx_monitor_reply_get_measures":                        true,
	"net_codec_handshake_reply_versions":                                   true,
	"net_codec_local_state_query_query_debug_ledger_config":                true,
	"net_codec_block_fetch_block_garbage_payload":                          true,
	"net_codec_chain_sync_roll_forward_header_garbage":                     true,
}

func netSplit(all []entry, divergent bool) []entry {
	var out []entry
	for _, e := range all {
		if netDivergent[e.name] == divergent {
			out = append(out, e)
		}
	}
	return out
}

func netCases(t *testing.T) []entry { return netSplit(netAllCases(t), false) }

func netAllCases(t *testing.T) []entry {
	var out []entry
	out = append(out, netChainSyncCases(t)...)
	out = append(out, netBlockFetchCases(t)...)
	out = append(out, netTxSubmissionCases(t)...)
	out = append(out, netStateQueryCases(t)...)
	out = append(out, netLocalTxCases(t)...)
	out = append(out, netHandshakeCases(t)...)
	out = append(out, netSmallProtocolCases(t)...)
	out = append(out, netMuxCases()...)
	out = append(out, netAgencyCases()...)
	return out
}

func netChainSyncCases(t *testing.T) []entry {
	cs := func(name, why string, v uint32, state string, msg []byte) entry {
		return netEntry("codec_chain_sync_"+name, why, netCS(netCodecReq(v, state, msg)))
	}
	tip := netTipTerm(1000, 0xbb, 500)
	out := []entry{
		cs("request_next", "client message in StIdle", ntnV14, "StIdle", netMsg(t, 0)),
		cs("await_reply", "server message in StCanAwait", ntnV14, "StCanAwait", netMsg(t, 1)),
		cs("roll_backward", "point and tip decoded in StCanAwait", ntnV14, "StCanAwait", netMsg(t, 3, netPt(5, 0xaa), tip)),
		cs("roll_backward_must_reply", "RollBackward is also legal in StMustReply", ntnV16, "StMustReply", netMsg(t, 3, netPt(5, 0xaa), tip)),
		cs("roll_backward_origin", "the origin point is the empty array and has no slot", ntnV14, "StCanAwait", netMsg(t, 3, netOrigin, tip)),
		cs("roll_backward_tip_genesis", "a tip at genesis is the origin point with block number 0", ntnV14, "StCanAwait", netMsg(t, 3, netPt(5, 0xaa), []any{netOrigin, 0})),
		cs("find_intersect_empty", "an empty point list is the empty array", ntnV14, "StIdle", netMsg(t, 4, []any{})),
		cs("find_intersect_points", "points are decoded as a list", ntnV14, "StIdle", netMsg(t, 4, []any{netPt(9, 1), netPt(5, 2), netOrigin})),
		cs("find_intersect_indefinite", "the reference writes the list indefinite; either form decodes", ntnV14, "StIdle",
			append(append([]byte{0x82, 0x04, 0x9f}, netEnc(t, netPt(9, 1))...), 0xff)),
		cs("intersect_found", "point and tip in StIntersect", ntnV14, "StIntersect", netMsg(t, 5, netPt(5, 0xaa), tip)),
		cs("intersect_found_origin", "intersection at the origin with a genesis tip", ntnV14, "StIntersect", netMsg(t, 5, netOrigin, []any{netOrigin, 0})),
		cs("intersect_not_found", "only a tip", ntnV14, "StIntersect", netMsg(t, 6, tip)),
		cs("done", "client terminates from StIdle", ntnV14, "StIdle", netMsg(t, 7)),
		// Rejections.
		cs("reject_wrong_state", "RequestNext is a client message, not decodable in StMustReply", ntnV14, "StMustReply", netMsg(t, 0)),
		cs("reject_await_reply_in_must_reply", "AwaitReply is only legal in StCanAwait", ntnV14, "StMustReply", netMsg(t, 1)),
		cs("reject_done_state", "StDone has no agency, so nothing decodes", ntnV14, "StDone", netMsg(t, 7)),
		cs("reject_unknown_tag", "tag 9 is not a ChainSync message", ntnV14, "StIdle", netMsg(t, 9)),
		cs("reject_trailing", "bytes after the message", ntnV14, "StIdle", append(netMsg(t, 0), 0x00)),
		cs("reject_truncated", "the array announces two items and has one", ntnV14, "StIntersect", []byte{0x82, 0x06}),
		cs("reject_not_array", "a bare integer", ntnV14, "StIdle", []byte{0x00}),
		cs("reject_short_hash", "a 31-byte point hash", ntnV14, "StCanAwait", netMsg(t, 3, []any{uint64(5), rep(0xaa, 31)}, tip)),
		cs("reject_extra_item", "RequestNext with an item", ntnV14, "StIdle", netMsg(t, 0, 1)),
	}
	// Real headers (node-to-node) and blocks (node-to-client).
	for i, era := range netEras {
		hdr := golden(t, goldenN2N, "Header_"+era)
		v := uint32(ntnV14 + i%3)
		out = append(out, cs("roll_forward_header_"+era, "RollForward carrying the reference's serialised "+era+" header", v, "StCanAwait", netMsg(t, 2, raw(hdr), tip)))
	}
	for _, era := range []string{"Byron_regular", "Shelley", "Babbage", "Conway"} {
		blk := golden(t, goldenN2C, "Block_"+era)
		out = append(out, cs("roll_forward_block_"+era, "node-to-client RollForward carries the whole "+era+" block", ntcV19, "StMustReply", netMsg(t, 2, raw(blk), tip)))
	}
	out = append(out,
		cs("n2c_request_next", "node-to-client RequestNext at the lowest version", ntcV16, "StIdle", netMsg(t, 0)),
		cs("n2c_roll_backward", "node-to-client RollBackward at the highest version", ntcV23, "StCanAwait", netMsg(t, 3, netPt(5, 0xaa), tip)),
		cs("n2c_intersect_found", "node-to-client IntersectFound", ntcV19, "StIntersect", netMsg(t, 5, netPt(5, 0xaa), tip)),
		cs("roll_forward_header_garbage", "the header bytes inside the wrapper are not a header", ntnV14, "StCanAwait", netMsg(t, 2, []any{6, cbor.Tag{Number: 24, Content: []byte{0x01}}}, tip)),
		cs("reject_n2n_header_in_n2c", "a header is not a block: the node-to-client RollForward needs a whole block", ntcV19, "StCanAwait", netMsg(t, 2, raw(golden(t, goldenN2N, "Header_Conway")), tip)),
		cs("reject_n2c_block_in_n2n", "a block is not a header: the node-to-node RollForward needs a header", ntnV14, "StCanAwait", netMsg(t, 2, raw(golden(t, goldenN2C, "Block_Conway")), tip)),
		cs("unsupported_version_old", "node-to-node version 13 is outside the domain", 13, "StIdle", netMsg(t, 0)),
		cs("unsupported_version_new", "node-to-node version 17 is outside the domain", 17, "StIdle", netMsg(t, 0)),
		cs("unsupported_version_n2c_old", "node-to-client version 15 is outside the domain", 0x8000|15, "StIdle", netMsg(t, 0)),
	)
	return out
}

func netBlockFetchCases(t *testing.T) []entry {
	bf := func(name, why string, state string, msg []byte) entry {
		return netEntry("codec_block_fetch_"+name, why, netBF(netCodecReq(ntnV14, state, msg)))
	}
	out := []entry{
		bf("request_range", "two points", "BFIdle", netMsg(t, 0, netPt(10, 1), netPt(20, 2))),
		bf("request_range_from_origin", "the origin as the start point has no slot", "BFIdle", netMsg(t, 0, netOrigin, netPt(20, 2))),
		bf("client_done", "client ends the protocol", "BFIdle", netMsg(t, 1)),
		bf("start_batch", "server begins a batch", "BFBusy", netMsg(t, 2)),
		bf("no_blocks", "server has none of the range", "BFBusy", netMsg(t, 3)),
		bf("batch_done", "server ends the batch", "BFStreaming", netMsg(t, 5)),
		bf("block_garbage_payload", "the block bytes inside the wrapper are not a block", "BFStreaming", netMsg(t, 4, cbor.Tag{Number: 24, Content: []byte{0x82, 0x07, 0x01}})),
		bf("reject_start_batch_in_idle", "a server message in the client's state", "BFIdle", netMsg(t, 2)),
		bf("reject_block_in_busy", "a block must follow StartBatch", "BFBusy", netMsg(t, 4, raw(golden(t, goldenN2N, "Block_Conway")))),
		bf("reject_done_state", "BFDone has no agency", "BFDone", netMsg(t, 1)),
		bf("reject_unknown_tag", "tag 6 is not a BlockFetch message", "BFIdle", netMsg(t, 6)),
		bf("reject_trailing", "bytes after the message", "BFBusy", append(netMsg(t, 2), 0x00)),
		bf("reject_request_range_one_point", "RequestRange needs two points", "BFIdle", netMsg(t, 0, netPt(10, 1))),
	}
	for _, era := range netEras {
		out = append(out, bf("block_"+era, "a "+era+" block in the reference's serialisation", "BFStreaming", netMsg(t, 4, raw(golden(t, goldenN2N, "Block_"+era)))))
	}
	return out
}

func netTxSubmissionCases(t *testing.T) []entry {
	tx := func(name, why string, state string, msg []byte) entry {
		return netEntry("codec_tx_submission2_"+name, why, netTX(netCodecReq(ntnV14, state, msg)))
	}
	txid := raw(golden(t, goldenN2N, "GenTxId_Conway"))
	txid2 := raw(golden(t, goldenN2N, "GenTxId_Babbage"))
	body := raw(golden(t, goldenN2N, "GenTx_Conway"))
	indef := func(tag byte, items ...[]byte) []byte {
		b := []byte{0x82, tag, 0x9f}
		for _, it := range items {
			b = append(b, it...)
		}
		return append(b, 0xff)
	}
	enc := func(v any) []byte { return netEnc(t, v) }
	return []entry{
		tx("init", "client opens the protocol", "StInit", netMsg(t, 6)),
		tx("request_tx_ids_blocking", "blocking request: acknowledge 3, request 5", "StIdle", netMsg(t, 0, true, 3, 5)),
		tx("request_tx_ids_non_blocking", "non-blocking request with no acknowledgement", "StIdle", netMsg(t, 0, false, 0, 10)),
		tx("request_tx_ids_max", "the Word16 maximum for both counts", "StIdle", netMsg(t, 0, false, 65535, 65535)),
		tx("reply_tx_ids_non_blocking", "ids with sizes, as the reference writes them (indefinite list)", "StTxIdsNonBlocking",
			indef(1, enc([]any{txid, 100}), enc([]any{txid2, 200}))),
		tx("reply_tx_ids_non_blocking_empty", "an empty non-blocking reply is legal", "StTxIdsNonBlocking", []byte{0x82, 0x01, 0x9f, 0xff}),
		tx("reply_tx_ids_blocking", "a blocking reply carries at least one id", "StTxIdsBlocking", indef(1, enc([]any{txid, 100}))),
		tx("request_txs", "ask for two transactions", "StIdle", indef(2, enc(txid), enc(txid2))),
		tx("reply_txs", "one transaction body", "StTxs", indef(3, enc(body))),
		tx("reply_txs_empty", "no transactions", "StTxs", []byte{0x82, 0x03, 0x9f, 0xff}),
		tx("done", "done is legal after a blocking request", "StTxIdsBlocking", netMsg(t, 4)),
		tx("reject_reply_tx_ids_blocking_empty", "a blocking reply must not be empty", "StTxIdsBlocking", []byte{0x82, 0x01, 0x9f, 0xff}),
		tx("reject_reply_tx_ids_blocking_empty_definite", "a blocking reply must not be empty (definite list)", "StTxIdsBlocking", netMsg(t, 1, []any{})),
		tx("reply_tx_ids_definite_list", "the reference decodes these lists only when indefinite", "StTxIdsNonBlocking", netMsg(t, 1, []any{[]any{txid, 100}})),
		tx("request_txs_definite_list", "the reference decodes these lists only when indefinite", "StIdle", netMsg(t, 2, []any{txid})),
		tx("reply_txs_definite_list", "the reference decodes these lists only when indefinite", "StTxs", netMsg(t, 3, []any{body})),
		tx("reject_done_in_non_blocking", "done is only legal in the blocking state", "StTxIdsNonBlocking", netMsg(t, 4)),
		tx("reject_request_in_init", "StInit expects MsgInit", "StInit", netMsg(t, 0, true, 0, 1)),
		tx("reject_init_in_idle", "MsgInit is only legal once", "StIdle", netMsg(t, 6)),
		tx("reject_ack_too_big", "acknowledgement above Word16", "StIdle", netMsg(t, 0, true, 65536, 1)),
		tx("reject_unknown_tag", "tag 5 is not used", "StIdle", netMsg(t, 5)),
		tx("reject_done_state", "StDone has no agency", "StDone", netMsg(t, 4)),
		tx("reject_trailing", "bytes after the message", "StInit", append(netMsg(t, 6), 0x00)),
	}
}

func netStateQueryCases(t *testing.T) []entry {
	lsq := func(name, why string, v uint32, state string, msg, query []byte) entry {
		return netEntry("codec_local_state_query_"+name, why, netLSQ(netCodecReq(v, state, msg), query))
	}
	g := func(name string) []byte { return golden(t, goldenN2C, name) }
	out := []entry{
		lsq("acquire_point", "acquire a specific point", ntcV19, "StIdle", netMsg(t, 0, netPt(77, 0xaa)), nil),
		lsq("acquire_volatile_tip", "acquire the volatile tip", ntcV19, "StIdle", netMsg(t, 8), nil),
		lsq("acquire_immutable_tip", "acquiring the immutable tip needs version 16", ntcV16, "StIdle", netMsg(t, 10), nil),
		lsq("acquired", "server confirms", ntcV19, "StAcquiring", netMsg(t, 1), nil),
		lsq("failure_too_old", "acquire failed: point too old", ntcV19, "StAcquiring", netMsg(t, 2, 0), nil),
		lsq("failure_not_on_chain", "acquire failed: point not on chain", ntcV19, "StAcquiring", netMsg(t, 2, 1), nil),
		lsq("release", "client releases", ntcV19, "StAcquired", netMsg(t, 5), nil),
		lsq("reacquire_point", "reacquire at a point", ntcV23, "StAcquired", netMsg(t, 6, netPt(77, 0xaa)), nil),
		lsq("reacquire_volatile", "reacquire the volatile tip", ntcV23, "StAcquired", netMsg(t, 9), nil),
		lsq("reacquire_immutable", "reacquire the immutable tip", ntcV23, "StAcquired", netMsg(t, 11), nil),
		lsq("done", "client terminates", ntcV19, "StIdle", netMsg(t, 7), nil),
		lsq("reject_failure_code", "failure code 2 does not exist", ntcV19, "StAcquiring", netMsg(t, 2, 2), nil),
		lsq("reject_query_in_idle", "a query needs an acquired state", ntcV19, "StIdle", netMsg(t, 3, raw(g("Query_Conway_GetEpochNo"))), nil),
		lsq("reject_release_in_idle", "release is only legal when acquired", ntcV19, "StIdle", netMsg(t, 5), nil),
		lsq("reject_done_state", "StDone has no agency", ntcV19, "StDone", netMsg(t, 7), nil),
		lsq("reject_unknown_tag", "tag 12 does not exist", ntcV19, "StIdle", netMsg(t, 12), nil),
		lsq("reject_trailing", "bytes after the message", ntcV19, "StAcquired", append(netMsg(t, 5), 0), nil),
		lsq("unsupported_n2n_version", "local state query is node-to-client only", ntnV14, "StIdle", netMsg(t, 7), nil),
	}
	// Queries and their results from the reference's own golden files.
	type qr struct{ name, query, result string }
	for _, c := range []qr{
		{"epoch_no", "Query_Conway_GetEpochNo", "Result_Conway_EpochNo"},
		{"ledger_tip", "Query_Conway_GetLedgerTip", "Result_Conway_LedgerTip"},
		{"current_pparams_babbage", "Query_Babbage_GetCurrentPParams", "Result_Babbage_EmptyPParams"},
		{"genesis_config_shelley", "Query_Shelley_GetGenesisConfig", "Result_Shelley_GenesisConfig"},
		{"non_myopic_rewards_alonzo", "Query_Alonzo_GetNonMyopicMemberRewards", "Result_Alonzo_NonMyopicMemberRewards"},
		{"stake_distribution2_conway", "Query_Conway_GetStakeDistribution2", "Result_Conway_StakeDistribution2"},
		{"max_major_protocol_version_mary", "Query_Mary_GetMaxMajorProtocolVersion", "Result_Mary_MaxMajorProtocolVersion"},
		{"hard_fork", "Query_HardFork", "Result_HardFork"},
		{"byron", "Query_Byron", "Result_Byron"},
	} {
		q, r := g(c.query), g(c.result)
		out = append(out,
			lsq("query_"+c.name, "MsgQuery carrying "+c.query, ntcV19, "StAcquired", netMsg(t, 3, raw(q)), nil),
			lsq("result_"+c.name, "MsgResult decoded by the result type of "+c.query, ntcV19, "StQuerying", netMsg(t, 4, raw(r)), q),
		)
	}
	out = append(out,
		lsq("query_system_start", "GetSystemStart is the bare tag 1", ntcV19, "StAcquired", netMsg(t, 3, []any{1}), nil),
		lsq("query_chain_block_no", "GetChainBlockNo is the bare tag 2", ntcV19, "StAcquired", netMsg(t, 3, []any{2}), nil),
		lsq("query_chain_point", "GetChainPoint is the bare tag 3", ntcV19, "StAcquired", netMsg(t, 3, []any{3}), nil),
		lsq("query_anytime_shelley", "QueryAnytime GetEraStart", ntcV19, "StAcquired", netMsg(t, 3, raw(g("Query_AnytimeShelley"))), nil),
		lsq("query_anytime_byron", "QueryAnytime GetEraStart for Byron", ntcV19, "StAcquired", netMsg(t, 3, raw(g("Query_AnytimeByron"))), nil),
		lsq("query_debug_ledger_config", "DebugLedgerConfig is the bare tag 4", ntcV23, "StAcquired", netMsg(t, 3, []any{4}), nil),
		lsq("reject_query_unknown", "query tag 99 does not exist", ntcV19, "StAcquired", netMsg(t, 3, []any{99}), nil),
		lsq("reject_result_bad_query", "the query in flight does not decode", ntcV19, "StQuerying", netMsg(t, 4, raw(g("Result_Conway_EpochNo"))), []byte{0xff}),
	)
	return out
}

func netLocalTxCases(t *testing.T) []entry {
	lts := func(name, why string, state string, msg []byte) entry {
		return netEntry("codec_local_tx_submission_"+name, why, netLTS(netCodecReq(ntcV19, state, msg)))
	}
	ltm := func(name, why string, v uint32, state string, msg []byte) entry {
		return netEntry("codec_local_tx_monitor_"+name, why, netLTM(netCodecReq(v, state, msg)))
	}
	g := func(name string) []byte { return golden(t, goldenN2C, name) }
	out := []entry{
		lts("accept", "server accepts", "StBusy", netMsg(t, 1)),
		lts("done", "client terminates", "StIdle", netMsg(t, 3)),
		lts("reject_accept_in_idle", "accept is a server message", "StIdle", netMsg(t, 1)),
		lts("reject_submit_in_busy", "submit is a client message", "StBusy", netMsg(t, 0, raw(g("GenTx_Conway")))),
		lts("reject_done_state", "StDone has no agency", "StDone", netMsg(t, 3)),
		lts("reject_unknown_tag", "tag 4 does not exist", "StIdle", netMsg(t, 4)),
		lts("reject_trailing", "bytes after the message", "StBusy", append(netMsg(t, 1), 0)),
		ltm("acquire", "client acquires a snapshot", ntcV19, "StIdle", netMsg(t, 1)),
		ltm("acquired", "server reports the snapshot slot", ntcV19, "StAcquiring", netMsg(t, 2, 1234)),
		ltm("await_acquire", "the same wire message in StAcquired is MsgAwaitAcquire", ntcV19, "StAcquired", netMsg(t, 1)),
		ltm("release", "client releases", ntcV19, "StAcquired", netMsg(t, 3)),
		ltm("next_tx", "client asks for the next transaction", ntcV19, "StAcquired", netMsg(t, 5)),
		ltm("reply_next_tx_none", "the mempool is exhausted", ntcV19, "StBusyNextTx", netMsg(t, 6)),
		ltm("get_sizes", "client asks for the mempool sizes", ntcV19, "StAcquired", netMsg(t, 9)),
		ltm("reply_get_sizes", "capacity, size and count", ntcV19, "StBusyGetSizes", netMsg(t, 10, []any{65536, 1200, 3})),
		ltm("reply_has_tx_true", "the transaction is in the snapshot", ntcV19, "StBusyHasTx", netMsg(t, 8, true)),
		ltm("reply_has_tx_false", "the transaction is not in the snapshot", ntcV19, "StBusyHasTx", netMsg(t, 8, false)),
		ltm("done", "client terminates from StIdle", ntcV19, "StIdle", netMsg(t, 0)),
		ltm("get_measures", "MsgGetMeasures is tag 11", ntcV23, "StAcquired", netMsg(t, 11)),
		ltm("reply_get_measures", "transaction count and a map of measures", ntcV23, "StBusyGetMeasures", netMsg(t, 12, 3, map[string][]any{"transaction_bytes": {1200, 65536}})),
		ltm("reject_done_in_acquired", "done is only legal in StIdle", ntcV19, "StAcquired", netMsg(t, 0)),
		ltm("reject_acquired_in_idle", "acquired is a server message", ntcV19, "StIdle", netMsg(t, 2, 1)),
		ltm("reject_done_state", "StDone has no agency", ntcV19, "StDone", netMsg(t, 0)),
		ltm("reject_unknown_tag", "tag 4 does not exist", ntcV19, "StAcquired", netMsg(t, 4)),
		ltm("reject_trailing", "bytes after the message", ntcV19, "StIdle", append(netMsg(t, 1), 0)),
		ltm("unsupported_n2n_version", "local tx monitor is node-to-client only", ntnV14, "StIdle", netMsg(t, 1)),
	}
	for _, era := range []string{"Byron", "Shelley", "Allegra", "Mary", "Alonzo", "Babbage", "Conway"} {
		out = append(out,
			lts("submit_"+era, "submit a "+era+" transaction", "StIdle", netMsg(t, 0, raw(g("GenTx_"+era)))),
			lts("reject_"+era, "reject carrying the reference's "+era+" ApplyTxErr", "StBusy", netMsg(t, 2, raw(g("ApplyTxErr_"+era)))),
		)
	}
	for _, era := range []string{"Conway", "Babbage", "Byron"} {
		out = append(out,
			ltm("has_tx_"+era, "client asks whether a "+era+" transaction id is in the snapshot", ntcV19, "StAcquired", netMsg(t, 7, raw(g("GenTxId_"+era)))),
			ltm("reply_next_tx_"+era, "server returns a "+era+" transaction", ntcV19, "StBusyNextTx", netMsg(t, 6, raw(g("GenTx_"+era)))),
		)
	}
	return out
}

func netHandshakeCases(t *testing.T) []entry {
	hs := func(name, why string, v uint32, state string, msg []byte) entry {
		return netEntry("codec_handshake_"+name, why, netHS(netCodecReq(v, state, msg)))
	}
	const magic = 764824073
	ntn := func(peerSharing int, query bool) []any { return []any{magic, false, peerSharing, query} }
	ntn16 := func(peras bool) []any { return []any{magic, true, 1, false, peras} }
	ntc := func(query bool) []any { return []any{magic, query} }
	ordered := func(items ...[2]any) []byte {
		b := []byte{0x82, 0x00, 0xa0 | byte(len(items))}
		for _, it := range items {
			b = append(b, netEnc(t, it[0])...)
			b = append(b, netEnc(t, it[1])...)
		}
		return b
	}
	return []entry{
		hs("propose_n2n", "versions 14 to 16 in ascending order", ntnV14, "StPropose",
			ordered([2]any{14, ntn(0, false)}, [2]any{15, ntn(1, false)}, [2]any{16, ntn16(true)})),
		hs("propose_n2n_single", "a single version", ntnV14, "StPropose", ordered([2]any{14, ntn(1, true)})),
		hs("propose_n2c", "node-to-client versions in wire form", ntcV19, "StPropose",
			ordered([2]any{0x8000 | 16, ntc(false)}, [2]any{0x8000 | 19, ntc(true)}, [2]any{0x8000 | 23, ntc(false)})),
		hs("propose_n2n_unknown_skipped", "version 99 is skipped, 14 is kept", ntnV14, "StPropose",
			ordered([2]any{14, ntn(0, false)}, [2]any{99, []any{1}})),
		hs("propose_n2n_old_skipped", "version 13 is not recognised and is skipped", ntnV14, "StPropose",
			ordered([2]any{13, ntn(0, false)}, [2]any{14, ntn(0, false)})),
		hs("reply_versions", "tag 0 in StConfirm is the reference's MsgReplyVersions (simultaneous open)", ntnV14, "StConfirm", ordered([2]any{14, ntn(0, false)})),
		hs("accept_n2n", "server accepts version 16", ntnV16, "StConfirm", netMsg(t, 1, 16, ntn16(false))),
		hs("accept_n2c", "server accepts a node-to-client version", ntcV19, "StConfirm", netMsg(t, 1, 0x8000|19, ntc(true))),
		hs("query_reply", "a query reply lists the supported versions", ntnV14, "StConfirm", netEnc(t, []any{3, map[any]any{14: ntn(0, true)}})),
		hs("refuse_version_mismatch", "refusal with the supported versions", ntnV14, "StConfirm", netMsg(t, 2, []any{0, []any{14, 15}})),
		hs("refuse_decode_error", "refusal: version data did not decode", ntnV14, "StConfirm", netMsg(t, 2, []any{1, 14, "bad data"})),
		hs("refuse_refused", "refusal: refused by policy", ntnV14, "StConfirm", netMsg(t, 2, []any{2, 14, "magic mismatch"})),
		hs("reject_propose_in_confirm_state_done", "StDone has no agency", ntnV14, "StDone", netMsg(t, 1, 14, ntn(0, false))),
		hs("reject_accept_in_propose", "accept is a server message", ntnV14, "StPropose", netMsg(t, 1, 14, ntn(0, false))),
		hs("reject_unknown_tag", "tag 4 does not exist", ntnV14, "StPropose", netMsg(t, 4)),
		hs("reject_unordered_versions", "versions must be ascending", ntnV14, "StPropose",
			ordered([2]any{15, ntn(0, false)}, [2]any{14, ntn(0, false)})),
		hs("reject_accept_unknown_version", "an accepted version must be one the codec knows", ntnV14, "StConfirm", netMsg(t, 1, 99, ntn(0, false))),
		hs("reject_bad_peer_sharing", "peer sharing 2 is neither disabled nor enabled", ntnV14, "StPropose", ordered([2]any{14, ntn(2, false)})),
		hs("reject_short_version_data", "version data with three items", ntnV14, "StPropose", ordered([2]any{14, []any{magic, false, 0}})),
		hs("reject_trailing", "bytes after the message", ntnV14, "StConfirm", append(netMsg(t, 1, 14, ntn(0, false)), 0)),
	}
}

func netSmallProtocolCases(t *testing.T) []entry {
	ka := func(name, why string, state string, msg []byte) entry {
		return netEntry("codec_keep_alive_"+name, why, netKA(netCodecReq(ntnV14, state, msg)))
	}
	ps := func(name, why string, state string, msg []byte) entry {
		return netEntry("codec_peer_sharing_"+name, why, netPS(netCodecReq(ntnV14, state, msg)))
	}
	v4 := func(a, b, c, d byte, port uint16) []any {
		// The reference writes an IPv4 address as one word in host order.
		return []any{0, binary.LittleEndian.Uint32([]byte{a, b, c, d}), port}
	}
	v6 := func(port uint16, w ...uint32) []any {
		return []any{1, w[0], w[1], w[2], w[3], port}
	}
	return []entry{
		ka("keep_alive", "client ping with cookie 42", "StClient", netMsg(t, 0, 42)),
		ka("keep_alive_max_cookie", "the largest Word16 cookie", "StClient", netMsg(t, 0, 65535)),
		ka("response", "server echoes the cookie", "StServer", netMsg(t, 1, 256)),
		ka("done", "client terminates", "StClient", netMsg(t, 2)),
		ka("reject_response_in_client", "a server message in the client's state", "StClient", netMsg(t, 1, 1)),
		ka("reject_keep_alive_in_server", "a client message in the server's state", "StServer", netMsg(t, 0, 1)),
		ka("reject_cookie_too_big", "cookie above Word16", "StClient", netMsg(t, 0, 65536)),
		ka("reject_done_state", "StDone has no agency", "StDone", netMsg(t, 2)),
		ka("reject_unknown_tag", "tag 3 does not exist", "StClient", netMsg(t, 3)),
		ka("reject_trailing", "bytes after the message", "StClient", append(netMsg(t, 0, 1), 0)),
		ka("reject_missing_cookie", "MsgKeepAlive without a cookie", "StClient", netMsg(t, 0)),
		ps("share_request", "ask for five peers", "StIdle", netMsg(t, 0, 5)),
		ps("share_request_zero", "ask for none", "StIdle", netMsg(t, 0, 0)),
		ps("share_request_max", "the largest amount", "StIdle", netMsg(t, 0, 255)),
		ps("share_peers_empty", "no peers to share", "StBusy", netMsg(t, 1, []any{})),
		ps("share_peers_v4", "one IPv4 peer", "StBusy", netMsg(t, 1, []any{v4(192, 0, 2, 7, 3001)})),
		ps("share_peers_v6", "one IPv6 peer, four big-endian words", "StBusy", netMsg(t, 1, []any{v6(3001, 0x20010db8, 0, 0, 1)})),
		ps("share_peers_mixed", "an IPv4 and an IPv6 peer", "StBusy", netMsg(t, 1, []any{v4(10, 0, 0, 1, 6000), v6(6001, 0, 0, 0, 1)})),
		ps("done", "client terminates", "StIdle", netMsg(t, 2)),
		ps("reject_amount_too_big", "amount above Word8", "StIdle", netMsg(t, 0, 256)),
		ps("reject_peers_in_idle", "peers are a server message", "StIdle", netMsg(t, 1, []any{})),
		ps("reject_request_in_busy", "request is a client message", "StBusy", netMsg(t, 0, 1)),
		ps("reject_done_state", "StDone has no agency", "StDone", netMsg(t, 2)),
		ps("reject_unknown_address_kind", "address kind 2 does not exist", "StBusy", netMsg(t, 1, []any{[]any{2, 1, 2}})),
		ps("reject_trailing", "bytes after the message", "StIdle", append(netMsg(t, 0, 1), 0)),
	}
}

func netMuxCases() []entry {
	hdr := func(ts uint32, proto, length uint16) []byte {
		b := make([]byte, 8)
		binary.BigEndian.PutUint32(b, ts)
		binary.BigEndian.PutUint16(b[4:], proto)
		binary.BigEndian.PutUint16(b[6:], length)
		return b
	}
	mux := func(name, why string, h []byte) entry { return netEntry("codec_mux_sdu_"+name, why, netMux(h)) }
	return []entry{
		mux("initiator", "initiator SDU for ChainSync", hdr(0x01020304, 2, 513)),
		mux("responder", "the high bit marks the responder direction", hdr(7, 0x8000|10, 1)),
		mux("max_length", "the largest payload length", hdr(0xffffffff, 0x7fff, 0xffff)),
		mux("handshake", "protocol number 0", hdr(0, 0, 100)),
		mux("trailing_payload", "bytes after the eighth are ignored", append(hdr(1, 3, 4), 1, 2, 3, 4)),
		mux("reject_zero_length", "an SDU announcing no payload is refused", hdr(7, 2, 0)),
		mux("reject_short", "seven bytes", hdr(1, 2, 3)[:7]),
		mux("reject_empty", "no bytes", nil),
	}
}

type netTr struct {
	p          v1.MiniProtocol
	state, msg string
	why        string
}

func netAgencyCases() []entry {
	cs, bf, tx := v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC, v1.MiniProtocol_MINI_PROTOCOL_BLOCK_FETCH, v1.MiniProtocol_MINI_PROTOCOL_TX_SUBMISSION2
	lsq, lts, ltm := v1.MiniProtocol_MINI_PROTOCOL_LOCAL_STATE_QUERY, v1.MiniProtocol_MINI_PROTOCOL_LOCAL_TX_SUBMISSION, v1.MiniProtocol_MINI_PROTOCOL_LOCAL_TX_MONITOR
	hs, ka, ps := v1.MiniProtocol_MINI_PROTOCOL_HANDSHAKE, v1.MiniProtocol_MINI_PROTOCOL_KEEP_ALIVE, v1.MiniProtocol_MINI_PROTOCOL_PEER_SHARING
	names := map[v1.MiniProtocol]string{cs: "chain_sync", bf: "block_fetch", tx: "tx_submission2", lsq: "local_state_query", lts: "local_tx_submission", ltm: "local_tx_monitor", hs: "handshake", ka: "keep_alive", ps: "peer_sharing"}
	legal := []netTr{
		{cs, "StIdle", "MsgRequestNext", "server gets agency to answer"},
		{cs, "StIdle", "MsgFindIntersect", "server answers the intersection"},
		{cs, "StIdle", "MsgDone", "protocol ends"},
		{cs, "StCanAwait", "MsgAwaitReply", "server asks the client to wait"},
		{cs, "StCanAwait", "MsgRollForward", "server answers at once"},
		{cs, "StCanAwait", "MsgRollBackward", "server rolls back at once"},
		{cs, "StMustReply", "MsgRollForward", "server's delayed answer"},
		{cs, "StMustReply", "MsgRollBackward", "server's delayed rollback"},
		{cs, "StIntersect", "MsgIntersectFound", "intersection found"},
		{cs, "StIntersect", "MsgIntersectNotFound", "intersection not found"},
		{bf, "BFIdle", "MsgRequestRange", "server must start streaming"},
		{bf, "BFIdle", "MsgClientDone", "protocol ends"},
		{bf, "BFBusy", "MsgStartBatch", "server streams blocks"},
		{bf, "BFBusy", "MsgNoBlocks", "range unavailable"},
		{bf, "BFStreaming", "MsgBlock", "streaming continues"},
		{bf, "BFStreaming", "MsgBatchDone", "batch ends"},
		{tx, "StInit", "MsgInit", "client agency in StIdle"},
		{tx, "StIdle", "MsgRequestTxIdsBlocking", "the blocking request"},
		{tx, "StIdle", "MsgRequestTxIdsNonBlocking", "the non-blocking request"},
		{tx, "StIdle", "MsgRequestTxs", "ask for bodies"},
		{tx, "StTxIdsBlocking", "MsgReplyTxIds", "blocking reply"},
		{tx, "StTxIdsNonBlocking", "MsgReplyTxIds", "non-blocking reply"},
		{tx, "StTxIdsBlocking", "MsgDone", "only the blocking state may end"},
		{tx, "StTxs", "MsgReplyTxs", "bodies delivered"},
		{lsq, "StIdle", "MsgAcquire", "server acquires"},
		{lsq, "StIdle", "MsgDone", "protocol ends"},
		{lsq, "StAcquiring", "MsgAcquired", "acquired"},
		{lsq, "StAcquiring", "MsgFailure", "acquire failed"},
		{lsq, "StAcquired", "MsgQuery", "server answers"},
		{lsq, "StAcquired", "MsgReAcquire", "reacquire"},
		{lsq, "StAcquired", "MsgRelease", "release"},
		{lsq, "StQuerying", "MsgResult", "result returns to acquired"},
		{lts, "StIdle", "MsgSubmitTx", "server decides"},
		{lts, "StIdle", "MsgDone", "protocol ends"},
		{lts, "StBusy", "MsgAcceptTx", "accepted"},
		{lts, "StBusy", "MsgRejectTx", "rejected"},
		{ltm, "StIdle", "MsgAcquire", "server acquires"},
		{ltm, "StIdle", "MsgDone", "protocol ends"},
		{ltm, "StAcquiring", "MsgAcquired", "acquired"},
		{ltm, "StAcquired", "MsgAwaitAcquire", "wait for a new snapshot"},
		{ltm, "StAcquired", "MsgRelease", "release"},
		{ltm, "StAcquired", "MsgNextTx", "busy with the next-tx query"},
		{ltm, "StAcquired", "MsgHasTx", "busy with the has-tx query"},
		{ltm, "StAcquired", "MsgGetSizes", "busy with the sizes query"},
		{ltm, "StBusyNextTx", "MsgReplyNextTx", "reply to next-tx"},
		{ltm, "StBusyHasTx", "MsgReplyHasTx", "reply to has-tx"},
		{ltm, "StBusyGetSizes", "MsgReplyGetSizes", "reply to sizes"},
		{hs, "StPropose", "MsgProposeVersions", "server chooses"},
		{hs, "StConfirm", "MsgAcceptVersion", "accepted"},
		{hs, "StConfirm", "MsgRefuse", "refused"},
		{hs, "StConfirm", "MsgQueryReply", "query reply"},
		{ka, "StClient", "MsgKeepAlive", "server answers"},
		{ka, "StServer", "MsgKeepAliveResponse", "client agency again"},
		{ka, "StClient", "MsgDone", "protocol ends"},
		{ps, "StIdle", "MsgShareRequest", "server answers"},
		{ps, "StBusy", "MsgSharePeers", "client agency again"},
		{ps, "StIdle", "MsgDone", "protocol ends"},
	}
	illegal := []netTr{
		{cs, "StIdle", "MsgAwaitReply", "a server message in the client's state"},
		{cs, "StDone", "MsgDone", "nobody has agency in StDone"},
		{cs, "StCanAwait", "MsgRequestNext", "a client message in the server's state"},
		{cs, "StIdle", "MsgNoSuchMessage", "an unknown message name"},
		{bf, "BFIdle", "MsgBlock", "a block needs a batch"},
		{bf, "BFStreaming", "MsgRequestRange", "no request while streaming"},
		{tx, "StIdle", "MsgInit", "init is only legal once"},
		{tx, "StTxIdsNonBlocking", "MsgDone", "done needs the blocking state"},
		{lsq, "StIdle", "MsgQuery", "query needs an acquired state"},
		{lsq, "StQuerying", "MsgRelease", "release waits for the result"},
		{lts, "StBusy", "MsgDone", "done is a client message"},
		{ltm, "StAcquired", "MsgReplyHasTx", "a reply without a query"},
		{hs, "StPropose", "MsgAcceptVersion", "accept is a server message"},
		{ka, "StServer", "MsgKeepAlive", "ping in the server's state"},
		{ps, "StBusy", "MsgShareRequest", "request in the server's state"},
	}
	var out []entry
	for _, c := range legal {
		out = append(out, netEntry(fmt.Sprintf("agency_%s_%s_%s", names[c.p], c.state, c.msg), c.why, netAg(c.p, c.state, c.msg)))
	}
	for _, c := range illegal {
		out = append(out, netEntry(fmt.Sprintf("agency_illegal_%s_%s_%s", names[c.p], c.state, c.msg), c.why, netAg(c.p, c.state, c.msg)))
	}
	return out
}

func netDivergenceCases(t *testing.T) []entry { return netSplit(netAllCases(t), true) }
