package main

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"sort"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/muxer"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/handshake"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/txsubmission"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

var netHandshake = &netProtocol{
	ntn: true, ntc: true,
	states: map[string]string{"StPropose": "Propose", "StConfirm": "Confirm", "StDone": "Done"},
	stateMap: func(ntc bool) protocol.StateMap {
		if ntc {
			return handshake.StateMapNtC
		}
		return handshake.StateMapNtN
	},
	decode: func(_ bool, t uint, d []byte) (protocol.Message, error) {
		return handshake.NewMsgFromCbor(t, d)
	},
	names: map[uint8]string{
		0: "MsgProposeVersions", 1: "MsgAcceptVersion", 2: "MsgRefuse", 3: "MsgQueryReply",
	},
}

// netKnownVersion reports whether a wire version number is in the domain of
// the family: the reference version codec skips numbers it does not know.
func netKnownVersion(ntc bool, v uint64) bool {
	if ntc {
		return v&netNtCFlag != 0 && v&^netNtCFlag >= netMinNtC && v&^netNtCFlag <= netMaxNtC && v < 1<<16
	}
	return v >= netMinNtN && v <= netMaxNtN
}

func netHandshakeVersion(ntc bool, v uint16, data []byte) (*v1.HandshakeVersion, error) {
	vd, err := protocol.GetProtocolVersion(v).NewVersionDataFromCborFunc(data)
	if err != nil {
		return nil, err
	}
	out := &v1.HandshakeVersion{Version: uint32(v), NetworkMagic: vd.NetworkMagic(), Query: vd.Query()}
	if !ntc {
		out.InitiatorOnly = vd.DiffusionMode()
		if vd.PeerSharing() {
			out.PeerSharing = 1
		}
	}
	return out, nil
}

func netCodecHandshake(_ context.Context, req *v1.NetCodecHandshakeRequest) (*v1.NetCodecHandshakeResponse, error) {
	d, rej, err := netDecode(netHandshake, req.GetCodec())
	if err != nil || rej != nil {
		return &v1.NetCodecHandshakeResponse{Rejection: rej}, err
	}
	// The reference version codec skips version numbers it does not know, so
	// its decoded map holds only the known ones; drop the same entries
	// before re-encoding.
	prune := func(m map[uint16]cbor.RawMessage) {
		for k := range m {
			if !netKnownVersion(d.ntc, uint64(k)) {
				delete(m, k)
			}
		}
	}
	switch m := d.msg.(type) {
	case *handshake.MsgProposeVersions:
		prune(m.VersionMap)
	case *handshake.MsgQueryReply:
		prune(m.VersionMap)
	}
	enc, err := netReencode(d.msg)
	if err != nil {
		return &v1.NetCodecHandshakeResponse{Rejection: netRejection("DingoError", err)}, nil
	}
	resp := &v1.NetCodecHandshakeResponse{MessageType: d.name, Cbor: enc}
	var failed *v1.Rejection
	add := func(v uint16, data []byte) {
		hv, err := netHandshakeVersion(d.ntc, v, data)
		if err != nil {
			failed = netRejection(netTagKey, err)
			return
		}
		resp.Versions = append(resp.Versions, hv)
	}
	versionMap := func(m map[uint16]cbor.RawMessage) {
		keys := make([]int, 0, len(m))
		for k := range m {
			keys = append(keys, int(k))
		}
		sort.Ints(keys)
		for _, k := range keys {
			if add(uint16(k), m[uint16(k)]); failed != nil { //nolint:gosec // bounded by netKnownVersion
				return
			}
		}
	}
	switch m := d.msg.(type) {
	case *handshake.MsgProposeVersions:
		versionMap(m.VersionMap)
	case *handshake.MsgQueryReply:
		versionMap(m.VersionMap)
	case *handshake.MsgAcceptVersion:
		if !netKnownVersion(d.ntc, uint64(m.Version)) {
			failed = netRejection(netTagKey, fmt.Errorf("version %d not recognized", m.Version))
		} else {
			add(m.Version, m.VersionData)
		}
	}
	if failed != nil {
		return &v1.NetCodecHandshakeResponse{Rejection: failed}, nil
	}
	return resp, nil
}

func netCodecMuxSdu(_ context.Context, req *v1.NetCodecMuxSduRequest) (*v1.NetCodecMuxSduResponse, error) {
	h, err := muxer.DecodeSegmentHeader(req.GetHeader())
	if err != nil {
		return &v1.NetCodecMuxSduResponse{Rejection: netRejection(netTagKey, err)}, nil
	}
	mode := v1.MuxMode_MUX_MODE_INITIATOR
	if h.IsResponse() {
		mode = v1.MuxMode_MUX_MODE_RESPONDER
	}
	return &v1.NetCodecMuxSduResponse{
		Timestamp: h.Timestamp,
		Mode:      mode,
		Protocol:  uint32(h.GetProtocolId()),
		Length:    uint32(h.PayloadLength),
	}, nil
}

// netAgencyProtocols maps the envelope enum onto the protocol tables. The
// state machines of ChainSync and Handshake have the same transitions in both
// families, so the node-to-node one answers.
func netAgencyProtocol(p v1.MiniProtocol) *netProtocol {
	switch p {
	case v1.MiniProtocol_MINI_PROTOCOL_CHAIN_SYNC:
		return netChainSync
	case v1.MiniProtocol_MINI_PROTOCOL_BLOCK_FETCH:
		return netBlockFetch
	case v1.MiniProtocol_MINI_PROTOCOL_TX_SUBMISSION2:
		return netTxSubmission2
	case v1.MiniProtocol_MINI_PROTOCOL_LOCAL_STATE_QUERY:
		return netLocalStateQuery
	case v1.MiniProtocol_MINI_PROTOCOL_LOCAL_TX_SUBMISSION:
		return netLocalTxSubmission
	case v1.MiniProtocol_MINI_PROTOCOL_LOCAL_TX_MONITOR:
		return netLocalTxMonitor
	case v1.MiniProtocol_MINI_PROTOCOL_HANDSHAKE:
		return netHandshake
	case v1.MiniProtocol_MINI_PROTOCOL_KEEP_ALIVE:
		return netKeepAlive
	case v1.MiniProtocol_MINI_PROTOCOL_PEER_SHARING:
		return netPeerSharing
	}
	return nil
}

// netAgencyMessage resolves a Haskell constructor name to the gouroboros
// message type, and a probe message for transitions that depend on content.
// MsgRequestTxIds carries its blocking style as a suffix, and the
// node-to-client MsgAwaitAcquire is the Acquire message sent in StAcquired.
func netAgencyMessage(p *netProtocol, name string) (uint8, protocol.Message, bool) {
	switch name {
	case "MsgAwaitAcquire":
		return netAgencyMessage(p, "MsgAcquire")
	case "MsgRequestTxIdsBlocking":
		return txsubmission.MessageTypeRequestTxIds, &txsubmission.MsgRequestTxIds{Blocking: true}, true
	case "MsgRequestTxIdsNonBlocking":
		return txsubmission.MessageTypeRequestTxIds, &txsubmission.MsgRequestTxIds{}, true
	}
	var best uint8
	found := false
	for t, n := range p.names {
		if n == name && (!found || t < best) {
			best, found = t, true
		}
	}
	return best, nil, found
}

func netAgency(_ context.Context, req *v1.NetAgencyRequest) (*v1.NetAgencyResponse, error) {
	p := netAgencyProtocol(req.GetProtocol())
	if p == nil {
		return nil, dispatch.BadRequest(errors.New("protocol unspecified"))
	}
	goState, err := netState(p, req.GetState())
	if err != nil {
		return nil, err
	}
	illegal := func(err error) (*v1.NetAgencyResponse, error) {
		return &v1.NetAgencyResponse{Rejection: netRejection(netIllegalTr, err)}, nil
	}
	sm := p.stateMap(false)
	state, ok := netFindState(sm, goState)
	if !ok {
		return illegal(fmt.Errorf("state %s has no transitions", req.GetState()))
	}
	msgType, probe, ok := netAgencyMessage(p, req.GetMessageType())
	if !ok {
		return illegal(fmt.Errorf("unknown message %q", req.GetMessageType()))
	}
	next, agency, err := protocol.NextState(sm, nil, state, msgType, probe)
	if err != nil {
		return illegal(err)
	}
	resp := &v1.NetAgencyResponse{NextState: netHaskellState(p, next.Name, req.GetMessageType())}
	switch agency {
	case protocol.AgencyClient:
		resp.Agency = v1.Agency_AGENCY_CLIENT
	case protocol.AgencyServer:
		resp.Agency = v1.Agency_AGENCY_SERVER
	default:
		resp.Agency = v1.Agency_AGENCY_NOBODY
	}
	return resp, nil
}

// netHaskellState inverts the state table. The reference splits the monitor's
// Busy state by the query in flight, which the message that entered it names.
func netHaskellState(p *netProtocol, goName, msg string) string {
	if p == netLocalTxMonitor && goName == "Busy" {
		switch msg {
		case "MsgNextTx":
			return "StBusyNextTx"
		case "MsgHasTx":
			return "StBusyHasTx"
		case "MsgGetSizes":
			return "StBusyGetSizes"
		}
	}
	var names []string
	for h, g := range p.states {
		if g == goName {
			names = append(names, h)
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		return goName
	}
	return names[0]
}

// netLeafNames maps a gouroboros Shelley query type to the reference
// constructor.
var netLeafNames = map[string]string{
	"ShelleyLedgerTipQuery":                           "GetLedgerTip",
	"ShelleyEpochNoQuery":                             "GetEpochNo",
	"ShelleyNonMyopicMemberRewardsQuery":              "GetNonMyopicMemberRewards",
	"ShelleyCurrentProtocolParamsQuery":               "GetCurrentPParams",
	"ShelleyProposedProtocolParamsUpdatesQuery":       "GetProposedPParamsUpdates",
	"ShelleyStakeDistributionQuery":                   "GetStakeDistribution",
	"ShelleyUtxoByAddressQuery":                       "GetUTxOByAddress",
	"ShelleyUtxoWholeQuery":                           "GetUTxOWhole",
	"ShelleyDebugEpochStateQuery":                     "DebugEpochState",
	"ShelleyCborQuery":                                "GetCBOR",
	"ShelleyFilteredDelegationAndRewardAccountsQuery": "GetFilteredDelegationsAndRewardAccounts",
	"ShelleyGenesisConfigQuery":                       "GetGenesisConfig",
	"ShelleyDebugNewEpochStateQuery":                  "DebugNewEpochState",
	"ShelleyDebugChainDepStateQuery":                  "DebugChainDepState",
	"ShelleyRewardProvenanceQuery":                    "GetRewardProvenance",
	"ShelleyUtxoByTxinQuery":                          "GetUTxOByTxIn",
	"ShelleyStakePoolsQuery":                          "GetStakePools",
	"ShelleyStakePoolParamsQuery":                     "GetStakePoolParams",
	"ShelleyRewardInfoPoolsQuery":                     "GetRewardInfoPools",
	"ShelleyPoolStateQuery":                           "GetPoolState",
	"ShelleyStakeSnapshotsQuery":                      "GetStakeSnapshots",
	"ShelleyPoolDistrQuery":                           "GetPoolDistr",
	"ShelleyStakeDelegDepositsQuery":                  "GetStakeDelegDeposits",
	"ShelleyConstitutionQuery":                        "GetConstitution",
	"ShelleyGovStateQuery":                            "GetGovState",
	"ShelleyDRepStateQuery":                           "GetDRepState",
	"ShelleyDRepStakeDistrQuery":                      "GetDRepStakeDistr",
	"ShelleyCommitteeMembersStateQuery":               "GetCommitteeMembersState",
	"ShelleyFilteredVoteDelegateesQuery":              "GetFilteredVoteDelegatees",
	"ShelleyAccountStateQuery":                        "GetAccountState",
	"ShelleySPOStakeDistrQuery":                       "GetSPOStakeDistr",
	"ShelleyGetProposalsQuery":                        "GetProposals",
	"ShelleyGetRatifyStateQuery":                      "GetRatifyState",
	"ShelleyPoolDistr2Query":                          "GetPoolDistr2",
	"ShelleyStakeDistribution2Query":                  "GetStakeDistribution2",
	"ShelleyMaxMajorProtocolVersionQuery":             "GetMaxMajorProtocolVersion",
	"ShelleyDRepDelegationsQuery":                     "GetDRepDelegations",
}

// netQueryPath names a decoded query by its reference constructor path.
func netQueryPath(q any) string {
	switch v := q.(type) {
	case *localstatequery.BlockQuery:
		switch inner := v.Query.(type) {
		case *localstatequery.ShelleyQuery:
			return "BlockQuery." + netLeaf(inner.Query)
		case *localstatequery.HardForkQuery:
			switch inner.Query.(type) {
			case *localstatequery.HardForkEraHistoryQuery:
				return "BlockQuery.QueryHardFork.GetInterpreter"
			case *localstatequery.HardForkCurrentEraQuery:
				return "BlockQuery.QueryHardFork.GetCurrentEra"
			}
		}
		return "BlockQuery"
	case *localstatequery.SystemStartQuery:
		return "GetSystemStart"
	case *localstatequery.ChainBlockNoQuery:
		return "GetChainBlockNo"
	case *localstatequery.ChainPointQuery:
		return "GetChainPoint"
	}
	return ""
}

func netLeaf(q any) string {
	t := reflect.TypeOf(q)
	for t != nil && t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	if t == nil {
		return ""
	}
	if n, ok := netLeafNames[t.Name()]; ok {
		return n
	}
	return strings.TrimSuffix(t.Name(), "Query")
}
