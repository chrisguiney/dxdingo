package main

import (
	"errors"
	"fmt"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Mini-protocol codecs. A request names the protocol state the receiver is
// in and carries one wire message. The adapter decodes it with the
// gouroboros message decoder, checks that the state machine accepts that
// message in that state (the reference codec decodes per state token, so a
// message legal elsewhere fails to decode), and re-encodes the decoded
// message with the gouroboros encoder.

// Negotiated version domain: the node-to-node versions 14 to 16 and the
// node-to-client versions 16 to 23, the latter written in wire form with the
// high bit set (32784 to 32791). Anything else is UNSUPPORTED on both nodes.
const (
	netNtCFlag   = 0x8000
	netMinNtN    = 14
	netMaxNtN    = 16
	netMinNtC    = 16
	netMaxNtC    = 23
	netTagKey    = "DeserialiseFailure"
	netIllegalTr = "IllegalTransition"
)

// netProtocol describes one mini-protocol in Haskell terms.
type netProtocol struct {
	// states maps a Haskell state name to the gouroboros state name.
	states map[string]string
	// stateMap returns the state machine; ntc selects the node-to-client
	// variant where gouroboros keeps two.
	stateMap func(ntc bool) protocol.StateMap
	// decode parses a message of an already known type.
	decode func(ntc bool, msgType uint, data []byte) (protocol.Message, error)
	// names maps a gouroboros message type to the Haskell constructor.
	names map[uint8]string
	// ntn and ntc say which wire families the protocol runs on.
	ntn, ntc bool
	// pass the message itself to NextState, for MatchFuncs that need it.
	matchMsg bool
}

// netVersion validates the negotiated version against the protocol's family
// and reports whether it is a node-to-client version.
func netVersion(p *netProtocol, version uint32) (bool, error) {
	if version&netNtCFlag != 0 {
		v := version &^ netNtCFlag
		if !p.ntc || v < netMinNtC || v > netMaxNtC {
			return false, dispatch.Unsupported(fmt.Errorf("node-to-client version %d", version))
		}
		return true, nil
	}
	if !p.ntn || version < netMinNtN || version > netMaxNtN {
		return false, dispatch.Unsupported(fmt.Errorf("node-to-node version %d", version))
	}
	return false, nil
}

func netState(p *netProtocol, name string) (string, error) {
	goName, ok := p.states[name]
	if !ok {
		return "", dispatch.BadRequest(fmt.Errorf("unknown state %q", name))
	}
	return goName, nil
}

func netFindState(m protocol.StateMap, name string) (protocol.State, bool) {
	for s := range m {
		if s.Name == name {
			return s, true
		}
	}
	return protocol.State{}, false
}

func netRejection(name string, err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "net", Name: name, Detail: err.Error()}}}
}

// netDecoded is a message accepted in a state.
type netDecoded struct {
	msg  protocol.Message
	name string
	ntc  bool
}

// netDecode runs the shared codec path. A nil result with a nil error means
// the message was rejected and the rejection is returned.
func netDecode(p *netProtocol, c *v1.NetCodecRequest) (*netDecoded, *v1.Rejection, error) {
	if c == nil {
		return nil, nil, dispatch.BadRequest(errors.New("codec request missing"))
	}
	ntc, err := netVersion(p, c.GetVersion())
	if err != nil {
		return nil, nil, err
	}
	goState, err := netState(p, c.GetState())
	if err != nil {
		return nil, nil, err
	}
	sm := p.stateMap(ntc)
	state, ok := netFindState(sm, goState)
	if !ok {
		return nil, nil, dispatch.Unsupported(fmt.Errorf("state %q does not exist in this version", c.GetState()))
	}
	id, err := cbor.DecodeIdFromList(c.GetMessage())
	if err != nil {
		return nil, netRejection(netTagKey, err), nil
	}
	msg, err := p.decode(ntc, uint(id), c.GetMessage()) //nolint:gosec // DecodeIdFromList returns a non-negative tag
	if err != nil {
		return nil, netRejection(netTagKey, err), nil
	}
	var probe protocol.Message
	if p.matchMsg {
		probe = msg
	}
	if _, _, err := protocol.NextState(sm, nil, state, msg.Type(), probe); err != nil {
		return nil, netRejection(netTagKey, err), nil
	}
	name, ok := p.names[msg.Type()]
	if !ok {
		return nil, netRejection(netTagKey, fmt.Errorf("message type %d", msg.Type())), nil
	}
	return &netDecoded{msg: msg, name: name, ntc: ntc}, nil, nil
}

// netReencode encodes the decoded message with the gouroboros encoder and
// drops the stored wire bytes first, so the result is a real re-encoding.
func netReencode(m protocol.Message) ([]byte, error) {
	m.SetCbor(nil)
	return cbor.Encode(m)
}

func registerNet(r *dispatch.Registry) {
	dispatch.Register(r, netCodecChainSync)
	dispatch.Register(r, netCodecBlockFetch)
	dispatch.Register(r, netCodecTxSubmission2)
	dispatch.Register(r, netCodecLocalStateQuery)
	dispatch.Register(r, netCodecLocalTxSubmission)
	dispatch.Register(r, netCodecLocalTxMonitor)
	dispatch.Register(r, netCodecHandshake)
	dispatch.Register(r, netCodecKeepAlive)
	dispatch.Register(r, netCodecPeerSharing)
	dispatch.Register(r, netCodecMuxSdu)
	dispatch.Register(r, netAgency)
}
