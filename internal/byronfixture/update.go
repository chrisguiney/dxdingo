package byronfixture

import (
	"github.com/blinklabs-io/gouroboros/ledger/byron"
	"golang.org/x/crypto/blake2b"
)

// ProposalSpec describes an update proposal. Absent options leave the
// parameter unchanged.
type ProposalSpec struct {
	// Version is the proposed protocol version (major, minor, alt).
	Version [3]uint64
	// MaxBlock, MaxTx and Script update the parameter when set.
	MaxBlock, MaxTx *uint64
	Script          *uint64
	// AppName and AppVer are the software version; the name defaults to
	// "csl-daedalus".
	AppName string
	AppVer  uint64
}

func optional(item []byte) []byte {
	if item == nil {
		return Array()
	}
	return Array(item)
}

func optUint(v *uint64) []byte {
	if v == nil {
		return nil
	}
	return Uint(*v)
}

// signed is the message a Byron signature covers: the sign tag, the protocol
// magic in CBOR and the signed bytes.
func signed(tag byte, magic uint32, body []byte) []byte {
	return cat([]byte{tag}, Uint(uint64(magic)), body)
}

// Proposal builds the update proposal CBOR signed by k under magic. A corrupt
// proposal has its signature changed in one bit.
func (k Key) Proposal(magic uint32, s ProposalSpec, corrupt bool) []byte {
	name := s.AppName
	if name == "" {
		name = "csl-daedalus"
	}
	mod := Array(
		optional(optUint(s.Script)), Array(), optional(optUint(s.MaxBlock)), Array(),
		optional(optUint(s.MaxTx)), Array(), Array(), Array(), Array(), Array(),
		Array(), Array(), Array(), Array(),
	)
	fields := [][]byte{
		Array(Uint(s.Version[0]), Uint(s.Version[1]), Uint(s.Version[2])),
		mod,
		Array(textString(name), Uint(s.AppVer)),
		EmptyMap, // metadata
		EmptyMap, // attributes
	}
	body := cat([]byte{0x85}, cat(fields...))
	sig := k.Sign(signed(byron.SignTagUSProposal, magic, body))
	if corrupt {
		sig[0] ^= 1
	}
	return Array(fields[0], fields[1], fields[2], fields[3], fields[4], Bytes(k.XPub), Bytes(sig))
}

func textString(s string) []byte {
	return cat(head(3, uint64(len(s))), []byte(s))
}

// ProposalID is the proposal's id: Blake2b-256 of its CBOR.
func ProposalID(proposal []byte) [32]byte {
	return blake2b.Sum256(proposal)
}

// Vote builds the vote of k for the proposal under magic, approving or not.
func (k Key) Vote(magic uint32, id [32]byte, approve bool) []byte {
	flag := []byte{0xf4}
	if approve {
		flag = []byte{0xf5}
	}
	idCbor := Bytes(id[:])
	msg := cat([]byte{0x82}, idCbor, flag)
	sig := k.Sign(signed(byron.SignTagUSVote, magic, msg))
	return Array(Bytes(k.XPub), idCbor, flag, Bytes(sig))
}

// UpdatePayload is a block's update payload: at most one proposal and the
// votes.
func UpdatePayload(proposal []byte, votes ...[]byte) []byte {
	var props [][]byte
	if proposal != nil {
		props = [][]byte{proposal}
	}
	// The reference's decoder requires the vote list indefinite-length.
	return Array(Array(props...), IndefArray(votes...))
}
