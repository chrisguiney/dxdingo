package ledgerfixture

import (
	"fmt"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/common"
)

// Praos (Babbage and Conway) header body fields that commit to the body: the
// body size and hash, and the protocol version as one [major, minor] element.
const (
	hdrBodySize = 6
	hdrBodyHash = 7
	hdrProtVer  = 9
	praosFields = 10
)

// TPraos (Shelley to Alonzo) header body fields: the protocol version is the
// last two elements, major then minor.
const (
	tpraosBodySize = 7
	tpraosBodyHash = 8
	tpraosProtMaj  = 13
	tpraosFields   = 15
)

// Edit breaks a block's header against its body.
type Edit struct {
	// SizeDelta is added to the body size the header declares.
	SizeDelta int
	// FlipHash changes the body hash the header declares.
	FlipHash bool
	// HeaderMajor, when set, is the protocol major version the header carries.
	HeaderMajor uint64
}

// Body is the transactions of a block, each a complete transaction CBOR.
type Body [][]byte

// parts are the body components in wire order: transaction bodies, witness
// sets, auxiliary data by index and, from Alonzo, the indexes of invalid
// transactions. A transaction of three elements (before Alonzo) has no
// is_valid flag.
func (b Body) parts() ([][]byte, error) {
	bodies, wits := []cbor.RawMessage{}, []cbor.RawMessage{}
	aux := map[uint]cbor.RawMessage{}
	invalid := []uint{}
	flagged := true
	for i, tx := range b {
		var f []cbor.RawMessage
		if _, err := cbor.Decode(tx, &f); err != nil || (len(f) != 4 && len(f) != 3) {
			return nil, fmt.Errorf("transaction %d: want a 3 or 4 element array: %v", i, err)
		}
		flagged = len(f) == 4
		bodies, wits = append(bodies, f[0]), append(wits, f[1])
		auxAt := len(f) - 1
		if f[auxAt][0] != 0xf6 {
			aux[uint(i)] = f[auxAt]
		}
		if flagged && f[2][0] == 0xf4 {
			invalid = append(invalid, uint(i))
		}
	}
	list := []any{bodies, wits, aux}
	if flagged {
		list = append(list, invalid)
	}
	var out [][]byte
	for _, v := range list {
		enc, err := cbor.Encode(v)
		if err != nil {
			return nil, err
		}
		out = append(out, enc)
	}
	return out, nil
}

// BodyOf returns the transactions of a block of any era from Shelley, each in
// the era's transaction wire shape.
func BodyOf(block []byte) (Body, error) {
	var f []cbor.RawMessage
	if _, err := cbor.Decode(block, &f); err != nil || (len(f) != 5 && len(f) != 4) {
		return nil, fmt.Errorf("block: want a 4 or 5 element array: %v", err)
	}
	flagged := len(f) == 5
	var bodies, wits []cbor.RawMessage
	var aux map[uint]cbor.RawMessage
	var invalid []uint
	targets := []any{&bodies, &wits, &aux}
	if flagged {
		targets = append(targets, &invalid)
	}
	for i, d := range targets {
		if _, err := cbor.Decode(f[i+1], d); err != nil {
			return nil, err
		}
	}
	bad := map[uint]bool{}
	for _, i := range invalid {
		bad[i] = true
	}
	var out Body
	for i := range bodies {
		valid, auxRaw := cbor.RawMessage{0xf5}, cbor.RawMessage{0xf6}
		if bad[uint(i)] {
			valid = cbor.RawMessage{0xf4}
		}
		if a, ok := aux[uint(i)]; ok {
			auxRaw = a
		}
		parts := []cbor.RawMessage{bodies[i], wits[i], valid, auxRaw}
		if !flagged {
			parts = []cbor.RawMessage{bodies[i], wits[i], auxRaw}
		}
		tx, err := cbor.Encode(parts)
		if err != nil {
			return nil, err
		}
		out = append(out, tx)
	}
	return out, nil
}

// BlockWith returns block with its body replaced by txs. The header keeps
// every field but the body size and hash, which are recomputed so the header
// commits to the new body; the edit then breaks it.
func BlockWith(block []byte, txs Body, e Edit) ([]byte, error) {
	var f []cbor.RawMessage
	if _, err := cbor.Decode(block, &f); err != nil || (len(f) != 5 && len(f) != 4) {
		return nil, fmt.Errorf("block: want a 4 or 5 element array: %v", err)
	}
	parts, err := txs.parts()
	if err != nil {
		return nil, err
	}
	var hashes []byte
	size := 0
	for _, p := range parts {
		h := common.Blake2b256Hash(p)
		hashes = append(hashes, h.Bytes()...)
		size += len(p)
	}
	hash := common.Blake2b256Hash(hashes).Bytes()
	if e.FlipHash {
		hash[0] ^= 0xff
	}
	var header []cbor.RawMessage
	if _, err := cbor.Decode(f[0], &header); err != nil || len(header) != 2 {
		return nil, fmt.Errorf("header: want a 2 element array: %v", err)
	}
	var hb []cbor.RawMessage
	if _, err := cbor.Decode(header[0], &hb); err != nil {
		return nil, fmt.Errorf("header body: %v", err)
	}
	sizeAt, hashAt := hdrBodySize, hdrBodyHash
	switch len(hb) {
	case praosFields:
	case tpraosFields:
		sizeAt, hashAt = tpraosBodySize, tpraosBodyHash
	default:
		return nil, fmt.Errorf("header body: want %d or %d elements, got %d", praosFields, tpraosFields, len(hb))
	}
	if hb[sizeAt], err = cbor.Encode(uint64(size + e.SizeDelta)); err != nil {
		return nil, err
	}
	if hb[hashAt], err = cbor.Encode(hash); err != nil {
		return nil, err
	}
	if e.HeaderMajor != 0 {
		if len(hb) == praosFields {
			hb[hdrProtVer], err = cbor.Encode([]uint64{e.HeaderMajor, 0})
		} else {
			hb[tpraosProtMaj], err = cbor.Encode(e.HeaderMajor)
		}
		if err != nil {
			return nil, err
		}
	}
	if header[0], err = cbor.Encode(hb); err != nil {
		return nil, err
	}
	if f[0], err = cbor.Encode(header); err != nil {
		return nil, err
	}
	out := []byte{0x80 | byte(len(parts)+1)}
	out = append(out, f[0]...)
	for _, p := range parts {
		out = append(out, p...)
	}
	return out, nil
}

// splitTx returns the elements of a transaction: four, or three before Alonzo.
func splitTx(tx []byte) ([]cbor.RawMessage, error) {
	var f []cbor.RawMessage
	if _, err := cbor.Decode(tx, &f); err != nil || (len(f) != 4 && len(f) != 3) {
		return nil, fmt.Errorf("transaction: want a 3 or 4 element array: %v", err)
	}
	return f, nil
}
