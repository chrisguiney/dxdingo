// Package byronfixture builds Byron-era wire structures (addresses,
// transactions, witnesses) from deterministic keys. Both dxnode-go's tests and
// the corpus generator use it, so a case means the same bytes in each. The
// encoders are hand-written CBOR so the bytes are the reference wire format
// and not whatever a library chooses to emit.
package byronfixture

import (
	"crypto/ed25519"
	"crypto/sha3"
	"encoding/binary"
	"hash/crc32"

	"golang.org/x/crypto/blake2b"
)

func head(major byte, n uint64) []byte {
	switch {
	case n < 24:
		return []byte{major<<5 | byte(n)}
	case n < 1<<8:
		return []byte{major<<5 | 24, byte(n)}
	case n < 1<<16:
		return binary.BigEndian.AppendUint16([]byte{major<<5 | 25}, uint16(n))
	case n < 1<<32:
		return binary.BigEndian.AppendUint32([]byte{major<<5 | 26}, uint32(n))
	}
	return binary.BigEndian.AppendUint64([]byte{major<<5 | 27}, n)
}

func cat(parts ...[]byte) []byte {
	var out []byte
	for _, p := range parts {
		out = append(out, p...)
	}
	return out
}

// Uint is the CBOR unsigned integer n.
func Uint(n uint64) []byte { return head(0, n) }

// Bytes is the CBOR byte string b.
func Bytes(b []byte) []byte { return cat(head(2, uint64(len(b))), b) }

// Array is a definite-length CBOR array of the encoded items.
func Array(items ...[]byte) []byte { return cat(head(4, uint64(len(items))), cat(items...)) }

// IndefArray is an indefinite-length array, the encoding the reference emits
// for a transaction's input and output lists and which its decoder requires
// there. The witness vector, by contrast, is a definite-length array.
func IndefArray(items ...[]byte) []byte {
	return cat([]byte{0x9f}, cat(items...), []byte{0xff})
}

// Tag24 is CBOR-in-CBOR: tag 24 over the byte string b.
func Tag24(b []byte) []byte { return cat([]byte{0xd8, 0x18}, Bytes(b)) }

// EmptyMap is the empty attribute map.
var EmptyMap = []byte{0xa0}

// Key is a Byron signing key: an Ed25519 seed whose extended public key is the
// public key followed by a fixed chain code.
type Key struct {
	priv ed25519.PrivateKey
	// XPub is the 64-byte extended verification key.
	XPub []byte
}

// NewKey derives a deterministic key from one byte.
func NewKey(n byte) Key {
	seed := make([]byte, 32)
	for i := range seed {
		seed[i] = n
	}
	priv := ed25519.NewKeyFromSeed(seed)
	cc := make([]byte, 32)
	for i := range cc {
		cc[i] = n ^ 0x5a
	}
	return Key{priv: priv, XPub: cat(priv.Public().(ed25519.PublicKey), cc)}
}

// Sign signs msg with the key.
func (k Key) Sign(msg []byte) []byte { return ed25519.Sign(k.priv, msg) }

// NetworkAttrs is the address attribute map: empty on mainnet, and the
// protocol magic under key 2 (itself CBOR-encoded inside a byte string) on any
// other network. A nil magic is mainnet.
func NetworkAttrs(magic *uint32) []byte {
	if magic == nil {
		return EmptyMap
	}
	return cat(head(5, 1), Uint(2), Bytes(Uint(uint64(*magic))))
}

// AddressRoot is Blake2b-224(SHA3-256(cbor [addrType 0, [0, xpub], attrs])).
func (k Key) AddressRoot(attrs []byte) []byte {
	pre := Array(Uint(0), Array(Uint(0), Bytes(k.XPub)), attrs)
	sum := sha3.Sum256(pre)
	h, _ := blake2b.New(28, nil)
	h.Write(sum[:])
	return h.Sum(nil)
}

// Address is the wire address [tag24([root, attrs, 0]), crc32] for a
// verification-key address.
func (k Key) Address(magic *uint32) []byte {
	attrs := NetworkAttrs(magic)
	payload := Array(Bytes(k.AddressRoot(attrs)), attrs, Uint(0))
	return Array(Tag24(payload), Uint(uint64(crc32.ChecksumIEEE(payload))))
}

// TxOut is [address, lovelace].
func TxOut(address []byte, lovelace uint64) []byte { return Array(address, Uint(lovelace)) }

// TxIn names a spent output.
type TxIn struct {
	ID    [32]byte
	Index uint32
}

func (i TxIn) wire() []byte {
	return Array(Uint(0), Tag24(Array(Bytes(i.ID[:]), Uint(uint64(i.Index)))))
}

// Tx builds a transaction body from inputs and encoded outputs and returns it
// with its TxId, the Blake2b-256 of the body bytes.
func Tx(ins []TxIn, outs [][]byte) (body []byte, id [32]byte) {
	var in, out [][]byte
	for _, i := range ins {
		in = append(in, i.wire())
	}
	out = outs
	body = Array(IndefArray(in...), IndefArray(out...), EmptyMap)
	return body, blake2b.Sum256(body)
}

// SigningMessage is what a verification-key witness signs: the SignTx tag, the
// CBOR protocol magic, and the CBOR byte string of the TxId.
func SigningMessage(magic uint32, id [32]byte) []byte {
	return cat([]byte{0x01}, Uint(uint64(magic)), Bytes(id[:]))
}

// Witness is [0, tag24([xpub, signature])], signed over the TxId.
func (k Key) Witness(magic uint32, id [32]byte) []byte {
	return k.WitnessSignature(k.Sign(SigningMessage(magic, id)))
}

// WitnessSignature wraps a given signature as a verification-key witness.
func (k Key) WitnessSignature(sig []byte) []byte {
	return Array(Uint(0), Tag24(Array(Bytes(k.XPub), Bytes(sig))))
}

// TxAux is [tx, [witness...]].
func TxAux(body []byte, witnesses ...[]byte) []byte {
	return Array(body, Array(witnesses...))
}
