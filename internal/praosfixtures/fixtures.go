// Package praosfixtures holds real mainnet block headers, with the epoch
// nonce under which their VRF proofs verify where the gouroboros test suite
// records one. Both the Go unit tests and the corpus generator read them.
package praosfixtures

import (
	"encoding/hex"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Mainnet KES parameters from the Shelley genesis.
const (
	SlotsPerKESPeriod = 129600
	MaxKESEvolutions  = 62
)

// Provenance: shelley, babbage and conway are the
// headers of the gouroboros internal/testdata blocks of the same names (mainnet
// blocks); babbage_m8 (protocol major 8), conway_a (block 10882991) and
// conway_b (block 10873394) are the headers of mainnet blocks in
// ledger/verify_block_test.go, which records their epoch nonces.

// Header is a block header as it appears in a block: the CBOR array of the
// header body and the KES signature.
type Header struct {
	Name string
	Era  v1.Era
	CBOR []byte
	// EpochNonce is the nonce under which the header's VRF proofs verify;
	// nil when no test in gouroboros records the nonce of the header's epoch.
	EpochNonce []byte
}

func mustHex(s string) []byte {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return b
}

// All returns the fixtures. Each call returns fresh copies.
func All() []Header {
	return []Header{
		{"shelley", v1.Era_ERA_SHELLEY, mustHex(shelleyBlockHeaderHex), mustHex("829749cb2701843214ae3aee67ae12ec9bdb3502e060ac0b75275d0f52af349c")},
		{"babbage", v1.Era_ERA_BABBAGE, mustHex(babbageBlockHeaderHex), nil},
		{"conway", v1.Era_ERA_CONWAY, mustHex(conwayBlockHeaderHex), nil},
		{"babbage_m8", v1.Era_ERA_BABBAGE, mustHex(babbageM8HeaderHex), mustHex("95d9b8cbdcde43708d2cf6d1b82e3e7455a6c7a1cc68096905d4d06a48a99159")},
		{"conway_a", v1.Era_ERA_CONWAY, mustHex(conwayAHeaderHex), mustHex("4ef95a10f639d0cf16bb963c3a580d4bf2a95b6ae7848702665884843e3c661d")},
		{"conway_b", v1.Era_ERA_CONWAY, mustHex(conwayBHeaderHex), mustHex("4ef95a10f639d0cf16bb963c3a580d4bf2a95b6ae7848702665884843e3c661d")},
	}
}

// ByName returns the named fixture.
func ByName(name string) Header {
	for _, h := range All() {
		if h.Name == name {
			return h
		}
	}
	panic("praosfixtures: no fixture " + name)
}
