// Package lsfixture builds Conway NewEpochState encodings from Cardano
// Blueprint ledger vectors.
//
// A Blueprint vector holds a LedgerState exported by an older ledger than the
// pinned one, with two differences the pinned ledger cannot read: the
// protocol parameters in the GovState are replaced by their hash (the files
// of pparams-by-hash hold them), and the DState carries a UMap where the
// pinned ledger keeps a plain map of account states. Both are converted here.
// The PState is rewritten too: the pinned ledger keeps a StakePoolState per
// pool where the vector has the registration parameters.
// A NewEpochState wraps the result, with empty stake snapshots, no reward
// update and an empty pool distribution unless the options supply some.
package lsfixture

import (
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	"github.com/blinklabs-io/gouroboros/cbor"
)

// Blocks maps a pool key hash to the blocks it made.
type Blocks map[string]uint64

// Stake is one delegated credential in a stake snapshot.
type Stake struct {
	Script bool
	Hash   []byte
	Amount uint64
	Pool   []byte
}

// Options are the parts of a NewEpochState the LedgerState does not carry.
type Options struct {
	Epoch              uint64
	Treasury, Reserves uint64
	Prev, Cur          Blocks
	Mark, Set, Go      []Stake
	// NoUTxO empties the UTxO map, which leaves every other table of the
	// state as the vector has it.
	NoUTxO bool
}

// FromBlueprint reads a vector file and the parameter files under
// pparamsDir; see FromVector.
func FromBlueprint(vectorPath, pparamsDir, state string, opt Options) ([]byte, error) {
	raw, err := os.ReadFile(vectorPath)
	if err != nil {
		return nil, err
	}
	return FromVector(raw, func(hash string) ([]byte, error) {
		return os.ReadFile(filepath.Join(pparamsDir, hash))
	}, state, opt)
}

// FromVector takes the field of a vector's JSON named by state
// ("oldLedgerState" or "newLedgerState") and returns a NewEpochState.
// pparams returns the parameter file of a hex hash.
func FromVector(vector []byte, pparams func(hash string) ([]byte, error), state string, opt Options) ([]byte, error) {
	var v map[string]any
	if err := json.Unmarshal(vector, &v); err != nil {
		return nil, err
	}
	s, _ := v[state].(string)
	ls, err := hex.DecodeString(s)
	if err != nil || len(ls) == 0 {
		return nil, fmt.Errorf("vector has no %s", state)
	}
	ls, err = fillPParams(pparams, ls)
	if err != nil {
		return nil, err
	}
	ls, err = convertUMap(ls)
	if err != nil {
		return nil, err
	}
	ls, err = convertPState(ls)
	if err != nil {
		return nil, err
	}
	if opt.NoUTxO {
		if ls, err = emptyUTxO(ls); err != nil {
			return nil, err
		}
	}
	return Wrap(ls, opt), nil
}

// Wrap builds the NewEpochState around a pinned-ledger LedgerState:
// [epoch, bprev, bcur, [[treasury, reserves], ls, snapshots, nonmyopic],
// SNothing, [{}, 1], null].
func Wrap(ls []byte, opt Options) []byte {
	out := []byte{0x87}
	out = head(out, 0, opt.Epoch)
	out = append(out, blocks(opt.Prev)...)
	out = append(out, blocks(opt.Cur)...)
	out = append(out, 0x84, 0x82)
	out = head(out, 0, opt.Treasury)
	out = head(out, 0, opt.Reserves)
	out = append(out, ls...)
	out = append(out, 0x84)
	for _, st := range [][]Stake{opt.Mark, opt.Set, opt.Go} {
		out = append(out, snapshot(st)...)
	}
	out = append(out, 0x00)
	// NonMyopic [{}, 0]; reward update SNothing; pool distribution [{}, 1] (the total stake is non-zero);
	// stashed AVVM addresses, which Conway encodes as null.
	out = append(out, 0x82, 0xa0, 0x00, 0x80, 0x82, 0xa0, 0x01, 0xf6)
	return out
}

func head(dst []byte, major byte, n uint64) []byte {
	m := major << 5
	switch {
	case n < 24:
		return append(dst, m|byte(n))
	case n < 1<<8:
		return append(dst, m|24, byte(n))
	case n < 1<<16:
		return append(dst, m|25, byte(n>>8), byte(n))
	case n < 1<<32:
		return append(dst, m|26, byte(n>>24), byte(n>>16), byte(n>>8), byte(n))
	}
	return append(dst, m|27, byte(n>>56), byte(n>>48), byte(n>>40), byte(n>>32), byte(n>>24), byte(n>>16), byte(n>>8), byte(n))
}

func bstr(dst, b []byte) []byte { return append(head(dst, 2, uint64(len(b))), b...) }

func blocks(b Blocks) []byte {
	keys := make([]string, 0, len(b))
	for k := range b {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	out := head(nil, 5, uint64(len(keys)))
	for _, k := range keys {
		out = bstr(out, []byte(k))
		out = head(out, 0, b[k])
	}
	return out
}

// snapshot encodes the two-field SnapShot the pinned ledger writes:
// [ActiveStake, StakePoolsSnapShot] with no pool entries.
func snapshot(st []Stake) []byte {
	sort.Slice(st, func(i, j int) bool {
		if st[i].Script != st[j].Script {
			return !st[i].Script
		}
		return string(st[i].Hash) < string(st[j].Hash)
	})
	out := []byte{0x82}
	out = head(out, 5, uint64(len(st)))
	for _, s := range st {
		kind := uint64(0)
		if s.Script {
			kind = 1
		}
		out = append(out, 0x82)
		out = head(out, 0, kind)
		out = bstr(out, s.Hash)
		out = append(out, 0x82)
		out = head(out, 0, s.Amount)
		out = bstr(out, s.Pool)
	}
	return append(out, 0xa0)
}

func splitArray(b []byte) ([]cbor.RawMessage, error) {
	var parts []cbor.RawMessage
	if _, err := cbor.Decode(b, &parts); err != nil {
		return nil, err
	}
	return parts, nil
}

func joinArray(parts []cbor.RawMessage) []byte {
	out := head(nil, 4, uint64(len(parts)))
	for _, p := range parts {
		out = append(out, p...)
	}
	return out
}

type kv struct{ k, v cbor.RawMessage }

func splitMap(b []byte) ([]kv, error) {
	if len(b) == 0 || b[0]>>5 != 5 {
		return nil, errors.New("not a map")
	}
	var n uint64
	hdr := 1
	switch ai := b[0] & 0x1f; {
	case ai < 24:
		n = uint64(ai)
	case ai == 24:
		n, hdr = uint64(b[1]), 2
	case ai == 25:
		n, hdr = uint64(b[1])<<8|uint64(b[2]), 3
	default:
		return nil, errors.New("unsupported map header")
	}
	rest := b[hdr:]
	out := make([]kv, 0, n)
	for i := uint64(0); i < n; i++ {
		var k, v cbor.RawMessage
		l, err := cbor.Decode(rest, &k)
		if err != nil {
			return nil, err
		}
		rest = rest[l:]
		if l, err = cbor.Decode(rest, &v); err != nil {
			return nil, err
		}
		rest = rest[l:]
		out = append(out, kv{k, v})
	}
	return out, nil
}

func joinMap(es []kv) []byte {
	out := head(nil, 5, uint64(len(es)))
	for _, e := range es {
		out = append(out, e.k...)
		out = append(out, e.v...)
	}
	return out
}

// fillPParams swaps every 32-byte string that names a parameter file for the
// parameters: the GovState's current, previous and future parameters, and the
// copies inside the DRep pulser's state. pparams errs for a hash it has no
// file for, which is how an unrelated hash is told apart.
func fillPParams(pparams func(string) ([]byte, error), ls []byte) ([]byte, error) {
	var out []byte
	for i := 0; i < len(ls); {
		if i+34 <= len(ls) && ls[i] == 0x58 && ls[i+1] == 0x20 {
			if pp, err := pparams(hex.EncodeToString(ls[i+2 : i+34])); err == nil {
				out = append(out, pp...)
				i += 34
				continue
			}
		}
		out = append(out, ls[i])
		i++
	}
	return out, nil
}

// convertUMap rewrites the DState's UMap [elems, ptrs] as the pinned
// ledger's account map: credential to [balance, deposit, pool|null, drep|null].
func convertUMap(ls []byte) ([]byte, error) {
	top, err := splitArray(ls)
	if err != nil {
		return nil, err
	}
	cert, err := splitArray(top[0])
	if err != nil || len(cert) < 3 {
		return nil, fmt.Errorf("cert state: %v", err)
	}
	dstate, err := splitArray(cert[2])
	if err != nil || len(dstate) < 1 {
		return nil, fmt.Errorf("dstate: %v", err)
	}
	umap, err := splitArray(dstate[0])
	if err != nil || len(umap) != 2 {
		return nil, fmt.Errorf("umap: %v", err)
	}
	elems, err := splitMap(umap[0])
	if err != nil {
		return nil, err
	}
	null := cbor.RawMessage{0xf6}
	var accounts []kv
	for _, e := range elems {
		el, err := splitArray(e.v)
		if err != nil || len(el) != 4 {
			return nil, fmt.Errorf("umap element: %v", err)
		}
		rd, err := splitArray(el[0])
		if err != nil {
			return nil, err
		}
		if len(rd) == 0 {
			continue
		}
		pair, err := splitArray(rd[0])
		if err != nil || len(pair) != 2 {
			return nil, fmt.Errorf("reward-deposit pair: %v", err)
		}
		pool, drep := null, null
		if p, err := splitArray(el[2]); err == nil && len(p) == 1 {
			pool = p[0]
		}
		if d, err := splitArray(el[3]); err == nil && len(d) == 1 {
			drep = d[0]
		}
		accounts = append(accounts, kv{e.k, joinArray([]cbor.RawMessage{pair[0], pair[1], pool, drep})})
	}
	dstate[0] = joinMap(accounts)
	cert[2] = joinArray(dstate)
	top[0] = joinArray(cert)
	return joinArray(top), nil
}

// TxOutBytes returns the values of the UTxO map of a NewEpochState that are
// byte strings, which is how the ledger stores a MemPack-encoded output.
func TxOutBytes(nes []byte) ([][]byte, error) {
	top, err := splitArray(nes)
	if err != nil || len(top) < 4 {
		return nil, fmt.Errorf("new epoch state: %v", err)
	}
	es, err := splitArray(top[3])
	if err != nil || len(es) < 2 {
		return nil, fmt.Errorf("epoch state: %v", err)
	}
	ls, err := splitArray(es[1])
	if err != nil || len(ls) < 2 {
		return nil, fmt.Errorf("ledger state: %v", err)
	}
	us, err := splitArray(ls[1])
	if err != nil || len(us) < 1 {
		return nil, fmt.Errorf("utxo state: %v", err)
	}
	entries, err := splitMap(us[0])
	if err != nil {
		return nil, err
	}
	var out [][]byte
	for _, e := range entries {
		var b []byte
		if _, err := cbor.Decode(e.v, &b); err == nil {
			out = append(out, b)
		}
	}
	return out, nil
}

func credCbor(script bool, hash []byte) cbor.RawMessage {
	kind := uint64(0)
	if script {
		kind = 1
	}
	out := head([]byte{0x82}, 0, kind)
	return bstr(out, hash)
}

// convertPState rewrites the PState
// [poolParams, futurePoolParams, retiring, deposits] as the pinned ledger's
// [vrfKeyHashes, stakePools, futurePoolParams, retiring]. A StakePoolState
// is the registration with the reward account as a credential, the pool's
// deposit and the set of accounts delegating to it.
func convertPState(ls []byte) ([]byte, error) {
	top, err := splitArray(ls)
	if err != nil {
		return nil, err
	}
	cert, err := splitArray(top[0])
	if err != nil || len(cert) < 3 {
		return nil, fmt.Errorf("cert state: %v", err)
	}
	pstate, err := splitArray(cert[1])
	if err != nil || len(pstate) != 4 {
		return nil, fmt.Errorf("pstate: %v", err)
	}
	dstate, err := splitArray(cert[2])
	if err != nil {
		return nil, err
	}
	accounts, err := splitMap(dstate[0])
	if err != nil {
		return nil, err
	}
	delegators := map[string][]struct {
		script bool
		hash   []byte
	}{}
	for _, a := range accounts {
		var key struct {
			cbor.StructAsArray
			Kind uint
			Hash []byte
		}
		if _, err := cbor.Decode(a.k, &key); err != nil {
			return nil, err
		}
		vals, err := splitArray(a.v)
		if err != nil || len(vals) != 4 {
			return nil, fmt.Errorf("account: %v", err)
		}
		var pool []byte
		if _, err := cbor.Decode(vals[2], &pool); err == nil && pool != nil {
			delegators[string(pool)] = append(delegators[string(pool)], struct {
				script bool
				hash   []byte
			}{key.Kind == 1, key.Hash})
		}
	}
	params, err := splitMap(pstate[0])
	if err != nil {
		return nil, err
	}
	deposits, err := splitMap(pstate[3])
	if err != nil {
		return nil, err
	}
	deposit := map[string]cbor.RawMessage{}
	for _, d := range deposits {
		var id []byte
		if _, err := cbor.Decode(d.k, &id); err != nil {
			return nil, err
		}
		deposit[string(id)] = d.v
	}
	vrfCount := map[string]uint64{}
	var vrfOrder []string
	var pools []kv
	for _, p := range params {
		var id []byte
		if _, err := cbor.Decode(p.k, &id); err != nil {
			return nil, err
		}
		f, err := splitArray(p.v)
		if err != nil || len(f) != 9 {
			return nil, fmt.Errorf("pool params: %v", err)
		}
		var vrf, reward []byte
		if _, err := cbor.Decode(f[1], &vrf); err != nil {
			return nil, err
		}
		if _, err := cbor.Decode(f[5], &reward); err != nil || len(reward) != 29 {
			return nil, fmt.Errorf("reward account: %v", err)
		}
		if vrfCount[string(vrf)] == 0 {
			vrfOrder = append(vrfOrder, string(vrf))
		}
		vrfCount[string(vrf)]++
		meta := cbor.RawMessage{0x80}
		if len(f[8]) > 0 && f[8][0] != 0xf6 {
			meta = append(cbor.RawMessage{0x81}, f[8]...)
		}
		dep := cbor.RawMessage{0x00}
		if d, ok := deposit[string(id)]; ok {
			dep = d
		}
		ds := delegators[string(id)]
		sort.Slice(ds, func(i, j int) bool {
			if ds[i].script != ds[j].script {
				return ds[i].script
			}
			return string(ds[i].hash) < string(ds[j].hash)
		})
		set := []byte{0xd9, 0x01, 0x02}
		set = head(set, 4, uint64(len(ds)))
		for _, d := range ds {
			set = append(set, credCbor(d.script, d.hash)...)
		}
		pools = append(pools, kv{p.k, joinArray([]cbor.RawMessage{
			f[1], f[2], f[3], f[4], credCbor(reward[0]>>4&1 == 1, reward[1:]), f[6], f[7], meta, dep, set,
		})})
	}
	sort.Strings(vrfOrder)
	var vrfs []kv
	for _, v := range vrfOrder {
		vrfs = append(vrfs, kv{bstr(nil, []byte(v)), head(nil, 0, vrfCount[v])})
	}
	pstate = []cbor.RawMessage{joinMap(vrfs), joinMap(pools), pstate[1], pstate[2]}
	cert[1] = joinArray(pstate)
	top[0] = joinArray(cert)
	return joinArray(top), nil
}

func emptyUTxO(ls []byte) ([]byte, error) {
	top, err := splitArray(ls)
	if err != nil || len(top) < 2 {
		return nil, fmt.Errorf("ledger state: %v", err)
	}
	us, err := splitArray(top[1])
	if err != nil || len(us) < 1 {
		return nil, fmt.Errorf("utxo state: %v", err)
	}
	us[0] = cbor.RawMessage{0xa0}
	top[1] = joinArray(us)
	return joinArray(top), nil
}
