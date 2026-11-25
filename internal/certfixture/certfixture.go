// Package certfixture builds certificate-rule requests: Conway transactions
// carrying hand-built certificates and withdrawals over a small ledger state.
//
// The state holds two stake accounts (one with a balance, one drained), one
// pool, one DRep and a three-member committee, so each rule has a registered
// and an unregistered case to hit.
package certfixture

import (
	"bytes"
	"fmt"
	"math/big"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// Epoch is the ledger epoch every fixture runs in.
const Epoch = 600

// Hash returns a 28-byte hash made of b.
func Hash(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

// Key returns a key-hash credential.
func Key(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: Hash(b)}
}

func credCbor(c *v1.Credential) []any { return []any{uint(c.GetKind()) - 1, c.GetHash()} }

// Cert is one certificate as CBOR.
type Cert []any

// DRepCbor encodes a DRep target.
func DRepCbor(d *v1.DRep) []any {
	switch t := d.GetTarget().(type) {
	case *v1.DRep_AlwaysAbstain:
		return []any{uint(2)}
	case *v1.DRep_AlwaysNoConfidence:
		return []any{uint(3)}
	case *v1.DRep_Credential:
		return []any{uint(t.Credential.GetKind()) - 1, t.Credential.GetHash()}
	}
	return nil
}

// Abstain is the always-abstain DRep.
var Abstain = &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}

// DRepKey is a key-hash DRep target.
func DRepKey(b byte) *v1.DRep {
	return &v1.DRep{Target: &v1.DRep_Credential{Credential: Key(b)}}
}

func StakeReg(c *v1.Credential) Cert                 { return Cert{uint(0), credCbor(c)} }
func StakeDereg(c *v1.Credential) Cert               { return Cert{uint(1), credCbor(c)} }
func StakeDeleg(c *v1.Credential, pool []byte) Cert  { return Cert{uint(2), credCbor(c), pool} }
func RegCert(c *v1.Credential, deposit uint64) Cert  { return Cert{uint(7), credCbor(c), deposit} }
func UnregCert(c *v1.Credential, refund uint64) Cert { return Cert{uint(8), credCbor(c), refund} }
func VoteDeleg(c *v1.Credential, d *v1.DRep) Cert    { return Cert{uint(9), credCbor(c), DRepCbor(d)} }
func PoolRetire(pool []byte, epoch uint64) Cert      { return Cert{uint(4), pool, epoch} }
func AuthHot(cold, hot *v1.Credential) Cert          { return Cert{uint(14), credCbor(cold), credCbor(hot)} }
func ResignCold(cold *v1.Credential) Cert            { return Cert{uint(15), credCbor(cold), nil} }
func UnregDRep(c *v1.Credential, refund uint64) Cert { return Cert{uint(17), credCbor(c), refund} }
func UpdateDRep(c *v1.Credential, url string, h []byte) Cert {
	return Cert{uint(18), credCbor(c), []any{url, h}}
}

// Hash32 returns a 32-byte hash made of b.
func Hash32(b byte) []byte                          { return bytes.Repeat([]byte{b}, 32) }
func RegDRep(c *v1.Credential, deposit uint64) Cert { return Cert{uint(16), credCbor(c), deposit, nil} }

func StakeRegDeleg(c *v1.Credential, pool []byte, deposit uint64) Cert {
	return Cert{uint(11), credCbor(c), pool, deposit}
}

// PoolReg registers pool with a reward account on the given network (1 is
// mainnet) and margin 1/20.
func PoolReg(pool []byte, vrf byte, pledge, cost uint64, rewardNetwork byte, owner byte) Cert {
	reward := append([]byte{0xe0 | rewardNetwork}, Hash(owner)...)
	return Cert{
		uint(3), pool, bytes.Repeat([]byte{vrf}, 32), pledge, cost,
		cbor.Rat{Rat: big.NewRat(1, 20)}, reward, []any{Hash(owner)}, []any{}, nil,
	}
}

// RewardAddr is the mainnet reward address of a credential.
func RewardAddr(c *v1.Credential) []byte {
	h := byte(0xe1)
	if c.GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
		h = 0xf1
	}
	return append([]byte{h}, c.GetHash()...)
}

// Withdrawal withdraws amount from the account of cred.
type Withdrawal struct {
	Cred   *v1.Credential
	Amount uint64
}

// Tx builds a Conway transaction body with the certificates and withdrawals.
func Tx(certs []Cert, withdrawals []Withdrawal) []byte {
	body := map[uint]any{
		0: []any{[]any{bytes.Repeat([]byte{9}, 32), uint(0)}},
		1: []any{[]any{append([]byte{0x61}, Hash(0x77)...), uint(1_000_000)}},
		2: uint(200_000),
	}
	if len(certs) > 0 {
		body[4] = certs
	}
	if len(withdrawals) > 0 {
		// A map with byte-string keys: assembled by hand because a Go map
		// cannot key on []byte.
		raw := []byte{0xa0 | byte(len(withdrawals))}
		for _, w := range withdrawals {
			k, _ := cbor.Encode(RewardAddr(w.Cred))
			v, _ := cbor.Encode(w.Amount)
			raw = append(append(raw, k...), v...)
		}
		body[5] = cbor.RawMessage(raw)
	}
	out, err := cbor.Encode([]any{body, map[uint]any{}, true, nil})
	if err != nil {
		panic(err)
	}
	return out
}

// Env returns the Conway context, parameters and base state.
func Env() (*v1.LedgerContext, *v1.ProtocolParameters, *v1.LedgerState) {
	ctx, _, pp := rulefixture.Env(v1.Era_ERA_CONWAY)
	ctx.Epoch = Epoch
	ctx.ProtocolVersion = &v1.ProtocolVersion{Major: 10}
	pp.ProtocolVersion = &v1.ProtocolVersion{Major: 10}
	return ctx, pp, State()
}

// PoolID is the id of the fixture's pool.
func PoolID() []byte { return Hash(0x51) }

// State is the base ledger state.
func State() *v1.LedgerState {
	return &v1.LedgerState{
		Accounts: []*v1.Account{
			{Credential: Key(0xa1), Deposit: 2_000_000, Balance: 5_000_000, PoolId: PoolID()},
			{Credential: Key(0xa2), Deposit: 2_000_000},
		},
		Pools: []*v1.Pool{{
			PoolId: PoolID(), Stage: v1.PoolStage_POOL_STAGE_CURRENT, VrfKeyHash: bytes.Repeat([]byte{0x61}, 32),
			Pledge: 100, Cost: 170_000_000, Margin: &v1.Rational{Numerator: "1", Denominator: "20"},
			RewardAccount: Key(0xa1), Owners: [][]byte{Hash(0xa1)}, Deposit: 500_000_000,
		}},
		Dreps: []*v1.DRepRecord{{Credential: Key(0xd1), Deposit: 500_000_000, Expiry: 650}},
		Committee: &v1.Committee{
			Threshold: &v1.Rational{Numerator: "2", Denominator: "3"},
			Members: []*v1.CommitteeMember{
				{Cold: Key(0xc1), Expiry: 700},
				{Cold: Key(0xc2), Hot: Key(0xe2), Expiry: 700},
				{Cold: Key(0xc3), Expiry: 700, Resigned: true},
			},
		},
	}
}

// Digest summarises a delta as row counts, the form both nodes' tests
// compare: accounts added, changed and removed, pools changed and removed,
// DReps changed and removed, and whether the committee changed.
func Digest(d *v1.LedgerDelta) string {
	c := 0
	if d.GetCommitteeAfter() != nil {
		c = 1
	}
	return fmt.Sprintf("A+%d A~%d A-%d P~%d P-%d D~%d D-%d C%d",
		len(d.GetAccountsAdded()), len(d.GetAccountsChanged()), len(d.GetAccountsRemoved()),
		len(d.GetPoolsChanged()), len(d.GetPoolsRemoved()),
		len(d.GetDrepsChanged()), len(d.GetDrepsRemoved()), c)
}
