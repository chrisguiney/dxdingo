package ledgerfixture

import (
	"bytes"
	"crypto/ed25519"
	"math/big"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// LegacyLedgerBody describes a Shelley-to-Babbage transaction over the legacy
// LEDGER fixture: one input, one output, a fee and a far time to live.
type LegacyLedgerBody struct {
	Input byte
	// InputTx, when set, is the id of the transaction whose output 0 is spent
	// instead of the fixture output named by Input.
	InputTx     []byte
	Output, Fee uint64
	Certs       []cf.Cert
	Withdrawals []cf.Withdrawal
	Signers     []Signer
	// Invalid clears the is_valid flag (Alonzo and Babbage only).
	Invalid bool
	// PayToPayer pays the output to the payer's address.
	PayToPayer bool
}

func (b LegacyLedgerBody) input() []any {
	if b.InputTx != nil {
		return []any{b.InputTx, uint(0)}
	}
	return ledgerInput(b.Input)
}

// OutputAddr is the address the transaction pays: the payer when it chains
// from another transaction's output, otherwise a fixed enterprise address.
func (b LegacyLedgerBody) OutputAddr() []byte {
	if b.InputTx != nil || b.PayToPayer {
		return payer.Addr()
	}
	return append([]byte{0x61}, cf.Hash(0x77)...)
}

// Tx builds and signs the transaction in the era's wire shape: three
// elements before Alonzo, four from it.
func (b LegacyLedgerBody) Tx(era v1.Era) []byte {
	body := map[uint]any{
		0: []any{b.input()},
		1: []any{[]any{b.OutputAddr(), b.Output}},
		2: b.Fee,
		3: uint(1) << 40,
	}
	if len(b.Certs) > 0 {
		body[4] = b.Certs
	}
	if len(b.Withdrawals) > 0 {
		kv := make([][2]raw, len(b.Withdrawals))
		for i, w := range b.Withdrawals {
			kv[i] = [2]raw{enc(cf.RewardAddr(w.Cred)), enc(w.Amount)}
		}
		body[5] = rmap(kv...)
	}
	bodyBytes := enc(body)
	hash := common.Blake2b256Hash(bodyBytes)
	var wits [][]any
	for _, s := range b.Signers {
		wits = append(wits, []any{s.vkey(), ed25519.Sign(s.priv, hash.Bytes())})
	}
	ws := map[uint]any{}
	if len(wits) > 0 {
		ws[0] = wits
	}
	parts := []any{bodyBytes, ws, nil}
	if era == v1.Era_ERA_ALONZO || era == v1.Era_ERA_BABBAGE {
		parts = []any{bodyBytes, ws, !b.Invalid, nil}
	}
	out, err := cbor.Encode(parts)
	if err != nil {
		panic(err)
	}
	return out
}

// LegacyLedgerCase is one LEDGER scenario of a Shelley-family era. Want and
// Delta are the answer in every era the case runs in; Only restricts the case
// to the listed eras.
type LegacyLedgerCase struct {
	Name   string
	Body   LegacyLedgerBody
	Mutate func(*v1.RuleLedgerRequest)
	Want   []string
	Delta  string
	Only   []v1.Era
}

func (c LegacyLedgerCase) runsIn(era v1.Era) bool {
	if len(c.Only) == 0 {
		return true
	}
	for _, e := range c.Only {
		if e == era {
			return true
		}
	}
	return false
}

// poolRegCert registers the pool of a signer, with the signer as its owner
// and reward account on mainnet.
func poolRegCert(s Signer, vrf byte) cf.Cert {
	return cf.Cert{
		uint(3), s.Hash(), bytes.Repeat([]byte{vrf}, 32), uint(100), uint(170_000_000),
		cbor.Rat{Rat: big.NewRat(1, 20)}, append([]byte{0xe1}, s.Hash()...), []any{s.Hash()}, []any{}, nil,
	}
}

// LegacyLedgerState is the legacy certificate fixture (two accounts and a
// pool) plus an output of the payer and the accounts of the fixture's signers.
func LegacyLedgerState() *v1.LedgerState {
	st := LegacyState()
	st.Utxo = []*v1.UtxoEntry{LedgerUtxo(0x31, payer.Addr(), 300_000_000_000)}
	st.Accounts = append(st.Accounts,
		&v1.Account{Credential: drawer.Cred(), Deposit: 2_000_000, Balance: 5_000_000},
		&v1.Account{Credential: drepKey.Cred(), Deposit: 2_000_000},
	)
	st.Pots = &v1.Pots{Treasury: 1_000_000_000_000}
	return st
}

// LegacyLedgerRequest builds the request of a case in an era.
func LegacyLedgerRequest(era v1.Era, c LegacyLedgerCase) *v1.RuleLedgerRequest {
	ctx, g, pp := rulefixture.Env(era)
	ctx.Epoch, ctx.Slot = LedgerEpoch, LedgerEpoch*432_000+1_000
	req := &v1.RuleLedgerRequest{Context: ctx, Globals: g, Pparams: pp, State: LegacyLedgerState(), Tx: c.Body.Tx(era)}
	if c.Mutate != nil {
		c.Mutate(req)
	}
	return req
}

// LegacyLedgerCases returns the Shelley-to-Babbage LEDGER scenarios. Each
// spends the 300000 ADA output of the payer; Output is chosen so inputs equal
// outputs plus fee plus deposits.
func LegacyLedgerCases() []LegacyLedgerCase {
	const total = 300_000_000_000
	base := func(f func(*LegacyLedgerBody)) LegacyLedgerBody {
		b := LegacyLedgerBody{Input: 0x31, Output: total - Fee, Fee: Fee, Signers: []Signer{payer}}
		f(&b)
		return b
	}
	return []LegacyLedgerCase{
		{Name: "ledger payment", Body: base(func(*LegacyLedgerBody) {}), Delta: "U+=1 U-=1 fee=200000"},
		{Name: "ledger payment that does not balance", Body: base(func(b *LegacyLedgerBody) { b.Output++ }),
			Want: []string{"UTXO/ValueNotConservedUTxO"}},
		{Name: "ledger payment without a signature", Body: base(func(b *LegacyLedgerBody) { b.Signers = nil }),
			Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger payment below the minimum fee", Body: base(func(b *LegacyLedgerBody) { b.Fee, b.Output = 1000, total-1000 }),
			Want: []string{"UTXO/FeeTooSmallUTxO"}},
		{Name: "ledger payment over the maximum size", Body: base(func(*LegacyLedgerBody) {}),
			Mutate: func(r *v1.RuleLedgerRequest) { r.Pparams.MaxTxSize = ptr32(100) },
			Want:   []string{"UTXO/MaxTxSizeUTxO"}},
		{Name: "ledger payment to the wrong network", Body: base(func(*LegacyLedgerBody) {}),
			Mutate: func(r *v1.RuleLedgerRequest) { r.Context.NetworkId = 0 },
			Want:   []string{"UTXO/WrongNetwork"}},
		{Name: "ledger stake registration pays the deposit",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeReg(staker.Cred())}
				b.Output -= 2_000_000
				b.Signers = []Signer{payer, staker}
			}),
			Delta: "U+=1 U-=1 A+=1 fee=200000 dep=2000000"},
		{Name: "ledger stake registration of a registered credential",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeReg(drawer.Cred())}
				b.Output -= 2_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"DELEG/StakeKeyAlreadyRegisteredDELEG"}},
		{Name: "ledger stake registration that is not paid for",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeReg(staker.Cred())}
				b.Signers = []Signer{payer, staker}
			}),
			Want: []string{"UTXO/ValueNotConservedUTxO"}},
		{Name: "ledger stake deregistration refunds the deposit",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeDereg(drepKey.Cred())}
				b.Output += 2_000_000
				b.Signers = []Signer{payer, drepKey}
			}),
			Delta: "U+=1 U-=1 A-=1 fee=200000 dep=-2000000"},
		{Name: "ledger stake deregistration with a balance",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeDereg(drawer.Cred())}
				b.Output += 2_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"DELEG/StakeKeyNonZeroAccountBalanceDELEG"}},
		{Name: "ledger delegation to a registered pool",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeDeleg(drawer.Cred(), cf.PoolID())}
				b.Signers = []Signer{payer, drawer}
			}),
			Delta: "U+=1 U-=1 A~=1 fee=200000"},
		{Name: "ledger delegation to an unregistered pool",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeDeleg(drawer.Cred(), cf.Hash(0x99))}
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"DELEG/DelegateeNotRegisteredDELEG"}},
		{Name: "ledger pool registration pays the deposit",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{poolRegCert(owner, 0x62)}
				b.Output -= 500_000_000
				b.Signers = []Signer{payer, owner}
			}),
			Delta: "U+=1 U-=1 P~=1 fee=200000 dep=500000000"},
		{Name: "ledger pool registration without the operator signature",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{poolRegCert(owner, 0x62)}
				b.Output -= 500_000_000
			}),
			Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger pool retirement of an unregistered pool",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.PoolRetire(owner.Hash(), 610)}
				b.Signers = []Signer{payer, owner}
			}),
			Want: []string{"POOL/StakePoolNotRegisteredOnKeyPOOL"}},
		{Name: "ledger withdrawal drains the account",
			Body: base(func(b *LegacyLedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 5_000_000}}
				b.Output += 5_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Delta: "U+=1 U-=1 A~=1 fee=200000"},
		{Name: "ledger withdrawal of less than the balance",
			Body: base(func(b *LegacyLedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 4_000_000}}
				b.Output += 4_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"LEDGER/ShelleyIncompleteWithdrawals"}},
		{Name: "ledger withdrawal from an unregistered account",
			Body: base(func(b *LegacyLedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: staker.Cred(), Amount: 1_000_000}}
				b.Output += 1_000_000
				b.Signers = []Signer{payer, staker}
			}),
			Want: []string{"LEDGER/ShelleyWithdrawalsMissingAccounts"}},
		{Name: "ledger withdrawal and deregistration of one account",
			Body: base(func(b *LegacyLedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 5_000_000}}
				b.Certs = []cf.Cert{cf.StakeDereg(drawer.Cred())}
				b.Output += 5_000_000 + 2_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Delta: "U+=1 U-=1 A-=1 fee=200000 dep=-2000000"},
		{Name: "ledger failures of several rules",
			Body: base(func(b *LegacyLedgerBody) {
				b.Certs = []cf.Cert{cf.StakeReg(drawer.Cred())}
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 4_000_000}}
				b.Signers = nil
			}),
			Want: []string{"DELEG/StakeKeyAlreadyRegisteredDELEG", "LEDGER/ShelleyIncompleteWithdrawals", "UTXO/ValueNotConservedUTxO", "UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger transaction flagged invalid whose scripts pass",
			Body: base(func(b *LegacyLedgerBody) { b.Invalid = true }),
			Only: []v1.Era{v1.Era_ERA_ALONZO, v1.Era_ERA_BABBAGE},
			Want: []string{"UTXOS/ValidationTagMismatch"}},
	}
}

func ptr32(x uint32) *uint32 { return &x }

// LegacyLedgerScenarios returns every case in every era as a call. The name
// has the era after the first word.
func LegacyLedgerScenarios() []Scenario {
	var out []Scenario
	for _, era := range LegacyEras {
		short := strings.ToLower(strings.TrimPrefix(era.String(), "ERA_"))
		for _, c := range LegacyLedgerCases() {
			if !c.runsIn(era) {
				continue
			}
			first, rest, _ := strings.Cut(c.Name, " ")
			out = append(out, Scenario{
				Name: first + " " + short + " " + rest, Fn: "rule_ledger", Want: c.Want, Delta: c.Delta,
				Call: &v1.Call{Request: &v1.Call_RuleLedger{RuleLedger: LegacyLedgerRequest(era, c)}},
			})
		}
	}
	return out
}
