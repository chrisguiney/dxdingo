package ledgerfixture

import (
	"bytes"
	"crypto/ed25519"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
)

// Signer is a key that signs transactions: its hash is a payment, stake or
// DRep credential of the fixture state.
type Signer struct {
	priv ed25519.PrivateKey
}

// NewSigner derives a key from a byte.
func NewSigner(b byte) Signer { return Signer{ed25519.NewKeyFromSeed(bytes.Repeat([]byte{b}, 32))} }

func (s Signer) vkey() []byte { return s.priv.Public().(ed25519.PublicKey) }

// Hash is the key hash.
func (s Signer) Hash() []byte { return common.Blake2b224Hash(s.vkey()).Bytes() }

// Cred is the key-hash credential of the signer.
func (s Signer) Cred() *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: s.Hash()}
}

// Addr is the mainnet enterprise address of the signer.
func (s Signer) Addr() []byte { return append([]byte{0x61}, s.Hash()...) }

// Ledger fixture constants.
const (
	LedgerEpoch = 600
	Fee         = 200_000
)

func ledgerInput(b byte) []any { return []any{bytes.Repeat([]byte{b}, 32), uint(0)} }

// LedgerUtxo is a legacy-format output paying coin to addr.
func LedgerUtxo(in byte, addr []byte, coin uint64) *v1.UtxoEntry {
	out, err := cbor.Encode([]any{addr, coin})
	if err != nil {
		panic(err)
	}
	return &v1.UtxoEntry{Txin: &v1.TxIn{TxId: bytes.Repeat([]byte{in}, 32), Index: 0}, Txout: out}
}

// LedgerBody describes a Conway transaction body.
type LedgerBody struct {
	Input byte
	// InputTx, when set, is the id of the transaction whose output 0 is spent
	// instead of the fixture output named by Input.
	InputTx []byte
	// RefInput, when set, is the id of a transaction whose output 0 is a
	// reference input.
	RefInput    []byte
	Output      uint64
	OutputAddr  []byte
	Fee         uint64
	Certs       []cf.Cert
	Withdrawals []cf.Withdrawal
	Proposals   []raw
	Votes       []Vote
	Signers     []Signer
	Invalid     bool
}

func (b LedgerBody) input() []any {
	if b.InputTx != nil {
		return []any{b.InputTx, uint(0)}
	}
	return ledgerInput(b.Input)
}

// Tx builds and signs the transaction.
func (b LedgerBody) Tx() []byte {
	addr := b.OutputAddr
	if addr == nil {
		addr = append([]byte{0x61}, cf.Hash(0x77)...)
	}
	body := map[uint]any{
		0: []any{b.input()},
		1: []any{[]any{addr, b.Output}},
		2: b.Fee,
	}
	if b.RefInput != nil {
		body[18] = []any{[]any{b.RefInput, uint(0)}}
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
	if len(b.Votes) > 0 {
		body[19] = votingProcedures(b.Votes)
	}
	if len(b.Proposals) > 0 {
		ps := make([]any, len(b.Proposals))
		for i, p := range b.Proposals {
			ps[i] = p
		}
		body[20] = ps
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
	out, err := cbor.Encode([]any{bodyBytes, ws, !b.Invalid, nil})
	if err != nil {
		panic(err)
	}
	return out
}

// LedgerCase is one LEDGER scenario over the fixture state.
type LedgerCase struct {
	Name   string
	PV     uint32
	Body   LedgerBody
	Mutate func(*v1.LedgerState)
	Want   []string
	Delta  string
}

var (
	payer   = NewSigner(1)
	staker  = NewSigner(2)
	drepKey = NewSigner(3)
	drawer  = NewSigner(4)
	owner   = NewSigner(5)
)

// LedgerState is the GOV fixture state plus accounts, a DRep and an output
// owned by the fixture's signers.
func LedgerState() *v1.LedgerState {
	st := GovState()
	st.Utxo = []*v1.UtxoEntry{
		LedgerUtxo(0x31, payer.Addr(), 300_000_000_000),
	}
	st.Accounts = append(st.Accounts,
		&v1.Account{Credential: drawer.Cred(), Deposit: 2_000_000, Balance: 5_000_000, Drep: cf.Abstain},
		&v1.Account{Credential: drepKey.Cred(), Deposit: 2_000_000},
	)
	st.Dreps = append(st.Dreps, &v1.DRepRecord{Credential: drepKey.Cred(), Deposit: 500_000_000, Expiry: 650})
	return st
}

// LedgerRequest builds the request of a case.
func LedgerRequest(c LedgerCase) *v1.RuleLedgerRequest {
	ctx, pp, _ := cf.Env()
	pv := c.PV
	if pv == 0 {
		pv = 11
	}
	ctx.ProtocolVersion.Major, pp.ProtocolVersion.Major = pv, pv
	ctx.Epoch, ctx.Slot = LedgerEpoch, LedgerEpoch*432_000+1_000
	st := LedgerState()
	if c.Mutate != nil {
		c.Mutate(st)
	}
	return &v1.RuleLedgerRequest{Context: ctx, Globals: ledgerGlobals(), Pparams: pp, State: st, Tx: c.Body.Tx()}
}

func ledgerGlobals() *v1.Globals {
	return &v1.Globals{
		NetworkMagic: 764824073, EpochLength: 432000, SlotLength: &v1.Rational{Numerator: "1", Denominator: "1"}, SecurityParam: 2160,
		ActiveSlotCoeff: &v1.Rational{Numerator: "1", Denominator: "20"}, SlotsPerKesPeriod: 129600, MaxKesEvolutions: 62,
		MaxLovelaceSupply: 45_000_000_000_000_000, SystemStartUnix: 1506203091, Quorum: 5,
	}
}

// LedgerCases returns the LEDGER scenarios. Each spends the 300000 ADA
// output of the payer; Output is chosen so inputs equal outputs plus fee plus
// deposits.
func LedgerCases() []LedgerCase {
	const total = 300_000_000_000
	base := func(f func(*LedgerBody)) LedgerBody {
		b := LedgerBody{Input: 0x31, Output: total - Fee, Fee: Fee, Signers: []Signer{payer}}
		f(&b)
		return b
	}
	a1 := rewardAddr(1, cf.Key(0xa1))
	return []LedgerCase{
		{Name: "ledger payment", Body: base(func(*LedgerBody) {}), Delta: "U+=1 U-=1 fee=200000"},
		{Name: "ledger payment that does not balance", Body: base(func(b *LedgerBody) { b.Output++ }),
			Want: []string{"UTXO/ValueNotConservedUTxO"}},
		{Name: "ledger payment without a signature", Body: base(func(b *LedgerBody) { b.Signers = nil }),
			Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger payment below the minimum fee", Body: base(func(b *LedgerBody) { b.Fee, b.Output = 1000, total-1000 }),
			Want: []string{"UTXO/FeeTooSmallUTxO"}},
		{Name: "ledger stake registration pays the deposit",
			Body: base(func(b *LedgerBody) {
				b.Certs = []cf.Cert{cf.RegCert(staker.Cred(), 2_000_000)}
				b.Output -= 2_000_000
				b.Signers = []Signer{payer, staker}
			}),
			Delta: "U+=1 U-=1 A+=1 fee=200000 dep=2000000"},
		{Name: "ledger stake registration of a registered credential",
			Body: base(func(b *LedgerBody) {
				b.Certs = []cf.Cert{cf.RegCert(drawer.Cred(), 2_000_000)}
				b.Output -= 2_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"DELEG/StakeKeyRegisteredDELEG"}},
		{Name: "ledger stake registration that is not paid for",
			Body: base(func(b *LedgerBody) {
				b.Certs = []cf.Cert{cf.RegCert(staker.Cred(), 2_000_000)}
				b.Signers = []Signer{payer, staker}
			}),
			Want: []string{"UTXO/ValueNotConservedUTxO"}},
		{Name: "ledger stake deregistration refunds the deposit",
			Body: base(func(b *LedgerBody) {
				b.Certs = []cf.Cert{cf.UnregCert(drepKey.Cred(), 2_000_000)}
				b.Output += 2_000_000
				b.Signers = []Signer{payer, drepKey}
			}),
			Delta: "U+=1 U-=1 A-=1 fee=200000 dep=-2000000"},
		{Name: "ledger withdrawal drains the account",
			Body: base(func(b *LedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 5_000_000}}
				b.Output += 5_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Delta: "U+=1 U-=1 A~=1 fee=200000"},
		{Name: "ledger withdrawal of less than the balance",
			Body: base(func(b *LedgerBody) {
				b.Withdrawals = []cf.Withdrawal{{Cred: drawer.Cred(), Amount: 4_000_000}}
				b.Output += 4_000_000
				b.Signers = []Signer{payer, drawer}
			}),
			Want: []string{"LEDGER/ConwayIncompleteWithdrawals"}},
		{Name: "ledger info proposal",
			Body: base(func(b *LedgerBody) {
				b.Proposals = []raw{Proposal(GovDeposit, a1, InfoAction())}
				b.Output -= GovDeposit
			}),
			Delta: "U+=1 U-=1 G+=1 fee=200000 dep=100000000000"},
		{Name: "ledger proposal with a wrong deposit",
			Body: base(func(b *LedgerBody) {
				b.Proposals = []raw{Proposal(GovDeposit-1, a1, InfoAction())}
				b.Output -= GovDeposit - 1
			}),
			Want: []string{"GOV/ProposalDepositIncorrect", "UTXO/ValueNotConservedUTxO"}},
		{Name: "ledger drep vote",
			Body: base(func(b *LedgerBody) {
				b.Votes = []Vote{{VoterType: 2, Voter: drepKey.Hash(), Action: govID(ProposalTxID(), 0), Value: 1}}
				b.Signers = []Signer{payer, drepKey}
			}),
			Delta: "U+=1 U-=1 D~=1 V+=1 fee=200000"},
		{Name: "ledger drep vote without the drep signature",
			Body: base(func(b *LedgerBody) {
				b.Votes = []Vote{{VoterType: 2, Voter: drepKey.Hash(), Action: govID(ProposalTxID(), 0), Value: 1}}
			}),
			Want: []string{"UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger drep registration",
			Body: base(func(b *LedgerBody) {
				b.Certs = []cf.Cert{cf.RegDRep(staker.Cred(), 500_000_000)}
				b.Output -= 500_000_000
				b.Signers = []Signer{payer, staker}
			}),
			Delta: "U+=1 U-=1 D~=1 fee=200000 dep=500000000"},
		{Name: "ledger failures of several rules",
			Body: base(func(b *LedgerBody) {
				b.Proposals = []raw{Proposal(GovDeposit-1, a1, InfoAction())}
				b.Certs = []cf.Cert{cf.RegCert(drawer.Cred(), 2_000_000)}
				b.Signers = nil
			}),
			Want: []string{"DELEG/StakeKeyRegisteredDELEG", "GOV/ProposalDepositIncorrect", "UTXO/ValueNotConservedUTxO", "UTXOW/MissingVKeyWitnessesUTXOW"}},
		{Name: "ledger transaction flagged invalid whose scripts pass", Body: base(func(b *LedgerBody) { b.Invalid = true }),
			Want: []string{"UTXOS/ValidationTagMismatch"}},
	}
}

// LedgerScenarios returns the cases as calls.
func LedgerScenarios() []Scenario {
	var out []Scenario
	for _, c := range LedgerCases() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_ledger", Want: c.Want, Delta: c.Delta,
			Call: &v1.Call{Request: &v1.Call_RuleLedger{RuleLedger: LedgerRequest(c)}},
		})
	}
	return out
}

// LedgerDivergences are the LEDGER scenarios on which the nodes disagree.
func LedgerDivergences() []LedgerCase {
	cases := []LedgerCase{
		// The reference reports ConwayWdrlNotDelegatedToDRep beside the missing
		// account; gouroboros stops at the first withdrawal failure.
		{Name: "ledger withdrawal from an unregistered account",
			Body: LedgerBody{Input: 0x31, Output: 300_000_000_000 - Fee + 1_000_000, Fee: Fee,
				Withdrawals: []cf.Withdrawal{{Cred: staker.Cred(), Amount: 1_000_000}}, Signers: []Signer{payer, staker}},
			Want: []string{"LEDGER/ConwayWdrlNotDelegatedToDRep", "LEDGER/ConwayWithdrawalsMissingAccounts"}},
	}
	return cases
}

// LedgerDivergenceScenarios returns LedgerDivergences as calls.
func LedgerDivergenceScenarios() []Scenario {
	var out []Scenario
	for _, c := range LedgerDivergences() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_ledger", Want: c.Want, Delta: c.Delta,
			Call: &v1.Call{Request: &v1.Call_RuleLedger{RuleLedger: LedgerRequest(c)}},
		})
	}
	return out
}
