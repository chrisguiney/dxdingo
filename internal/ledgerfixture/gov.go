package ledgerfixture

import (
	"bytes"
	"math/big"

	"github.com/blinklabs-io/gouroboros/cbor"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
)

type raw = cbor.RawMessage

func enc(v any) raw {
	b, err := cbor.Encode(v)
	if err != nil {
		panic(err)
	}
	return b
}

// rmap assembles a CBOR map from encoded pairs; a Go map cannot key on a
// byte string or an array.
func rmap(kv ...[2]raw) raw {
	out := []byte{0xa0 | byte(len(kv))}
	for _, p := range kv {
		out = append(append(out, p[0]...), p[1]...)
	}
	return out
}

// GovDeposit is the governance action deposit of the fixture's parameters.
const GovDeposit = 100_000_000_000

// GovEpoch is the ledger epoch of the GOV fixtures.
const GovEpoch = 600

func anchor() []any { return []any{"https://example.com/a", bytes.Repeat([]byte{0x33}, 32)} }

// rewardAddr is a reward address on a network.
func rewardAddr(net byte, c *v1.Credential) []byte {
	h := byte(0xe0) | net
	if c.GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
		h = 0xf0 | net
	}
	return append([]byte{h}, c.GetHash()...)
}

// ProposalTxID is the id of the proposal the GOV state already holds.
func ProposalTxID() []byte { return bytes.Repeat([]byte{0xaa}, 32) }

func govID(tx []byte, idx uint32) []any { return []any{tx, uint(idx)} }

// Proposal is a proposal procedure: deposit, return account, action CBOR.
func Proposal(deposit uint64, returnAccount []byte, action raw) raw {
	return enc([]any{deposit, returnAccount, action, anchor()})
}

// Action encodings.
func InfoAction() raw { return enc([]any{uint(6)}) }

func ParamChangeAction(prev []any, update map[uint]any) raw {
	return enc([]any{uint(0), prev, update, nil})
}

func HardForkAction(prev []any, major, minor uint) raw {
	return enc([]any{uint(1), prev, []any{major, minor}})
}

func NoConfidenceAction(prev []any) raw { return enc([]any{uint(3), prev}) }

// WithdrawalAction withdraws amount to each account.
func WithdrawalAction(amount uint64, accounts ...[]byte) raw {
	var kv [][2]raw
	for _, a := range accounts {
		kv = append(kv, [2]raw{enc(a), enc(amount)})
	}
	return enc([]any{uint(2), rmap(kv...), nil})
}

// UpdateCommitteeAction removes and adds committee members (cold key hash
// bytes) with the given term epochs.
func UpdateCommitteeAction(removed []byte, added byte, epoch uint64) raw {
	rm := []any{}
	if removed != nil {
		rm = []any{[]any{uint(0), removed}}
	}
	var addKV [][2]raw
	if added != 0 {
		addKV = append(addKV, [2]raw{enc([]any{uint(0), cf.Hash(added)}), enc(epoch)})
	}
	return enc([]any{uint(4), nil, rm, rmap(addKV...), cbor.Rat{Rat: big.NewRat(2, 3)}})
}

// Vote is one voting procedure: a voter and the action voted on.
type Vote struct {
	VoterType uint
	Voter     []byte
	Action    []any
	Value     uint
}

func votingProcedures(votes []Vote) raw {
	type group struct {
		voter raw
		kv    [][2]raw
	}
	var order []string
	groups := map[string]*group{}
	for _, v := range votes {
		voter := enc([]any{v.VoterType, v.Voter})
		k := string(voter)
		g, ok := groups[k]
		if !ok {
			g = &group{voter: voter}
			groups[k] = g
			order = append(order, k)
		}
		g.kv = append(g.kv, [2]raw{enc(v.Action), enc([]any{v.Value, nil})})
	}
	var kv [][2]raw
	for _, k := range order {
		kv = append(kv, [2]raw{groups[k].voter, rmap(groups[k].kv...)})
	}
	return rmap(kv...)
}

// GovTx builds a Conway transaction body carrying proposals and votes.
func GovTx(proposals []raw, votes []Vote) []byte {
	body := map[uint]any{
		0: []any{[]any{bytes.Repeat([]byte{9}, 32), uint(0)}},
		1: []any{[]any{append([]byte{0x61}, cf.Hash(0x77)...), uint(1_000_000)}},
		2: uint(200_000),
	}
	if len(proposals) > 0 {
		ps := make([]any, len(proposals))
		for i, p := range proposals {
			ps[i] = p
		}
		body[20] = ps
	}
	if len(votes) > 0 {
		body[19] = votingProcedures(votes)
	}
	out, err := cbor.Encode([]any{body, map[uint]any{}, true, nil})
	if err != nil {
		panic(err)
	}
	return out
}

// GovCase is one GOV scenario over the fixture state.
type GovCase struct {
	Name      string
	PV        uint32
	Proposals []raw
	Votes     []Vote
	Mutate    func(*v1.LedgerState)
	Want      []string
	Delta     string
}

// GovState is the certificate fixture state plus one active Info proposal.
func GovState() *v1.LedgerState {
	st := cf.State()
	st.Proposals = []*v1.Proposal{{
		Id: &v1.GovActionId{TxId: ProposalTxID()}, ActionType: v1.GovActionType_GOV_ACTION_TYPE_INFO,
		Action: InfoAction(), ProposedEpoch: GovEpoch - 2, ExpiresEpoch: GovEpoch + 4,
		Deposit: GovDeposit, ReturnAccount: cf.Key(0xa1),
	}}
	st.Pots = &v1.Pots{Treasury: 1_000_000_000_000}
	st.EnactState = &v1.EnactState{Constitution: &v1.Constitution{AnchorUrl: "https://example.com/c", AnchorHash: bytes.Repeat([]byte{0x44}, 32)}}
	return st
}

// GovRequest builds the request of a case.
func GovRequest(c GovCase) *v1.RuleGovRequest {
	ctx, pp, _ := cf.Env()
	pv := c.PV
	if pv == 0 {
		pv = 11
	}
	ctx.ProtocolVersion.Major, pp.ProtocolVersion.Major = pv, pv
	ctx.Epoch, ctx.Slot = GovEpoch, GovEpoch*432_000+1_000
	st := GovState()
	if c.Mutate != nil {
		c.Mutate(st)
	}
	return &v1.RuleGovRequest{Context: ctx, Pparams: pp, State: st, Tx: GovTx(c.Proposals, c.Votes)}
}

// GovCases returns the GOV scenarios.
func GovCases() []GovCase {
	a1 := rewardAddr(1, cf.Key(0xa1))
	existing := govID(ProposalTxID(), 0)
	missing := govID(bytes.Repeat([]byte{0xbb}, 32), 0)
	info := InfoAction()
	drepVote := func(b byte, a []any) []Vote { return []Vote{{VoterType: 2, Voter: cf.Hash(b), Action: a, Value: 1}} }
	expire := func(st *v1.LedgerState) { st.Proposals[0].ExpiresEpoch = GovEpoch - 1 }
	return []GovCase{
		{Name: "gov info proposal", Proposals: []raw{Proposal(GovDeposit, a1, info)}, Delta: "G+=1"},
		{Name: "gov wrong proposal deposit", Proposals: []raw{Proposal(GovDeposit-1, a1, info)}, Want: []string{"GOV/ProposalDepositIncorrect"}},
		{Name: "gov return account not registered", Proposals: []raw{Proposal(GovDeposit, rewardAddr(1, cf.Key(0xb9)), info)},
			Want: []string{"GOV/ProposalReturnAccountDoesNotExist"}},
		{Name: "gov return account on the wrong network", Proposals: []raw{Proposal(GovDeposit, rewardAddr(0, cf.Key(0xa1)), info)},
			Want: []string{"GOV/ProposalProcedureNetworkIdMismatch"}},
		{Name: "gov two proposals", Proposals: []raw{Proposal(GovDeposit, a1, info), Proposal(GovDeposit, a1, NoConfidenceAction(nil))},
			Delta: "G+=2"},
		{Name: "gov treasury withdrawal", Proposals: []raw{Proposal(GovDeposit, a1, WithdrawalAction(5_000_000, a1))}, Delta: "G+=1"},
		{Name: "gov treasury withdrawal to an unregistered account",
			Proposals: []raw{Proposal(GovDeposit, a1, WithdrawalAction(5_000_000, rewardAddr(1, cf.Key(0xb9))))},
			Want:      []string{"GOV/TreasuryWithdrawalReturnAccountsDoNotExist"}},
		{Name: "gov treasury withdrawal of zero", Proposals: []raw{Proposal(GovDeposit, a1, WithdrawalAction(0, a1))},
			Want: []string{"GOV/ZeroTreasuryWithdrawals"}},
		{Name: "gov treasury withdrawal on the wrong network",
			Proposals: []raw{Proposal(GovDeposit, a1, WithdrawalAction(5_000_000, rewardAddr(0, cf.Key(0xa1))))},
			Want:      []string{"GOV/TreasuryWithdrawalsNetworkIdMismatch"}},
		{Name: "gov treasury withdrawal during bootstrap", PV: 9, Proposals: []raw{Proposal(GovDeposit, a1, WithdrawalAction(5_000_000, a1))},
			Want: []string{"GOV/DisallowedProposalDuringBootstrap"}},
		{Name: "gov parameter change with an unknown parent", Proposals: []raw{Proposal(GovDeposit, a1, ParamChangeAction(missing, map[uint]any{0: uint(45)}))},
			Want: []string{"GOV/InvalidPrevGovActionId"}},
		{Name: "gov parameter change without a parent", Proposals: []raw{Proposal(GovDeposit, a1, ParamChangeAction(nil, map[uint]any{0: uint(45)}))},
			Delta: "G+=1"},
		{Name: "gov hard fork that cannot follow", Proposals: []raw{Proposal(GovDeposit, a1, HardForkAction(nil, 11, 2))},
			Want: []string{"GOV/ProposalCantFollow"}},
		{Name: "gov hard fork to the next major version", Proposals: []raw{Proposal(GovDeposit, a1, HardForkAction(nil, 12, 0))},
			Delta: "G+=1"},
		{Name: "gov committee update adding and removing one member",
			Proposals: []raw{Proposal(GovDeposit, a1, UpdateCommitteeAction(cf.Hash(0xc1), 0xc9, GovEpoch+20))}, Delta: "G+=1"},
		{Name: "gov committee update adding and removing the same member",
			Proposals: []raw{Proposal(GovDeposit, a1, UpdateCommitteeAction(cf.Hash(0xc9), 0xc9, GovEpoch+20))},
			Want:      []string{"GOV/ConflictingCommitteeUpdate"}},
		{Name: "gov committee update with an expired term",
			Proposals: []raw{Proposal(GovDeposit, a1, UpdateCommitteeAction(nil, 0xc9, GovEpoch-1))},
			Want:      []string{"GOV/ExpirationEpochTooSmall"}},
		{Name: "gov drep votes on an active proposal", Votes: drepVote(0xd1, existing), Delta: "V+=1"},
		{Name: "gov drep votes on an unknown proposal", Votes: drepVote(0xd1, missing), Want: []string{"GOV/GovActionsDoNotExist"}},
		{Name: "gov unregistered drep votes", Votes: drepVote(0xd9, existing), Want: []string{"GOV/VotersDoNotExist"}},
		{Name: "gov drep votes on an info action during bootstrap", PV: 9, Votes: drepVote(0xd1, existing), Delta: "V+=1"},
		{Name: "gov drep votes on a parameter change during bootstrap", PV: 9,
			Votes: drepVote(0xd1, govID(bytes.Repeat([]byte{0xae}, 32), 0)), Mutate: withParamProposal,
			Want: []string{"GOV/DisallowedVotesDuringBootstrap"}},
		{Name: "gov drep votes on a parameter change after bootstrap",
			Votes: drepVote(0xd1, govID(bytes.Repeat([]byte{0xae}, 32), 0)), Mutate: withParamProposal, Delta: "V+=1"},
		{Name: "gov vote on an expired proposal", Votes: drepVote(0xd1, existing), Mutate: expire, Want: []string{"GOV/VotingOnExpiredGovAction"}},
		{Name: "gov committee member votes with an authorized hot key",
			Votes: []Vote{{VoterType: 0, Voter: cf.Hash(0xe2), Action: existing, Value: 1}}, Delta: "V+=1"},
		{Name: "gov committee member votes with an unknown hot key",
			Votes: []Vote{{VoterType: 0, Voter: cf.Hash(0xe9), Action: existing, Value: 1}},
			Want:  []string{"GOV/UnelectedCommitteeVoters", "GOV/VotersDoNotExist"}},
		{Name: "gov stake pool votes on an info action",
			Votes: []Vote{{VoterType: 4, Voter: cf.PoolID(), Action: existing, Value: 1}}, Delta: "V+=1"},
		{Name: "gov stake pool votes on a treasury withdrawal",
			Votes:  []Vote{{VoterType: 4, Voter: cf.PoolID(), Action: govID(bytes.Repeat([]byte{0xac}, 32), 0), Value: 1}},
			Mutate: withTreasuryProposal, Want: []string{"GOV/DisallowedVoters"}},
		{Name: "gov committee member votes on a no confidence motion",
			Votes:  []Vote{{VoterType: 0, Voter: cf.Hash(0xe2), Action: govID(bytes.Repeat([]byte{0xad}, 32), 0), Value: 1}},
			Mutate: withNoConfidenceProposal, Want: []string{"GOV/DisallowedVoters"}},
		{Name: "gov proposal and vote in one transaction",
			Proposals: []raw{Proposal(GovDeposit, a1, info)}, Votes: drepVote(0xd1, existing), Delta: "G+=1 V+=1"},
		{Name: "gov failures of proposals and votes together",
			Proposals: []raw{Proposal(GovDeposit-1, a1, info)}, Votes: drepVote(0xd9, existing),
			Want: []string{"GOV/ProposalDepositIncorrect", "GOV/VotersDoNotExist"}},
	}
}

func withProposal(id byte, typ v1.GovActionType, action raw) func(*v1.LedgerState) {
	return func(st *v1.LedgerState) {
		st.Proposals = append(st.Proposals, &v1.Proposal{
			Id: &v1.GovActionId{TxId: bytes.Repeat([]byte{id}, 32)}, ActionType: typ, Action: action,
			ProposedEpoch: GovEpoch - 2, ExpiresEpoch: GovEpoch + 4, Deposit: GovDeposit, ReturnAccount: cf.Key(0xa1),
		})
	}
}

var (
	withTreasuryProposal = withProposal(0xac, v1.GovActionType_GOV_ACTION_TYPE_TREASURY_WITHDRAWALS,
		WithdrawalAction(5_000_000, rewardAddr(1, cf.Key(0xa1))))
	withParamProposal = withProposal(0xae, v1.GovActionType_GOV_ACTION_TYPE_PARAMETER_CHANGE,
		ParamChangeAction(nil, map[uint]any{0: uint(45)}))
	withNoConfidenceProposal = withProposal(0xad, v1.GovActionType_GOV_ACTION_TYPE_NO_CONFIDENCE, NoConfidenceAction(nil))
)

// GovScenarios returns the cases as calls.
func GovScenarios() []Scenario {
	var out []Scenario
	for _, c := range GovCases() {
		out = append(out, Scenario{
			Name: c.Name, Fn: "rule_gov", Want: c.Want, Delta: c.Delta,
			Call: &v1.Call{Request: &v1.Call_RuleGov{RuleGov: GovRequest(c)}},
		})
	}
	return out
}
