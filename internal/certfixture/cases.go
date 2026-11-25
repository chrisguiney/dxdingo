package certfixture

import v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"

// Case is one certificate-rule scenario over State. Rule is "deleg", "pool",
// "gov" or "certs"; Want lists the expected failures as RULE/Constructor, empty
// when the rule accepts. PV overrides the protocol major version (default 10).
type Case struct {
	Name        string
	Rule        string
	Certs       []Cert
	Withdrawals []Withdrawal
	CertIndex   *uint32
	Mutate      func(*v1.LedgerState)
	PV          uint32
	Want        []string
	// Delta is the Digest of the delta an accepting rule produces.
	Delta string
}

func u32(x uint32) *uint32 { return &x }

// Cases returns the shared scenarios: the Go node's tests, the corpus
// generator and the Haskell tests all run these.
func Cases() []Case {
	b1, b2 := Key(0xb1), Key(0xb2)
	p2 := Hash(0x52)
	cases := []Case{
		{Name: "deleg registers a new credential", Rule: "deleg", Certs: []Cert{RegCert(b1, 2_000_000)}},
		{Name: "deleg legacy registration pays the parameter deposit", Rule: "deleg", Certs: []Cert{StakeReg(b1)}},
		{Name: "deleg re-registering a registered key", Rule: "deleg", Certs: []Cert{RegCert(Key(0xa1), 2_000_000)},
			Want: []string{"DELEG/StakeKeyRegisteredDELEG"}},
		{Name: "deleg wrong registration deposit", Rule: "deleg", Certs: []Cert{RegCert(b1, 1_000_000)},
			Want: []string{"DELEG/IncorrectDepositDELEG"}},
		{Name: "deleg wrong registration deposit after PV10", Rule: "deleg", PV: 11, Certs: []Cert{RegCert(b1, 1_000_000)},
			Want: []string{"DELEG/DepositIncorrectDELEG"}},
		{Name: "deleg deregistering a drained account", Rule: "deleg", Certs: []Cert{UnregCert(Key(0xa2), 2_000_000)}},
		{Name: "deleg deregistering an account with a balance", Rule: "deleg", Certs: []Cert{UnregCert(Key(0xa1), 2_000_000)},
			Want: []string{"DELEG/StakeKeyHasNonZeroAccountBalanceDELEG"}},
		{Name: "deleg deregistering an unregistered key", Rule: "deleg", Certs: []Cert{UnregCert(b1, 2_000_000)},
			Want: []string{"DELEG/StakeKeyNotRegisteredDELEG"}},
		{Name: "deleg wrong refund", Rule: "deleg", Certs: []Cert{UnregCert(Key(0xa2), 1_000_000)},
			Want: []string{"DELEG/IncorrectDepositDELEG"}},
		{Name: "deleg delegates to a registered pool", Rule: "deleg", Certs: []Cert{StakeDeleg(Key(0xa2), PoolID())}},
		{Name: "deleg delegates to an unregistered pool", Rule: "deleg", Certs: []Cert{StakeDeleg(Key(0xa2), Hash(0x99))},
			Want: []string{"DELEG/DelegateeStakePoolNotRegisteredDELEG"}},
		{Name: "deleg delegates an unregistered credential", Rule: "deleg", Certs: []Cert{StakeDeleg(b1, PoolID())},
			Want: []string{"DELEG/StakeKeyNotRegisteredDELEG"}},
		{Name: "deleg votes for a registered drep", Rule: "deleg", Certs: []Cert{VoteDeleg(Key(0xa2), DRepKey(0xd1))}},
		{Name: "deleg votes for an unregistered drep", Rule: "deleg", Certs: []Cert{VoteDeleg(Key(0xa2), DRepKey(0xd9))},
			Want: []string{"DELEG/DelegateeDRepNotRegisteredDELEG"}},
		{Name: "deleg votes always-abstain", Rule: "deleg", Certs: []Cert{VoteDeleg(Key(0xa2), Abstain)}},
		{Name: "deleg registers and delegates", Rule: "deleg", Certs: []Cert{StakeRegDeleg(b1, PoolID(), 2_000_000)}},
		{Name: "deleg runs only its own certificates in order", Rule: "deleg",
			Certs: []Cert{RegCert(b1, 2_000_000), RegCert(b2, 2_000_000), StakeDeleg(b1, PoolID())}},
		{Name: "deleg reports every failing certificate", Rule: "deleg",
			Certs: []Cert{RegCert(b1, 2_000_000), StakeReg(b1), StakeDeleg(b2, PoolID())},
			Want:  []string{"DELEG/StakeKeyNotRegisteredDELEG", "DELEG/StakeKeyRegisteredDELEG"}},
		{Name: "deleg cert_index selects one certificate", Rule: "deleg", CertIndex: u32(1),
			Certs: []Cert{RegCert(Key(0xa1), 2_000_000), RegCert(b1, 2_000_000)}},
		{Name: "pool registers a new pool", Rule: "pool", Certs: []Cert{PoolReg(p2, 0x62, 100, 170_000_000, 1, 0xb1)}},
		{Name: "pool cost below the minimum", Rule: "pool", Certs: []Cert{PoolReg(p2, 0x62, 100, 1, 1, 0xb1)},
			Want: []string{"POOL/StakePoolCostTooLowPOOL"}},
		{Name: "pool reward account on the wrong network", Rule: "pool", Certs: []Cert{PoolReg(p2, 0x62, 100, 170_000_000, 0, 0xb1)},
			Want: []string{"POOL/WrongNetworkPOOL"}},
		{Name: "pool re-registration becomes future parameters", Rule: "pool", Certs: []Cert{PoolReg(PoolID(), 0x63, 200, 180_000_000, 1, 0xa1)}},
		{Name: "pool retirement within emax", Rule: "pool", Certs: []Cert{PoolRetire(PoolID(), 618)}},
		{Name: "pool retirement at the current epoch", Rule: "pool", Certs: []Cert{PoolRetire(PoolID(), 600)},
			Want: []string{"POOL/StakePoolRetirementWrongEpochPOOL"}},
		{Name: "pool retirement past emax", Rule: "pool", Certs: []Cert{PoolRetire(PoolID(), 619)},
			Want: []string{"POOL/StakePoolRetirementWrongEpochPOOL"}},
		{Name: "pool retirement of an unregistered pool", Rule: "pool", Certs: []Cert{PoolRetire(p2, 610)},
			Want: []string{"POOL/StakePoolNotRegisteredOnKeyPOOL"}},
		{Name: "govcert registers a drep", Rule: "gov", Certs: []Cert{RegDRep(Key(0xd2), 500_000_000)}},
		{Name: "govcert registering a registered drep", Rule: "gov", Certs: []Cert{RegDRep(Key(0xd1), 500_000_000)},
			Want: []string{"GOVCERT/ConwayDRepAlreadyRegistered"}},
		{Name: "govcert wrong drep deposit", Rule: "gov", Certs: []Cert{RegDRep(Key(0xd2), 1)},
			Want: []string{"GOVCERT/ConwayDRepIncorrectDeposit"}},
		{Name: "govcert deregisters a drep", Rule: "gov", Certs: []Cert{UnregDRep(Key(0xd1), 500_000_000)}},
		{Name: "govcert wrong drep refund", Rule: "gov", Certs: []Cert{UnregDRep(Key(0xd1), 1)},
			Want: []string{"GOVCERT/ConwayDRepIncorrectRefund"}},
		{Name: "govcert deregistering an unknown drep", Rule: "gov", Certs: []Cert{UnregDRep(Key(0xd9), 500_000_000)},
			Want: []string{"GOVCERT/ConwayDRepNotRegistered"}},
		{Name: "govcert updates a drep anchor", Rule: "gov", Certs: []Cert{UpdateDRep(Key(0xd1), "https://example.com/d1", Hash32(0x11))}},
		{Name: "govcert authorizes a hot key", Rule: "gov", Certs: []Cert{AuthHot(Key(0xc1), Key(0xe1))}},
		{Name: "govcert authorizing an unknown member", Rule: "gov", Certs: []Cert{AuthHot(Key(0xc9), Key(0xe9))},
			Want: []string{"GOVCERT/ConwayCommitteeIsUnknown"}},
		{Name: "govcert authorizing a resigned member", Rule: "gov", Certs: []Cert{AuthHot(Key(0xc3), Key(0xe3))},
			Want: []string{"GOVCERT/ConwayCommitteeHasPreviouslyResigned"}},
		{Name: "govcert resigns a member", Rule: "gov", Certs: []Cert{ResignCold(Key(0xc2))}},
		{Name: "certs withdrawal drains the account", Rule: "certs", Withdrawals: []Withdrawal{{Cred: Key(0xa1), Amount: 5_000_000}}},
		{Name: "certs partial withdrawal", Rule: "certs", Withdrawals: []Withdrawal{{Cred: Key(0xa1), Amount: 1_000_000}},
			Want: []string{"CERTS/WithdrawalsNotInRewardsCERTS"}},
		{Name: "certs withdrawal from an unregistered account", Rule: "certs", Withdrawals: []Withdrawal{{Cred: b1, Amount: 0}},
			Want: []string{"CERTS/WithdrawalsNotInRewardsCERTS"}},
		{Name: "certs withdrawals are not checked after PV10", Rule: "certs", PV: 11, Withdrawals: []Withdrawal{{Cred: Key(0xa1), Amount: 1_000_000}}},
		{Name: "certs run every rule over the sequence", Rule: "certs",
			Certs: []Cert{RegCert(b1, 2_000_000), StakeDeleg(b1, PoolID()), RegDRep(Key(0xd2), 500_000_000)}},
		{Name: "certs report failures from several rules", Rule: "certs",
			Certs: []Cert{RegCert(b1, 2_000_000), StakeDeleg(b1, Hash(0x99)), RegDRep(Key(0xd1), 500_000_000)},
			Want:  []string{"DELEG/DelegateeStakePoolNotRegisteredDELEG", "GOVCERT/ConwayDRepAlreadyRegistered"}},
	}
	for i := range cases {
		cases[i].Delta = deltaDigests[cases[i].Name]
	}
	return cases
}

// deltaDigests holds the expected Digest of every accepting case.
var deltaDigests = map[string]string{
	"deleg registers a new credential":                     "A+1 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg legacy registration pays the parameter deposit": "A+1 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg deregistering a drained account":                "A+0 A~0 A-1 P~0 P-0 D~0 D-0 C0",
	"deleg delegates to a registered pool":                 "A+0 A~1 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg votes for a registered drep":                    "A+0 A~1 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg votes always-abstain":                           "A+0 A~1 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg registers and delegates":                        "A+1 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg runs only its own certificates in order":        "A+2 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"deleg cert_index selects one certificate":             "A+1 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"pool registers a new pool":                            "A+0 A~0 A-0 P~1 P-0 D~0 D-0 C0",
	"pool re-registration becomes future parameters":       "A+0 A~0 A-0 P~1 P-0 D~0 D-0 C0",
	"pool retirement within emax":                          "A+0 A~0 A-0 P~1 P-0 D~0 D-0 C0",
	"govcert registers a drep":                             "A+0 A~0 A-0 P~0 P-0 D~1 D-0 C0",
	"govcert deregisters a drep":                           "A+0 A~0 A-0 P~0 P-0 D~0 D-1 C0",
	"govcert updates a drep anchor":                        "A+0 A~0 A-0 P~0 P-0 D~1 D-0 C0",
	"govcert authorizes a hot key":                         "A+0 A~0 A-0 P~0 P-0 D~0 D-0 C1",
	"govcert resigns a member":                             "A+0 A~0 A-0 P~0 P-0 D~0 D-0 C1",
	"certs withdrawal drains the account":                  "A+0 A~1 A-0 P~0 P-0 D~0 D-0 C0",
	"certs withdrawals are not checked after PV10":         "A+0 A~0 A-0 P~0 P-0 D~0 D-0 C0",
	"certs run every rule over the sequence":               "A+1 A~0 A-0 P~0 P-0 D~1 D-0 C0",
}
