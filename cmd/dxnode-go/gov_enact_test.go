package main

import (
	"bytes"
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected deltas are derived by hand from Rules/Enact.hs (enactmentTransition)
// and the EPOCH rule of Rules/Epoch.hs that pays enacted withdrawals to
// registered accounts, leaves the rest in the treasury, and returns the
// enacted proposal's deposit. Same cases as Test/Gov.hs.

// enactParams are full Conway parameters with the voting thresholds of
// govParams.
func enactParams() *v1.ProtocolParameters {
	pp := govParams(10)
	u64 := func(x uint64) *uint64 { return &x }
	u32 := func(x uint32) *uint32 { return &x }
	pp.MinFeeA, pp.MinFeeB = u64(44), u64(155381)
	pp.MaxBlockBodySize, pp.MaxTxSize, pp.MaxBlockHeaderSize = u32(65536), u32(16384), u32(1100)
	pp.KeyDeposit, pp.PoolDeposit, pp.MaxEpoch, pp.NOpt = u64(2000000), u64(500000000), u64(18), u32(500)
	pp.A0, pp.Rho, pp.Tau = rat("3", "10"), rat("3", "1000"), rat("1", "5")
	pp.MinPoolCost, pp.CoinsPerUtxoByte = u64(170000000), u64(4310)
	pp.PriceMem, pp.PriceSteps = rat("577", "10000"), rat("721", "10000000")
	pp.MaxTxExMem, pp.MaxTxExSteps, pp.MaxBlockExMem, pp.MaxBlockExSteps = u64(14000000), u64(10000000000), u64(62000000), u64(20000000000)
	pp.MaxValueSize, pp.CollateralPercentage, pp.MaxCollateralInputs = u32(5000), u32(150), u32(3)
	pp.CommitteeMaxTermLength, pp.GovActionLifetime, pp.GovActionDeposit = u64(146), u64(6), u64(100000000000)
	pp.DrepDeposit, pp.DrepActivity = u64(500000000), u64(20)
	pp.MinFeeRefScriptCostPerByte = rat("15", "1")
	return pp
}

func enactRequest(id byte, action []byte, st *v1.LedgerState) *v1.GovEnactRequest {
	if st == nil {
		st = &v1.LedgerState{}
	}
	return &v1.GovEnactRequest{Pparams: enactParams(), State: st, ActionId: actionID(id), Action: action}
}

func creditMap(cs []*v1.Credit) map[byte]uint64 {
	out := map[byte]uint64{}
	for _, c := range cs {
		out[c.GetCredential().GetHash()[0]] += c.GetAmount()
	}
	return out
}

func mustEnact(t *testing.T, req *v1.GovEnactRequest) *v1.GovEnactResponse {
	t.Helper()
	got, err := govEnact(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	if got.GetRejection() != nil {
		t.Fatalf("rejected: %v", got.GetRejection())
	}
	return got
}

func TestGovEnactParameterChange(t *testing.T) {
	t.Parallel()
	got := mustEnact(t, enactRequest(1, pcAction(0), nil))
	d := got.GetDelta()
	// The update sets key 2, the maximum block body size.
	if g := d.GetPparamsAfter().GetMaxBlockBodySize(); g != 0x16000 {
		t.Errorf("max block body size %d, want %d", g, 0x16000)
	}
	// Everything else is kept.
	want := proto.Clone(enactParams()).(*v1.ProtocolParameters)
	if g := d.GetPparamsAfter().GetMaxTxSize(); g != want.GetMaxTxSize() {
		t.Errorf("max tx size %d changed", g)
	}
	if !proto.Equal(d.GetEnactStateAfter().GetPrevPparamUpdate(), actionID(1)) {
		t.Errorf("prev pparam update %v", d.GetEnactStateAfter().GetPrevPparamUpdate())
	}
	if d.GetEnactStateAfter().GetPrevHardFork() != nil || len(got.GetWithdrawals()) != 0 || d.GetTreasuryDelta() != 0 {
		t.Errorf("unexpected effects: %v", got)
	}
}

func TestGovEnactHardFork(t *testing.T) {
	t.Parallel()
	got := mustEnact(t, enactRequest(1, hfAction(0), nil))
	d := got.GetDelta()
	if v := d.GetPparamsAfter().GetProtocolVersion(); v.GetMajor() != 10 || v.GetMinor() != 0 {
		t.Errorf("protocol version %v", v)
	}
	if !proto.Equal(d.GetEnactStateAfter().GetPrevHardFork(), actionID(1)) {
		t.Errorf("prev hard fork %v", d.GetEnactStateAfter().GetPrevHardFork())
	}
}

// 500 goes to the registered account a1, 300 to an account that is not
// registered and stays in the treasury: the treasury falls by 500.
func TestGovEnactTreasuryWithdrawal(t *testing.T) {
	t.Parallel()
	st := &v1.LedgerState{
		Pots:     &v1.Pots{Treasury: 2000},
		Accounts: []*v1.Account{{Credential: cred(0x61), Balance: 7}},
	}
	got := mustEnact(t, enactRequest(1, withdrawalAction([2]uint64{0x61, 500}, [2]uint64{0x62, 300}), st))
	if m := creditMap(got.GetWithdrawals()); len(m) != 1 || m[0x61] != 500 {
		t.Errorf("withdrawals %v, want 500 to 61", m)
	}
	d := got.GetDelta()
	if len(d.GetAccountsChanged()) != 1 || d.GetAccountsChanged()[0].GetBalance() != 507 {
		t.Errorf("accounts changed %v, want 61 at 507", d.GetAccountsChanged())
	}
	if d.GetTreasuryDelta() != -500 {
		t.Errorf("treasury delta %d, want -500", d.GetTreasuryDelta())
	}
}

// The deposit of the enacted proposal goes back to its return account, or to
// the treasury when that account is not registered.
func TestGovEnactReturnsTheDeposit(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		registered bool
		credit     uint64
		treasury   int64
	}{"registered": {true, 100, 0}, "unregistered": {false, 0, 100}} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			p := proposal(1, actInfo)
			p.Deposit, p.ReturnAccount = 100, cred(0x61)
			st := &v1.LedgerState{Proposals: []*v1.Proposal{p}}
			if tc.registered {
				st.Accounts = []*v1.Account{{Credential: cred(0x61), Balance: 7}}
			}
			got := mustEnact(t, enactRequest(1, actInfo, st))
			if m := creditMap(got.GetWithdrawals()); m[0x61] != tc.credit {
				t.Errorf("credit %d, want %d", m[0x61], tc.credit)
			}
			if got.GetDelta().GetTreasuryDelta() != tc.treasury {
				t.Errorf("treasury delta %d, want %d", got.GetDelta().GetTreasuryDelta(), tc.treasury)
			}
		})
	}
}

func TestGovEnactNoConfidence(t *testing.T) {
	t.Parallel()
	st := &v1.LedgerState{Committee: testCommittee(false)}
	got := mustEnact(t, enactRequest(1, actNoConfidence, st))
	d := got.GetDelta()
	if c := d.GetCommitteeAfter(); c == nil || len(c.GetMembers()) != 0 || c.GetThreshold() != nil {
		t.Errorf("committee after %v, want an empty committee with no threshold", c)
	}
	if !proto.Equal(d.GetEnactStateAfter().GetPrevCommittee(), actionID(1)) {
		t.Errorf("prev committee %v", d.GetEnactStateAfter().GetPrevCommittee())
	}
}

// Removes member 11 (credential kind key, hash 0x11 repeated), adds a member
// with hash 0x77 expiring at 100 and sets the quorum to 3/4.
func updateCommitteeAction() []byte {
	out := mustHex("8504f6" + "d90102" + "81" + "82" + "00" + "581c")
	out = append(out, ids(0x11)...)
	out = append(out, mustHex("a1"+"82"+"00"+"581c")...)
	out = append(out, ids(0x77)...)
	return append(out, mustHex("1864"+"d81e"+"82"+"03"+"04")...)
}

func TestGovEnactUpdateCommittee(t *testing.T) {
	t.Parallel()
	st := &v1.LedgerState{Committee: testCommittee(false)}
	got := mustEnact(t, enactRequest(1, updateCommitteeAction(), st))
	c := got.GetDelta().GetCommitteeAfter()
	var cold []byte
	for _, m := range c.GetMembers() {
		cold = append(cold, m.GetCold().GetHash()[0])
		if m.GetExpiry() == 0 {
			t.Errorf("member %x has no expiry", m.GetCold().GetHash()[0])
		}
	}
	// Members 12 and 13 stay, 11 leaves, 77 joins; sorted by hash.
	if !bytes.Equal(cold, []byte{0x12, 0x13, 0x77}) {
		t.Errorf("members %x, want 12 13 77", cold)
	}
	if ratString(c.GetThreshold()) != "3/4" {
		t.Errorf("threshold %s, want 3/4", ratString(c.GetThreshold()))
	}
	if !proto.Equal(got.GetDelta().GetEnactStateAfter().GetPrevCommittee(), actionID(1)) {
		t.Errorf("prev committee %v", got.GetDelta().GetEnactStateAfter().GetPrevCommittee())
	}
}

func TestGovEnactNewConstitution(t *testing.T) {
	t.Parallel()
	got := mustEnact(t, enactRequest(1, actConstitution, nil))
	es := got.GetDelta().GetEnactStateAfter()
	if es.GetConstitution().GetAnchorUrl() != "x" || !bytes.Equal(es.GetConstitution().GetAnchorHash(), make([]byte, 32)) {
		t.Errorf("constitution %v", es.GetConstitution())
	}
	if !proto.Equal(es.GetPrevConstitution(), actionID(1)) {
		t.Errorf("prev constitution %v", es.GetPrevConstitution())
	}
}

// Enacting leaves the other purposes' roots as they were.
func TestGovEnactKeepsOtherRoots(t *testing.T) {
	t.Parallel()
	st := &v1.LedgerState{EnactState: &v1.EnactState{
		PrevHardFork: actionID(9), PrevCommittee: actionID(8),
		Constitution: &v1.Constitution{AnchorUrl: "https://c", AnchorHash: bytes.Repeat([]byte{3}, 32)},
	}, Committee: testCommittee(false)}
	got := mustEnact(t, enactRequest(1, pcAction(0), st))
	es := got.GetDelta().GetEnactStateAfter()
	if !proto.Equal(es.GetPrevHardFork(), actionID(9)) || !proto.Equal(es.GetPrevCommittee(), actionID(8)) {
		t.Errorf("roots %v", es)
	}
	if es.GetConstitution().GetAnchorUrl() != "https://c" {
		t.Errorf("constitution %v", es.GetConstitution())
	}
}

func TestGovEnactDomain(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		req  *v1.GovEnactRequest
		code v1.HarnessErrorCode
	}{
		"withdrawal above the treasury": {
			enactRequest(1, withdrawalAction([2]uint64{0x61, 500}), &v1.LedgerState{Pots: &v1.Pots{Treasury: 499}}),
			v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED,
		},
		"parent is not the root": {
			enactRequest(1, pcAction(9), nil), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED,
		},
		"undecodable action": {
			enactRequest(1, []byte{0xff}, nil), v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST,
		},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			_, err := govEnact(context.Background(), tc.req)
			if code := dispatch.Code(err); code != tc.code {
				t.Fatalf("code %v, err %v", code, err)
			}
		})
	}
}
