package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected values are derived by hand from Cardano.Ledger.Conway.Governance.
// Internal (voting thresholds) and Rules.Ratify (accepted ratios). The same
// cases are in haskell/dxnode-hs/test/Test/Gov.hs.

func mustHex(s string) []byte {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return b
}

// GovAction CBOR. Tags: 0 parameter change, 1 hard fork, 2 treasury
// withdrawals, 3 no confidence, 4 update committee, 5 new constitution, 6 info.
var (
	actNoConfidence = mustHex("8203f6")
	actInfo         = mustHex("8106")
	actHardFork     = mustHex("8301f6820a00")
	actTreasury     = mustHex("8302a0f6")
	actUpdateCmte   = mustHex("8504f6d9010280a0d81e820102")
	actConstitution []byte
	// maxBBSize (network and security groups), keyDeposit (economic),
	// nOpt (technical), committeeMaxTermLength (governance).
	actPCNetworkSecurity = mustHex("8400f6a1021a00016000f6")
	actPCEconomic        = mustHex("8400f6a1051a001e8480f6")
	actPCTechnical       = mustHex("8400f6a1081901f4f6")
	actPCGov             = mustHex("8400f6a1181c0af6")
)

func init() {
	// new constitution: anchor [url "x", 32-byte hash], no guardrail script.
	actConstitution = append(mustHex("8305f68282617858"+"20"), append(bytes.Repeat([]byte{0}, 32), 0xf6)...)
}

func ids(b byte) []byte { return bytes.Repeat([]byte{b}, 28) }

func cred(b byte) *v1.Credential {
	return &v1.Credential{Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: ids(b)}
}

func pv(major uint32) *v1.ProtocolVersion { return &v1.ProtocolVersion{Major: major} }

func govParams(major uint32) *v1.ProtocolParameters {
	minSize := uint32(3)
	return &v1.ProtocolParameters{
		ProtocolVersion: pv(major),
		PoolVotingThresholds: &v1.PoolVotingThresholds{
			MotionNoConfidence: rat("51", "100"), CommitteeNormal: rat("52", "100"),
			CommitteeNoConfidence: rat("53", "100"), HardForkInitiation: rat("54", "100"),
			PpSecurityGroup: rat("55", "100"),
		},
		DrepVotingThresholds: &v1.DRepVotingThresholds{
			MotionNoConfidence: rat("61", "100"), CommitteeNormal: rat("62", "100"),
			CommitteeNoConfidence: rat("63", "100"), UpdateToConstitution: rat("64", "100"),
			HardForkInitiation: rat("65", "100"), PpNetworkGroup: rat("66", "100"),
			PpEconomicGroup: rat("67", "100"), PpTechnicalGroup: rat("68", "100"),
			PpGovGroup: rat("69", "100"), TreasuryWithdrawal: rat("70", "100"),
		},
		CommitteeMinSize: &minSize,
	}
}

// govCommittee has three active members with quorum 2/3 and, optionally, a
// resigned and an expired one that never count.
func testCommittee(extra bool) *v1.Committee {
	c := &v1.Committee{Threshold: rat("2", "3")}
	for i := byte(1); i <= 3; i++ {
		c.Members = append(c.Members, &v1.CommitteeMember{Cold: cred(0x10 + i), Hot: cred(0x20 + i), Expiry: 100})
	}
	if extra {
		c.Members = append(c.Members,
			&v1.CommitteeMember{Cold: cred(0x14), Hot: cred(0x24), Expiry: 100, Resigned: true},
			&v1.CommitteeMember{Cold: cred(0x15), Hot: cred(0x25), Expiry: 10},
			&v1.CommitteeMember{Cold: cred(0x16), Expiry: 100})
	}
	return c
}

func TestGovThresholds(t *testing.T) {
	t.Parallel()
	type want struct{ spo, drep, cc string } // "" is unset, else "n/d"
	for name, tc := range map[string]struct {
		action    []byte
		major     uint32
		committee *v1.Committee
		want      want
	}{
		"no confidence":                 {actNoConfidence, 10, testCommittee(false), want{"51/100", "61/100", ""}},
		"info":                          {actInfo, 10, testCommittee(false), want{"", "", ""}},
		"hard fork":                     {actHardFork, 10, testCommittee(false), want{"27/50", "13/20", "2/3"}},
		"treasury withdrawals":          {actTreasury, 10, testCommittee(false), want{"", "7/10", "2/3"}},
		"new constitution":              {actConstitution, 10, testCommittee(false), want{"", "16/25", "2/3"}},
		"update committee, committee":   {actUpdateCmte, 10, testCommittee(false), want{"13/25", "31/50", ""}},
		"update committee, none":        {actUpdateCmte, 10, nil, want{"53/100", "63/100", ""}},
		"parameter change network+sec":  {actPCNetworkSecurity, 10, testCommittee(false), want{"11/20", "33/50", "2/3"}},
		"parameter change economic":     {actPCEconomic, 10, testCommittee(false), want{"", "67/100", "2/3"}},
		"parameter change technical":    {actPCTechnical, 10, testCommittee(false), want{"", "17/25", "2/3"}},
		"parameter change governance":   {actPCGov, 10, testCommittee(false), want{"", "69/100", "2/3"}},
		"no committee":                  {actHardFork, 10, nil, want{"27/50", "13/20", ""}},
		"below minimum committee size":  {actHardFork, 10, withoutMember(testCommittee(false)), want{"27/50", "13/20", ""}},
		"inactive members do not count": {actHardFork, 10, testCommittee(true), want{"27/50", "13/20", "2/3"}},
		"bootstrap zeroes drep":         {actHardFork, 9, testCommittee(false), want{"27/50", "0/1", "2/3"}},
		"bootstrap waives minimum size": {actHardFork, 9, withoutMember(testCommittee(false)), want{"27/50", "0/1", "2/3"}},
		"bootstrap info":                {actInfo, 9, testCommittee(false), want{"", "", ""}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govThresholds(context.Background(), &v1.GovThresholdsRequest{
				ProtocolVersion: pv(tc.major), Pparams: govParams(tc.major), Committee: tc.committee,
				Action: tc.action, Epoch: 50,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil {
				t.Fatalf("rejected: %v", got.GetRejection())
			}
			for role, pair := range map[string][2]any{
				"spo": {got.GetSpo(), tc.want.spo}, "drep": {got.GetDrep(), tc.want.drep}, "committee": {got.GetCommittee(), tc.want.cc},
			} {
				if g := ratString(pair[0].(*v1.Rational)); g != pair[1].(string) {
					t.Errorf("%s: got %q, want %q", role, g, pair[1])
				}
			}
		})
	}
}

func withoutMember(c *v1.Committee) *v1.Committee {
	c.Members = c.Members[:2]
	return c
}

func ratString(r *v1.Rational) string {
	if r == nil {
		return ""
	}
	return r.GetNumerator() + "/" + r.GetDenominator()
}

func TestGovThresholdsBadAction(t *testing.T) {
	t.Parallel()
	_, err := govThresholds(context.Background(), &v1.GovThresholdsRequest{
		ProtocolVersion: pv(10), Pparams: govParams(10), Action: []byte{0xff},
	})
	if err == nil {
		t.Fatal("accepted an undecodable action")
	}
	if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("code %v", code)
	}
}

func TestGovThresholdsOutOfRangeThresholdIsUnsupported(t *testing.T) {
	t.Parallel()
	pp := govParams(10)
	pp.PoolVotingThresholds.HardForkInitiation = rat("3", "2")
	_, err := govThresholds(context.Background(), &v1.GovThresholdsRequest{
		ProtocolVersion: pv(10), Pparams: pp, Action: actHardFork,
	})
	if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("code %v, err %v", code, err)
	}
}

func pool(b byte, stake uint64) *v1.PoolStake { return &v1.PoolStake{PoolId: ids(b), Stake: stake} }

func spoVote(b byte, v v1.Vote, id *v1.GovActionId) *v1.VoteRecord {
	return &v1.VoteRecord{ActionId: id, Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_STAKE_POOL, PoolId: ids(b)}, Vote: v}
}

func actionID(b byte) *v1.GovActionId { return &v1.GovActionId{TxId: bytes.Repeat([]byte{b}, 32)} }

// Four pools with stake 10, 20, 30, 40. Pool 1 votes yes, 2 no, 3 abstain;
// pool 4 is silent.
func spoRequest(major uint32, action []byte, p4Drep *v1.DRep) *v1.GovSpoRatioRequest {
	id := actionID(7)
	req := &v1.GovSpoRatioRequest{
		ProtocolVersion: pv(major), ActionId: id, Action: action,
		PoolDistribution: []*v1.PoolStake{pool(1, 10), pool(2, 20), pool(3, 30), pool(4, 40)},
		Votes: []*v1.VoteRecord{
			spoVote(1, v1.Vote_VOTE_YES, id), spoVote(2, v1.Vote_VOTE_NO, id), spoVote(3, v1.Vote_VOTE_ABSTAIN, id),
			// A vote on another action never counts.
			spoVote(4, v1.Vote_VOTE_YES, actionID(8)),
		},
	}
	if p4Drep != nil {
		req.Pools = []*v1.Pool{{PoolId: ids(4), RewardAccount: cred(0x44)}}
		req.Accounts = []*v1.Account{{Credential: cred(0x44), Drep: p4Drep}}
	}
	return req
}

func alwaysAbstain() *v1.DRep { return &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}} }
func alwaysNoConfidence() *v1.DRep {
	return &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
}

func TestGovSpoRatio(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		req  *v1.GovSpoRatioRequest
		want string
	}{
		// yes 10 / (total 100 - abstain 30); the silent pool is an implicit no.
		"silent pool is no":                       {spoRequest(10, actPCNetworkSecurity, nil), "1/7"},
		"silent pool abstains in bootstrap":       {spoRequest(9, actPCNetworkSecurity, nil), "1/3"},
		"hard fork: silent pool is no":            {spoRequest(10, actHardFork, alwaysAbstain()), "1/7"},
		"hard fork bootstrap: silent pool is no":  {spoRequest(9, actHardFork, nil), "1/7"},
		"always-no-confidence yes on motion":      {spoRequest(10, actNoConfidence, alwaysNoConfidence()), "5/7"},
		"always-no-confidence is no elsewhere":    {spoRequest(10, actPCNetworkSecurity, alwaysNoConfidence()), "1/7"},
		"always-abstain abstains":                 {spoRequest(10, actPCNetworkSecurity, alwaysAbstain()), "1/3"},
		"always-abstain ignored in bootstrap yes": {spoRequest(9, actNoConfidence, alwaysNoConfidence()), "1/3"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govSpoRatio(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || ratString(got.GetRatio()) != tc.want {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
}

func TestGovSpoRatioAllAbstainIsZero(t *testing.T) {
	t.Parallel()
	id := actionID(7)
	got, err := govSpoRatio(context.Background(), &v1.GovSpoRatioRequest{
		ProtocolVersion: pv(10), ActionId: id, Action: actPCNetworkSecurity,
		PoolDistribution: []*v1.PoolStake{pool(1, 10)},
		Votes:            []*v1.VoteRecord{spoVote(1, v1.Vote_VOTE_ABSTAIN, id)},
	})
	if err != nil || ratString(got.GetRatio()) != "0/1" {
		t.Fatalf("got %v, %v", got, err)
	}
}

func TestGovSpoRatioEmptyDistributionIsUnsupported(t *testing.T) {
	t.Parallel()
	_, err := govSpoRatio(context.Background(), &v1.GovSpoRatioRequest{
		ProtocolVersion: pv(10), ActionId: actionID(7), Action: actPCNetworkSecurity,
	})
	if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("code %v, err %v", code, err)
	}
}

func dstake(d *v1.DRep, stake uint64) *v1.DRepStake { return &v1.DRepStake{Drep: d, Stake: stake} }

func keyDRep(b byte) *v1.DRep { return &v1.DRep{Target: &v1.DRep_Credential{Credential: cred(b)}} }

func drepVote(b byte, v v1.Vote, id *v1.GovActionId) *v1.VoteRecord {
	return &v1.VoteRecord{ActionId: id, Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_DREP, Credential: cred(b)}, Vote: v}
}

// DReps 1..5 hold 10, 20, 30, 40, 50 with expiry 100 (5 expires at 40, 6 is
// unregistered); 1 votes yes, 2 no, 3 abstains, 4 is silent. Always-abstain
// holds 7 and always-no-confidence 9.
func drepRequest(epoch uint64, action []byte) *v1.GovDRepRatioRequest {
	id := actionID(7)
	return &v1.GovDRepRatioRequest{
		Epoch: epoch, ActionId: id, Action: action,
		DrepDistribution: []*v1.DRepStake{
			dstake(keyDRep(1), 10), dstake(keyDRep(2), 20), dstake(keyDRep(3), 30), dstake(keyDRep(4), 40),
			dstake(keyDRep(5), 50), dstake(keyDRep(6), 60), dstake(alwaysAbstain(), 7), dstake(alwaysNoConfidence(), 9),
		},
		Dreps: []*v1.DRepRecord{
			{Credential: cred(1), Expiry: 100}, {Credential: cred(2), Expiry: 100}, {Credential: cred(3), Expiry: 100},
			{Credential: cred(4), Expiry: 100}, {Credential: cred(5), Expiry: 40},
		},
		Votes: []*v1.VoteRecord{
			drepVote(1, v1.Vote_VOTE_YES, id), drepVote(2, v1.Vote_VOTE_NO, id), drepVote(3, v1.Vote_VOTE_ABSTAIN, id),
			drepVote(4, v1.Vote_VOTE_YES, actionID(8)),
		},
	}
}

func TestGovDRepRatio(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		req  *v1.GovDRepRatioRequest
		want string
	}{
		// yes 10; total = 10+20+40 (3 abstains, 5 expired, 6 unregistered)
		// + 9 always-no-confidence = 79.
		"parameter change":    {drepRequest(50, actPCNetworkSecurity), "10/79"},
		"no confidence":       {drepRequest(50, actNoConfidence), "19/79"},
		"expiry is inclusive": {drepRequest(40, actPCNetworkSecurity), "10/129"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govDRepRatio(context.Background(), tc.req)
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || ratString(got.GetRatio()) != tc.want {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
}

func ccVote(b byte, v v1.Vote, id *v1.GovActionId) *v1.VoteRecord {
	return &v1.VoteRecord{ActionId: id, Voter: &v1.Voter{Role: v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, Credential: cred(b)}, Vote: v}
}

func TestGovCommitteeRatio(t *testing.T) {
	t.Parallel()
	id := actionID(7)
	for name, tc := range map[string]struct {
		votes []*v1.VoteRecord
		want  string
	}{
		// Three active members. Silent members count as no.
		"yes, no, silent":               {[]*v1.VoteRecord{ccVote(0x21, v1.Vote_VOTE_YES, id), ccVote(0x22, v1.Vote_VOTE_NO, id)}, "1/3"},
		"abstain leaves out":            {[]*v1.VoteRecord{ccVote(0x21, v1.Vote_VOTE_YES, id), ccVote(0x22, v1.Vote_VOTE_ABSTAIN, id)}, "1/2"},
		"all yes":                       {[]*v1.VoteRecord{ccVote(0x21, v1.Vote_VOTE_YES, id), ccVote(0x22, v1.Vote_VOTE_YES, id), ccVote(0x23, v1.Vote_VOTE_YES, id)}, "1/1"},
		"inactive hot keys do not vote": {[]*v1.VoteRecord{ccVote(0x24, v1.Vote_VOTE_YES, id), ccVote(0x25, v1.Vote_VOTE_YES, id), ccVote(0x21, v1.Vote_VOTE_YES, id)}, "1/3"},
		"other action":                  {[]*v1.VoteRecord{ccVote(0x21, v1.Vote_VOTE_YES, actionID(8))}, "0/1"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govCommitteeRatio(context.Background(), &v1.GovCommitteeRatioRequest{
				Epoch: 50, ProtocolVersion: pv(10), Pparams: govParams(10), Committee: testCommittee(true), ActionId: id, Votes: tc.votes,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || ratString(got.GetRatio()) != tc.want {
				t.Fatalf("got %v, want %s", got, tc.want)
			}
		})
	}
}

func TestGovCommitteeRatioNoCommitteeIsZero(t *testing.T) {
	t.Parallel()
	got, err := govCommitteeRatio(context.Background(), &v1.GovCommitteeRatioRequest{
		Epoch: 50, ProtocolVersion: pv(10), Pparams: govParams(10), ActionId: actionID(7),
	})
	if err != nil || ratString(got.GetRatio()) != "0/1" {
		t.Fatalf("got %v, %v", got, err)
	}
}

func TestGovCommitteeQuorum(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		major     uint32
		committee *v1.Committee
		want      string
	}{
		"in force":                 {10, testCommittee(true), "2/3"},
		"no committee":             {10, nil, ""},
		"below minimum size":       {10, withoutMember(testCommittee(false)), ""},
		"bootstrap waives minimum": {9, withoutMember(testCommittee(false)), "2/3"},
		"bootstrap empty members":  {9, &v1.Committee{Threshold: rat("0", "1")}, "0/1"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govCommitteeQuorum(context.Background(), &v1.GovCommitteeQuorumRequest{
				Epoch: 50, ProtocolVersion: pv(tc.major), Pparams: govParams(tc.major), Committee: tc.committee,
			})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || ratString(got.GetQuorum()) != tc.want {
				t.Fatalf("got %v, want %q", got, tc.want)
			}
		})
	}
}
