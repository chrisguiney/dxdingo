package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
	lf "github.com/chrisguiney/dxdingo/internal/ledgerfixture"
)

func TestRuleGovCases(t *testing.T) {
	t.Parallel()
	for _, c := range lf.GovCases() {
		t.Run(c.Name, func(t *testing.T) {
			t.Parallel()
			resp, err := ruleGov(context.Background(), lf.GovRequest(c))
			require.NoError(t, err)
			var got []string
			for _, f := range resp.GetRejection().GetFailures() {
				got = append(got, f.GetRule()+"/"+f.GetName())
			}
			sort.Strings(got)
			require.Equal(t, c.Want, got)
			if c.Want == nil {
				require.NotNil(t, resp.GetDelta())
				require.Equal(t, c.Delta, lf.Digest(resp.GetDelta()))
			} else {
				require.Nil(t, resp.GetDelta())
			}
		})
	}
}

// The digest counts vote rows; this checks the voter each row names.
func TestRuleGovVoteRowsNameTheirVoter(t *testing.T) {
	t.Parallel()
	byName := map[string]lf.GovCase{}
	for _, c := range lf.GovCases() {
		byName[c.Name] = c
	}
	for _, tc := range []struct {
		name string
		role v1.VoterRole
		kind v1.CredentialKind
		hash []byte
	}{
		{"gov drep votes on an active proposal", v1.VoterRole_VOTER_ROLE_DREP, v1.CredentialKind_CREDENTIAL_KIND_KEY, cf.Hash(0xd1)},
		{"gov committee member votes with an authorized hot key", v1.VoterRole_VOTER_ROLE_CONSTITUTIONAL_COMMITTEE, v1.CredentialKind_CREDENTIAL_KIND_KEY, cf.Hash(0xe2)},
	} {
		resp, err := ruleGov(context.Background(), lf.GovRequest(byName[tc.name]))
		require.NoError(t, err)
		require.Len(t, resp.GetDelta().GetVotesAdded(), 1)
		v := resp.GetDelta().GetVotesAdded()[0]
		require.Equal(t, tc.role, v.GetVoter().GetRole())
		require.Equal(t, tc.kind, v.GetVoter().GetCredential().GetKind())
		require.Equal(t, tc.hash, v.GetVoter().GetCredential().GetHash())
		require.Equal(t, v1.Vote_VOTE_YES, v.GetVote())
		require.Equal(t, lf.ProposalTxID(), v.GetActionId().GetTxId())
	}
	resp, err := ruleGov(context.Background(), lf.GovRequest(byName["gov stake pool votes on an info action"]))
	require.NoError(t, err)
	v := resp.GetDelta().GetVotesAdded()[0]
	require.Equal(t, v1.VoterRole_VOTER_ROLE_STAKE_POOL, v.GetVoter().GetRole())
	require.Equal(t, cf.PoolID(), v.GetVoter().GetPoolId())
}
