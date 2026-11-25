package main

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	cf "github.com/chrisguiney/dxdingo/internal/certfixture"
)

func runCertCase(t *testing.T, c cf.Case) ([]string, *v1.LedgerDelta) {
	t.Helper()
	ctx, pp, st := cf.Env()
	if c.PV != 0 {
		ctx.ProtocolVersion.Major = c.PV
		pp.ProtocolVersion.Major = c.PV
	}
	if c.Mutate != nil {
		c.Mutate(st)
	}
	tx := cf.Tx(c.Certs, c.Withdrawals)
	var rej *v1.Rejection
	var delta *v1.LedgerDelta
	switch c.Rule {
	case "deleg":
		r, err := ruleDeleg(context.Background(), &v1.RuleDelegRequest{Context: ctx, Pparams: pp, State: st, Tx: tx, CertIndex: c.CertIndex})
		require.NoError(t, err)
		rej, delta = r.Rejection, r.Delta
	case "pool":
		r, err := rulePool(context.Background(), &v1.RulePoolRequest{Context: ctx, Pparams: pp, State: st, Tx: tx, CertIndex: c.CertIndex})
		require.NoError(t, err)
		rej, delta = r.Rejection, r.Delta
	case "gov":
		r, err := ruleGovCert(context.Background(), &v1.RuleGovCertRequest{Context: ctx, Pparams: pp, State: st, Tx: tx, CertIndex: c.CertIndex})
		require.NoError(t, err)
		rej, delta = r.Rejection, r.Delta
	case "certs":
		r, err := ruleCerts(context.Background(), &v1.RuleCertsRequest{Context: ctx, Pparams: pp, State: st, Tx: tx})
		require.NoError(t, err)
		rej, delta = r.Rejection, r.Delta
	}
	var got []string
	for _, f := range rej.GetFailures() {
		got = append(got, f.GetRule()+"/"+f.GetName())
	}
	sort.Strings(got)
	return got, delta
}

// certChecks asserts the delta of accepted cases, by case name.
var certChecks = map[string]func(*testing.T, *v1.LedgerDelta){
	"deleg registers a new credential": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsAdded, 1)
		require.Equal(t, uint64(2_000_000), d.AccountsAdded[0].Deposit)
		require.Empty(t, d.AccountsChanged)
	},
	"deleg legacy registration pays the parameter deposit": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsAdded, 1)
		require.Equal(t, uint64(2_000_000), d.AccountsAdded[0].Deposit)
	},
	"deleg deregistering a drained account": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsRemoved, 1)
	},
	"deleg delegates to a registered pool": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsChanged, 1)
		require.Equal(t, cf.PoolID(), d.AccountsChanged[0].PoolId)
	},
	"deleg votes for a registered drep": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsChanged, 1)
		require.NotNil(t, d.AccountsChanged[0].Drep)
	},
	"deleg registers and delegates": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsAdded, 1)
		require.Equal(t, cf.PoolID(), d.AccountsAdded[0].PoolId)
	},
	"deleg runs only its own certificates in order": func(t *testing.T, d *v1.LedgerDelta) { require.Len(t, d.AccountsAdded, 2) },
	"deleg cert_index selects one certificate":      func(t *testing.T, d *v1.LedgerDelta) { require.Len(t, d.AccountsAdded, 1) },
	"pool registers a new pool": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.PoolsChanged, 1)
		require.Equal(t, v1.PoolStage_POOL_STAGE_CURRENT, d.PoolsChanged[0].Stage)
		require.Equal(t, uint64(500_000_000), d.PoolsChanged[0].Deposit)
	},
	"pool re-registration becomes future parameters": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.PoolsChanged, 1)
		require.Equal(t, v1.PoolStage_POOL_STAGE_FUTURE, d.PoolsChanged[0].Stage)
		require.Equal(t, uint64(200), d.PoolsChanged[0].Pledge)
	},
	"pool retirement within emax": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.PoolsChanged, 1)
		require.Equal(t, uint64(618), d.PoolsChanged[0].GetRetiringEpoch())
	},
	"govcert registers a drep": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.DrepsChanged, 1)
		require.Equal(t, uint64(500_000_000), d.DrepsChanged[0].Deposit)
		require.Equal(t, uint64(620), d.DrepsChanged[0].Expiry)
	},
	"govcert deregisters a drep": func(t *testing.T, d *v1.LedgerDelta) { require.Len(t, d.DrepsRemoved, 1) },
	"govcert updates a drep anchor": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.DrepsChanged, 1)
		require.Equal(t, "https://example.com/d1", d.DrepsChanged[0].AnchorUrl)
	},
	"govcert authorizes a hot key": func(t *testing.T, d *v1.LedgerDelta) { require.NotNil(t, d.CommitteeAfter) },
	"govcert resigns a member":     func(t *testing.T, d *v1.LedgerDelta) { require.NotNil(t, d.CommitteeAfter) },
	"certs withdrawal drains the account": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsChanged, 1)
		require.Equal(t, uint64(0), d.AccountsChanged[0].Balance)
	},
	"certs run every rule over the sequence": func(t *testing.T, d *v1.LedgerDelta) {
		require.Len(t, d.AccountsAdded, 1)
		require.Equal(t, cf.PoolID(), d.AccountsAdded[0].PoolId)
		require.Len(t, d.DrepsChanged, 1)
	},
}

func TestRuleCertsCases(t *testing.T) {
	t.Parallel()
	for _, c := range cf.Cases() {
		t.Run(c.Name, func(t *testing.T) {
			t.Parallel()
			got, delta := runCertCase(t, c)
			require.Equal(t, c.Want, got)
			if c.Want == nil {
				require.NotNil(t, delta)
				require.Equal(t, c.Delta, cf.Digest(delta))
				if check := certChecks[c.Name]; check != nil {
					check(t, delta)
				}
			} else {
				require.Nil(t, delta)
			}
		})
	}
}
