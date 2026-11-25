package main

import (
	"bytes"
	"context"
	"fmt"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/dxoracle"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// drepDistribution runs dingo's production DRep stake loader, whose sums are
// SQL, against a scratch database seeded from the request.
//
// Instant stake becomes one live UTxO per credential. Every listed account
// and DRep is registered (active); the request has no deregistered rows.
func drepDistribution(ctx context.Context, req *v1.StakeDRepDistributionRequest) (*v1.StakeDRepDistributionResponse, error) {
	in, err := drepDistributionInput(req)
	if err != nil {
		return nil, dispatch.BadRequest(err)
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	entries, err := oracle.DRepStakeDistribution(ctx, in)
	if err != nil {
		return &v1.StakeDRepDistributionResponse{Rejection: dingoRejection("drep-distribution", err)}, nil
	}
	resp := &v1.StakeDRepDistributionResponse{}
	for _, e := range entries {
		d, err := drepFromEntry(e)
		if err != nil {
			return nil, err
		}
		resp.Dreps = append(resp.Dreps, &v1.DRepStake{Drep: d, Stake: e.Stake})
	}
	sortDRepStakes(resp.Dreps)
	return resp, nil
}

func drepDistributionInput(req *v1.StakeDRepDistributionRequest) (dxoracle.DRepStakeDistributionInput, error) {
	in := dxoracle.DRepStakeDistributionInput{ActiveProposalEpoch: req.GetEpoch()}
	for _, s := range req.GetInstantStake() {
		tag, err := credentialTag(s.GetCredential())
		if err != nil {
			return in, fmt.Errorf("instant_stake: %w", err)
		}
		in.Utxos = append(in.Utxos, dxoracle.Utxo{Tag: tag, Key: s.GetCredential().GetHash(), Amount: s.GetAmount()})
	}
	for _, a := range req.GetAccounts() {
		tag, err := credentialTag(a.GetCredential())
		if err != nil {
			return in, fmt.Errorf("accounts: %w", err)
		}
		del, err := delegation(a.GetDrep())
		if err != nil {
			return in, fmt.Errorf("accounts: %w", err)
		}
		in.Accounts = append(in.Accounts, dxoracle.Account{
			Tag: tag, Key: a.GetCredential().GetHash(), Reward: a.GetBalance(), Active: true, Delegation: del,
		})
	}
	for _, d := range req.GetDreps() {
		tag, err := credentialTag(d.GetCredential())
		if err != nil {
			return in, fmt.Errorf("dreps: %w", err)
		}
		in.Dreps = append(in.Dreps, dxoracle.DRep{Tag: tag, Key: d.GetCredential().GetHash(), ExpiryEpoch: d.GetExpiry(), Active: true})
	}
	for _, p := range req.GetProposals() {
		tag, err := credentialTag(p.GetReturnAccount())
		if err != nil {
			return in, fmt.Errorf("proposals: %w", err)
		}
		in.Proposals = append(in.Proposals, dxoracle.Proposal{
			Deposit:       p.GetDeposit(),
			ReturnAccount: dxoracle.Credential{Tag: tag, Key: p.GetReturnAccount().GetHash()},
			ProposedEpoch: p.GetProposedEpoch(),
			ExpiresEpoch:  p.GetExpiresEpoch(),
		})
	}
	return in, nil
}

// credentialTag maps to the Cardano CBOR credential tag dingo stores.
func credentialTag(c *v1.Credential) (uint8, error) {
	switch c.GetKind() {
	case v1.CredentialKind_CREDENTIAL_KIND_KEY:
		return 0, nil
	case v1.CredentialKind_CREDENTIAL_KIND_SCRIPT:
		return 1, nil
	}
	return 0, fmt.Errorf("credential kind %v", c.GetKind())
}

func delegation(d *v1.DRep) (dxoracle.Delegation, error) {
	switch t := d.GetTarget().(type) {
	case nil:
		return dxoracle.Delegation{Type: dxoracle.DelegateNone}, nil
	case *v1.DRep_AlwaysAbstain:
		return dxoracle.Delegation{Type: dxoracle.DelegateAlwaysAbstain}, nil
	case *v1.DRep_AlwaysNoConfidence:
		return dxoracle.Delegation{Type: dxoracle.DelegateAlwaysNoConfidence}, nil
	case *v1.DRep_Credential:
		tag, err := credentialTag(t.Credential)
		if err != nil {
			return dxoracle.Delegation{}, err
		}
		kind := dxoracle.DelegateKeyHash
		if tag == 1 {
			kind = dxoracle.DelegateScriptHash
		}
		return dxoracle.Delegation{Type: kind, Key: t.Credential.GetHash()}, nil
	}
	return dxoracle.Delegation{}, fmt.Errorf("drep target %T", d.GetTarget())
}

func drepFromEntry(e models.DRepStakeDistributionEntry) (*v1.DRep, error) {
	switch uint64(e.DRepType) {
	case models.DrepTypeAddrKeyHash:
		return &v1.DRep{Target: &v1.DRep_Credential{Credential: &v1.Credential{
			Kind: v1.CredentialKind_CREDENTIAL_KIND_KEY, Hash: e.Credential,
		}}}, nil
	case models.DrepTypeScriptHash:
		return &v1.DRep{Target: &v1.DRep_Credential{Credential: &v1.Credential{
			Kind: v1.CredentialKind_CREDENTIAL_KIND_SCRIPT, Hash: e.Credential,
		}}}, nil
	case models.DrepTypeAlwaysAbstain:
		return &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}, nil
	case models.DrepTypeAlwaysNoConfidence:
		return &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}, nil
	}
	return nil, fmt.Errorf("dingo drep type %d", e.DRepType)
}

// sortDRepStakes puts the set in canonical order: credential DReps by kind
// then hash, then always-abstain, then always-no-confidence.
func sortDRepStakes(ds []*v1.DRepStake) {
	rank := func(d *v1.DRep) int {
		switch {
		case d.GetCredential() != nil:
			return int(d.GetCredential().GetKind())
		case d.GetAlwaysAbstain():
			return 10
		default:
			return 11
		}
	}
	sort.Slice(ds, func(i, j int) bool {
		ri, rj := rank(ds[i].GetDrep()), rank(ds[j].GetDrep())
		if ri != rj {
			return ri < rj
		}
		return bytes.Compare(ds[i].GetDrep().GetCredential().GetHash(), ds[j].GetDrep().GetCredential().GetHash()) < 0
	})
}

// dingoRejection reports a dingo error where the reference would return a
// value, so the comparator shows a mismatch rather than skipping the call.
func dingoRejection(rule string, err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: rule, Name: "DingoError", Detail: err.Error()}}}
}

func registerStake(r *dispatch.Registry) {
	dispatch.Register(r, drepDistribution)
}
