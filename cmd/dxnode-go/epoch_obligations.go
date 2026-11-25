package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"

	"github.com/blinklabs-io/dingo/dxoracle"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// epochObligations stores the request's deposits through dingo's own write
// paths and sums the rows read back with dxoracle.ComputeObligations.
//
// Domain, shared with dxnode-hs: repeated accounts, pools, DReps or proposals
// and pools that are not in the current stage are UNSUPPORTED, as is a deposit
// total that exceeds Word64.
func epochObligations(ctx context.Context, req *v1.EpochObligationsRequest) (*v1.EpochObligationsResponse, error) {
	// The reference rejects repeats and non-current pools before it reads any
	// credential, so a repeat of a malformed key is UNSUPPORTED, not BAD_REQUEST.
	for _, kind := range []struct {
		name string
		keys []string
	}{
		{"account", mapKeys(req.GetAccounts(), func(a *v1.Account) string { return credKeyRewardsStake(a.GetCredential()) })},
		{"pool", mapKeys(req.GetPools(), func(p *v1.Pool) string { return string(p.GetPoolId()) })},
		{"drep", mapKeys(req.GetDreps(), func(d *v1.DRepRecord) string { return credKeyRewardsStake(d.GetCredential()) })},
		{"proposal", mapKeys(req.GetProposals(), func(p *v1.Proposal) string {
			return fmt.Sprintf("%x/%d", p.GetId().GetTxId(), p.GetId().GetIndex())
		})},
	} {
		seen := map[string]struct{}{}
		for _, k := range kind.keys {
			if _, dup := seen[k]; dup {
				return nil, unsupportedf("duplicate %s", kind.name)
			}
			seen[k] = struct{}{}
		}
	}
	for _, p := range req.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT {
			return nil, unsupportedf("pool stage is not current")
		}
	}
	var in dxoracle.ObligationsInput
	for _, a := range req.GetAccounts() {
		c, err := obligationCredential("accounts", a.GetCredential())
		if err != nil {
			return nil, err
		}
		in.Accounts = append(in.Accounts, dxoracle.CertAccount{Credential: c, Deposit: a.GetDeposit()})
	}
	for _, p := range req.GetPools() {
		if _, err := rewardPoolID("pools", p.GetPoolId()); err != nil {
			return nil, err
		}
		reward, err := obligationCredential("pools", p.GetRewardAccount())
		if err != nil {
			return nil, err
		}
		in.Pools = append(in.Pools, dxoracle.CertPool{
			PoolKeyHash: p.GetPoolId(), VrfKeyHash: make([]byte, 32), Deposit: p.GetDeposit(),
			Margin: big.NewRat(0, 1), RewardAccount: reward,
		})
	}
	for _, d := range req.GetDreps() {
		c, err := obligationCredential("dreps", d.GetCredential())
		if err != nil {
			return nil, err
		}
		in.DReps = append(in.DReps, dxoracle.CertDRep{Credential: c, Deposit: d.GetDeposit()})
	}
	for _, p := range req.GetProposals() {
		if len(p.GetId().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("proposals: tx id must be 32 bytes"))
		}
		if _, err := rewardCredential("proposals", p.GetReturnAccount()); err != nil {
			return nil, err
		}
		tag, _ := credentialTag(p.GetReturnAccount())
		in.Proposals = append(in.Proposals, dxoracle.Proposal{
			Deposit:       p.GetDeposit(),
			ReturnAccount: dxoracle.Credential{Tag: tag, Key: p.GetReturnAccount().GetHash()},
			ProposedEpoch: p.GetProposedEpoch(), ExpiresEpoch: p.GetExpiresEpoch(),
		})
	}
	certs, err := theCertOracle()
	if err != nil {
		return nil, err
	}
	props, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	got, err := dxoracle.StoredObligations(ctx, certs, props, in)
	if errors.Is(err, dxoracle.ErrDepositOverflow) {
		return nil, unsupportedf("coin does not fit a Word64")
	}
	if err != nil {
		return &v1.EpochObligationsResponse{Rejection: dingoRejection("OBLIGATIONS", err)}, nil
	}
	return &v1.EpochObligationsResponse{
		StakeDeposits: got.Stake, PoolDeposits: got.Pool, DrepDeposits: got.DRep,
		ProposalDeposits: got.Proposal, Total: got.Total,
	}, nil
}

func obligationCredential(name string, c *v1.Credential) (lcommon.Credential, error) {
	if _, err := rewardCredential(name, c); err != nil {
		return lcommon.Credential{}, err
	}
	tag, _ := credentialTag(c)
	return lcommon.Credential{CredType: uint(tag), Credential: lcommon.NewBlake2b224(bytes.Clone(c.GetHash()))}, nil
}

func mapKeys[T any](xs []T, key func(T) string) []string {
	out := make([]string, len(xs))
	for i, x := range xs {
		out[i] = key(x)
	}
	return out
}
