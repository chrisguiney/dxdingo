package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func registerEpoch(r *dispatch.Registry) {
	dispatch.Register(r, epochDonations)
	dispatch.Register(r, epochPoolReap)
	dispatch.Register(r, epochMir)
	dispatch.Register(r, epochPParamUpdate)
	dispatch.Register(r, epochHardFork)
	dispatch.Register(r, epochSnapshotRotate)
	dispatch.Register(r, epochNewEpoch)
	dispatch.Register(r, epochObligations)
}

// epochDonations maps to epochengine.ComputeEpochDonations. The reference
// treasury is an unbounded Coin, so a sum the Word64 response cannot carry is
// UNSUPPORTED rather than an error.
func epochDonations(_ context.Context, req *v1.EpochDonationsRequest) (*v1.EpochDonationsResponse, error) {
	got, err := epochengine.ComputeEpochDonations(req.GetTreasury(), req.GetDonations())
	if err != nil {
		return nil, dispatch.Unsupported(err)
	}
	return &v1.EpochDonationsResponse{Treasury: got}, nil
}

func stakeCred(c *v1.Credential) (epochengine.StakeCredential, error) {
	tag, err := credentialTag(c)
	if err != nil {
		return epochengine.StakeCredential{}, dispatch.BadRequest(err)
	}
	if len(c.GetHash()) != 28 {
		return epochengine.StakeCredential{}, dispatch.BadRequest(errors.New("credential hash must be 28 bytes"))
	}
	return epochengine.StakeCredential{Tag: tag, Hash: c.GetHash()}, nil
}

// noDuplicates reports UNSUPPORTED when a key repeats: the reference tables
// are maps, so a repeated row has no defined meaning.
func noDuplicates(what string, keys []string) error {
	seen := make(map[string]bool, len(keys))
	for _, k := range keys {
		if seen[k] {
			return dispatch.Unsupported(fmt.Errorf("duplicate %s", what))
		}
		seen[k] = true
	}
	return nil
}

func credKeyEpoch(c *v1.Credential) string { return fmt.Sprintf("%d/%x", c.GetKind(), c.GetHash()) }

func credProto(c epochengine.StakeCredential) *v1.Credential {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if c.Tag == 1 {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.Credential{Kind: kind, Hash: c.Hash}
}

func lessCred(a, b *v1.Credential) bool {
	if a.GetKind() != b.GetKind() {
		return a.GetKind() < b.GetKind()
	}
	return bytes.Compare(a.GetHash(), b.GetHash()) < 0
}

// addsToWord64 reports whether the sum of the values fits a Word64.
func addsToWord64(vs ...uint64) bool {
	sum := new(big.Int)
	for _, v := range vs {
		sum.Add(sum, new(big.Int).SetUint64(v))
	}
	return sum.IsUint64()
}

// epochPoolReap maps to epochengine.ComputePoolReapRefunds. Dingo refunds per
// pool; the reference merges refunds per reward account, so the adapter does
// the same merge on the result. Pools in the FUTURE stage change the reward
// account before the reap and are outside the request's model.
func epochPoolReap(_ context.Context, req *v1.EpochPoolReapRequest) (*v1.EpochPoolReapResponse, error) {
	var poolIDs, acctKeys []string
	for _, p := range req.GetPools() {
		poolIDs = append(poolIDs, string(p.GetPoolId()))
	}
	for _, a := range req.GetAccounts() {
		acctKeys = append(acctKeys, credKeyEpoch(a.GetCredential()))
	}
	if err := noDuplicates("pool", poolIDs); err != nil {
		return nil, err
	}
	if err := noDuplicates("account", acctKeys); err != nil {
		return nil, err
	}
	var deposits []uint64
	for _, p := range req.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT {
			return nil, dispatch.Unsupported(errors.New("pool stage is not current"))
		}
		if p.RetiringEpoch != nil && p.GetRetiringEpoch() == req.GetEpoch() {
			deposits = append(deposits, p.GetDeposit())
		}
	}
	if !addsToWord64(deposits...) {
		return nil, dispatch.Unsupported(errors.New("retiring deposits overflow Word64"))
	}
	var pools []epochengine.PoolReapPool
	for _, a := range req.GetAccounts() {
		if _, err := stakeCred(a.GetCredential()); err != nil {
			return nil, err
		}
		if n := len(a.GetPoolId()); n != 0 && n != 28 {
			return nil, dispatch.BadRequest(errors.New("pool id must be 28 bytes"))
		}
	}
	for _, p := range req.GetPools() {
		if len(p.GetPoolId()) != 28 {
			return nil, dispatch.BadRequest(errors.New("pool id must be 28 bytes"))
		}
		if p.RetiringEpoch == nil || p.GetRetiringEpoch() != req.GetEpoch() {
			continue
		}
		acct, err := stakeCred(p.GetRewardAccount())
		if err != nil {
			return nil, err
		}
		pools = append(pools, epochengine.PoolReapPool{PoolKeyHash: p.GetPoolId(), RewardAccount: acct, Deposit: p.GetDeposit()})
	}
	var delegs []epochengine.PoolReapDelegation
	var registered []epochengine.StakeCredential
	for _, a := range req.GetAccounts() {
		c, err := stakeCred(a.GetCredential())
		if err != nil {
			return nil, err
		}
		registered = append(registered, c)
		if len(a.GetPoolId()) > 0 {
			delegs = append(delegs, epochengine.PoolReapDelegation{Credential: c, PoolKeyHash: a.GetPoolId()})
		}
	}
	res, err := epochengine.ComputePoolReapRefunds(pools, delegs, registered)
	if err != nil {
		return &v1.EpochPoolReapResponse{Rejection: dingoRejection("POOLREAP", err)}, nil
	}
	out := &v1.EpochPoolReapResponse{UnclaimedToTreasury: res.UnclaimedToTreasury}
	for _, p := range pools {
		out.Retired = append(out.Retired, p.PoolKeyHash)
	}
	sort.Slice(out.Retired, func(i, j int) bool { return bytes.Compare(out.Retired[i], out.Retired[j]) < 0 })
	merged := map[string]*v1.Credit{}
	for _, r := range res.Refunds {
		key := fmt.Sprintf("%d/%x", r.Credential.Tag, r.Credential.Hash)
		if c, ok := merged[key]; ok {
			c.Amount += r.Amount
			continue
		}
		c := &v1.Credit{Credential: credProto(r.Credential), Amount: r.Amount}
		merged[key] = c
		out.Refunds = append(out.Refunds, c)
	}
	sort.Slice(out.Refunds, func(i, j int) bool { return lessCred(out.Refunds[i].Credential, out.Refunds[j].Credential) })
	for _, u := range res.Undelegated {
		out.Undelegated = append(out.Undelegated, credProto(u))
	}
	sort.Slice(out.Undelegated, func(i, j int) bool { return lessCred(out.Undelegated[i], out.Undelegated[j]) })
	return out, nil
}

// epochMir maps to epochengine.ComputeMIRBoundary.
//
// Domain, shared with dxnode-hs: the reference InstantaneousRewards holds
// non-negative Coin per credential, so a negative delta or a repeated
// (source, credential) pair (which the DELEG rule would combine by era) is
// UNSUPPORTED; so is any input whose pots or credits cannot be carried by the
// Word64 response.
func epochMir(_ context.Context, req *v1.EpochMirRequest) (*v1.EpochMirResponse, error) {
	if err := mirDomain(req); err != nil {
		return nil, err
	}
	registered := map[string]bool{}
	for _, c := range req.GetRegistered() {
		sc, err := stakeCred(c)
		if err != nil {
			return nil, err
		}
		registered[models.NewStakeCredentialRef(sc.Tag, sc.Hash).MapKey()] = true
	}
	var effects []models.MIREffect
	for i, m := range req.GetMir() {
		sc, err := stakeCred(m.GetCredential())
		if err != nil {
			return nil, err
		}
		effects = append(effects, models.MIREffect{
			ID: uint(i + 1), Pot: mirPot(m.GetSource()),
			Rewards: []models.MIRReward{{Credential: sc.Hash, CredentialTag: sc.Tag, Amount: big.NewInt(m.GetAmount())}},
		})
	}
	if n := req.GetReservesToTreasury(); n > 0 {
		effects = append(effects, models.MIREffect{ID: uint(len(effects) + 1), Pot: epochengine.MIRPotReserves, OtherPot: n})
	}
	if n := req.GetTreasuryToReserves(); n > 0 {
		effects = append(effects, models.MIREffect{ID: uint(len(effects) + 1), Pot: epochengine.MIRPotTreasury, OtherPot: n})
	}
	plan, err := epochengine.ComputeMIRBoundary(effects, registered, mirEraID(req.GetProtocolVersion().GetMajor()), req.GetReserves(), req.GetTreasury())
	if err != nil {
		return &v1.EpochMirResponse{Rejection: dingoRejection("MIR", err)}, nil
	}
	out := &v1.EpochMirResponse{Reserves: plan.Reserves, Treasury: plan.Treasury}
	switch plan.Outcome {
	case epochengine.MIROutcomeDiscarded:
		return &v1.EpochMirResponse{Rejection: dingoRejection("MIR", errors.New(plan.DiscardReason))}, nil
	case epochengine.MIROutcomeOverBudget:
		return out, nil
	}
	out.Applied = true
	merged := map[string]*v1.Credit{}
	for _, c := range plan.Credits {
		key := fmt.Sprintf("%d/%x", c.Credential.Tag, c.Credential.Hash)
		if e, ok := merged[key]; ok {
			e.Amount += c.Amount
			continue
		}
		e := &v1.Credit{Credential: credProto(c.Credential), Amount: c.Amount}
		merged[key] = e
		out.Credited = append(out.Credited, e)
	}
	sort.Slice(out.Credited, func(i, j int) bool { return lessCred(out.Credited[i].Credential, out.Credited[j].Credential) })
	return out, nil
}

func mirPot(s v1.MirSource) uint {
	if s == v1.MirSource_MIR_SOURCE_TREASURY {
		return epochengine.MIRPotTreasury
	}
	return epochengine.MIRPotReserves
}

// mirEraID picks the pre- or post-Alonzo accumulation; protocol major 5 is
// Alonzo's first. It only matters for repeated entries, which mirDomain
// excludes, but dingo takes it as an input.
func mirEraID(major uint32) uint {
	if major >= 5 {
		return alonzo.EraIdAlonzo
	}
	return alonzo.EraIdAlonzo - 1
}

func mirDomain(req *v1.EpochMirRequest) error {
	if major := req.GetProtocolVersion().GetMajor(); major < 2 || major >= 9 {
		return dispatch.Unsupported(errors.New("MIR exists in protocol versions 2 to 8"))
	}
	seen := map[string]bool{}
	amounts := []uint64{req.GetReserves(), req.GetTreasury()}
	for _, m := range req.GetMir() {
		if m.GetSource() != v1.MirSource_MIR_SOURCE_RESERVES && m.GetSource() != v1.MirSource_MIR_SOURCE_TREASURY {
			return dispatch.Unsupported(errors.New("mir source unspecified"))
		}
		if m.GetAmount() < 0 {
			return dispatch.Unsupported(errors.New("negative mir delta"))
		}
		key := fmt.Sprintf("%d/%d/%x", m.GetSource(), m.GetCredential().GetKind(), m.GetCredential().GetHash())
		if seen[key] {
			return dispatch.Unsupported(errors.New("repeated mir entry"))
		}
		seen[key] = true
		amounts = append(amounts, uint64(m.GetAmount()))
	}
	amounts = append(amounts, req.GetReservesToTreasury(), req.GetTreasuryToReserves())
	if !addsToWord64(amounts...) {
		return dispatch.Unsupported(errors.New("pots and deltas overflow Word64"))
	}
	return nil
}
