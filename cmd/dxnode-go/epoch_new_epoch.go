package main

import (
	"bytes"
	"context"
	"errors"
	"math"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/dingo/ledger/eras"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// epochNewEpoch runs dingo's production epoch rollover over a scratch ledger
// at the end of the epoch before req's, with the node's snapshot hooks wired.
//
// Domain, shared with dxnode-hs: Conway only; the request holds accounts,
// pools, pots, the snapshots in force and the instant stake. A pending
// reward_update is accepted when it equals the update dingo's reward
// calculation derives from blocks_previous, the GO snapshot, the pools and
// the pots (pots.fees is the fee pot the round pays out), under the scratch
// ledger's globals; the boundary then applies it. blocks_previous is read
// only with a reward_update. An account's DRep delegation is kept and reported
// on a changed row.
//
// Governance: state.dreps, proposals, votes, committee and enact_state, with
// pool_distribution and drep_distribution, are the pulser snapshot taken at
// the boundary into the previous epoch. Dingo decides RATIFY over those
// tables for that boundary (dxoracle.PriorBoundary), and the boundary under
// test enacts what it accepted and drops what it expired; enacted, expired
// and the governance-driven parts of the delta are what that boundary did.
// The domain is gov_ratify's taken at the previous epoch, and a hard fork
// proposal enacted at the boundary runs HARDFORK: the response's pparams_after
// carries the new protocol version and a delegation to a DRep nobody
// registered is cleared at protocol 10. The instant stake is instant_stake or,
// when state.utxo is given instead, what stake_instant_stake derives from the
// UTxO table (both together are UNSUPPORTED; a table before Conway is
// UNSUPPORTED too, see pointers below). Before Conway the update system
// and MIR are in the domain: state.pparam_proposals are adopted at the
// boundary when globals.quorum genesis keys agree (the epoch length must be
// the scratch ledger's 432000) and are reported as pparams_after; mir, with
// reserves_to_treasury and treasury_to_reserves, is applied after the reward
// update and before SNAP. Genesis delegations are UNSUPPORTED because
// neither node reads them at NEWEPOCH (they only validate a proposal when it
// is made), registration pointers because dingo's instant stake leaves
// pointer addresses out while the reference counts them before Conway. The response reports only the pots both
// ledgers keep: reserves and treasury. Dingo keeps no fee, deposit or
// donation pot, so those deltas are left unset on both sides.
func epochNewEpoch(ctx context.Context, req *v1.EpochNewEpochRequest) (*v1.EpochNewEpochResponse, error) {
	era := req.GetContext().GetEra()
	desc, ok := newEpochEras[era]
	if !ok {
		return nil, unsupportedf("NEWEPOCH is answered for Shelley through Conway")
	}
	conwayEra := era == v1.Era_ERA_CONWAY
	st := req.GetState()
	if len(st.GetGenesisDelegations()) > 0 {
		return nil, unsupportedf("genesis delegations are not read")
	}
	instant := req.GetInstantStake()
	if len(st.GetUtxo()) > 0 && !conwayEra {
		return nil, unsupportedf("a UTxO table is read for Conway only: before it pointer addresses carry stake")
	}
	if len(st.GetUtxo()) > 0 {
		if len(instant) > 0 {
			return nil, unsupportedf("a UTxO table and instant_stake both give the instant stake")
		}
		derived, err := instantStake(ctx, &v1.StakeInstantStakeRequest{Utxo: st.GetUtxo()})
		if err != nil {
			return nil, err
		}
		if derived.GetRejection() != nil {
			return &v1.EpochNewEpochResponse{Rejection: derived.GetRejection()}, nil
		}
		instant = derived.GetStake()
	}
	if req.GetContext().GetEpoch() < 5 {
		return nil, unsupportedf("epoch below 5 has no reward round to run")
	}
	pp, err := newEpochPParams(era, req.GetPparams())
	if err != nil {
		return nil, err
	}
	in := dxoracle.NewEpochInput{
		Era:       &desc,
		Epoch:     req.GetContext().GetEpoch() - 1,
		PParams:   pp,
		Reserves:  st.GetPots().GetReserves(),
		Treasury:  st.GetPots().GetTreasury(),
		Donations: st.GetPots().GetDonations(),
	}
	if in.PriorMark, in.PriorSet, in.PriorGo, err = priorSnapshots(req.GetSnapshots()); err != nil {
		return nil, err
	}
	var rej *v1.Rejection
	if in.PParamUpdates, in.UpdateQuorum, rej, err = newEpochUpdates(req, era); err != nil || rej != nil {
		return &v1.EpochNewEpochResponse{Rejection: rej}, err
	}
	if in.MIRs, err = newEpochMIRs(req, era); err != nil {
		return nil, err
	}
	var gov *dxoracle.GovState
	if !conwayEra && newEpochHasGovernance(req) {
		return nil, unsupportedf("governance state does not exist before Conway")
	}
	if newEpochHasGovernance(req) {
		if gov, err = newEpochGovState(ctx, req, pp.(*conway.ConwayProtocolParameters)); err != nil {
			return nil, err
		}
		in.Gov = gov
	}
	deposits := map[string]uint64{}
	seenAccounts := map[string]struct{}{}
	for _, a := range st.GetAccounts() {
		if _, err := rewardCredential("accounts", a.GetCredential()); err != nil {
			return nil, err
		}
		key := credKeyRewardsStake(a.GetCredential())
		if _, dup := seenAccounts[key]; dup {
			return nil, dispatch.BadRequest(errors.New("accounts: repeated credential"))
		}
		seenAccounts[key] = struct{}{}
		if len(a.GetPoolId()) != 0 {
			if _, err := rewardPoolID("accounts", a.GetPoolId()); err != nil {
				return nil, err
			}
		}
		if a.GetPointer() != nil {
			return nil, unsupportedf("accounts: pointers are not read")
		}
		if a.GetDrep() != nil && a.GetDrep().GetTarget() == nil {
			return nil, dispatch.BadRequest(errors.New("accounts: drep target unset"))
		}
		if !conwayEra && a.GetDrep() != nil {
			return nil, unsupportedf("accounts: a DRep delegation does not exist before Conway")
		}
		drep, err := delegation(a.GetDrep())
		if err != nil {
			return nil, dispatch.BadRequest(err)
		}
		tag, _ := credentialTag(a.GetCredential())
		deposits[key] = a.GetDeposit()
		in.Accounts = append(in.Accounts, dxoracle.NewEpochAccount{
			Tag: tag, Key: a.GetCredential().GetHash(), Reward: a.GetBalance(), Pool: a.GetPoolId(), Drep: drep,
		})
	}
	seenPools := map[string]struct{}{}
	for _, p := range st.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT {
			return nil, unsupportedf("pools: stage is not current")
		}
		if _, err := rewardPoolID("pools", p.GetPoolId()); err != nil {
			return nil, err
		}
		if _, dup := seenPools[string(p.GetPoolId())]; dup {
			return nil, dispatch.BadRequest(errors.New("pools: repeated pool"))
		}
		seenPools[string(p.GetPoolId())] = struct{}{}
		if _, err := rewardCredential("pools", p.GetRewardAccount()); err != nil {
			return nil, err
		}
		tag, _ := credentialTag(p.GetRewardAccount())
		pool := dxoracle.NewEpochPool{
			Key: p.GetPoolId(), Pledge: p.GetPledge(), Cost: p.GetCost(), Deposit: p.GetDeposit(),
			RewardTag: tag, RewardKey: p.GetRewardAccount().GetHash(),
		}
		if p.RetiringEpoch != nil {
			r := p.GetRetiringEpoch()
			pool.RetiringEpoch = &r
		}
		in.Pools = append(in.Pools, pool)
	}
	totals := map[string]uint64{}
	for _, e := range instant {
		if _, err := rewardCredential("instant_stake", e.GetCredential()); err != nil {
			return nil, err
		}
		key := credKeyRewardsStake(e.GetCredential())
		if totals[key] > math.MaxUint64-e.GetAmount() {
			return nil, unsupportedf("instant_stake: total exceeds Word64")
		}
		totals[key] += e.GetAmount()
		tag, _ := credentialTag(e.GetCredential())
		in.Stake = append(in.Stake, dxoracle.SnapshotStake{Tag: tag, Key: e.GetCredential().GetHash(), Amount: e.GetAmount()})
	}
	for _, a := range st.GetAccounts() {
		key := credKeyRewardsStake(a.GetCredential())
		if totals[key] > math.MaxUint64-a.GetBalance() {
			return nil, unsupportedf("accounts: stake of one credential exceeds Word64")
		}
	}
	if req.GetRewardUpdate() != nil {
		if in.Rewards, rej, err = pendingRewards(ctx, req); err != nil || rej != nil {
			return &v1.EpochNewEpochResponse{Rejection: rej}, err
		}
	}
	out, err := dxoracle.NewEpoch(ctx, in)
	if err != nil {
		return &v1.EpochNewEpochResponse{Rejection: dingoRejection("NEWEPOCH", err)}, nil
	}
	delta := &v1.LedgerDelta{
		ReservesDelta: int64(out.Reserves) - int64(in.Reserves),
		TreasuryDelta: int64(out.Treasury) - int64(in.Treasury),
	}
	for i, a := range out.Accounts {
		before := in.Accounts[i]
		if a.Reward == before.Reward && bytes.Equal(a.Pool, before.Pool) && a.Drep.Type == before.Drep.Type && bytes.Equal(a.Drep.Key, before.Drep.Key) {
			continue
		}
		cred := protoCredential(a.Tag, a.Key)
		delta.AccountsChanged = append(delta.AccountsChanged, &v1.Account{
			Credential: cred, Deposit: deposits[credKeyRewardsStake(cred)], Balance: a.Reward, PoolId: a.Pool, Drep: drepProto(a.Drep),
		})
	}
	sort.Slice(delta.AccountsChanged, func(i, j int) bool {
		return credKeyRewardsStake(delta.AccountsChanged[i].GetCredential()) < credKeyRewardsStake(delta.AccountsChanged[j].GetCredential())
	})
	delta.PoolsRemoved = out.PoolsRemoved
	if ops, ok := pparamEras[era]; ok {
		got, err := ops.to(out.PParams)
		if err != nil {
			return nil, err
		}
		was, err := ops.to(pp)
		if err != nil {
			return nil, err
		}
		if !proto.Equal(got, was) {
			delta.PparamsAfter = got
		}
	}
	if conwayEra {
		after, ok := out.PParams.(*conway.ConwayProtocolParameters)
		if !ok {
			return nil, errors.New("NEWEPOCH left parameters of an unexpected type")
		}
		if got, was := conwayPParamsToProto(after), conwayPParamsToProto(pp.(*conway.ConwayProtocolParameters)); !proto.Equal(got, was) {
			delta.PparamsAfter = got
		}
	}
	resp := &v1.EpochNewEpochResponse{Delta: delta}
	if out.Gov != nil {
		applyNewEpochGov(req, gov, out.Gov, resp)
	}
	for _, part := range []struct {
		kind v1.SnapshotKind
		rows []dxoracle.SnapshotStake
	}{
		{v1.SnapshotKind_SNAPSHOT_KIND_MARK, out.Mark},
		{v1.SnapshotKind_SNAPSHOT_KIND_SET, out.Set},
		{v1.SnapshotKind_SNAPSHOT_KIND_GO, out.Go},
	} {
		for _, r := range part.rows {
			resp.Snapshots = append(resp.Snapshots, &v1.SnapshotEntry{
				Snapshot: part.kind, Credential: protoCredential(r.Tag, r.Key), Amount: r.Amount, PoolId: r.Pool,
			})
		}
	}
	return resp, nil
}

// drepProto is the DRep a delegation names, nil for none.
func drepProto(d dxoracle.Delegation) *v1.DRep {
	switch d.Type {
	case dxoracle.DelegateAlwaysAbstain:
		return &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
	case dxoracle.DelegateAlwaysNoConfidence:
		return &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
	case dxoracle.DelegateKeyHash:
		return &v1.DRep{Target: &v1.DRep_Credential{Credential: protoCredential(0, d.Key)}}
	case dxoracle.DelegateScriptHash:
		return &v1.DRep{Target: &v1.DRep_Credential{Credential: protoCredential(1, d.Key)}}
	}
	return nil
}

// newEpochEras are the eras NEWEPOCH is answered in.
var newEpochEras = map[v1.Era]eras.EraDesc{
	v1.Era_ERA_SHELLEY: eras.ShelleyEraDesc, v1.Era_ERA_ALLEGRA: eras.AllegraEraDesc, v1.Era_ERA_MARY: eras.MaryEraDesc,
	v1.Era_ERA_ALONZO: eras.AlonzoEraDesc, v1.Era_ERA_BABBAGE: eras.BabbageEraDesc, v1.Era_ERA_CONWAY: eras.ConwayEraDesc,
}

// newEpochPParams reads the parameters as the era's table.
func newEpochPParams(era v1.Era, pp *v1.ProtocolParameters) (lcommon.ProtocolParameters, error) {
	if ops, ok := pparamEras[era]; ok {
		return ops.from(pp)
	}
	return govFullPParams(pp)
}
