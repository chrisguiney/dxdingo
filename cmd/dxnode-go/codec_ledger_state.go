package main

import (
	"bytes"
	"context"
	"encoding/hex"
	"fmt"
	"sort"

	"github.com/blinklabs-io/dingo/ledgerstate"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/dijkstra"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// lsEraIndexes are dingo's era indexes of the eras with a ledger state, after
// Byron at 0.
var lsEraIndexes = map[v1.Era]int{
	v1.Era_ERA_SHELLEY:  ledgerstate.EraShelley,
	v1.Era_ERA_ALLEGRA:  ledgerstate.EraAllegra,
	v1.Era_ERA_MARY:     ledgerstate.EraMary,
	v1.Era_ERA_ALONZO:   ledgerstate.EraAlonzo,
	v1.Era_ERA_BABBAGE:  ledgerstate.EraBabbage,
	v1.Era_ERA_CONWAY:   conwayEraIndex,
	v1.Era_ERA_DIJKSTRA: conwayEraIndex + 1,
}

// codecLedgerState decodes ledger-state encodings with dingo's ledgerstate
// package, Shelley through Dijkstra. Byron has no such state.
func codecLedgerState(_ context.Context, req *v1.CodecLedgerStateRequest) (*v1.CodecLedgerStateResponse, error) {
	eraIndex, ok := lsEraIndexes[req.GetEra()]
	if !ok {
		return nil, dispatch.Unsupported(fmt.Errorf("no ledger state codec for %v", req.GetEra()))
	}
	switch req.GetFormat() {
	case v1.LedgerStateFormat_LEDGER_STATE_FORMAT_MEMPACK_TXOUT:
		return codecMempackTxOut(req.GetCbor())
	case v1.LedgerStateFormat_LEDGER_STATE_FORMAT_NEW_EPOCH_STATE:
		return codecNewEpochState(eraIndex, req.GetCbor())
	default:
		return nil, dispatch.BadRequest(fmt.Errorf("ledger state format %v", req.GetFormat()))
	}
}

func codecMempackTxOut(b []byte) (*v1.CodecLedgerStateResponse, error) {
	row, err := ledgerstate.ParseMempackTxOut(b)
	if err != nil {
		return &v1.CodecLedgerStateResponse{Rejection: decodeFailure(err)}, nil
	}
	return &v1.CodecLedgerStateResponse{Cbor: row.Cbor}, nil
}

const conwayEraIndex = ledgerstate.EraConway

func credProtoCodec(kind uint64, hash []byte) *v1.Credential {
	k := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if kind == ledgerstate.CredentialTypeScript {
		k = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.Credential{Kind: k, Hash: hash}
}

func lsCredential(c ledgerstate.Credential) *v1.Credential { return credProtoCodec(c.Type, c.Hash) }

func credLessCodec(a, b *v1.Credential) bool {
	if a.GetKind() != b.GetKind() {
		return a.GetKind() < b.GetKind()
	}
	return bytes.Compare(a.GetHash(), b.GetHash()) < 0
}

// govActionType maps the ledger's GovAction tag (0 to 6) to the enum.
func govActionType(tag uint8) v1.GovActionType {
	if tag > 6 {
		return v1.GovActionType_GOV_ACTION_TYPE_UNSPECIFIED
	}
	return v1.GovActionType(tag + 1)
}

func govIDProto(tx []byte, idx uint32) *v1.GovActionId {
	return &v1.GovActionId{TxId: tx, Index: idx}
}

func codecNewEpochState(eraIndex int, b []byte) (*v1.CodecLedgerStateResponse, error) {
	reject := func(err error) (*v1.CodecLedgerStateResponse, error) {
		return &v1.CodecLedgerStateResponse{Rejection: decodeFailure(err)}, nil
	}
	raw, err := ledgerstate.ParseNewEpochState(eraIndex, b)
	if err != nil {
		return reject(err)
	}
	cs, err := ledgerstate.ParseCertState(raw.CertStateData)
	if err != nil {
		return reject(err)
	}
	gov, err := ledgerstate.ParseGovState(raw.GovStateData, eraIndex)
	if err != nil {
		return reject(err)
	}
	snaps, err := ledgerstate.ParseSnapShots(raw.SnapShotsData)
	if err != nil {
		return reject(err)
	}
	pp, err := ledgerstate.DecodePParams(eraIndex, raw.PParamsData)
	if err != nil {
		return reject(err)
	}
	state := &v1.LedgerState{Pots: &v1.Pots{Reserves: raw.Reserves, Treasury: raw.Treasury, Fees: raw.Fees, Donations: raw.Donation}}
	if _, err := ledgerstate.ParseUTxOsStreaming(raw.UTxOData, func(batch []ledgerstate.ParsedUTxO) error {
		for _, u := range batch {
			state.Utxo = append(state.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: u.TxHash, Index: u.OutputIndex}, Txout: u.Cbor})
		}
		return nil
	}); err != nil {
		return reject(err)
	}
	resp := &v1.CodecLedgerStateResponse{Epoch: raw.Epoch, State: state}
	switch cp := pp.(type) {
	case *conway.ConwayProtocolParameters:
		resp.Pparams = conwayPParamsProtoCodec(cp)
	case *dijkstra.DijkstraProtocolParameters:
		resp.Pparams = conwayPParamsProtoCodec(&cp.ConwayProtocolParameters)
	default:
		return reject(fmt.Errorf("protocol parameters are %T, not Conway or Dijkstra", pp))
	}
	lsAccounts(state, cs)
	lsPools(state, cs)
	lsDReps(state, cs)
	lsGov(state, cs, gov)
	resp.Snapshots = append(resp.Snapshots, lsSnapshot(v1.SnapshotKind_SNAPSHOT_KIND_MARK, snaps.Mark)...)
	resp.Snapshots = append(resp.Snapshots, lsSnapshot(v1.SnapshotKind_SNAPSHOT_KIND_SET, snaps.Set)...)
	resp.Snapshots = append(resp.Snapshots, lsSnapshot(v1.SnapshotKind_SNAPSHOT_KIND_GO, snaps.Go)...)
	resp.BlocksCurrent = lsBlocks(raw.BlocksCur)
	resp.BlocksPrevious = lsBlocks(raw.BlocksPrev)
	sortLedgerState(resp)
	return resp, nil
}

func lsBlocks(m map[string]uint64) []*v1.BlocksMade {
	var out []*v1.BlocksMade
	for pool, n := range m {
		out = append(out, &v1.BlocksMade{PoolId: []byte(pool), Count: n})
	}
	return out
}

func lsAccounts(state *v1.LedgerState, cs *ledgerstate.ParsedCertState) {
	for _, a := range cs.Accounts {
		acct := &v1.Account{Credential: lsCredential(a.StakingKey), Balance: a.Reward, PoolId: a.PoolKeyHash}
		if a.Deposit != nil {
			acct.Deposit = *a.Deposit
		}
		switch a.DRepCred.Type {
		case ledgerstate.CredentialTypeAbstain:
			acct.Drep = &v1.DRep{Target: &v1.DRep_AlwaysAbstain{AlwaysAbstain: true}}
		case ledgerstate.CredentialTypeNoConfidence:
			acct.Drep = &v1.DRep{Target: &v1.DRep_AlwaysNoConfidence{AlwaysNoConfidence: true}}
		default:
			if len(a.DRepCred.Hash) > 0 {
				acct.Drep = &v1.DRep{Target: &v1.DRep_Credential{Credential: lsCredential(a.DRepCred)}}
			}
		}
		state.Accounts = append(state.Accounts, acct)
	}
}

func lsPool(p *ledgerstate.ParsedPool, stage v1.PoolStage) *v1.Pool {
	out := &v1.Pool{
		PoolId: p.PoolKeyHash, Stage: stage, VrfKeyHash: p.VrfKeyHash, Pledge: p.Pledge, Cost: p.Cost,
		Margin:        &v1.Rational{Numerator: fmt.Sprint(p.MarginNum), Denominator: fmt.Sprint(p.MarginDen)},
		RewardAccount: credProtoCodec(uint64(p.RewardAccountCredentialTag), p.RewardAccount),
		Owners:        p.Owners, Deposit: p.Deposit,
	}
	if stage == v1.PoolStage_POOL_STAGE_CURRENT {
		out.RetiringEpoch = p.RetiringEpoch
	}
	sort.Slice(out.Owners, func(i, j int) bool { return bytes.Compare(out.Owners[i], out.Owners[j]) < 0 })
	return out
}

func lsPools(state *v1.LedgerState, cs *ledgerstate.ParsedCertState) {
	for i := range cs.Pools {
		p := &cs.Pools[i]
		state.Pools = append(state.Pools, lsPool(p, v1.PoolStage_POOL_STAGE_CURRENT))
		if p.FutureParams != nil {
			state.Pools = append(state.Pools, lsPool(p.FutureParams, v1.PoolStage_POOL_STAGE_FUTURE))
		}
	}
}

func lsDReps(state *v1.LedgerState, cs *ledgerstate.ParsedCertState) {
	for _, d := range cs.DReps {
		state.Dreps = append(state.Dreps, &v1.DRepRecord{
			Credential: lsCredential(d.Credential), Deposit: d.Deposit, Expiry: d.ExpiryEpoch,
			AnchorUrl: d.AnchorURL, AnchorHash: d.AnchorHash,
		})
	}
}

func lsGov(state *v1.LedgerState, cs *ledgerstate.ParsedCertState, gov *ledgerstate.ParsedGovState) {
	if gov == nil {
		return
	}
	if gov.CommitteeQuorum != nil && gov.CommitteeQuorum.Rat != nil {
		c := &v1.Committee{Threshold: convert.RatProto(gov.CommitteeQuorum.Rat)}
		hot := map[string]*v1.Credential{}
		for _, h := range cs.CommitteeHotKeys {
			hot[credKeyOf(lsCredential(h.Cold))] = lsCredential(h.Hot)
		}
		resigned := map[string]bool{}
		for _, r := range cs.CommitteeResignations {
			resigned[credKeyOf(lsCredential(r))] = true
		}
		for _, m := range gov.Committee {
			cold := lsCredential(m.ColdCredential)
			c.Members = append(c.Members, &v1.CommitteeMember{
				Cold: cold, Hot: hot[credKeyOf(cold)], Expiry: m.ExpiresEpoch, Resigned: resigned[credKeyOf(cold)],
			})
		}
		state.Committee = c
	}
	for _, p := range gov.Proposals {
		pr := &v1.Proposal{
			Id: govIDProto(p.TxHash, p.ActionIndex), ActionType: govActionType(p.ActionType), Action: p.GovActionCbor,
			ProposedEpoch: p.ProposedIn, ExpiresEpoch: p.ExpiresAfter, Deposit: p.Deposit,
		}
		if len(p.ReturnAddr) == 29 {
			pr.ReturnAccount = credProtoCodec(uint64(p.ReturnAddr[0]>>4&1), p.ReturnAddr[1:])
		}
		if p.ParentTxHash != nil && p.ParentActionIdx != nil {
			pr.Parent = govIDProto(p.ParentTxHash, *p.ParentActionIdx)
		}
		state.Proposals = append(state.Proposals, pr)
	}
	es := &v1.EnactState{}
	if r := gov.PrevGovActionIds; r != nil {
		id := func(p *ledgerstate.ParsedGovActionId) *v1.GovActionId {
			if p == nil {
				return nil
			}
			return govIDProto(p.TxHash, p.ActionIndex)
		}
		es.PrevPparamUpdate, es.PrevHardFork, es.PrevCommittee, es.PrevConstitution = id(r.PParamUpdate), id(r.HardFork), id(r.Committee), id(r.Constitution)
	}
	if c := gov.Constitution; c != nil {
		es.Constitution = &v1.Constitution{AnchorUrl: c.AnchorURL, AnchorHash: c.AnchorHash, GuardrailScriptHash: c.PolicyHash}
	}
	state.EnactState = es
}

func credKeyOf(c *v1.Credential) string { return fmt.Sprintf("%d/%x", c.GetKind(), c.GetHash()) }

func lsSnapshot(kind v1.SnapshotKind, s ledgerstate.ParsedSnapShot) []*v1.SnapshotEntry {
	var out []*v1.SnapshotEntry
	for key, amount := range s.Stake {
		hash, err := hex.DecodeString(key)
		if err != nil {
			hash = []byte(key)
		}
		out = append(out, &v1.SnapshotEntry{
			Snapshot: kind, Credential: credProtoCodec(uint64(s.StakeTags[key]), hash), Amount: amount, PoolId: s.Delegations[key],
		})
	}
	return out
}

func conwayPParamsProtoCodec(cp *conway.ConwayProtocolParameters) *v1.ProtocolParameters {
	p := &v1.ProtocolParameters{}
	o := pparamsOut{p}
	minA, minB, bodySize, txSize, headerSize := cp.MinFeeA, cp.MinFeeB, cp.MaxBlockBodySize, cp.MaxTxSize, cp.MaxBlockHeaderSize
	keyDep, poolDep, maxEpoch, nOpt := cp.KeyDeposit, cp.PoolDeposit, cp.MaxEpoch, cp.NOpt
	o.base(&minA, &minB, &bodySize, &txSize, &headerSize, &keyDep, &poolDep, &maxEpoch, &nOpt, cp.A0, cp.Rho, cp.Tau, &cp.MinPoolCost)
	o.protocolVersion(&cp.ProtocolVersion)
	p.CoinsPerUtxoByte = &cp.AdaPerUtxoByte
	vs, cpct, mci := cp.MaxValueSize, cp.CollateralPercentage, cp.MaxCollateralInputs
	o.plutus(cp.CostModels, &cp.ExecutionCosts, &cp.MaxTxExUnits, &cp.MaxBlockExUnits, &vs, &cpct, &mci)
	t := cp.PoolVotingThresholds
	p.PoolVotingThresholds = &v1.PoolVotingThresholds{
		MotionNoConfidence: ratOf(&t.MotionNoConfidence), CommitteeNormal: ratOf(&t.CommitteeNormal),
		CommitteeNoConfidence: ratOf(&t.CommitteeNoConfidence), HardForkInitiation: ratOf(&t.HardForkInitiation),
		PpSecurityGroup: ratOf(&t.PpSecurityGroup),
	}
	d := cp.DRepVotingThresholds
	p.DrepVotingThresholds = &v1.DRepVotingThresholds{
		MotionNoConfidence: ratOf(&d.MotionNoConfidence), CommitteeNormal: ratOf(&d.CommitteeNormal),
		CommitteeNoConfidence: ratOf(&d.CommitteeNoConfidence), UpdateToConstitution: ratOf(&d.UpdateToConstitution),
		HardForkInitiation: ratOf(&d.HardForkInitiation), PpNetworkGroup: ratOf(&d.PpNetworkGroup),
		PpEconomicGroup: ratOf(&d.PpEconomicGroup), PpTechnicalGroup: ratOf(&d.PpTechnicalGroup),
		PpGovGroup: ratOf(&d.PpGovGroup), TreasuryWithdrawal: ratOf(&d.TreasuryWithdrawal),
	}
	mcs := cp.MinCommitteeSize
	p.CommitteeMinSize = u32(&mcs)
	p.CommitteeMaxTermLength, p.GovActionLifetime, p.GovActionDeposit = &cp.CommitteeTermLimit, &cp.GovActionValidityPeriod, &cp.GovActionDeposit
	p.DrepDeposit, p.DrepActivity = &cp.DRepDeposit, &cp.DRepInactivityPeriod
	p.MinFeeRefScriptCostPerByte = ratOf(cp.MinFeeRefScriptCostPerByte)
	return p
}

// sortLedgerState puts every set in canonical order: credentials and ids by
// kind and bytes, pools by id and stage, snapshot entries by snapshot and
// credential.
func sortLedgerState(r *v1.CodecLedgerStateResponse) {
	s := r.GetState()
	sort.Slice(s.Utxo, func(i, j int) bool {
		a, b := s.Utxo[i].GetTxin(), s.Utxo[j].GetTxin()
		if c := bytes.Compare(a.GetTxId(), b.GetTxId()); c != 0 {
			return c < 0
		}
		return a.GetIndex() < b.GetIndex()
	})
	sort.Slice(s.Accounts, func(i, j int) bool {
		return credLessCodec(s.Accounts[i].GetCredential(), s.Accounts[j].GetCredential())
	})
	sort.Slice(s.Pools, func(i, j int) bool {
		if c := bytes.Compare(s.Pools[i].GetPoolId(), s.Pools[j].GetPoolId()); c != 0 {
			return c < 0
		}
		return s.Pools[i].GetStage() < s.Pools[j].GetStage()
	})
	sort.Slice(s.Dreps, func(i, j int) bool { return credLessCodec(s.Dreps[i].GetCredential(), s.Dreps[j].GetCredential()) })
	if c := s.GetCommittee(); c != nil {
		sort.Slice(c.Members, func(i, j int) bool { return credLessCodec(c.Members[i].GetCold(), c.Members[j].GetCold()) })
	}
	sortProposals(s.Proposals)
	sort.Slice(r.Snapshots, func(i, j int) bool {
		a, b := r.Snapshots[i], r.Snapshots[j]
		if a.GetSnapshot() != b.GetSnapshot() {
			return a.GetSnapshot() < b.GetSnapshot()
		}
		return credLessCodec(a.GetCredential(), b.GetCredential())
	})
	blocks := func(bs []*v1.BlocksMade) {
		sort.Slice(bs, func(i, j int) bool { return bytes.Compare(bs[i].GetPoolId(), bs[j].GetPoolId()) < 0 })
	}
	blocks(r.BlocksCurrent)
	blocks(r.BlocksPrevious)
}

func sortProposals(ps []*v1.Proposal) {
	sort.Slice(ps, func(i, j int) bool {
		a, b := ps[i].GetId(), ps[j].GetId()
		if c := bytes.Compare(a.GetTxId(), b.GetTxId()); c != 0 {
			return c < 0
		}
		return a.GetIndex() < b.GetIndex()
	})
}
