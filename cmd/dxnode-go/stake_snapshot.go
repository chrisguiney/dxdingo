package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math"
	"sort"

	"github.com/blinklabs-io/dingo/dxoracle"
	"github.com/blinklabs-io/gouroboros/cbor"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// liveStakeSlot is the slot the scratch rebuild runs at; no row depends on it.
const liveStakeSlot = 1

func sortStakeEntries(es []*v1.StakeEntry) {
	sort.Slice(es, func(i, j int) bool { return credKeyRewardsStake(es[i].GetCredential()) < credKeyRewardsStake(es[j].GetCredential()) })
}

// instantStake runs dingo's reward live stake rebuild over the request's
// outputs and reports each credential's UTxO stake. The rebuild classifies an
// output by its address (models.UtxoLedgerToModel), so pointer addresses,
// enterprise addresses and zero-value outputs add to no credential.
func instantStake(ctx context.Context, req *v1.StakeInstantStakeRequest) (*v1.StakeInstantStakeResponse, error) {
	seen := map[string]struct{}{}
	in := dxoracle.LiveStakeInput{Slot: liveStakeSlot}
	for _, u := range req.GetUtxo() {
		if len(u.GetTxin().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("utxo: tx_id is not 32 bytes"))
		}
		ref := fmt.Sprintf("%x#%d", u.GetTxin().GetTxId(), u.GetTxin().GetIndex())
		if _, dup := seen[ref]; dup {
			return nil, dispatch.BadRequest(errors.New("utxo: repeated input"))
		}
		seen[ref] = struct{}{}
		in.Outputs = append(in.Outputs, u.GetTxout())
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	rows, err := oracle.LiveStake(ctx, in)
	if err != nil {
		if errors.Is(err, dxoracle.ErrDecodeOutput) {
			return &v1.StakeInstantStakeResponse{Rejection: &v1.Rejection{Failures: []*v1.Failure{{
				Rule: "instant-stake", Name: "DeserialiseFailure", Detail: err.Error(),
			}}}}, nil
		}
		return &v1.StakeInstantStakeResponse{Rejection: dingoRejection("instant-stake", err)}, nil
	}
	resp := &v1.StakeInstantStakeResponse{}
	for _, r := range rows {
		if r.UtxoStake > 0 {
			resp.Stake = append(resp.Stake, &v1.StakeEntry{Credential: protoCredential(r.Tag, r.Key), Amount: r.UtxoStake})
		}
	}
	sortStakeEntries(resp.Stake)
	return resp, nil
}

// stakeOutput is a Shelley output paying amount to a base address whose
// stake part is the credential: the form the rebuild reads instant stake
// from.
func stakeOutput(c *v1.Credential, amount uint64) ([]byte, error) {
	addrType := uint8(lcommon.AddressTypeKeyKey)
	if c.GetKind() == v1.CredentialKind_CREDENTIAL_KIND_SCRIPT {
		addrType = lcommon.AddressTypeKeyScript
	}
	addr, err := lcommon.NewAddressFromParts(addrType, 0, bytes.Repeat([]byte{0x70}, 28), c.GetHash())
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("instant_stake: %w", err))
	}
	return cbor.Encode(&shelley.ShelleyTransactionOutput{OutputAddress: addr, OutputAmount: amount})
}

// stakeSnapshot runs the reward live stake rebuild over the instant stake
// (one output per entry, so repeated credentials add up) and the accounts'
// balances and delegations, and reports the rows of registered credentials
// that delegate to a pool: the active stake a snapshot holds.
func stakeSnapshot(ctx context.Context, req *v1.StakeSnapshotRequest) (*v1.StakeSnapshotResponse, error) {
	if _, err := currentPools(req.GetPools()); err != nil {
		return nil, err
	}
	in := dxoracle.LiveStakeInput{Slot: liveStakeSlot}
	seen := map[string]struct{}{}
	for _, a := range req.GetAccounts() {
		tag, err := credentialTag(a.GetCredential())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("accounts: %w", err))
		}
		if len(a.GetCredential().GetHash()) != 28 {
			return nil, dispatch.BadRequest(errors.New("accounts: credential hash is not 28 bytes"))
		}
		if _, dup := seen[credKeyRewardsStake(a.GetCredential())]; dup {
			return nil, dispatch.BadRequest(errors.New("accounts: repeated credential"))
		}
		seen[credKeyRewardsStake(a.GetCredential())] = struct{}{}
		if len(a.GetPoolId()) != 0 {
			if _, err := rewardPoolID("accounts", a.GetPoolId()); err != nil {
				return nil, err
			}
		}
		in.Accounts = append(in.Accounts, dxoracle.LiveStakeAccount{
			Tag: tag, Key: a.GetCredential().GetHash(), Reward: a.GetBalance(), Active: true, Pool: a.GetPoolId(),
		})
	}
	totals := map[string]uint64{}
	for _, e := range req.GetInstantStake() {
		if _, err := rewardCredential("instant_stake", e.GetCredential()); err != nil {
			return nil, err
		}
		key := credKeyRewardsStake(e.GetCredential())
		if totals[key] > math.MaxUint64-e.GetAmount() {
			return nil, unsupportedf("instant_stake: total exceeds Word64")
		}
		totals[key] += e.GetAmount()
		out, err := stakeOutput(e.GetCredential(), e.GetAmount())
		if err != nil {
			return nil, err
		}
		in.Outputs = append(in.Outputs, out)
	}
	oracle, err := scratchOracle()
	if err != nil {
		return nil, err
	}
	rows, err := oracle.LiveStake(ctx, in)
	if err != nil {
		return &v1.StakeSnapshotResponse{Rejection: dingoRejection("snapshot", err)}, nil
	}
	resp := &v1.StakeSnapshotResponse{}
	for _, r := range rows {
		if !r.Registered || len(r.Pool) == 0 || r.TotalStake == 0 {
			continue
		}
		resp.Snapshot = append(resp.Snapshot, &v1.SnapshotEntry{
			Snapshot: v1.SnapshotKind_SNAPSHOT_KIND_MARK, Credential: protoCredential(r.Tag, r.Key),
			Amount: r.TotalStake, PoolId: r.Pool,
		})
		resp.Total += r.TotalStake
	}
	if len(resp.Snapshot) == 0 {
		return nil, unsupportedf("snapshot: no active stake")
	}
	sort.Slice(resp.Snapshot, func(i, j int) bool {
		return credKeyRewardsStake(resp.Snapshot[i].GetCredential()) < credKeyRewardsStake(resp.Snapshot[j].GetCredential())
	})
	return resp, nil
}
