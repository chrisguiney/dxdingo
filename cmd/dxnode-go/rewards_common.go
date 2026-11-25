package main

import (
	"errors"
	"fmt"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/ledger/epochengine"
	"github.com/blinklabs-io/dingo/ledger/rewards"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Protocol majors the reward functions accept. Shelley is 2; the reward
// quirks gated on the major (cross-pool dedup below 3, the prefilter below
// 7) all sit inside this range. Later majors change pparams shapes, not the
// reward arithmetic.
const (
	minRewardMajor = 2
	maxRewardMajor = 11
	// decentralizationMajorLimit is the last major whose pparams carry d;
	// from Babbage on the ledger reads d as zero.
	decentralizationMajorLimit = 6
)

func unsupportedf(format string, args ...any) error {
	return dispatch.Unsupported(fmt.Errorf(format, args...))
}

// rewardMajor returns the request's protocol major or UNSUPPORTED outside
// the supported range.
func rewardMajor(pvRewardsStake *v1.ProtocolVersion) (uint32, error) {
	major := pvRewardsStake.GetMajor()
	if major < minRewardMajor || major > maxRewardMajor {
		return 0, unsupportedf("protocol major %d outside [%d, %d]", major, minRewardMajor, maxRewardMajor)
	}
	return major, nil
}

// rewardDomain holds the pparams the reward functions read, after the
// domain checks shared with dxnode-hs.
type rewardDomain struct {
	major uint32
	a0    *big.Rat
	rho   *big.Rat
	tau   *big.Rat
	d     *big.Rat
	nOpt  uint32
}

// rewardPParams checks the pparams. rho and tau are required only when
// needMonetary is set; a missing one is zero then.
func rewardPParams(pvRewardsStake *v1.ProtocolVersion, pp *v1.ProtocolParameters, needMonetary bool) (rewardDomain, error) {
	var dom rewardDomain
	major, err := rewardMajor(pvRewardsStake)
	if err != nil {
		return dom, err
	}
	dom.major = major
	if dom.a0, err = ratArg("a0", pp.GetA0()); err != nil {
		return dom, err
	}
	if err := nonNegativeInterval("a0", dom.a0); err != nil {
		return dom, err
	}
	if pp.NOpt == nil || pp.GetNOpt() == 0 || pp.GetNOpt() > 0xffff {
		return dom, unsupportedf("n_opt must be in [1, 65535]")
	}
	dom.nOpt = pp.GetNOpt()
	dom.rho, dom.tau, dom.d = new(big.Rat), new(big.Rat), new(big.Rat)
	if needMonetary {
		if dom.rho, err = ratArg("rho", pp.GetRho()); err != nil {
			return dom, err
		}
		if err := unitInterval("rho", dom.rho); err != nil {
			return dom, err
		}
		if dom.tau, err = ratArg("tau", pp.GetTau()); err != nil {
			return dom, err
		}
		if err := unitInterval("tau", dom.tau); err != nil {
			return dom, err
		}
	}
	if major <= decentralizationMajorLimit {
		if dom.d, err = ratArg("d", pp.GetD()); err != nil {
			return dom, err
		}
		if err := unitInterval("d", dom.d); err != nil {
			return dom, err
		}
	}
	return dom, nil
}

// genesisStub supplies the Shelley genesis fields RewardParametersFromPParams
// reads.
type genesisStub struct{ g *shelley.ShelleyGenesis }

func (s genesisStub) ShelleyGenesis() *shelley.ShelleyGenesis { return s.g }

// rewardGlobals are the Globals fields the reward update reads.
type rewardGlobals struct {
	asc         *big.Rat
	epochLength uint64
	maxSupply   uint64
	securityK   uint64
}

func rewardGlobalsArg(g *v1.Globals) (rewardGlobals, error) {
	var out rewardGlobals
	var err error
	if out.asc, err = ratArg("active_slot_coeff", g.GetActiveSlotCoeff()); err != nil {
		return out, err
	}
	if err := positiveUnitInterval("active_slot_coeff", out.asc); err != nil {
		return out, err
	}
	out.epochLength, out.maxSupply, out.securityK = g.GetEpochLength(), g.GetMaxLovelaceSupply(), g.GetSecurityParam()
	if out.epochLength == 0 || out.securityK == 0 {
		return out, unsupportedf("epoch_length and security_param must be nonzero")
	}
	return out, nil
}

// rewardParameters runs dingo's pparams-to-parameters conversion on a
// pparams value of the type the major selects.
func rewardParameters(dom rewardDomain, glob rewardGlobals) (rewards.Parameters, error) {
	cr := func(r *big.Rat) *cbor.Rat { return &cbor.Rat{Rat: new(big.Rat).Set(r)} }
	var pparams common.ProtocolParameters
	switch {
	case dom.major <= 3:
		pparams = &shelley.ShelleyProtocolParameters{
			NOpt: uint(dom.nOpt), A0: cr(dom.a0), Rho: cr(dom.rho), Tau: cr(dom.tau),
			Decentralization: cr(dom.d), ProtocolMajor: uint(dom.major),
		}
	case dom.major == 4:
		pparams = &mary.MaryProtocolParameters{
			NOpt: uint(dom.nOpt), A0: cr(dom.a0), Rho: cr(dom.rho), Tau: cr(dom.tau),
			Decentralization: cr(dom.d), ProtocolMajor: uint(dom.major),
		}
	case dom.major <= 6:
		pparams = &alonzo.AlonzoProtocolParameters{
			NOpt: uint(dom.nOpt), A0: cr(dom.a0), Rho: cr(dom.rho), Tau: cr(dom.tau),
			Decentralization: cr(dom.d), ProtocolMajor: uint(dom.major),
		}
	case dom.major <= 8:
		pparams = &babbage.BabbageProtocolParameters{
			NOpt: uint(dom.nOpt), A0: cr(dom.a0), Rho: cr(dom.rho), Tau: cr(dom.tau),
			ProtocolMajor: uint(dom.major),
		}
	default:
		pparams = &conway.ConwayProtocolParameters{
			NOpt: uint(dom.nOpt), A0: cr(dom.a0), Rho: cr(dom.rho), Tau: cr(dom.tau),
			ProtocolVersion: common.ProtocolParametersProtocolVersion{Major: uint(dom.major)},
		}
	}
	genesis := &shelley.ShelleyGenesis{
		ActiveSlotsCoeff:  common.GenesisRat{Rat: new(big.Rat).Set(glob.asc)},
		EpochLength:       int(glob.epochLength), //nolint:gosec // request-sized
		MaxLovelaceSupply: glob.maxSupply,
	}
	return epochengine.RewardParametersFromPParams(pparams, genesisStub{genesis}, glob.epochLength)
}

// credKeyRewardsStake is the map key of a request credential.
func credKeyRewardsStake(c *v1.Credential) string { return string([]byte{byte(c.GetKind())}) + string(c.GetHash()) }

func rewardCredential(name string, c *v1.Credential) (rewards.Credential, error) {
	tag, err := credentialTag(c)
	if err != nil {
		return rewards.Credential{}, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
	}
	cred, err := rewards.NewCredential(tag, c.GetHash())
	if err != nil {
		return rewards.Credential{}, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
	}
	return cred, nil
}

func rewardPoolID(name string, id []byte) (rewards.PoolID, error) {
	p, err := rewards.NewPoolID(id)
	if err != nil {
		return p, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
	}
	return p, nil
}

// registeredSet decodes a credential list into a set.
func registeredSet(name string, creds []*v1.Credential) (map[rewards.Credential]struct{}, error) {
	out := make(map[rewards.Credential]struct{}, len(creds))
	for _, c := range creds {
		cred, err := rewardCredential(name, c)
		if err != nil {
			return nil, err
		}
		out[cred] = struct{}{}
	}
	return out, nil
}

// poolMembers decodes the delegators of one pool. A repeated credential is
// BAD_REQUEST; a zero amount is UNSUPPORTED (the ledger's active stake holds
// non-zero amounts only); a total above Word64 is UNSUPPORTED.
func poolMembers(members []*v1.StakeEntry) ([]rewards.Credential, []uint64, uint64, error) {
	seen := map[rewards.Credential]struct{}{}
	creds := make([]rewards.Credential, 0, len(members))
	amounts := make([]uint64, 0, len(members))
	var total uint64
	for _, m := range members {
		cred, err := rewardCredential("members", m.GetCredential())
		if err != nil {
			return nil, nil, 0, err
		}
		if _, dup := seen[cred]; dup {
			return nil, nil, 0, dispatch.BadRequest(errors.New("members: repeated credential"))
		}
		seen[cred] = struct{}{}
		if m.GetAmount() == 0 {
			return nil, nil, 0, unsupportedf("members: zero stake")
		}
		if total > ^uint64(0)-m.GetAmount() {
			return nil, nil, 0, unsupportedf("members: stake total exceeds Word64")
		}
		total += m.GetAmount()
		creds = append(creds, cred)
		amounts = append(amounts, m.GetAmount())
	}
	return creds, amounts, total, nil
}

// poolOwners decodes the owner key hashes. Hashes must be 28 bytes.
func poolOwners(owners [][]byte) (map[rewards.Credential]struct{}, error) {
	out := make(map[rewards.Credential]struct{}, len(owners))
	for _, o := range owners {
		cred, err := rewards.NewCredential(0, o)
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("owners: %w", err))
		}
		out[cred] = struct{}{}
	}
	return out, nil
}

// poolMargin checks and returns a pool's margin.
func poolMargin(p *v1.Pool) (*big.Rat, error) {
	m, err := ratArg("margin", p.GetMargin())
	if err != nil {
		return nil, err
	}
	return m, unitInterval("margin", m)
}

// buildPool assembles a dingo reward pool from the request's pool, its
// delegators and the registered set. An owner counts only when it delegates
// to the pool, as in the ledger's spssSelfDelegatedOwners.
func buildPool(
	p *v1.Pool,
	members []*v1.StakeEntry,
	registered map[rewards.Credential]struct{},
	blocks, blocksTotal uint64,
) (rewards.Pool, error) {
	id, err := rewardPoolID("pool_id", p.GetPoolId())
	if err != nil {
		return rewards.Pool{}, err
	}
	margin, err := poolMargin(p)
	if err != nil {
		return rewards.Pool{}, err
	}
	account, err := rewardCredential("reward_account", p.GetRewardAccount())
	if err != nil {
		return rewards.Pool{}, err
	}
	owners, err := poolOwners(p.GetOwners())
	if err != nil {
		return rewards.Pool{}, err
	}
	creds, amounts, total, err := poolMembers(members)
	if err != nil {
		return rewards.Pool{}, err
	}
	_, accountRegistered := registered[account]
	pool := rewards.Pool{
		ID: id, RewardAccount: account, Margin: margin,
		Pledge: p.GetPledge(), Cost: p.GetCost(), DelegatedStake: total,
		BlocksProduced: blocks, TotalBlocks: blocksTotal,
		RewardAccountRegistered: accountRegistered, RewardAccountEligible: accountRegistered,
		Owners: map[rewards.Credential]struct{}{},
	}
	for i, cred := range creds {
		_, reg := registered[cred]
		pool.Delegators = append(pool.Delegators, rewards.Delegator{
			Credential: cred, Stake: amounts[i], Registered: reg, Eligible: reg,
		})
		if _, owner := owners[cred]; owner {
			pool.Owners[cred] = struct{}{}
			pool.OwnerStake += amounts[i]
		}
	}
	return pool, nil
}

// protoReward converts a dingo account reward; zero amounts are omitted by
// the caller.
func protoReward(r rewards.AccountReward) *v1.Reward {
	kind := v1.RewardKind_REWARD_KIND_MEMBER
	if r.Type == rewards.RewardTypeLeader {
		kind = v1.RewardKind_REWARD_KIND_LEADER
	}
	return &v1.Reward{
		Credential: protoCredential(r.Credential.Tag, r.Credential.Hash[:]),
		PoolId:     append([]byte(nil), r.PoolID[:]...),
		Kind:       kind,
		Amount:     r.Amount,
	}
}

func protoCredential(tag uint8, hash []byte) *v1.Credential {
	kind := v1.CredentialKind_CREDENTIAL_KIND_KEY
	if tag == 1 {
		kind = v1.CredentialKind_CREDENTIAL_KIND_SCRIPT
	}
	return &v1.Credential{Kind: kind, Hash: append([]byte(nil), hash...)}
}

// sortRewards orders rewards by credential kind and hash, then pool id,
// then kind (leader before member), then amount.
func sortRewards(rs []*v1.Reward) {
	sort.Slice(rs, func(i, j int) bool {
		a, b := rs[i], rs[j]
		if ka, kb := credKeyRewardsStake(a.GetCredential()), credKeyRewardsStake(b.GetCredential()); ka != kb {
			return ka < kb
		}
		if c := string(a.GetPoolId()); c != string(b.GetPoolId()) {
			return c < string(b.GetPoolId())
		}
		if a.GetKind() != b.GetKind() {
			return a.GetKind() < b.GetKind()
		}
		return a.GetAmount() < b.GetAmount()
	})
}
