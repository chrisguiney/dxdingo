package corpusgen

import (
	"math"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateEpochObligationsCorpus(t *testing.T) {
	writeCorpus(t, "", epochObligationsCases())
}

func epochObligationsCases() []entry {
	acct := func(b byte, deposit uint64) *v1.Account { return &v1.Account{Credential: key(b), Deposit: deposit} }
	pool := func(id byte, deposit uint64) *v1.Pool {
		return &v1.Pool{
			PoolId: rep(id, 28), Stage: v1.PoolStage_POOL_STAGE_CURRENT, RewardAccount: key(9),
			Deposit: deposit, Margin: rat("1", "100"),
		}
	}
	drep := func(b byte, deposit uint64) *v1.DRepRecord {
		return &v1.DRepRecord{Credential: key(b), Deposit: deposit}
	}
	prop := func(tx byte, idx uint32, deposit uint64) *v1.Proposal {
		return &v1.Proposal{
			Id: &v1.GovActionId{TxId: rep(tx, 32), Index: idx}, Deposit: deposit,
			ReturnAccount: key(1), ProposedEpoch: 4, ExpiresEpoch: 10,
		}
	}
	oblig := func(name, why string, r *v1.EpochObligationsRequest) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_EpochObligations{EpochObligations: r}}}
	}
	return []entry{
		oblig("epoch_obligations_all_kinds", "every deposit kind contributes to its own total and the sum",
			&v1.EpochObligationsRequest{
				Accounts:  []*v1.Account{acct(1, 2_000_000), acct(2, 2_000_000)},
				Pools:     []*v1.Pool{pool(0xa, 500_000_000)},
				Dreps:     []*v1.DRepRecord{drep(3, 500_000_000)},
				Proposals: []*v1.Proposal{prop(7, 0, 100_000_000_000)},
			}),
		oblig("epoch_obligations_empty", "no deposits gives zero obligations", &v1.EpochObligationsRequest{}),
		oblig("epoch_obligations_stored_amount_not_parameter",
			"the deposit each account paid is counted, not the current key deposit",
			&v1.EpochObligationsRequest{Accounts: []*v1.Account{acct(1, 1_500_000), acct(2, 0)}}),
		oblig("epoch_obligations_same_tx_two_proposals", "two proposals of one transaction are distinct by index",
			&v1.EpochObligationsRequest{Proposals: []*v1.Proposal{prop(7, 0, 5), prop(7, 1, 6)}}),
		oblig("epoch_obligations_future_pool_unsupported", "a future-stage pool is outside the reference's pool map",
			&v1.EpochObligationsRequest{Pools: []*v1.Pool{{
				PoolId: rep(1, 28), Stage: v1.PoolStage_POOL_STAGE_FUTURE, RewardAccount: key(9), Deposit: 5, Margin: rat("1", "100"),
			}}}),
		oblig("epoch_obligations_repeated_account_unsupported", "a repeated account has no single deposit",
			&v1.EpochObligationsRequest{Accounts: []*v1.Account{acct(1, 5), acct(1, 6)}}),
		oblig("epoch_obligations_overflow_unsupported", "a deposit total beyond Word64 is not representable",
			&v1.EpochObligationsRequest{Accounts: []*v1.Account{acct(1, math.MaxUint64), acct(2, 1)}}),
	}
}
