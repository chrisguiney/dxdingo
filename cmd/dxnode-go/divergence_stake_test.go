package main

import (
	"context"
	"testing"

	"github.com/blinklabs-io/dingo/dxoracle"
)

// Divergence: a DRep (or predefined bucket) whose stake is zero.
// cardano-ledger's computeDRepDistr inserts an entry for every account that
// delegates to a registered DRep, with stake 0 when the account holds
// nothing (Test/Stake.hs, "divergence: ..."). dingo's
// governance.BuildDRepStakeDistribution drops a zero merged stake. This calls
// the dingo loader directly, with no adapter in between.
//
// A real chain reaches it: a registered stake credential with no UTxO and no
// rewards that delegates to a DRep. Ratification reads a missing entry as
// zero, so no ratification outcome changes; a distribution query lists the
// entry only on the Haskell node.
func TestDivergenceDRepZeroStakeEntryIsOmitted(t *testing.T) {
	oracle, err := scratchOracle()
	if err != nil {
		t.Fatal(err)
	}
	key := func(b byte) []byte {
		k := make([]byte, 28)
		for i := range k {
			k[i] = b
		}
		return k
	}
	entries, err := oracle.DRepStakeDistribution(context.Background(), dxoracle.DRepStakeDistributionInput{
		ActiveProposalEpoch: 10,
		Accounts: []dxoracle.Account{
			{Key: key(5), Active: true, Delegation: dxoracle.Delegation{Type: dxoracle.DelegateKeyHash, Key: key(0xa2)}},
			{Key: key(6), Active: true, Delegation: dxoracle.Delegation{Type: dxoracle.DelegateAlwaysAbstain}},
		},
		Dreps: []dxoracle.DRep{{Key: key(0xa2), ExpiryEpoch: 20, Active: true}},
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		t.Errorf("dingo listed %v with stake %d; the ledger lists a zero entry", e.DRepType, e.Stake)
	}
}
