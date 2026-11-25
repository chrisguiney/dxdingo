package ledgerfixture

import (
	"testing"

	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/stretchr/testify/require"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/rulefixture"
)

// Splitting a mainnet block into transactions and putting them back must give
// the block's own bytes: the oracle is the chain, not the builder.
func TestBlockWithRoundTripsAMainnetBlock(t *testing.T) {
	t.Parallel()
	for _, era := range []v1.Era{v1.Era_ERA_BABBAGE, v1.Era_ERA_CONWAY} {
		raw, _, err := rulefixture.BlockCbor(era)
		require.NoError(t, err)
		body, err := BodyOf(raw)
		require.NoError(t, err)
		again, err := BlockWith(raw, body, Edit{})
		require.NoError(t, err)
		require.Equal(t, raw, again, era.String())
	}
}

// A rebuilt body decodes under the decoder's body hash check, and the two
// breaks each trip it or leave it alone as named.
func TestBlockWithCommitsToTheNewBody(t *testing.T) {
	t.Parallel()
	raw, typ, err := rulefixture.BlockCbor(v1.Era_ERA_CONWAY)
	require.NoError(t, err)
	body, err := BodyOf(raw)
	require.NoError(t, err)
	subset := Body{body[0], body[2]}
	blk, err := BlockWith(raw, subset, Edit{})
	require.NoError(t, err)
	decoded, err := ledger.NewBlockFromCbor(typ, blk)
	require.NoError(t, err)
	require.Len(t, decoded.Transactions(), 2)

	flipped, err := BlockWith(raw, subset, Edit{FlipHash: true})
	require.NoError(t, err)
	_, err = ledger.NewBlockFromCbor(typ, flipped)
	require.Error(t, err)

	wrongSize, err := BlockWith(raw, subset, Edit{SizeDelta: 1})
	require.NoError(t, err)
	d, err := ledger.NewBlockFromCbor(typ, wrongSize)
	require.NoError(t, err)
	got, err := common.BlockBodySizeFromCbor(wrongSize)
	require.NoError(t, err)
	require.Equal(t, got+1, d.BlockBodySize())
}
