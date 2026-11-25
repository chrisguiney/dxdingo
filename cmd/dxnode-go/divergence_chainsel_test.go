package main

import (
	"fmt"
	"slices"
	"testing"

	"github.com/blinklabs-io/dingo/chainselection"
	ocommon "github.com/blinklabs-io/gouroboros/protocol/common"
)

// Confirmations for the chainsel_genesis_density divergences. They call
// chainselection.FindGenesisDensityLosers directly, bypassing the adapter, so
// the difference cannot come from request translation. The matching checks in
// Test/Chainsel.hs call the reference's densityDisconnect directly.

func fork(id string, idling bool, slots ...uint64) chainselection.GenesisDensityPeer {
	points := []ocommon.Point{ocommon.NewPoint(100, []byte("intersection"))}
	for i, s := range slots {
		points = append(points, ocommon.NewPoint(s, fmt.Appendf(nil, "%s-%d", id, i)))
	}
	return chainselection.GenesisDensityPeer{ID: id, Points: points, Idling: idling}
}

func losers(peers ...chainselection.GenesisDensityPeer) []string {
	var ids []string
	for _, l := range chainselection.FindGenesisDensityLosers(peers, 10, 2, map[string]struct{}{}) {
		ids = append(ids, l.ID)
	}
	slices.Sort(ids)
	return ids
}

// Window 101..110. a = [101, 105, 110] and b = [102, 108, 110] both have a
// complete window of 3 headers. The reference's densityDisconnect disconnects
// both ("having the same density is enough to disconnect peer0", so that
// ChainSync jumping loses either of two peers offering different chains of
// equal density). dingo marks a as it reports it, then b has no rival.
// A real chain reaches it with any two forks of equal density in a window.
func TestDivergenceDensityEqualDensityReportsOnePeer(t *testing.T) {
	t.Parallel()
	got := losers(fork("a", false, 101, 105, 110), fork("b", false, 102, 108, 110))
	if !slices.Equal(got, []string{"a"}) {
		t.Fatalf("dingo losers = %v, want [a] (the reference reports [a b])", got)
	}
}

// b sent MsgAwaitReply with no header after the intersection while a offers
// 10 > k headers. The reference disconnects b (peer0 idling, lb_a >= lb_b).
// dingo does not compare a pair when either peer has no header after the
// intersection. A real chain reaches it with a peer that idles at the
// intersection while another serves the chain.
func TestDivergenceDensityIdlePeerWithoutHeaders(t *testing.T) {
	t.Parallel()
	got := losers(fork("a", false, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110), fork("b", true))
	if len(got) != 0 {
		t.Fatalf("dingo losers = %v, want none (the reference reports [b])", got)
	}
}

// b has one header at 105 but has received a header at slot 120, past the
// window, that the node cannot yet validate (csLatestSlot). The reference
// then knows no header lies between and caps b at 1; dingo has no such input
// and lets b still reach 1 + 5 = 6 > a's 4. A real chain reaches it while
// syncing past the forecast horizon.
func TestDivergenceDensityLatestSlotIgnored(t *testing.T) {
	t.Parallel()
	got := losers(fork("a", false, 101, 102, 103, 104), fork("b", false, 105))
	if len(got) != 0 {
		t.Fatalf("dingo losers = %v, want none (the reference reports [b])", got)
	}
}
