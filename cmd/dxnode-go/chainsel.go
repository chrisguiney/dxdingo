package main

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"slices"
	"strings"

	"github.com/blinklabs-io/dingo/chainselection"
	"github.com/blinklabs-io/dingo/consensus/praos"
	ochainsync "github.com/blinklabs-io/gouroboros/protocol/chainsync"
	ocommon "github.com/blinklabs-io/gouroboros/protocol/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Chain comparison runs through dingo's consensus/praos package, the one
// consensus.Provider, the chain selector and the local-sibling check all
// call. gouroboros consensus.ComparePraosTips is a second, unrestricted-only
// copy that dingo does not use for selection.

const (
	praosVRFSize    = praos.VRFOutputSize
	praosIssuerSize = 32
)

// praosTip validates a ChainTip against what the reference types can hold:
// a 32-byte issuer verification key and a 64-byte VRF output. dingo's
// CompareVRFOutputs treats any other length as "absent" while the reference
// orders the bytes, so the pair is UNSUPPORTED rather than a false mismatch.
func praosTip(name string, t *v1.ChainTip) error {
	if t == nil {
		return dispatch.BadRequest(fmt.Errorf("%s: missing tip", name))
	}
	if len(t.GetIssuerVkey()) != praosIssuerSize {
		return dispatch.Unsupported(fmt.Errorf("%s: issuer_vkey must be %d bytes", name, praosIssuerSize))
	}
	if len(t.GetTiebreakVrf()) != praosVRFSize {
		return dispatch.Unsupported(fmt.Errorf("%s: tiebreak_vrf must be %d bytes", name, praosVRFSize))
	}
	return nil
}

func tiebreakerConfig(window *uint64) praos.PraosTiebreakerConfig {
	if window == nil {
		return praos.PraosTiebreakerConfigBeforeConway()
	}
	return praos.PraosTiebreakerConfig{Flavor: praos.PraosTiebreakerRestricted, MaxSlotDistance: *window}
}

// tipView builds the chainsync tip and the Praos view for one ChainTip. The
// request carries no block hash, and ComparePraosTips treats equal slot and
// hash as the same block, so each side gets a distinct synthetic hash.
func tipView(side byte, t *v1.ChainTip, cfg praos.PraosTiebreakerConfig) (ochainsync.Tip, praos.PraosTiebreakerView) {
	hash := make([]byte, 9)
	hash[0] = side
	binary.BigEndian.PutUint64(hash[1:], t.GetSlot())
	tp := ochainsync.Tip{
		Point:       ocommon.NewPoint(t.GetSlot(), hash),
		BlockNumber: t.GetBlockNo(),
	}
	return tp, praos.NewPraosTiebreakerViewFull(tp, t.GetIssuerVkey(), t.GetIssueNo(), t.GetTiebreakVrf(), cfg)
}

func orderOf(r praos.ChainComparisonResult) v1.Ordering {
	switch r {
	case praos.ChainABetter:
		return v1.Ordering_ORDERING_GT
	case praos.ChainBBetter:
		return v1.Ordering_ORDERING_LT
	}
	return v1.Ordering_ORDERING_EQ
}

func praosEra(era v1.Era) error {
	if era < v1.Era_ERA_SHELLEY || era > v1.Era_ERA_DIJKSTRA {
		return dispatch.Unsupported(fmt.Errorf("no Praos chain selection in %v", era))
	}
	return nil
}

// chainselCompare maps to praos.ComparePraosTips. prefer_b is dingo's
// PreferPraosCandidate(ours=a, candidate=b) after the block-number check, as
// preferCandidate cfg ours cand reports it for the reference.
func chainselCompare(_ context.Context, req *v1.ChainselCompareRequest) (*v1.ChainselCompareResponse, error) {
	if err := praosEra(req.GetEra()); err != nil {
		return nil, err
	}
	for name, t := range map[string]*v1.ChainTip{"a": req.GetA(), "b": req.GetB()} {
		if err := praosTip(name, t); err != nil {
			return nil, err
		}
	}
	cfg := tiebreakerConfig(req.RestrictedVrfWindow)
	tipA, viewA := tipView('a', req.GetA(), cfg)
	tipB, viewB := tipView('b', req.GetB(), cfg)
	got := praos.ComparePraosTips(tipA, tipB, viewA, viewB)
	return &v1.ChainselCompareResponse{Order: orderOf(got), PreferB: got == praos.ChainBBetter}, nil
}

// chainselVrfTiebreak maps to praos.PreferPraosCandidate, the tiebreaker
// below the block-number comparison, asked in both directions.
func chainselVrfTiebreak(_ context.Context, req *v1.ChainselVrfTiebreakRequest) (*v1.ChainselVrfTiebreakResponse, error) {
	for name, t := range map[string]*v1.ChainTip{"a": req.GetA(), "b": req.GetB()} {
		if err := praosTip(name, t); err != nil {
			return nil, err
		}
	}
	cfg := tiebreakerConfig(req.RestrictedVrfWindow)
	_, viewA := tipView('a', req.GetA(), cfg)
	_, viewB := tipView('b', req.GetB(), cfg)
	switch {
	case praos.PreferPraosCandidate(viewA, viewB):
		return &v1.ChainselVrfTiebreakResponse{Order: v1.Ordering_ORDERING_LT}, nil
	case praos.PreferPraosCandidate(viewB, viewA):
		return &v1.ChainselVrfTiebreakResponse{Order: v1.Ordering_ORDERING_GT}, nil
	}
	return &v1.ChainselVrfTiebreakResponse{Order: v1.Ordering_ORDERING_EQ}, nil
}

// densityPoints turns one candidate's slots into the header frontier dingo
// keeps: the shared intersection point first, then one header per slot. The
// request carries no hashes, so every candidate gets its own, and candidates
// fork right after the intersection.
func densityPoints(c *v1.DensityCandidate, loeSlot uint64) []ocommon.Point {
	points := make([]ocommon.Point, 0, len(c.GetSlots())+1)
	points = append(points, ocommon.NewPoint(loeSlot, []byte("intersection")))
	for i, s := range c.GetSlots() {
		points = append(points, ocommon.NewPoint(s, fmt.Appendf(nil, "%s/%d", c.GetPeer(), i)))
	}
	return points
}

// chainselGenesisDensity maps to chainselection.FindGenesisDensityLosers, the
// comparison the chain selector runs for the Genesis Density Disconnector.
// dingo knows a peer's headers and whether it idles; it has no counterpart to
// the reference's latest-received slot, so tip_slot is validated and not
// passed. The disconnect list is sorted by peer name.
func chainselGenesisDensity(_ context.Context, req *v1.ChainselGenesisDensityRequest) (*v1.ChainselGenesisDensityResponse, error) {
	if err := nonzero("genesis_window", req.GetGenesisWindow(), "security_param", req.GetSecurityParam()); err != nil {
		return nil, err
	}
	loe := req.GetLoeSlot()
	if req.GetGenesisWindow() > math.MaxInt64 || loe > math.MaxInt64-req.GetGenesisWindow() {
		return nil, dispatch.Unsupported(errors.New("loe_slot + genesis_window exceeds the slot range"))
	}
	seen := map[string]struct{}{}
	peers := make([]chainselection.GenesisDensityPeer, 0, len(req.GetCandidates()))
	for _, c := range req.GetCandidates() {
		if c.GetPeer() == "" {
			return nil, dispatch.Unsupported(errors.New("candidate with no peer name"))
		}
		if _, dup := seen[c.GetPeer()]; dup {
			return nil, dispatch.Unsupported(fmt.Errorf("duplicate candidate %q", c.GetPeer()))
		}
		seen[c.GetPeer()] = struct{}{}
		head := loe
		for _, s := range c.GetSlots() {
			if s <= head {
				return nil, dispatch.Unsupported(fmt.Errorf("%s: slots must ascend strictly after loe_slot", c.GetPeer()))
			}
			head = s
		}
		if c.GetTipSlot() < head {
			return nil, dispatch.Unsupported(fmt.Errorf("%s: tip_slot is behind the candidate's headers", c.GetPeer()))
		}
		peers = append(peers, chainselection.GenesisDensityPeer{
			ID: c.GetPeer(), Points: densityPoints(c, loe), Idling: c.GetIdling(),
		})
	}
	slices.SortFunc(peers, func(a, b chainselection.GenesisDensityPeer) int { return strings.Compare(a.ID, b.ID) })
	losers := chainselection.FindGenesisDensityLosers(peers, req.GetGenesisWindow(), req.GetSecurityParam(), map[string]struct{}{})
	var out []string
	for _, l := range losers {
		out = append(out, l.ID)
	}
	slices.Sort(out)
	return &v1.ChainselGenesisDensityResponse{Disconnect: out}, nil
}

func registerChainsel(r *dispatch.Registry) {
	dispatch.Register(r, chainselCompare)
	dispatch.Register(r, chainselVrfTiebreak)
	dispatch.Register(r, chainselGenesisDensity)
}
