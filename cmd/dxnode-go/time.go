package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math"
	"math/big"
	"strings"
	"time"

	"github.com/blinklabs-io/dingo/config/cardano"
	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/dingo/ledger/hardfork"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// The era history is dingo's hardfork.Summary. dingo keeps times as
// time.Duration (whole nanoseconds, int64) and the reference as
// NominalDiffTime (picoseconds), so a slot length, a bound time or a queried
// time that is not a whole number of nanoseconds, or that does not fit, is
// UNSUPPORTED on both nodes.

const nsPerSecond = 1_000_000_000

// maxNs bounds every time the adapter accepts. It leaves room for dingo's
// unchecked Duration arithmetic when it builds an era end.
var maxNs = new(big.Int).Lsh(big.NewInt(1), 62)

type timeBound struct {
	slot, epoch uint64
	ns          *big.Int
}

type timeEra struct {
	era      v1.Era
	start    timeBound
	end      *timeBound
	epochLen uint64
	slotNs   int64
	safeZone uint64
}

// nsArg reads a rational number of seconds as whole nanoseconds.
func nsArg(name string, r *v1.Rational) (*big.Int, error) {
	x, err := ratArg(name, r)
	if err != nil {
		return nil, err
	}
	x = new(big.Rat).Mul(x, big.NewRat(nsPerSecond, 1))
	if !x.IsInt() {
		return nil, dispatch.Unsupported(fmt.Errorf("%s is not a whole number of nanoseconds", name))
	}
	if x.Num().CmpAbs(maxNs) > 0 {
		return nil, dispatch.Unsupported(fmt.Errorf("%s does not fit a Duration", name))
	}
	return x.Num(), nil
}

func nsRat(ns *big.Int) *v1.Rational {
	return convert.RatProto(new(big.Rat).SetFrac(ns, big.NewInt(nsPerSecond)))
}

// parseHistory decodes an EraHistory and checks what the reference's
// invariantSummary checks: the eras line up, and every bounded era satisfies
// end slot = start slot + epochs * epoch length and end time = start time +
// slots * slot length. withEnds false is the era-summary input, whose end
// bounds must be unset.
func parseHistory(h *v1.EraHistory, withEnds bool) ([]timeEra, error) {
	if len(h.GetEras()) == 0 {
		return nil, dispatch.BadRequest(errors.New("era_history has no eras"))
	}
	out := make([]timeEra, 0, len(h.GetEras()))
	for i, e := range h.GetEras() {
		if e.GetEpochLength() == 0 {
			return nil, dispatch.Unsupported(fmt.Errorf("era %d: epoch_length is zero", i))
		}
		slotNs, err := nsArg("slot_length", e.GetSlotLength())
		if err != nil {
			return nil, err
		}
		if slotNs.Sign() <= 0 {
			return nil, dispatch.Unsupported(fmt.Errorf("era %d: slot_length must be positive", i))
		}
		startNs, err := nsArg("start_time", e.GetStartTime())
		if err != nil {
			return nil, err
		}
		te := timeEra{
			era:      e.GetEra(),
			start:    timeBound{e.GetStartSlot(), e.GetStartEpoch(), startNs},
			epochLen: e.GetEpochLength(),
			slotNs:   slotNs.Int64(),
			safeZone: e.GetSafeZone(),
		}
		hasEnd := e.EndSlot != nil || e.EndEpoch != nil || e.EndTime != nil
		switch {
		case hasEnd && !withEnds:
			return nil, dispatch.Unsupported(fmt.Errorf("era %d: end bounds must be unset", i))
		case hasEnd:
			if e.EndSlot == nil || e.EndEpoch == nil || e.GetEndTime() == nil {
				return nil, dispatch.BadRequest(fmt.Errorf("era %d: end bounds are partly set", i))
			}
			endNs, err := nsArg("end_time", e.GetEndTime())
			if err != nil {
				return nil, err
			}
			te.end = &timeBound{e.GetEndSlot(), e.GetEndEpoch(), endNs}
		case withEnds && i != len(h.GetEras())-1:
			return nil, dispatch.Unsupported(fmt.Errorf("era %d: only the last era may be unbounded", i))
		}
		out = append(out, te)
	}
	first := out[0].start
	if first.slot != 0 || first.epoch != 0 || first.ns.Sign() != 0 {
		return nil, dispatch.Unsupported(errors.New("the first era must start at slot 0, epoch 0, time 0"))
	}
	for i := range out {
		e := out[i]
		if e.end != nil {
			if err := checkEraEnd(i, e); err != nil {
				return nil, err
			}
		}
		if i+1 < len(out) {
			next := out[i+1].start
			if e.end == nil || e.end.slot != next.slot || e.end.epoch != next.epoch || e.end.ns.Cmp(next.ns) != 0 {
				if !(e.end == nil && !withEnds) {
					return nil, dispatch.Unsupported(fmt.Errorf("era %d does not end where era %d starts", i, i+1))
				}
				// Era-summary input: the past era ends where the next starts.
				end := next
				out[i].end = &end
				if err := checkEraEnd(i, out[i]); err != nil {
					return nil, err
				}
			}
		}
	}
	return out, nil
}

// checkEraEnd is invariantSummary's INV-1b and INV-2b for one bounded era.
func checkEraEnd(i int, e timeEra) error {
	if e.end.epoch < e.start.epoch {
		return dispatch.Unsupported(fmt.Errorf("era %d ends before it starts", i))
	}
	epochs := new(big.Int).SetUint64(e.end.epoch - e.start.epoch)
	slots := new(big.Int).Mul(epochs, new(big.Int).SetUint64(e.epochLen))
	wantSlot := slots.Add(slots, new(big.Int).SetUint64(e.start.slot))
	if wantSlot.Cmp(new(big.Int).SetUint64(e.end.slot)) != 0 {
		return dispatch.Unsupported(fmt.Errorf("era %d: end slot breaks end = start + epochs * epoch_length", i))
	}
	span := new(big.Int).Sub(new(big.Int).SetUint64(e.end.slot), new(big.Int).SetUint64(e.start.slot))
	wantNs := span.Mul(span, big.NewInt(e.slotNs))
	wantNs.Add(wantNs, e.start.ns)
	if wantNs.Cmp(e.end.ns) != 0 {
		return dispatch.Unsupported(fmt.Errorf("era %d: end time breaks end = start + slots * slot_length", i))
	}
	return nil
}

func hardforkSummary(h *v1.EraHistory, eraList []timeEra) *hardfork.Summary {
	s := &hardfork.Summary{SystemStart: time.Unix(h.GetSystemStartUnix(), 0).UTC()}
	for _, e := range eraList {
		es := hardfork.EraSummary{
			EraID: uint(e.era),
			Start: hardfork.Bound{RelativeTime: time.Duration(e.start.ns.Int64()), Slot: e.start.slot, Epoch: e.start.epoch},
			Params: hardfork.EraParams{
				EpochSize: e.epochLen, SlotLength: time.Duration(e.slotNs), SafeZoneSlots: e.safeZone, GenesisWindow: e.safeZone,
			},
		}
		if e.end != nil {
			es.End = &hardfork.Bound{RelativeTime: time.Duration(e.end.ns.Int64()), Slot: e.end.slot, Epoch: e.end.epoch}
		}
		s.Eras = append(s.Eras, es)
	}
	return s
}

// hardForkRejection maps dingo's horizon errors onto the reference's
// PastHorizon. A time before system start is outside every era, which the
// reference also reports as PastHorizon.
func hardForkRejection(err error) (*v1.Rejection, error) {
	switch {
	case errors.Is(err, hardfork.ErrPastHorizon), errors.Is(err, hardfork.ErrBeforeGenesis):
		return &v1.Rejection{Failures: []*v1.Failure{{Rule: "HardFork", Name: "PastHorizon", Detail: err.Error()}}}, nil
	case errors.Is(err, hardfork.ErrDurationOverflow):
		return nil, dispatch.Unsupported(err)
	}
	return dingoRejection("HardFork", err), nil
}

func historySummary(h *v1.EraHistory) (*hardfork.Summary, error) {
	eraList, err := parseHistory(h, true)
	if err != nil {
		return nil, err
	}
	return hardforkSummary(h, eraList), nil
}

// timeEraSummary maps to hardfork.BuildSummary, the function that takes the
// current era's safe zone and snaps it to an epoch boundary. The past eras
// are closed at the next era's start, as the reference's summarize does.
func timeEraSummary(_ context.Context, req *v1.TimeEraSummaryRequest) (*v1.TimeEraSummaryResponse, error) {
	h := req.GetEraHistory()
	eraList, err := parseHistory(h, false)
	if err != nil {
		return nil, err
	}
	s := hardforkSummary(h, eraList)
	n := len(s.Eras)
	built, err := hardfork.BuildSummary(
		hardfork.Shape{SystemStart: s.SystemStart}, s.Eras[:n-1], s.Eras[n-1], req.GetTipSlot(), hardfork.NewTransitionUnknown(),
	)
	if err != nil {
		return &v1.TimeEraSummaryResponse{Rejection: dingoRejection("HardFork", err)}, nil
	}
	// dingo's era-end arithmetic does not check for overflow; a wrapped end
	// breaks the invariants the reference states and is outside the domain.
	for i, e := range built.Eras {
		if e.End == nil {
			continue
		}
		te := eraList[i]
		te.end = &timeBound{e.End.Slot, e.End.Epoch, big.NewInt(int64(e.End.RelativeTime))}
		if err := checkEraEnd(i, te); err != nil {
			return nil, err
		}
	}
	out := &v1.EraHistory{SystemStartUnix: built.SystemStart.Unix()}
	for i, e := range built.Eras {
		es := &v1.EraSummary{
			Era:         eraList[i].era,
			StartSlot:   e.Start.Slot,
			StartEpoch:  e.Start.Epoch,
			StartTime:   nsRat(big.NewInt(int64(e.Start.RelativeTime))),
			EpochLength: e.Params.EpochSize,
			SlotLength:  nsRat(big.NewInt(int64(e.Params.SlotLength))),
			SafeZone:    e.Params.SafeZoneSlots,
		}
		if e.End != nil {
			es.EndSlot, es.EndEpoch = &e.End.Slot, &e.End.Epoch
			es.EndTime = nsRat(big.NewInt(int64(e.End.RelativeTime)))
		}
		out.Eras = append(out.Eras, es)
	}
	return &v1.TimeEraSummaryResponse{EraHistory: out}, nil
}

// timeSlotToEpoch maps to Summary.SlotToEpoch; the slot's place in its epoch
// follows from the epoch's start slot and length it returns.
func timeSlotToEpoch(_ context.Context, req *v1.TimeSlotToEpochRequest) (*v1.TimeSlotToEpochResponse, error) {
	s, err := historySummary(req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	info, err := s.SlotToEpoch(req.GetSlot())
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeSlotToEpochResponse{Rejection: rej}, uerr
	}
	in := req.GetSlot() - info.StartSlot
	return &v1.TimeSlotToEpochResponse{Epoch: info.Epoch, SlotInEpoch: in, SlotsLeft: info.LengthInSlots - in}, nil
}

// timeSlotToWallclock maps to Summary.SlotToTime; the slot length is the one
// Summary.SlotToEpoch reports for the slot's era.
func timeSlotToWallclock(_ context.Context, req *v1.TimeSlotToWallclockRequest) (*v1.TimeSlotToWallclockResponse, error) {
	s, err := historySummary(req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	at, err := s.SlotToTime(req.GetSlot())
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeSlotToWallclockResponse{Rejection: rej}, uerr
	}
	info, err := s.SlotToEpoch(req.GetSlot())
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeSlotToWallclockResponse{Rejection: rej}, uerr
	}
	return &v1.TimeSlotToWallclockResponse{
		Time:       nsRat(big.NewInt(int64(at.Sub(s.SystemStart)))),
		SlotLength: nsRat(big.NewInt(int64(info.SlotLength))),
	}, nil
}

// timeWallclockToSlot maps to Summary.TimeToSlot. Time spent in the slot is
// the queried time less the slot's start (Summary.SlotToTime), and time left
// is the slot length less that.
func timeWallclockToSlot(_ context.Context, req *v1.TimeWallclockToSlotRequest) (*v1.TimeWallclockToSlotResponse, error) {
	s, err := historySummary(req.GetEraHistory())
	if err != nil {
		return nil, err
	}
	ns, err := nsArg("time", req.GetTime())
	if err != nil {
		return nil, err
	}
	at := s.SystemStart.Add(time.Duration(ns.Int64()))
	slot, err := s.TimeToSlot(at)
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeWallclockToSlotResponse{Rejection: rej}, uerr
	}
	start, err := s.SlotToTime(slot)
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeWallclockToSlotResponse{Rejection: rej}, uerr
	}
	info, err := s.SlotToEpoch(slot)
	if err != nil {
		rej, uerr := hardForkRejection(err)
		return &v1.TimeWallclockToSlotResponse{Rejection: rej}, uerr
	}
	spent := at.Sub(start)
	return &v1.TimeWallclockToSlotResponse{
		Slot:      slot,
		TimeSpent: nsRat(big.NewInt(int64(spent))),
		TimeLeft:  nsRat(big.NewInt(int64(info.SlotLength - spent))),
	}, nil
}

// timeEraParams maps to eras.BuildEraParams and eras.StabilityWindowForEra,
// fed a node config built from the request's globals. dingo reads slot
// lengths in whole milliseconds and the reference's Shelley genesis in whole
// microseconds, so a slot length must be whole milliseconds.
func timeEraParams(_ context.Context, req *v1.TimeEraParamsRequest) (*v1.TimeEraParamsResponse, error) {
	era := req.GetEra()
	if era < v1.Era_ERA_BYRON || era > v1.Era_ERA_DIJKSTRA {
		return nil, dispatch.Unsupported(fmt.Errorf("no era parameters for %v", era))
	}
	g := req.GetGlobals()
	if g == nil {
		return nil, dispatch.BadRequest(errors.New("missing globals"))
	}
	slotLen, err := ratArg("slot_length", g.GetSlotLength())
	if err != nil {
		return nil, err
	}
	ms := new(big.Rat).Mul(slotLen, big.NewRat(1000, 1))
	if slotLen.Sign() <= 0 || !ms.IsInt() || slotLen.Cmp(big.NewRat(1_000_000, 1)) > 0 {
		return nil, dispatch.Unsupported(errors.New("slot_length must be positive whole milliseconds"))
	}
	k := g.GetSecurityParam()
	if k == 0 || k > 1<<40 {
		return nil, dispatch.Unsupported(errors.New("security_param must be in 1..2^40"))
	}
	if era != v1.Era_ERA_BYRON {
		if g.GetEpochLength() == 0 || g.GetEpochLength() > math.MaxInt64 {
			return nil, dispatch.Unsupported(errors.New("epoch_length must be in 1..2^63-1"))
		}
	}
	f, err := ratArg("active_slot_coeff", g.GetActiveSlotCoeff())
	if err != nil {
		return nil, err
	}
	if era != v1.Era_ERA_BYRON {
		if err := positiveUnitInterval("active_slot_coeff", f); err != nil {
			return nil, err
		}
		if f.Num().BitLen() > 62 || f.Denom().BitLen() > 62 {
			return nil, dispatch.Unsupported(errors.New("active_slot_coeff does not fit an int64 ratio"))
		}
		window := new(big.Int).Mul(big.NewInt(3), new(big.Int).SetUint64(k))
		window.Mul(window, f.Denom())
		window.Add(window, new(big.Int).Sub(f.Num(), big.NewInt(1)))
		window.Quo(window, f.Num())
		if !window.IsUint64() {
			return nil, dispatch.Unsupported(errors.New("stability window overflows Word64"))
		}
	}

	cfg := &cardano.CardanoNodeConfig{}
	if err := cfg.LoadByronGenesisFromReader(strings.NewReader(fmt.Sprintf(byronGenesisJSON, ms.Num().String(), k))); err != nil {
		return nil, fmt.Errorf("byron genesis: %w", err)
	}
	if err := cfg.LoadShelleyGenesisFromReader(bytes.NewReader(shelleyGenesisJSON(g, slotLen, f))); err != nil {
		return nil, fmt.Errorf("shelley genesis: %w", err)
	}
	desc := eras.GetEraById(uint(era) - 1)
	if desc == nil {
		return nil, dispatch.Unsupported(fmt.Errorf("dingo has no era %v", era))
	}
	params, err := eras.BuildEraParams(cfg, *desc)
	if err != nil {
		return &v1.TimeEraParamsResponse{Rejection: dingoRejection("HardFork", err)}, nil
	}
	stab, err := eras.StabilityWindowForEra(cfg, desc.Id)
	if err != nil {
		return &v1.TimeEraParamsResponse{Rejection: dingoRejection("HardFork", err)}, nil
	}
	return &v1.TimeEraParamsResponse{
		EpochLength:     params.EpochSize,
		SlotLength:      convert.RatProto(big.NewRat(int64(params.SlotLength/time.Millisecond), 1000)),
		SafeZone:        params.SafeZoneSlots,
		GenesisWindow:   params.GenesisWindow,
		StabilityWindow: stab,
	}, nil
}

// byronGenesisJSON carries slotDuration and k; the rest is filler for the
// keys the loader requires.
const byronGenesisJSON = `{"avvmDistr":{},"startTime":1506203091,"bootStakeholders":{},"heavyDelegation":{},
 "nonAvvmBalances":{},"vssCerts":{},"ftsSeed":"x",
 "blockVersionData":{"heavyDelThd":"300000000000","maxBlockSize":"2000000","maxHeaderSize":"2000000",
  "maxProposalSize":"700","maxTxSize":"4096","mpcThd":"20000000000000","scriptVersion":0,"slotDuration":"%s",
  "softforkRule":{"initThd":"900000000000000","minThd":"600000000000000","thdDecrement":"50000000000000"},
  "txFeePolicy":{"multiplier":"43946000000","summand":"155381000000000"},"unlockStakeEpoch":"18446744073709551615",
  "updateImplicit":"10000","updateProposalThd":"100000000000000","updateVoteThd":"1000000000000"},
 "protocolConsts":{"k":%d,"protocolMagic":764824073,"vssMaxTtl":6,"vssMinTtl":2}}`

// shelleyGenesisJSON is a Shelley genesis carrying the request's globals and
// filler for the keys dingo's loader requires but the era parameters ignore.
func shelleyGenesisJSON(g *v1.Globals, slotLen, f *big.Rat) []byte {
	return fmt.Appendf(nil, `{
 "systemStart":"2017-09-23T21:44:51Z","networkMagic":%d,"networkId":"Mainnet",
 "activeSlotsCoeff":{"numerator":%s,"denominator":%s},"securityParam":%d,"epochLength":%d,
 "slotsPerKESPeriod":129600,"maxKESEvolutions":62,"slotLength":%s,"updateQuorum":5,
 "maxLovelaceSupply":45000000000000000,
 "protocolParams":{"minFeeA":44,"minFeeB":155381,"maxBlockBodySize":65536,"maxTxSize":16384,
  "maxBlockHeaderSize":1100,"keyDeposit":2000000,"poolDeposit":500000000,"eMax":18,"nOpt":150,
  "a0":0.3,"rho":0.003,"tau":0.2,"decentralisationParam":0,"extraEntropy":{"tag":"NeutralNonce"},
  "protocolVersion":{"major":2,"minor":0},"minUTxOValue":1000000,"minPoolCost":340000000},
 "genDelegs":{},"initialFunds":{}}`,
		g.GetNetworkMagic(), f.Num(), f.Denom(), g.GetSecurityParam(), g.GetEpochLength(), slotLen.FloatString(3))
}

func registerTime(r *dispatch.Registry) {
	dispatch.Register(r, timeEraParams)
	dispatch.Register(r, timeEraSummary)
	dispatch.Register(r, timeSlotToEpoch)
	dispatch.Register(r, timeSlotToWallclock)
	dispatch.Register(r, timeWallclockToSlot)
}
