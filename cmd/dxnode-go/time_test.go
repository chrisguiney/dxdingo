package main

import (
	"context"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

func u64(n uint64) *uint64 { return &n }

// Mainnet's first three eras. Byron: 21600-slot epochs of 20 s; Shelley is
// epoch 208 at slot 4492800 (208 * 21600) and 89856000 s (4492800 * 20);
// Allegra is epoch 236 at slot 16588800 (4492800 + 28 * 432000) and
// 101952000 s (89856000 + 12096000).
func mainnetEras(closeLast bool) *v1.EraHistory {
	byron := &v1.EraSummary{
		Era: v1.Era_ERA_BYRON, StartTime: rat("0", "1"), EpochLength: 21600, SlotLength: rat("20", "1"), SafeZone: 4320,
		EndSlot: u64(4492800), EndEpoch: u64(208), EndTime: rat("89856000", "1"),
	}
	shelley := &v1.EraSummary{
		Era: v1.Era_ERA_SHELLEY, StartSlot: 4492800, StartEpoch: 208, StartTime: rat("89856000", "1"),
		EpochLength: 432000, SlotLength: rat("1", "1"), SafeZone: 129600,
		EndSlot: u64(16588800), EndEpoch: u64(236), EndTime: rat("101952000", "1"),
	}
	allegra := &v1.EraSummary{
		Era: v1.Era_ERA_ALLEGRA, StartSlot: 16588800, StartEpoch: 236, StartTime: rat("101952000", "1"),
		EpochLength: 432000, SlotLength: rat("1", "1"), SafeZone: 129600,
	}
	if closeLast {
		// Horizon one epoch in: slot 17020800, epoch 237, 102384000 s.
		allegra.EndSlot, allegra.EndEpoch, allegra.EndTime = u64(17020800), u64(237), rat("102384000", "1")
	}
	return &v1.EraHistory{Eras: []*v1.EraSummary{byron, shelley, allegra}, SystemStartUnix: 1506203091}
}

func TestTimeSlotToEpoch(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		slot                      uint64
		epoch, inEpoch, slotsLeft uint64
	}{
		"genesis":                   {0, 0, 0, 21600},
		"last Byron slot":           {4492799, 207, 21599, 1},
		"first Shelley slot":        {4492800, 208, 0, 432000},
		"mid Shelley epoch":         {4492800 + 432000 + 7, 209, 7, 431993},
		"last Shelley slot":         {16588799, 235, 431999, 1},
		"first Allegra slot":        {16588800, 236, 0, 432000},
		"deep in the unbounded era": {16588800 + 10*432000 + 5, 246, 5, 431995},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := timeSlotToEpoch(context.Background(), &v1.TimeSlotToEpochRequest{EraHistory: mainnetEras(false), Slot: tc.slot})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetEpoch() != tc.epoch || got.GetSlotInEpoch() != tc.inEpoch || got.GetSlotsLeft() != tc.slotsLeft {
				t.Fatalf("got %v, want epoch %d in %d left %d", got, tc.epoch, tc.inEpoch, tc.slotsLeft)
			}
		})
	}
}

func rejectedWith(rej *v1.Rejection, name string) bool {
	return len(rej.GetFailures()) == 1 && rej.GetFailures()[0].GetName() == name
}

func TestTimePastHorizon(t *testing.T) {
	t.Parallel()
	closed := mainnetEras(true)
	slotEpoch, err := timeSlotToEpoch(context.Background(), &v1.TimeSlotToEpochRequest{EraHistory: closed, Slot: 17020800})
	if err != nil || !rejectedWith(slotEpoch.GetRejection(), "PastHorizon") {
		t.Fatalf("slot at the horizon: %v %v, want PastHorizon", slotEpoch, err)
	}
	last, err := timeSlotToEpoch(context.Background(), &v1.TimeSlotToEpochRequest{EraHistory: closed, Slot: 17020799})
	if err != nil || last.GetRejection() != nil || last.GetEpoch() != 236 || last.GetSlotsLeft() != 1 {
		t.Fatalf("last slot before the horizon: %v %v", last, err)
	}
	wall, err := timeSlotToWallclock(context.Background(), &v1.TimeSlotToWallclockRequest{EraHistory: closed, Slot: 17020800})
	if err != nil || !rejectedWith(wall.GetRejection(), "PastHorizon") {
		t.Fatalf("slot_to_wallclock at the horizon: %v %v", wall, err)
	}
	for name, tm := range map[string]*v1.Rational{
		"time at the horizon":         rat("102384000", "1"),
		"time past the horizon":       rat("102384001", "1"),
		"time before system start":    rat("-1", "1"),
		"time just before the origin": rat("-1", "1000000000"),
	} {
		got, err := timeWallclockToSlot(context.Background(), &v1.TimeWallclockToSlotRequest{EraHistory: closed, Time: tm})
		if err != nil || !rejectedWith(got.GetRejection(), "PastHorizon") {
			t.Errorf("%s: %v %v, want PastHorizon", name, got, err)
		}
	}
}

func TestTimeSlotToWallclock(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		slot        uint64
		tm, slotLen string
	}{
		"genesis":            {0, "0", "20"},
		"last Byron slot":    {4492799, "89855980", "20"},
		"first Shelley slot": {4492800, "89856000", "1"},
		"mid Shelley":        {4492801, "89856001", "1"},
		"first Allegra slot": {16588800, "101952000", "1"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := timeSlotToWallclock(context.Background(), &v1.TimeSlotToWallclockRequest{EraHistory: mainnetEras(false), Slot: tc.slot})
			if err != nil {
				t.Fatal(err)
			}
			if !ratEq(got.GetTime(), tc.tm, "1") || !ratEq(got.GetSlotLength(), tc.slotLen, "1") {
				t.Fatalf("got time %v slot_length %v, want %s %s", got.GetTime(), got.GetSlotLength(), tc.tm, tc.slotLen)
			}
		})
	}
}

func ratEq(r *v1.Rational, n, d string) bool { return r.GetNumerator() == n && r.GetDenominator() == d }

// Times between slots: a Byron slot is 20 s, so 89855999.5 is 19.5 s into
// the last Byron slot (4492799) and 0.5 s from its end.
func TestTimeWallclockToSlot(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		tm          *v1.Rational
		slot        uint64
		spent, left [2]string
	}{
		"genesis":                            {rat("0", "1"), 0, [2]string{"0", "1"}, [2]string{"20", "1"}},
		"inside the last Byron slot":         {rat("179711999", "2"), 4492799, [2]string{"39", "2"}, [2]string{"1", "2"}},
		"exactly at the boundary":            {rat("89856000", "1"), 4492800, [2]string{"0", "1"}, [2]string{"1", "1"}},
		"half a second into Shelley":         {rat("179712001", "2"), 4492800, [2]string{"1", "2"}, [2]string{"1", "2"}},
		"one nanosecond into Byron":          {rat("1", "1000000000"), 0, [2]string{"1", "1000000000"}, [2]string{"19999999999", "1000000000"}},
		"one nanosecond before the boundary": {rat("89855999999999999", "1000000000"), 4492799, [2]string{"19999999999", "1000000000"}, [2]string{"1", "1000000000"}},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := timeWallclockToSlot(context.Background(), &v1.TimeWallclockToSlotRequest{EraHistory: mainnetEras(false), Time: tc.tm})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || got.GetSlot() != tc.slot ||
				!ratEq(got.GetTimeSpent(), tc.spent[0], tc.spent[1]) || !ratEq(got.GetTimeLeft(), tc.left[0], tc.left[1]) {
				t.Fatalf("got %v, want slot %d spent %v left %v", got, tc.slot, tc.spent, tc.left)
			}
		})
	}
}

// Safe zone and horizon: the current era's end is the first epoch boundary at
// or after max(tip+1, era start) + safe zone.
func TestTimeEraSummary(t *testing.T) {
	t.Parallel()
	open := func() *v1.EraHistory {
		h := mainnetEras(false)
		h.Eras[0].EndSlot, h.Eras[0].EndEpoch, h.Eras[0].EndTime = nil, nil, nil
		h.Eras[1].EndSlot, h.Eras[1].EndEpoch, h.Eras[1].EndTime = nil, nil, nil
		return h
	}
	for name, tc := range map[string]struct {
		tip               uint64
		endSlot, endEpoch uint64
		endTime           string
	}{
		// from = 20000001, target = 20129601: 8.2 epochs past 16588800, so 9.
		"tip inside the era": {20000000, 20476800, 245, "105840000"},
		// from = max(1, 16588800); 129600 slots reach into epoch 236, so 237.
		"tip before the era": {0, 17020800, 237, "102384000"},
		// from = 16891200, target = 17020800 = exactly one epoch: no bump.
		"target on an epoch boundary": {16891199, 17020800, 237, "102384000"},
		// one slot later the target passes the boundary.
		"target one slot past a boundary": {16891200, 17452800, 238, "102816000"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := timeEraSummary(context.Background(), &v1.TimeEraSummaryRequest{EraHistory: open(), TipSlot: tc.tip})
			if err != nil {
				t.Fatal(err)
			}
			if got.GetRejection() != nil || len(got.GetEraHistory().GetEras()) != 3 {
				t.Fatalf("got %v", got)
			}
			eras := got.GetEraHistory().GetEras()
			if eras[0].GetEndSlot() != 4492800 || eras[0].GetEndEpoch() != 208 || !ratEq(eras[0].GetEndTime(), "89856000", "1") {
				t.Errorf("Byron end = %v", eras[0])
			}
			if eras[1].GetEndSlot() != 16588800 || eras[1].GetEndEpoch() != 236 || !ratEq(eras[1].GetEndTime(), "101952000", "1") {
				t.Errorf("Shelley end = %v", eras[1])
			}
			cur := eras[2]
			if cur.GetEndSlot() != tc.endSlot || cur.GetEndEpoch() != tc.endEpoch || !ratEq(cur.GetEndTime(), tc.endTime, "1") {
				t.Errorf("current end = slot %d epoch %d time %v, want %d %d %s", cur.GetEndSlot(), cur.GetEndEpoch(), cur.GetEndTime(), tc.endSlot, tc.endEpoch, tc.endTime)
			}
		})
	}
	// SafeZone 0 is an unbounded era: no end.
	h := open()
	h.Eras[2].SafeZone = 0
	got, err := timeEraSummary(context.Background(), &v1.TimeEraSummaryRequest{EraHistory: h, TipSlot: 16588900})
	if err != nil || got.GetEraHistory().GetEras()[2].EndSlot != nil {
		t.Fatalf("unbounded era: %v %v", got, err)
	}
}

func TestTimeEraParams(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		era                              v1.Era
		g                                *v1.Globals
		epochLen, safe, genesisWin, stab uint64
		slotLen                          string
	}{
		// 3k/f = 3 * 2160 * 20 = 129600.
		"mainnet Shelley": {v1.Era_ERA_SHELLEY, &v1.Globals{EpochLength: 432000, SlotLength: rat("1", "1"), SecurityParam: 2160, ActiveSlotCoeff: rat("1", "20")}, 432000, 129600, 129600, 129600, "1"},
		// preview: 3 * 432 * 20 = 25920.
		"preview Conway": {v1.Era_ERA_CONWAY, &v1.Globals{EpochLength: 86400, SlotLength: rat("1", "1"), SecurityParam: 432, ActiveSlotCoeff: rat("1", "20")}, 86400, 25920, 25920, 25920, "1"},
		// ceil(3 * 5 / (1/3)) = 45; ceil(3 * 7 / (2/3)) = ceil(31.5) = 32.
		"exact quotient": {v1.Era_ERA_BABBAGE, &v1.Globals{EpochLength: 100, SlotLength: rat("1", "2"), SecurityParam: 5, ActiveSlotCoeff: rat("1", "3")}, 100, 45, 45, 45, "1/2"},
		"rounds up":      {v1.Era_ERA_MARY, &v1.Globals{EpochLength: 100, SlotLength: rat("1", "1"), SecurityParam: 7, ActiveSlotCoeff: rat("2", "3")}, 100, 32, 32, 32, "1"},
		// Byron: epoch = 10k slots, safe zone and genesis window 2k.
		"mainnet Byron": {v1.Era_ERA_BYRON, &v1.Globals{EpochLength: 432000, SlotLength: rat("20", "1"), SecurityParam: 2160, ActiveSlotCoeff: rat("1", "20")}, 21600, 4320, 4320, 4320, "20"},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := timeEraParams(context.Background(), &v1.TimeEraParamsRequest{Era: tc.era, Globals: tc.g})
			if err != nil {
				t.Fatal(err)
			}
			want := &v1.TimeEraParamsResponse{}
			_ = want
			if got.GetRejection() != nil || got.GetEpochLength() != tc.epochLen || got.GetSafeZone() != tc.safe ||
				got.GetGenesisWindow() != tc.genesisWin || got.GetStabilityWindow() != tc.stab {
				t.Fatalf("got %v, want epoch %d safe %d window %d stab %d", got, tc.epochLen, tc.safe, tc.genesisWin, tc.stab)
			}
			wantLen := tc.slotLen
			if wantLen == "1/2" {
				if !ratEq(got.GetSlotLength(), "1", "2") {
					t.Fatalf("slot length = %v", got.GetSlotLength())
				}
			} else if !ratEq(got.GetSlotLength(), wantLen, "1") {
				t.Fatalf("slot length = %v", got.GetSlotLength())
			}
		})
	}
}

func TestTimeDomain(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	unsupported := func(name string, err error) {
		t.Helper()
		if dispatch.Code(err) != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
			t.Errorf("%s: err = %v, want UNSUPPORTED", name, err)
		}
	}
	h := mainnetEras(false)
	h.Eras[1].StartSlot++ // no longer where Byron ends
	_, err := timeSlotToEpoch(ctx, &v1.TimeSlotToEpochRequest{EraHistory: h, Slot: 1})
	unsupported("eras that do not line up", err)

	h = mainnetEras(false)
	h.Eras[0].SlotLength = rat("1", "3")
	_, err = timeSlotToEpoch(ctx, &v1.TimeSlotToEpochRequest{EraHistory: h, Slot: 1})
	unsupported("slot length that is not whole nanoseconds", err)

	_, err = timeWallclockToSlot(ctx, &v1.TimeWallclockToSlotRequest{EraHistory: mainnetEras(false), Time: rat("1", "3")})
	unsupported("time that is not whole nanoseconds", err)

	_, err = timeEraParams(ctx, &v1.TimeEraParamsRequest{Era: v1.Era_ERA_SHELLEY, Globals: &v1.Globals{
		EpochLength: 100, SlotLength: rat("1", "1"), SecurityParam: 0, ActiveSlotCoeff: rat("1", "20")}})
	unsupported("zero security parameter", err)
	_, err = timeEraParams(ctx, &v1.TimeEraParamsRequest{Era: v1.Era_ERA_SHELLEY, Globals: &v1.Globals{
		EpochLength: 100, SlotLength: rat("1", "1"), SecurityParam: 5, ActiveSlotCoeff: rat("0", "1")}})
	unsupported("zero active slot coefficient", err)
	_, err = timeEraParams(ctx, &v1.TimeEraParamsRequest{Era: v1.Era_ERA_SHELLEY, Globals: &v1.Globals{
		EpochLength: 100, SlotLength: rat("1", "3"), SecurityParam: 5, ActiveSlotCoeff: rat("1", "20")}})
	unsupported("slot length that is not whole milliseconds", err)
	_, err = timeEraParams(ctx, &v1.TimeEraParamsRequest{Globals: &v1.Globals{
		EpochLength: 100, SlotLength: rat("1", "1"), SecurityParam: 5, ActiveSlotCoeff: rat("1", "20")}})
	unsupported("unspecified era", err)
}
