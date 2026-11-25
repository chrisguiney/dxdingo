package corpusgen

import (
	"math/big"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

func TestGenerateTimeCorpus(t *testing.T) {
	writeCorpus(t, "", timeCases())
}

// eraSpec describes one era: its length in epochs (0 for an empty era), epoch
// length in slots, slot length in seconds as a rational, and safe zone.
type eraSpec struct {
	era      v1.Era
	epochs   uint64 // epochs the era lasts; ignored for the last era
	epochLen uint64
	slotLen  [2]int64 // numerator, denominator of seconds
	safeZone uint64
}

var (
	mainnetByron   = eraSpec{v1.Era_ERA_BYRON, 208, 21600, [2]int64{20, 1}, 4320}
	mainnetShelley = eraSpec{v1.Era_ERA_SHELLEY, 28, 432000, [2]int64{1, 1}, 129600}
	mainnetAllegra = eraSpec{v1.Era_ERA_ALLEGRA, 15, 432000, [2]int64{1, 1}, 129600}
	mainnetMary    = eraSpec{v1.Era_ERA_MARY, 39, 432000, [2]int64{1, 1}, 129600}
	mainnetAlonzo  = eraSpec{v1.Era_ERA_ALONZO, 104, 432000, [2]int64{1, 1}, 129600}
	mainnetBabbage = eraSpec{v1.Era_ERA_BABBAGE, 113, 432000, [2]int64{1, 1}, 129600}
	mainnetConway  = eraSpec{v1.Era_ERA_CONWAY, 0, 432000, [2]int64{1, 1}, 129600}
)

func ratOf(r *big.Rat) *v1.Rational {
	return &v1.Rational{Numerator: r.Num().String(), Denominator: r.Denom().String()}
}

// history lays the eras out from slot 0 and closes every era but the last at
// the next one's start. closeLast also bounds the last era `lastEpochs`
// epochs in; withEnds false leaves every end unset (the era-summary input).
func history(withEnds bool, closeLastAfter uint64, eras ...eraSpec) *v1.EraHistory {
	h := &v1.EraHistory{SystemStartUnix: 1506203091}
	var slot, epoch uint64
	elapsed := new(big.Rat)
	for i, e := range eras {
		slotLen := big.NewRat(e.slotLen[0], e.slotLen[1])
		es := &v1.EraSummary{
			Era: e.era, StartSlot: slot, StartEpoch: epoch, StartTime: ratOf(elapsed),
			EpochLength: e.epochLen, SlotLength: ratOf(slotLen), SafeZone: e.safeZone,
		}
		epochs := e.epochs
		last := i == len(eras)-1
		if last {
			epochs = closeLastAfter
		}
		if withEnds && (!last || closeLastAfter > 0) {
			endSlot, endEpoch := slot+epochs*e.epochLen, epoch+epochs
			endTime := new(big.Rat).Add(elapsed, new(big.Rat).Mul(slotLen, new(big.Rat).SetUint64(epochs*e.epochLen)))
			es.EndSlot, es.EndEpoch, es.EndTime = &endSlot, &endEpoch, ratOf(endTime)
		}
		slot += epochs * e.epochLen
		epoch += epochs
		elapsed.Add(elapsed, new(big.Rat).Mul(slotLen, new(big.Rat).SetUint64(epochs*e.epochLen)))
		h.Eras = append(h.Eras, es)
	}
	return h
}

func timeCases() []entry {
	mainnet := func(closeLast uint64) *v1.EraHistory {
		return history(true, closeLast, mainnetByron, mainnetShelley, mainnetAllegra, mainnetMary, mainnetAlonzo, mainnetBabbage, mainnetConway)
	}
	open := func(eras ...eraSpec) *v1.EraHistory { return history(false, 0, eras...) }
	// Preview: Shelley onward from epoch 0 with 86400-slot epochs.
	previewShelley := eraSpec{v1.Era_ERA_SHELLEY, 0, 86400, [2]int64{1, 1}, 25920}
	previewAllegra := eraSpec{v1.Era_ERA_ALLEGRA, 0, 86400, [2]int64{1, 1}, 25920}
	previewMary := eraSpec{v1.Era_ERA_MARY, 0, 86400, [2]int64{1, 1}, 25920}
	previewAlonzo := eraSpec{v1.Era_ERA_ALONZO, 3, 86400, [2]int64{1, 1}, 25920}
	previewBabbage := eraSpec{v1.Era_ERA_BABBAGE, 0, 86400, [2]int64{1, 1}, 25920}
	previewByron := eraSpec{v1.Era_ERA_BYRON, 0, 21600, [2]int64{20, 1}, 4320}
	previewConway := eraSpec{v1.Era_ERA_CONWAY, 0, 86400, [2]int64{1, 1}, 25920}
	preview := func(closeLast uint64) *v1.EraHistory {
		return history(true, closeLast, previewByron, previewShelley, previewAllegra, previewMary, previewAlonzo, previewBabbage, previewConway)
	}
	// Mainnet boundaries, from the era layout above.
	byronEnd := uint64(4492800)          // 208 * 21600
	shelleyEnd := byronEnd + 28*432000   // epoch 236
	allegraEnd := shelleyEnd + 15*432000 // epoch 251
	maryEnd := allegraEnd + 39*432000    // epoch 290
	alonzoEnd := maryEnd + 104*432000    // epoch 394
	babbageEnd := alonzoEnd + 113*432000 // epoch 507
	horizon := babbageEnd + 2*432000     // two epochs into Conway

	slotEpoch := func(name, why string, h *v1.EraHistory, slot uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TimeSlotToEpoch{TimeSlotToEpoch: &v1.TimeSlotToEpochRequest{EraHistory: h, Slot: slot}}}}
	}
	slotWall := func(name, why string, h *v1.EraHistory, slot uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TimeSlotToWallclock{TimeSlotToWallclock: &v1.TimeSlotToWallclockRequest{EraHistory: h, Slot: slot}}}}
	}
	wallSlot := func(name, why string, h *v1.EraHistory, n, d int64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TimeWallclockToSlot{TimeWallclockToSlot: &v1.TimeWallclockToSlotRequest{EraHistory: h, Time: ratOf(big.NewRat(n, d))}}}}
	}
	summary := func(name, why string, h *v1.EraHistory, tip uint64) entry {
		return entry{name, why, &v1.Call{Request: &v1.Call_TimeEraSummary{TimeEraSummary: &v1.TimeEraSummaryRequest{EraHistory: h, TipSlot: tip}}}}
	}
	params := func(name, why string, era v1.Era, epochLen uint64, slotLen string, k uint64, f [2]string) entry {
		sl := map[string]*v1.Rational{"1": rat("1", "1"), "20": rat("20", "1"), "1/2": rat("1", "2"), "1/3": rat("1", "3"), "1/1000": rat("1", "1000")}[slotLen]
		return entry{name, why, &v1.Call{Request: &v1.Call_TimeEraParams{TimeEraParams: &v1.TimeEraParamsRequest{
			Era: era, Globals: &v1.Globals{NetworkMagic: 764824073, EpochLength: epochLen, SlotLength: sl, SecurityParam: k, ActiveSlotCoeff: rat(f[0], f[1])},
		}}}}
	}
	cases := []entry{
		slotEpoch("time_slot_to_epoch_genesis", "slot 0 starts Byron epoch 0", mainnet(2), 0),
		slotEpoch("time_slot_to_epoch_byron_last", "last slot of the Byron era", mainnet(2), byronEnd-1),
		slotEpoch("time_slot_to_epoch_shelley_first", "first Shelley slot: epoch 208", mainnet(2), byronEnd),
		slotEpoch("time_slot_to_epoch_shelley_mid", "an arbitrary Shelley slot", mainnet(2), byronEnd+432000*5+12345),
		slotEpoch("time_slot_to_epoch_shelley_last", "last Shelley slot", mainnet(2), shelleyEnd-1),
		slotEpoch("time_slot_to_epoch_allegra_first", "first Allegra slot", mainnet(2), shelleyEnd),
		slotEpoch("time_slot_to_epoch_mary_first", "first Mary slot", mainnet(2), allegraEnd),
		slotEpoch("time_slot_to_epoch_alonzo_first", "first Alonzo slot", mainnet(2), maryEnd),
		slotEpoch("time_slot_to_epoch_babbage_first", "first Babbage slot", mainnet(2), alonzoEnd),
		slotEpoch("time_slot_to_epoch_conway_first", "first Conway slot", mainnet(2), babbageEnd),
		slotEpoch("time_slot_to_epoch_last_before_horizon", "the last slot inside the horizon", mainnet(2), horizon-1),
		slotEpoch("time_slot_to_epoch_at_horizon", "the horizon itself is outside every era: PastHorizon", mainnet(2), horizon),
		slotEpoch("time_slot_to_epoch_past_horizon", "far past the horizon: PastHorizon", mainnet(2), horizon+10_000_000),
		slotEpoch("time_slot_to_epoch_unbounded", "the last era has no end", mainnet(0), horizon+10_000_000),
		slotEpoch("time_slot_to_epoch_preview_first", "preview: the empty eras before Alonzo are skipped", preview(2), 0),
		slotEpoch("time_slot_to_epoch_preview_alonzo_last", "preview: last Alonzo slot (3 epochs of 86400)", preview(2), 3*86400-1),
		slotEpoch("time_slot_to_epoch_preview_babbage", "preview: first slot after Alonzo", preview(2), 3*86400),
		slotWall("time_slot_to_wallclock_genesis", "slot 0 is time 0", mainnet(2), 0),
		slotWall("time_slot_to_wallclock_byron_last", "20 s slots: the last Byron slot starts 20 s before Shelley", mainnet(2), byronEnd-1),
		slotWall("time_slot_to_wallclock_shelley_first", "slot length switches to 1 s", mainnet(2), byronEnd),
		slotWall("time_slot_to_wallclock_shelley_next", "one second after the boundary", mainnet(2), byronEnd+1),
		slotWall("time_slot_to_wallclock_conway_first", "first Conway slot", mainnet(2), babbageEnd),
		slotWall("time_slot_to_wallclock_last_before_horizon", "last slot inside the horizon", mainnet(2), horizon-1),
		slotWall("time_slot_to_wallclock_at_horizon", "the horizon slot: PastHorizon", mainnet(2), horizon),
		slotWall("time_slot_to_wallclock_preview_alonzo", "preview: first Alonzo slot", preview(2), 0),
		wallSlot("time_wallclock_to_slot_genesis", "time 0 is slot 0", mainnet(2), 0, 1),
		wallSlot("time_wallclock_to_slot_byron_mid", "10 s into the first 20 s slot", mainnet(2), 10, 1),
		wallSlot("time_wallclock_to_slot_byron_last", "19.5 s into the last Byron slot, 0.5 s from its end", mainnet(2), 179711999, 2),
		wallSlot("time_wallclock_to_slot_boundary", "exactly the Byron to Shelley boundary", mainnet(2), 89856000, 1),
		wallSlot("time_wallclock_to_slot_shelley_half", "half a second into the first Shelley slot", mainnet(2), 179712001, 2),
		wallSlot("time_wallclock_to_slot_nanosecond", "a nanosecond into slot 0", mainnet(2), 1, 1_000_000_000),
		wallSlot("time_wallclock_to_slot_before_boundary", "a nanosecond before the boundary", mainnet(2), 89855999999999999, 1_000_000_000),
		wallSlot("time_wallclock_to_slot_conway", "mid Conway", mainnet(2), 89856000+int64(shelleyEnd-byronEnd)+int64(babbageEnd-shelleyEnd)+100, 1),
		wallSlot("time_wallclock_to_slot_at_horizon", "the horizon time: PastHorizon", mainnet(2), 89856000+int64(horizon-byronEnd), 1),
		wallSlot("time_wallclock_to_slot_before_start", "a negative time is before every era: PastHorizon", mainnet(2), -1, 1),
		wallSlot("time_wallclock_to_slot_sub_nanosecond", "a third of a second is not a whole number of nanoseconds: unsupported", mainnet(2), 1, 3),
		summary("time_era_summary_tip_inside_era", "tip 20000000: the horizon is 9 epochs into the 236..251 era span", open(mainnetByron, mainnetShelley, mainnetAllegra), 20000000),
		summary("time_era_summary_tip_before_era", "tip below the era start: the safe zone runs from the era start", open(mainnetByron, mainnetShelley, mainnetAllegra), 0),
		summary("time_era_summary_target_on_boundary", "tip+1+safe zone lands exactly on an epoch boundary: no extra epoch", open(mainnetByron, mainnetShelley, mainnetAllegra), 16891199),
		summary("time_era_summary_target_past_boundary", "one slot past that: one more epoch", open(mainnetByron, mainnetShelley, mainnetAllegra), 16891200),
		summary("time_era_summary_tip_at_era_start", "tip at the era's first slot", open(mainnetByron, mainnetShelley, mainnetAllegra), 16588800),
		summary("time_era_summary_tip_in_epoch_start", "tip at the last slot of an epoch", open(mainnetByron, mainnetShelley, mainnetAllegra), 16588800+432000-1),
		summary("time_era_summary_single_era", "one era: only its safe-zone end", open(mainnetByron), 1000),
		summary("time_era_summary_byron_safe_zone", "Byron's 2k safe zone from a tip near the era's end", open(mainnetByron), 21500),
		summary("time_era_summary_full_mainnet", "every mainnet era, tip late in Conway", open(mainnetByron, mainnetShelley, mainnetAllegra, mainnetMary, mainnetAlonzo, mainnetBabbage, mainnetConway), 180_000_000),
		summary("time_era_summary_unbounded", "safe zone 0: the last era stays open", open(mainnetByron, eraSpec{v1.Era_ERA_SHELLEY, 0, 432000, [2]int64{1, 1}, 0}), 5_000_000),
		summary("time_era_summary_preview", "preview-shaped history with empty eras", history(false, 0, previewByron, previewShelley, previewAllegra, previewMary, previewAlonzo, previewBabbage, previewConway), 200_000),
		params("time_era_params_mainnet_shelley", "3k/f = 3 * 2160 * 20 = 129600", v1.Era_ERA_SHELLEY, 432000, "1", 2160, [2]string{"1", "20"}),
		params("time_era_params_mainnet_conway", "same globals in Conway", v1.Era_ERA_CONWAY, 432000, "1", 2160, [2]string{"1", "20"}),
		params("time_era_params_mainnet_dijkstra", "same globals in Dijkstra", v1.Era_ERA_DIJKSTRA, 432000, "1", 2160, [2]string{"1", "20"}),
		params("time_era_params_preview", "3 * 432 * 20 = 25920", v1.Era_ERA_BABBAGE, 86400, "1", 432, [2]string{"1", "20"}),
		params("time_era_params_byron", "Byron: epoch 10k, safe zone and genesis window 2k, 20 s slots", v1.Era_ERA_BYRON, 432000, "20", 2160, [2]string{"1", "20"}),
		params("time_era_params_rounds_up", "ceil(3 * 7 / (2/3)) = ceil(31.5) = 32", v1.Era_ERA_MARY, 100, "1", 7, [2]string{"2", "3"}),
		params("time_era_params_exact_quotient", "3 * 5 / (1/3) = 45 exactly", v1.Era_ERA_ALONZO, 100, "1/2", 5, [2]string{"1", "3"}),
		params("time_era_params_full_slots", "f = 1: the window is 3k", v1.Era_ERA_SHELLEY, 100, "1", 10, [2]string{"1", "1"}),
		params("time_era_params_millisecond_slot", "a 1 ms slot length", v1.Era_ERA_SHELLEY, 100, "1/1000", 10, [2]string{"1", "20"}),
		params("time_era_params_zero_k", "security parameter 0 is unsupported on both nodes", v1.Era_ERA_SHELLEY, 100, "1", 0, [2]string{"1", "20"}),
		params("time_era_params_zero_coefficient", "active slot coefficient 0 is unsupported on both nodes", v1.Era_ERA_SHELLEY, 100, "1", 5, [2]string{"0", "1"}),
		params("time_era_params_third_second_slot", "a slot length of 1/3 s is not whole milliseconds: unsupported", v1.Era_ERA_SHELLEY, 100, "1/3", 5, [2]string{"1", "20"}),
	}
	return cases
}
