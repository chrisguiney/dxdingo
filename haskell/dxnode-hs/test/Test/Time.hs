{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Time (tests) where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..), Reply)
import Proto.Dxdingo.V1.State (EraHistory, EraSummary)
import Proto.Dxdingo.V1.Types (Era (..), Rejection)
import Test.Util

-- | Hand-derived from the hard-fork history definitions. Mainnet: Byron
-- epochs are 21600 slots of 20 s; Shelley is epoch 208 at slot 4492800
-- (208 * 21600) and 89856000 s (4492800 * 20); Allegra is epoch 236 at slot
-- 16588800 (4492800 + 28 * 432000) and 101952000 s (89856000 + 12096000).
-- The same cases run against dxnode-go.
tests :: [Check]
tests =
  [ ("slot to epoch: genesis", s2e 0 == (0, 0, 21600))
  , ("slot to epoch: last Byron slot", s2e 4492799 == (207, 21599, 1))
  , ("slot to epoch: first Shelley slot", s2e 4492800 == (208, 0, 432000))
  , ("slot to epoch: mid Shelley epoch", s2e (4492800 + 432000 + 7) == (209, 7, 431993))
  , ("slot to epoch: last Shelley slot", s2e 16588799 == (235, 431999, 1))
  , ("slot to epoch: first Allegra slot", s2e 16588800 == (236, 0, 432000))
  , ("slot to epoch: deep in the unbounded era", s2e (16588800 + 10 * 432000 + 5) == (246, 5, 431995))
  , ("past horizon: slot at the horizon", pastHorizon (slotEpochReply True 17020800 ^. #timeSlotToEpoch . #maybe'rejection))
  , ("past horizon: last slot before the horizon", s2eWith True 17020799 == (236, 431999, 1))
  , ("past horizon: slot_to_wallclock at the horizon", pastHorizon (call (#timeSlotToWallclock .~ (defMessage & #eraHistory .~ mainnet True & #slot .~ 17020800)) ^. #timeSlotToWallclock . #maybe'rejection))
  , ("past horizon: time at the horizon", pastHorizon (w2s True ("102384000", "1") ^. #timeWallclockToSlot . #maybe'rejection))
  , ("past horizon: time past the horizon", pastHorizon (w2s True ("102384001", "1") ^. #timeWallclockToSlot . #maybe'rejection))
  , ("past horizon: time before system start", pastHorizon (w2s True ("-1", "1") ^. #timeWallclockToSlot . #maybe'rejection))
  , ("past horizon: a nanosecond before the origin", pastHorizon (w2s True ("-1", "1000000000") ^. #timeWallclockToSlot . #maybe'rejection))
  , ("slot to wallclock: genesis", s2w 0 == (("0", "1"), ("20", "1")))
  , ("slot to wallclock: last Byron slot", s2w 4492799 == (("89855980", "1"), ("20", "1")))
  , ("slot to wallclock: first Shelley slot", s2w 4492800 == (("89856000", "1"), ("1", "1")))
  , ("slot to wallclock: first Allegra slot", s2w 16588800 == (("101952000", "1"), ("1", "1")))
  , ("wallclock to slot: genesis", w2sT ("0", "1") == (0, ("0", "1"), ("20", "1")))
  , ("wallclock to slot: inside the last Byron slot", w2sT ("179711999", "2") == (4492799, ("39", "2"), ("1", "2")))
  , ("wallclock to slot: exactly at the boundary", w2sT ("89856000", "1") == (4492800, ("0", "1"), ("1", "1")))
  , ("wallclock to slot: half a second into Shelley", w2sT ("179712001", "2") == (4492800, ("1", "2"), ("1", "2")))
  , ("wallclock to slot: one nanosecond into Byron", w2sT ("1", "1000000000") == (0, ("1", "1000000000"), ("19999999999", "1000000000")))
  , ("wallclock to slot: one nanosecond before the boundary", w2sT ("89855999999999999", "1000000000") == (4492799, ("19999999999", "1000000000"), ("1", "1000000000")))
  , ("era summary: tip inside the era", summary 20000000 == (20476800, 245, ("105840000", "1")))
  , ("era summary: tip before the era", summary 0 == (17020800, 237, ("102384000", "1")))
  , ("era summary: target on an epoch boundary", summary 16891199 == (17020800, 237, ("102384000", "1")))
  , ("era summary: target one slot past a boundary", summary 16891200 == (17452800, 238, ("102816000", "1")))
  , ("era summary: past eras close at the next start", pastEnds 20000000)
  , ("era summary: safe zone 0 is unbounded", unbounded)
  , ("era params: mainnet Shelley", params ERA_SHELLEY 432000 ("1", "1") 2160 ("1", "20") == (432000, ("1", "1"), 129600, 129600, 129600))
  , ("era params: preview Conway", params ERA_CONWAY 86400 ("1", "1") 432 ("1", "20") == (86400, ("1", "1"), 25920, 25920, 25920))
  , ("era params: exact quotient", params ERA_BABBAGE 100 ("1", "2") 5 ("1", "3") == (100, ("1", "2"), 45, 45, 45))
  , ("era params: quotient rounds up", params ERA_MARY 100 ("1", "1") 7 ("2", "3") == (100, ("1", "1"), 32, 32, 32))
  , ("era params: mainnet Byron", params ERA_BYRON 432000 ("20", "1") 2160 ("1", "20") == (21600, ("20", "1"), 4320, 4320, 4320))
  , ("domain: eras that do not line up", code (call (#timeSlotToEpoch .~ (defMessage & #eraHistory .~ misaligned & #slot .~ 1))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: slot length not whole nanoseconds", code (call (#timeSlotToEpoch .~ (defMessage & #eraHistory .~ thirdSlot & #slot .~ 1))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: time not whole nanoseconds", code (w2s False ("1", "3")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: zero security parameter", code (paramsCall ERA_SHELLEY 100 ("1", "1") 0 ("1", "20")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: zero active slot coefficient", code (paramsCall ERA_SHELLEY 100 ("1", "1") 5 ("0", "1")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: slot length not whole milliseconds", code (paramsCall ERA_SHELLEY 100 ("1", "3") 5 ("1", "20")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("domain: unspecified era", code (paramsCall ERA_UNSPECIFIED 100 ("1", "1") 5 ("1", "20")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  ]
  where
    era e sl ep sz st t slot epoch el safe =
      (defMessage :: EraSummary)
        & #era .~ e & #startSlot .~ slot & #startEpoch .~ epoch & #startTime .~ rat t
        & #epochLength .~ el & #slotLength .~ rat sl & #safeZone .~ safe
        & ending ep sz st
    ending ep sz st x = case (ep, sz, st) of
      (Just e, Just s, Just t) -> x & #endSlot .~ s & #endEpoch .~ e & #endTime .~ rat t
      _ -> x
    byron = era ERA_BYRON ("20", "1") (Just 208) (Just 4492800) (Just ("89856000", "1")) ("0", "1") 0 0 21600 4320
    shelley = era ERA_SHELLEY ("1", "1") (Just 236) (Just 16588800) (Just ("101952000", "1")) ("89856000", "1") 4492800 208 432000 129600
    allegra close =
      era ERA_ALLEGRA ("1", "1") (if close then Just 237 else Nothing) (if close then Just 17020800 else Nothing)
        (if close then Just ("102384000", "1") else Nothing) ("101952000", "1") 16588800 236 432000 129600
    mainnet close = (defMessage :: EraHistory) & #eras .~ [byron, shelley, allegra close] & #systemStartUnix .~ 1506203091
    misaligned = mainnet False & #eras .~ [byron, shelley & #startSlot .~ 4492801, allegra False]
    thirdSlot = mainnet False & #eras .~ [byron & #slotLength .~ rat ("1", "3"), shelley, allegra False]
    slotEpochReply close slot = call (#timeSlotToEpoch .~ (defMessage & #eraHistory .~ mainnet close & #slot .~ slot))
    s2eWith close slot =
      let r = slotEpochReply close slot ^. #timeSlotToEpoch in (r ^. #epoch, r ^. #slotInEpoch, r ^. #slotsLeft)
    s2e = s2eWith False
    s2w slot =
      let r = call (#timeSlotToWallclock .~ (defMessage & #eraHistory .~ mainnet False & #slot .~ slot)) ^. #timeSlotToWallclock
       in (ratP (r ^. #time), ratP (r ^. #slotLength))
    w2s close t = call (#timeWallclockToSlot .~ (defMessage & #eraHistory .~ mainnet close & #time .~ rat t))
    w2sT t =
      let r = w2s False t ^. #timeWallclockToSlot in (r ^. #slot, ratP (r ^. #timeSpent), ratP (r ^. #timeLeft))
    ratP r = (r ^. #numerator, r ^. #denominator)
    pastHorizon :: Maybe Rejection -> Bool
    pastHorizon = maybe False (\r -> map (^. #name) (r ^. #failures) == ["PastHorizon"])
    open = mainnet False & #eras .~ [byron' , shelley', allegra False]
    byron' = byron & #maybe'endSlot .~ Nothing & #maybe'endEpoch .~ Nothing & #maybe'endTime .~ Nothing
    shelley' = shelley & #maybe'endSlot .~ Nothing & #maybe'endEpoch .~ Nothing & #maybe'endTime .~ Nothing
    summaryReply h tip = call (#timeEraSummary .~ (defMessage & #eraHistory .~ h & #tipSlot .~ tip)) ^. #timeEraSummary
    summary tip =
      let cur = last' (summaryReply open tip ^. #eraHistory . #eras)
       in (cur ^. #endSlot, cur ^. #endEpoch, ratP (cur ^. #endTime))
    last' = foldr1 (\_ x -> x)
    pastEnds tip =
      let es = summaryReply open tip ^. #eraHistory . #eras
       in map (\e -> (e ^. #endSlot, e ^. #endEpoch, ratP (e ^. #endTime))) (take 2 es)
            == [(4492800, 208, ("89856000", "1")), (16588800, 236, ("101952000", "1"))]
    unbounded =
      let h = open & #eras .~ [byron', shelley', allegra False & #safeZone .~ 0]
       in (last' (summaryReply h 16588900 ^. #eraHistory . #eras) ^. #maybe'endSlot) == Nothing
    paramsCall e el sl k f =
      call (#timeEraParams .~ (defMessage & #era .~ e
        & #globals .~ (defMessage & #epochLength .~ el & #slotLength .~ rat sl & #securityParam .~ k & #activeSlotCoeff .~ rat f)))
    params e el sl k f =
      let r = paramsCall e el sl k f ^. #timeEraParams
       in (r ^. #epochLength, ratP (r ^. #slotLength), r ^. #safeZone, r ^. #genesisWindow, r ^. #stabilityWindow)
    code :: Reply -> HarnessErrorCode
    code r = r ^. #error . #code
