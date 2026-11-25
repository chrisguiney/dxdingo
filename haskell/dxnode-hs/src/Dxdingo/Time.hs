{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Hard-fork era history, answered by ouroboros-consensus: 'summarize' for
-- the era summary, the 'Qry' queries run through 'interpretQuery' for the
-- slot, epoch and wallclock conversions, and 'shelleyEraParams' and
-- 'byronEraParams' for the era parameters.
--
-- dxnode-go keeps times as whole nanoseconds, so a slot length, bound time or
-- queried time that is not a whole number of nanoseconds, or that exceeds
-- 2^62 ns, is UNSUPPORTED on both nodes.
module Dxdingo.Time
  ( dispatch
  , implemented
  ) where

import Cardano.Chain.Common (BlockCount (..), TxFeePolicy (..), TxSizeLinear (..), mkKnownLovelace, rationalToLovelacePortion)
import Cardano.Chain.Genesis
  ( Config (..)
  , GenesisAvvmBalances (..)
  , GenesisData (..)
  , GenesisDelegation (..)
  , GenesisHash (..)
  , GenesisKeyHashes (..)
  , GenesisNonAvvmBalances (..)
  )
import Cardano.Chain.UTxO (defaultUTxOConfiguration)
import Cardano.Chain.Slotting (EpochNumber (..))
import Cardano.Chain.Update (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Crypto (ProtocolMagicId (..), RequiresNetworkMagic (..), hashRaw)
import Cardano.Ledger.BaseTypes (Network (Mainnet), mkActiveSlotCoeff, unsafeNonZero)
import Cardano.Ledger.Core (emptyPParams)
import Cardano.Ledger.Shelley.Genesis
  ( ShelleyGenesis (..)
  , emptyGenesisStaking
  , toNominalDiffTimeMicroWithRounding
  )
import Cardano.Ledger.Shelley.StabilityWindow (computeStabilityWindow)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (RelativeTime (..), getSlotLength, mkSlotLength)
import qualified Data.Map.Strict as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import Data.SOP.Counting (AtMost (..), Exactly (..))
import Data.Foldable (toList)
import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), NominalDiffTime)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Word (Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..))
import Ouroboros.Consensus.Byron.Ledger.Ledger (byronEraParams)
import Ouroboros.Consensus.HardFork.History
  ( Bound (..)
  , EraEnd (..)
  , EraParams (..)
  , EraSummary (..)
  , PastHorizonException
  , Qry
  , SafeZone (..)
  , Shape (..)
  , Summary (..)
  , Transitions (..)
  , initBound
  , interpretQuery
  , mkInterpreter
  , slotToEpoch
  , slotToWallclock
  , summarize
  , wallclockToSlot
  )
import Ouroboros.Consensus.HardFork.History.EraParams (pattern NoPerasEnabled)
import Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyEraParams)
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.State as S
import Proto.Dxdingo.V1.Time
import qualified Proto.Dxdingo.V1.Types as P

----------------------------------------------------------------------
-- Domain

maxNs :: Integer
maxNs = 2 ^ (62 :: Int)

-- | A rational number of seconds as whole nanoseconds.
nsArg :: Text -> Maybe P.Rational -> Result Integer
nsArg name r = do
  x <- (* 1000000000) <$> rationalArg name r
  if denominator x /= 1
    then unsupported (name <> " is not a whole number of nanoseconds")
    else
      if abs (numerator x) > maxNs
        then unsupported (name <> " does not fit a Duration")
        else pure (numerator x)

data TB = TB {tbSlot :: Word64, tbEpoch :: Word64, tbNs :: Integer}

data TEra = TEra
  { teEra :: P.Era
  , teStart :: TB
  , teEnd :: Maybe TB
  , teEpochLen :: Word64
  , teSlotNs :: Integer
  , teSafe :: Word64
  }

-- | Decode an 'S.EraHistory' and check what 'invariantSummary' checks: the
-- eras line up, and every bounded era satisfies end slot = start slot +
-- epochs * epoch length and end time = start time + slots * slot length.
-- @withEnds@ false is the era-summary input, whose end bounds must be unset.
parseHistory :: Bool -> S.EraHistory -> Result [TEra]
parseHistory withEnds h = do
  let es = h ^. #eras
  if null es then badRequest "era_history has no eras" else pure ()
  eras <- traverse (uncurry one) (zip [0 :: Int ..] es)
  let first = teStart (head eras)
  if tbSlot first /= 0 || tbEpoch first /= 0 || tbNs first /= 0
    then unsupported "the first era must start at slot 0, epoch 0, time 0"
    else pure ()
  link (0 :: Int) eras
 where
  n = length (h ^. #eras)
  one i e = do
    if e ^. #epochLength == 0 then unsupported (idx i "epoch_length is zero") else pure ()
    slotNs <- nsArg "slot_length" (e ^. #maybe'slotLength)
    if slotNs <= 0 then unsupported (idx i "slot_length must be positive") else pure ()
    startNs <- nsArg "start_time" (e ^. #maybe'startTime)
    let base =
          TEra
            { teEra = e ^. #era
            , teStart = TB (e ^. #startSlot) (e ^. #startEpoch) startNs
            , teEnd = Nothing
            , teEpochLen = e ^. #epochLength
            , teSlotNs = slotNs
            , teSafe = e ^. #safeZone
            }
        hasEnd = any id [present (e ^. #maybe'endSlot), present (e ^. #maybe'endEpoch), present (e ^. #maybe'endTime)]
    case (hasEnd, withEnds) of
      (True, False) -> unsupported (idx i "end bounds must be unset")
      (True, True) -> case (e ^. #maybe'endSlot, e ^. #maybe'endEpoch, e ^. #maybe'endTime) of
        (Just s, Just ep, Just _) -> do
          endNs <- nsArg "end_time" (e ^. #maybe'endTime)
          pure base {teEnd = Just (TB s ep endNs)}
        _ -> badRequest (idx i "end bounds are partly set")
      (False, True) | i /= n - 1 -> unsupported (idx i "only the last era may be unbounded")
      _ -> pure base
  present = maybe False (const True)
  idx i m = "era " <> T.pack (show i) <> ": " <> m
  link _ [] = pure []
  link i (e : rest) = do
    e' <- case rest of
      next : _ -> case teEnd e of
        Just end
          | sameBound end (teStart next) -> pure e
          | otherwise -> unsupported (idx i "does not end where the next era starts")
        Nothing
          | not withEnds -> pure e {teEnd = Just (teStart next)}
          | otherwise -> unsupported (idx i "does not end where the next era starts")
      [] -> pure e
    case teEnd e' of
      Just _ -> checkEraEnd i e'
      Nothing -> pure ()
    (e' :) <$> link (i + 1) rest
  sameBound a b = tbSlot a == tbSlot b && tbEpoch a == tbEpoch b && tbNs a == tbNs b

-- | INV-1b and INV-2b for one bounded era.
checkEraEnd :: Int -> TEra -> Result ()
checkEraEnd i e = case teEnd e of
  Nothing -> pure ()
  Just end -> do
    let st = teStart e
        label m = "era " <> T.pack (show i) <> ": " <> m
    if tbEpoch end < tbEpoch st
      then unsupported (label "ends before it starts")
      else pure ()
    let slots = toInteger (tbEpoch end - tbEpoch st) * toInteger (teEpochLen e)
    if toInteger (tbSlot st) + slots /= toInteger (tbSlot end)
      then unsupported (label "end slot breaks end = start + epochs * epoch_length")
      else pure ()
    let span' = toInteger (tbSlot end) - toInteger (tbSlot st)
    if tbNs st + span' * teSlotNs e /= tbNs end
      then unsupported (label "end time breaks end = start + slots * slot_length")
      else pure ()

----------------------------------------------------------------------
-- Reference types

nominal :: Integer -> NominalDiffTime
nominal ns = fromRational (ns % 1000000000)

toRat :: NominalDiffTime -> Rational
toRat = toRational

eraParamsOf :: TEra -> EraParams
eraParamsOf e =
  EraParams
    { eraEpochSize = EpochSize (teEpochLen e)
    , eraSlotLength = mkSlotLength (nominal (teSlotNs e))
    , eraSafeZone = if teSafe e == 0 then UnsafeIndefiniteSafeZone else StandardSafeZone (teSafe e)
    , eraGenesisWin = GenesisWindow (teSafe e)
    , eraPerasRoundLength = NoPerasEnabled
    }

boundOf :: TB -> Bound
boundOf b =
  Bound
    { boundTime = RelativeTime (nominal (tbNs b))
    , boundSlot = SlotNo (tbSlot b)
    , boundEpoch = EpochNo (tbEpoch b)
    , boundPerasRound = boundPerasRound initBound
    }

summaryOf :: TEra -> EraSummary
summaryOf e =
  EraSummary
    { eraStart = (boundOf (teStart e)) {boundPerasRound = NoPerasEnabled}
    , eraEnd = maybe EraUnbounded (EraEnd . (\b -> (boundOf b) {boundPerasRound = NoPerasEnabled})) (teEnd e)
    , eraParams = eraParamsOf e
    }

-- | A non-empty list as the type-indexed list the reference's 'Summary' needs.
data SomeNE a where
  SomeNE :: NonEmpty xs a -> SomeNE a

toNE :: forall a. [a] -> Maybe (SomeNE a)
toNE [] = Nothing
toNE [x] = Just (SomeNE (NonEmptyOne x :: NonEmpty '[()] a))
toNE (x : xs) = do
  SomeNE (r :: NonEmpty ys a) <- toNE xs
  Just (SomeNE (NonEmptyCons x r :: NonEmpty (() ': ys) a))

-- | 'interpretQuery' over a summary built from the history.
runQry :: [TEra] -> Qry a -> Either PastHorizonException a
runQry eras q = case toNE (map summaryOf eras) of
  Just (SomeNE ne) -> interpretQuery (mkInterpreter (Summary ne)) q
  Nothing -> error "runQry: no eras"

horizonRejection :: P.Rejection
horizonRejection =
  defMessage
    & #failures
      .~ [defMessage & #rule .~ "HardFork" & #name .~ "PastHorizon" & #detail .~ "past the horizon"]

----------------------------------------------------------------------
-- Queries

slotToEpochH :: TimeSlotToEpochRequest -> Result TimeSlotToEpochResponse
slotToEpochH req = do
  eras <- parseHistory True (req ^. #eraHistory)
  pure $ case runQry eras (slotToEpoch (SlotNo (req ^. #slot))) of
    Left _ -> defMessage & #rejection .~ horizonRejection
    Right (EpochNo ep, inEpoch, left) ->
      defMessage & #epoch .~ ep & #slotInEpoch .~ inEpoch & #slotsLeft .~ left

slotToWallclockH :: TimeSlotToWallclockRequest -> Result TimeSlotToWallclockResponse
slotToWallclockH req = do
  eras <- parseHistory True (req ^. #eraHistory)
  pure $ case runQry eras (slotToWallclock (SlotNo (req ^. #slot))) of
    Left _ -> defMessage & #rejection .~ horizonRejection
    Right (RelativeTime t, sl) ->
      defMessage & #time .~ rationalProto (toRat t) & #slotLength .~ rationalProto (toRat (getSlotLength sl))

wallclockToSlotH :: TimeWallclockToSlotRequest -> Result TimeWallclockToSlotResponse
wallclockToSlotH req = do
  eras <- parseHistory True (req ^. #eraHistory)
  ns <- nsArg "time" (req ^. #maybe'time)
  pure $ case runQry eras (wallclockToSlot (RelativeTime (nominal ns))) of
    Left _ -> defMessage & #rejection .~ horizonRejection
    Right (SlotNo s, spent, left) ->
      defMessage & #slot .~ s & #timeSpent .~ rationalProto (toRat spent) & #timeLeft .~ rationalProto (toRat left)

-- | 'summarize' with every era after the first bounded by the next era's
-- start epoch, and the tip anchoring the current era's safe zone.
eraSummaryH :: TimeEraSummaryRequest -> Result TimeEraSummaryResponse
eraSummaryH req = do
  eras <- parseHistory False (req ^. #eraHistory)
  let params = map eraParamsOf eras
      transitions = map (EpochNo . tbEpoch . teStart) (tail eras)
  built <- case mkShape params transitions of
    Nothing -> badRequest "no eras"
    Just (SomeShape ex at) ->
      pure (toList (getSummary (summarize (At (SlotNo (req ^. #tipSlot))) (Shape ex) (Transitions at))))
  -- The reference wraps Word64 slot arithmetic; a wrapped end breaks the
  -- invariants it states and is outside the domain.
  mapM_ (\(i, e, b) -> checkEraEnd i e {teEnd = fromEnd (eraEnd b)}) (zip3 [0 ..] eras built)
  pure $
    defMessage
      & #eraHistory
        .~ ( defMessage
              & #systemStartUnix .~ (req ^. #eraHistory . #systemStartUnix)
              & #eras .~ zipWith summaryProto eras built
           )
 where
  fromEnd = \case
    EraUnbounded -> Nothing
    EraEnd b -> Just (tbOf b)

tbOf :: Bound -> TB
tbOf b =
  TB
    (unSlotNo (boundSlot b))
    (unEpochNo (boundEpoch b))
    (floor (toRat (unRel (boundTime b)) * 1000000000))
 where
  unRel (RelativeTime t) = t

summaryProto :: TEra -> EraSummary -> S.EraSummary
summaryProto e s =
  base & case eraEnd s of
    EraUnbounded -> id
    EraEnd b ->
      let tb = tbOf b
       in (#endSlot .~ tbSlot tb) . (#endEpoch .~ tbEpoch tb) . (#endTime .~ nsProto (tbNs tb))
 where
  st = tbOf (eraStart s)
  p = eraParams s
  base =
    defMessage
      & #era .~ teEra e
      & #startSlot .~ tbSlot st
      & #startEpoch .~ tbEpoch st
      & #startTime .~ nsProto (tbNs st)
      & #epochLength .~ unEpochSize (eraEpochSize p)
      & #slotLength .~ rationalProto (toRat (getSlotLength (eraSlotLength p)))
      & #safeZone
        .~ ( case eraSafeZone p of
              StandardSafeZone z -> z
              UnsafeIndefiniteSafeZone -> 0
           )

nsProto :: Integer -> P.Rational
nsProto ns = rationalProto (ns % 1000000000)

data SomeShape where
  SomeShape :: Exactly (x ': xs) EraParams -> AtMost xs EpochNo -> SomeShape

mkShape :: [EraParams] -> [EpochNo] -> Maybe SomeShape
mkShape [p] [] = Just (SomeShape (ExactlyCons p ExactlyNil :: Exactly '[()] EraParams) AtMostNil)
mkShape (p : ps) (e : es) = do
  SomeShape ex at <- mkShape ps es
  Just (SomeShape (ExactlyCons p ex) (AtMostCons e at))
mkShape _ _ = Nothing

----------------------------------------------------------------------
-- Era parameters

-- | 'shelleyEraParams' on a Shelley genesis built from the request's
-- globals, or 'byronEraParams' on a Byron config built from the same.
-- dxnode-go reads slot lengths in whole milliseconds, so the slot length must
-- be whole milliseconds.
eraParamsH :: TimeEraParamsRequest -> Result TimeEraParamsResponse
eraParamsH req = do
  let e = req ^. #era
  if e `elem` [P.ERA_BYRON, P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO, P.ERA_BABBAGE, P.ERA_CONWAY, P.ERA_DIJKSTRA]
    then pure ()
    else unsupported ("no era parameters for " <> T.pack (show e))
  g <- maybe (badRequest "missing globals") pure (req ^. #maybe'globals)
  slotLen <- rationalArg "slot_length" (g ^. #maybe'slotLength)
  let ms = slotLen * 1000
  if slotLen <= 0 || denominator ms /= 1 || slotLen > 1000000
    then unsupported "slot_length must be positive whole milliseconds"
    else pure ()
  let k = g ^. #securityParam
  if k == 0 || k > 2 ^ (40 :: Int) then unsupported "security_param must be in 1..2^40" else pure ()
  f <- rationalArg "active_slot_coeff" (g ^. #maybe'activeSlotCoeff)
  if e == P.ERA_BYRON
    then do
      let genesis = byronConfig (numerator ms) k
          p = byronEraParams genesis
      pure (paramsProto p (safeOf p))
    else do
      if g ^. #epochLength == 0 || g ^. #epochLength > 2 ^ (63 :: Int) - 1
        then unsupported "epoch_length must be in 1..2^63-1"
        else pure ()
      fi <- positiveUnitIntervalArg "active_slot_coeff" (g ^. #maybe'activeSlotCoeff)
      if numerator f >= 2 ^ (62 :: Int) || denominator f >= 2 ^ (62 :: Int)
        then unsupported "active_slot_coeff does not fit an int64 ratio"
        else pure ()
      if ceiling (3 * toRational k / f) > (toInteger (maxBound :: Word64))
        then unsupported "stability window overflows Word64"
        else pure ()
      let genesis =
            ShelleyGenesis
              { sgSystemStart = UTCTime (ModifiedJulianDay 0) 0
              , sgNetworkMagic = 0
              , sgNetworkId = Mainnet
              , sgActiveSlotsCoeff = fi
              , sgSecurityParam = unsafeNonZero k
              , sgEpochLength = EpochSize (g ^. #epochLength)
              , sgSlotsPerKESPeriod = 129600
              , sgMaxKESEvolutions = 62
              , sgSlotLength = toNominalDiffTimeMicroWithRounding (fromRational slotLen)
              , sgUpdateQuorum = 5
              , sgMaxLovelaceSupply = 45000000000000000
              , sgProtocolParams = emptyPParams
              , sgGenDelegs = Map.empty
              , sgInitialFunds = mempty
              , sgStaking = emptyGenesisStaking
              , sgExtraConfig = SNothing
              }
          p = shelleyEraParams genesis
      pure (paramsProto p (computeStabilityWindow k (mkActiveSlotCoeff fi)))
 where
  safeOf p = case eraSafeZone p of
    StandardSafeZone z -> z
    UnsafeIndefiniteSafeZone -> 0

paramsProto :: EraParams -> Word64 -> TimeEraParamsResponse
paramsProto p stab =
  defMessage
    & #epochLength .~ unEpochSize (eraEpochSize p)
    & #slotLength .~ rationalProto (toRat (getSlotLength (eraSlotLength p)))
    & #safeZone
      .~ ( case eraSafeZone p of
            StandardSafeZone z -> z
            UnsafeIndefiniteSafeZone -> 0
         )
    & #genesisWindow .~ unGenesisWindow (eraGenesisWin p)
    & #stabilityWindow .~ stab

-- | A Byron config carrying the slot duration (milliseconds) and k; the rest
-- is filler 'byronEraParams' does not read.
byronConfig :: Integer -> Word64 -> Config
byronConfig slotMs k =
  Config
    { configGenesisData =
        GenesisData
          { gdGenesisKeyHashes = GenesisKeyHashes Set.empty
          , gdHeavyDelegation = UnsafeGenesisDelegation mempty
          , gdStartTime = UTCTime (ModifiedJulianDay 0) 0
          , gdNonAvvmBalances = GenesisNonAvvmBalances mempty
          , gdProtocolParameters =
              ProtocolParameters
                { ppScriptVersion = 0
                , ppSlotDuration = fromInteger slotMs
                , ppMaxBlockSize = 2000000
                , ppMaxHeaderSize = 2000000
                , ppMaxTxSize = 8192
                , ppMaxProposalSize = 700
                , ppMpcThd = rationalToLovelacePortion 0.01
                , ppHeavyDelThd = rationalToLovelacePortion 0.005
                , ppUpdateVoteThd = rationalToLovelacePortion 0.001
                , ppUpdateProposalThd = rationalToLovelacePortion 0.1
                , ppUpdateProposalTTL = 10
                , ppSoftforkRule =
                    SoftforkRule
                      { srInitThd = rationalToLovelacePortion 0.9
                      , srMinThd = rationalToLovelacePortion 0.6
                      , srThdDecrement = rationalToLovelacePortion 0.05
                      }
                , ppTxFeePolicy = TxFeePolicyTxSizeLinear (TxSizeLinear (mkKnownLovelace @155381) 43.946)
                , ppUnlockStakeEpoch = EpochNumber maxBound
                }
          , gdK = BlockCount k
          , gdProtocolMagicId = ProtocolMagicId 0
          , gdAvvmDistr = GenesisAvvmBalances mempty
          }
    , configGenesisHash = GenesisHash (hashRaw mempty)
    , configReqNetMagic = RequiresNoMagic
    , configUTxOConfiguration = defaultUTxOConfiguration
    }

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'TimeEraParams q -> Just (respond #timeEraParams (eraParamsH q))
  Call'TimeEraSummary q -> Just (respond #timeEraSummary (eraSummaryH q))
  Call'TimeSlotToEpoch q -> Just (respond #timeSlotToEpoch (slotToEpochH q))
  Call'TimeSlotToWallclock q -> Just (respond #timeSlotToWallclock (slotToWallclockH q))
  Call'TimeWallclockToSlot q -> Just (respond #timeWallclockToSlot (wallclockToSlotH q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "time_era_params"
  , "time_era_summary"
  , "time_slot_to_epoch"
  , "time_slot_to_wallclock"
  , "time_wallclock_to_slot"
  ]
