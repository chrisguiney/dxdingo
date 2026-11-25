{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Argument decoding and failure naming shared by the composed ledger rules
-- (PPUP, GOV, LEDGER, BBODY). A copy of the builders in "Dxdingo.Rule", which
-- cannot be imported here because it delegates to the modules that use these.
module Dxdingo.Rule.Ledger.Base where


import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure, AlonzoUtxowPredFailure, UtxosEnv (UtxosEnv))
import Cardano.Ledger.Alonzo.Core
  ( AlonzoEraPParams
  , AlonzoEraTxBody (collateralInputsTxBodyL)
  , CoinPerWord (..)
  , ppCoinsPerUTxOWordL
  , ppCollateralPercentageL
  , ppCostModelsL
  , ppDL
  , ppMaxBlockExUnitsL
  , ppMaxCollateralInputsL
  , ppMaxTxExUnitsL
  , ppMaxValSizeL
  , ppPricesL
  )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (isValidTxL), pattern IsValid)
import Cardano.Ledger.Core
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Core (CoinPerByte (..), ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes
  ( BoundedRational (boundRational)
  , EpochInterval (..)
  , EpochSize (..)
  , Globals (..)
  , Network (..)
  , ProtVer (..)
  , ShelleyBase
  , mkActiveSlotCoeff
  , mkVersion
  , nonZero
  )
import Cardano.Ledger.Binary (decCBOR, decodeFull', decodeFullAnnotator)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (ConwayBbodyPredFailure, ConwayCertPredFailure, ConwayCertsPredFailure, ConwayDelegPredFailure, ConwayGovCertPredFailure, ConwayGovPredFailure, ConwayLedgerPredFailure, ConwayUtxoPredFailure, ConwayUtxosPredFailure, ConwayUtxowPredFailure)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Plutus.CostModels (mkCostModel, mkCostModels)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (mkStAnnTx))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersPredFailure, ShelleyPoolPredFailure, ShelleyPpupPredFailure, ShelleyUtxoPredFailure, ShelleyUtxowPredFailure, UtxoEnv (..))
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..), TxIn, mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Cardano.Slotting.Time (RelativeTime (..), SystemStart (..), mkSlotLength)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition.Extended (STS (..), TRC (..), applySTS)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.List (nub, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.SOP.Counting (exactlyOne, exactlyTwo)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (C1, Constructor (conName), D1, Generic, K1 (..), M1 (..), Rep, S1, from, (:+:) (..))
import Data.Word (Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.HardFork.History
  ( Bound (..)
  , EraEnd (..)
  , EraParams (..)
  , EraSummary (..)
  , SafeZone (..)
  , defaultEraParams
  , initBound
  , summaryToEpochInfo
  , summaryWithExactly
  )
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Rule
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P


-- ---------------------------------------------------------------------
-- Inputs

data RuleEra = RShelley | RAllegra | RMary | RAlonzo | RBabbage | RConway deriving (Eq, Show)

eraArg :: P.Era -> Result RuleEra
eraArg = \case
  P.ERA_SHELLEY -> pure RShelley
  P.ERA_ALLEGRA -> pure RAllegra
  P.ERA_MARY -> pure RMary
  P.ERA_ALONZO -> pure RAlonzo
  P.ERA_BABBAGE -> pure RBabbage
  P.ERA_CONWAY -> pure RConway
  e -> unsupported ("transaction rules are modelled for Shelley through Conway, not " <> T.pack (show e))

-- | Globals from the request. Fields the rules in scope never read still get
-- a value so the record is total.
globalsArg :: S.Globals -> Word -> Result Globals
globalsArg g netId = do
  slotLen <- rationalArg "slot_length" (g ^. #maybe'slotLength)
  if slotLen <= 0 then unsupported "slot_length must be positive" else pure ()
  fRat <- rationalArg "active_slot_coeff" (g ^. #maybe'activeSlotCoeff)
  f <- positiveUnitIntervalArg "active_slot_coeff" (g ^. #maybe'activeSlotCoeff)
  k <- maybe (unsupported "security_param must be nonzero") pure (nonZero (g ^. #securityParam))
  if g ^. #epochLength == 0 then unsupported "epoch_length must be nonzero" else pure ()
  let kk = fromIntegral (g ^. #securityParam) :: Rational
      window n = ceiling (n * kk / fRat) :: Word64
  pure
    Globals
      { epochInfo = fixedEpochInfo (EpochSize (g ^. #epochLength)) (mkSlotLength (fromRational slotLen))
      , slotsPerKESPeriod = g ^. #slotsPerKesPeriod
      , stabilityWindow = window 3
      , randomnessStabilisationWindow = window 4
      , securityParameter = k
      , maxKESEvo = g ^. #maxKesEvolutions
      , quorum = g ^. #quorum
      , maxLovelaceSupply = g ^. #maxLovelaceSupply
      , activeSlotCoeff = mkActiveSlotCoeff f
      , networkId = if netId == 0 then Testnet else Mainnet
      , systemStart = SystemStart (posixSecondsToUTCTime (fromIntegral (g ^. #systemStartUnix)))
      }

-- | Parameters every era from Shelley on has.
basePParams :: forall era. EraPParams era => S.ProtocolParameters -> Result (PParams era)
basePParams p = do
  a0 <- boundedArg "a0" (p ^. #maybe'a0)
  rho <- boundedArg "rho" (p ^. #maybe'rho)
  tau <- boundedArg "tau" (p ^. #maybe'tau)
  let pv = p ^. #protocolVersion
  v <- maybe (unsupported "protocol major version") pure (mkVersion (pv ^. #major))
  pure $
    emptyPParams
      & ppMinFeeAL .~ Coin (toInteger (p ^. #minFeeA))
      & ppMinFeeBL .~ Coin (toInteger (p ^. #minFeeB))
      & ppMaxBBSizeL .~ (p ^. #maxBlockBodySize)
      & ppMaxTxSizeL .~ (p ^. #maxTxSize)
      & ppMaxBHSizeL .~ fromIntegral (p ^. #maxBlockHeaderSize)
      & ppKeyDepositL .~ Coin (toInteger (p ^. #keyDeposit))
      & ppPoolDepositL .~ Coin (toInteger (p ^. #poolDeposit))
      & ppEMaxL .~ EpochInterval (fromIntegral (p ^. #maxEpoch))
      & ppNOptL .~ fromIntegral (p ^. #nOpt)
      & ppA0L .~ a0
      & ppRhoL .~ rho
      & ppTauL .~ tau
      & ppProtocolVersionL .~ ProtVer v (fromIntegral (pv ^. #minor))
      & ppMinPoolCostL .~ Coin (toInteger (p ^. #minPoolCost))

-- | Plutus parameters, Alonzo on.
alonzoPParams :: forall era. AlonzoEraPParams era => S.ProtocolParameters -> PParams era -> Result (PParams era)
alonzoPParams p base = do
  mem <- boundedArg "price_mem" (p ^. #maybe'priceMem)
  steps <- boundedArg "price_steps" (p ^. #maybe'priceSteps)
  v1 <- model PlutusV1 (p ^. #costModelPlutusV1)
  v2 <- model PlutusV2 (p ^. #costModelPlutusV2)
  v3 <- model PlutusV3 (p ^. #costModelPlutusV3)
  pure $
    base
      & ppCostModelsL .~ mkCostModels (Map.fromList (concat [v1, v2, v3]))
      & ppPricesL .~ Prices mem steps
      & ppMaxTxExUnitsL .~ ExUnits (fromIntegral (p ^. #maxTxExMem)) (fromIntegral (p ^. #maxTxExSteps))
      & ppMaxBlockExUnitsL .~ ExUnits (fromIntegral (p ^. #maxBlockExMem)) (fromIntegral (p ^. #maxBlockExSteps))
      & ppMaxValSizeL .~ (p ^. #maxValueSize)
      & ppCollateralPercentageL .~ fromIntegral (p ^. #collateralPercentage)
      & ppMaxCollateralInputsL .~ fromIntegral (p ^. #maxCollateralInputs)
  where
    model _ [] = pure []
    model lang xs = case mkCostModel lang xs of
      Right m -> pure [(lang, m)]
      Left e -> unsupported ("cost model " <> T.pack (show lang) <> ": " <> T.pack (show e))

-- | Shelley, Allegra and Mary share one parameter shape: a decentralisation
-- parameter and a flat minimum UTxO value, no Plutus fields.
shelleyFamilyPParams :: forall era. (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => S.ProtocolParameters -> Result (PParams era)
shelleyFamilyPParams p = do
  b <- basePParams p
  d <- boundedArg "d" (p ^. #maybe'd)
  pure (b & ppDL .~ d & ppMinUTxOValueL .~ Coin (toInteger (p ^. #minUtxoValue)))

pparamsShelley :: S.ProtocolParameters -> Result (PParams ShelleyEra)
pparamsShelley = shelleyFamilyPParams

pparamsAllegra :: S.ProtocolParameters -> Result (PParams AllegraEra)
pparamsAllegra = shelleyFamilyPParams

pparamsMary :: S.ProtocolParameters -> Result (PParams MaryEra)
pparamsMary = shelleyFamilyPParams

pparamsAlonzo :: S.ProtocolParameters -> Result (PParams AlonzoEra)
pparamsAlonzo p = do
  b <- basePParams p
  d <- boundedArg "d" (p ^. #maybe'd)
  a <- alonzoPParams p b
  pure (a & ppDL .~ d & ppCoinsPerUTxOWordL .~ CoinPerWord (Coin (toInteger (p ^. #coinsPerUtxoWord))))

pparamsBabbage :: S.ProtocolParameters -> Result (PParams BabbageEra)
pparamsBabbage p = do
  b <- basePParams p
  a <- alonzoPParams p b
  pure (a & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin (p ^. #coinsPerUtxoByte)))

pparamsConway :: S.ProtocolParameters -> Result (PParams ConwayEra)
pparamsConway p = do
  b <- basePParams p
  a <- alonzoPParams p b
  pure (a & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin (p ^. #coinsPerUtxoByte)))

-- | The UTxO table; each entry is a TxOut in the era's CBOR.
utxoArg :: forall era. EraTxOut era => S.LedgerState -> Result (UTxO era)
utxoArg st = UTxO . Map.fromList <$> traverse entry (st ^. #utxo)
  where
    entry e = do
      let tid = e ^. #txin . #txId
      h <- maybe (badRequest "utxo txin: tx_id must be 32 bytes") pure (hashFromBytes tid)
      out <- either (\err -> badRequest ("utxo txout: " <> T.pack (show err))) pure (decodeFull' (eraProtVerLow @era) (e ^. #txout))
      pure (mkTxInPartial (TxId (unsafeMakeSafeHash h)) (toInteger (e ^. #txin . #index)), out)

txArg :: forall era. EraTx era => ByteString -> Result (Tx TopTx era)
txArg bs =
  either (\err -> badRequest ("tx: " <> T.pack (show err))) pure $
    decodeFullAnnotator (eraProtVerLow @era) "Tx" decCBOR (BL.fromStrict bs)

-- ---------------------------------------------------------------------
-- Running a rule

rejection :: [(Text, Text)] -> Maybe P.Rejection
rejection [] = Nothing
rejection xs =
  Just
    ( defMessage
        & #failures
          .~ [defMessage & #rule .~ r & #name .~ n | (r, n) <- xs]
    )

-- | The constructor names of a predicate failure, flattened to the innermost
-- constructor and the STS rule that owns it. A failure type with an 'Embed'
-- instance is walked structurally through 'GHC.Generics'; any other type
-- falls back to parsing 'show'.
nameFailure :: (Show a, Embed a) => Text -> a -> [(Text, Text)]
nameFailure top f = fromMaybe (flattenShown top (show f)) (embed f)

-- | A predicate failure type the generic walk understands. A constructor
-- whose only field is itself an 'Embed' type is a wrapper and is replaced by
-- that field's own flattening; every other constructor is a leaf named after
-- the rule that owns its type. Types without an instance are leaves, so each
-- failure type that appears as a wrapper's field needs one.
class Embed x where
  embed :: x -> Maybe [(Text, Text)]
  embed _ = Nothing

instance {-# OVERLAPPABLE #-} Embed x

-- | The 'embed' of a failure type owned by the given STS rule.
owned :: (Generic a, GFlatten (Rep a)) => Text -> a -> Maybe [(Text, Text)]
owned rule = Just . gflatten rule . from

class GFlatten f where
  gflatten :: Text -> f p -> [(Text, Text)]

instance GFlatten f => GFlatten (D1 c f) where
  gflatten rule (M1 x) = gflatten rule x

instance (GFlatten f, GFlatten g) => GFlatten (f :+: g) where
  gflatten rule (L1 x) = gflatten rule x
  gflatten rule (R1 x) = gflatten rule x

instance (Constructor c, GField f) => GFlatten (C1 c f) where
  gflatten rule m@(M1 x) = fromMaybe [(rule, T.pack (conName m))] (gfield x)

-- | The flattening of a constructor's only field, when it is an 'Embed' type.
class GField f where
  gfield :: f p -> Maybe [(Text, Text)]

instance {-# OVERLAPPABLE #-} GField f where
  gfield _ = Nothing

instance Embed x => GField (S1 s (K1 i x)) where
  gfield (M1 (K1 x)) = embed x

instance Embed (ShelleyUtxowPredFailure ShelleyEra) where embed = owned "UTXOW"
instance Embed (ShelleyUtxowPredFailure AllegraEra) where embed = owned "UTXOW"
instance Embed (ShelleyUtxowPredFailure MaryEra) where embed = owned "UTXOW"
instance Embed (ShelleyUtxowPredFailure AlonzoEra) where embed = owned "UTXOW"
instance Embed (ShelleyUtxowPredFailure BabbageEra) where embed = owned "UTXOW"
instance Embed (ShelleyUtxoPredFailure ShelleyEra) where embed = owned "UTXO"
instance Embed (AllegraUtxoPredFailure AllegraEra) where embed = owned "UTXO"
instance Embed (AllegraUtxoPredFailure MaryEra) where embed = owned "UTXO"
instance Embed (AlonzoUtxowPredFailure AlonzoEra) where embed = owned "UTXOW"
instance Embed (AlonzoUtxowPredFailure BabbageEra) where embed = owned "UTXOW"
instance Embed (AlonzoUtxoPredFailure AlonzoEra) where embed = owned "UTXO"
instance Embed (AlonzoUtxoPredFailure BabbageEra) where embed = owned "UTXO"
instance Embed (AlonzoUtxosPredFailure AlonzoEra) where embed = owned "UTXOS"
instance Embed (AlonzoUtxosPredFailure BabbageEra) where embed = owned "UTXOS"
instance Embed (BabbageUtxowPredFailure BabbageEra) where embed = owned "UTXOW"
instance Embed (BabbageUtxoPredFailure BabbageEra) where embed = owned "UTXO"
instance Embed (ConwayUtxowPredFailure ConwayEra) where embed = owned "UTXOW"
instance Embed (ConwayUtxoPredFailure ConwayEra) where embed = owned "UTXO"
instance Embed (ConwayUtxosPredFailure ConwayEra) where embed = owned "UTXOS"
instance Embed (ConwayLedgerPredFailure ConwayEra) where embed = owned "LEDGER"
instance Embed (ShelleyLedgersPredFailure ConwayEra) where embed = owned "LEDGERS"
instance Embed (ConwayBbodyPredFailure ConwayEra) where embed = owned "BBODY"
instance Embed (ConwayGovPredFailure ConwayEra) where embed = owned "GOV"
instance Embed (ConwayCertsPredFailure ConwayEra) where embed = owned "CERTS"
instance Embed (ConwayCertPredFailure ConwayEra) where embed = owned "CERT"
instance Embed (ConwayDelegPredFailure ConwayEra) where embed = owned "DELEG"
instance Embed (ShelleyPoolPredFailure ConwayEra) where embed = owned "POOL"
instance Embed (ConwayGovCertPredFailure ConwayEra) where embed = owned "GOVCERT"
instance Embed (ShelleyPpupPredFailure ShelleyEra) where embed = owned "PPUP"
instance Embed (ShelleyPpupPredFailure AllegraEra) where embed = owned "PPUP"
instance Embed (ShelleyPpupPredFailure MaryEra) where embed = owned "PPUP"
instance Embed (ShelleyPpupPredFailure AlonzoEra) where embed = owned "PPUP"
instance Embed (ShelleyPpupPredFailure BabbageEra) where embed = owned "PPUP"

-- | Fallback for failure types without an 'Embed' instance: walk the showed
-- failure down the wrapper constructors to the innermost one.
flattenShown :: Text -> String -> [(Text, Text)]
flattenShown rule s = case words s of
  [] -> []
  (c : _)
    | Just next <- lookup c wrappers
    , Just inner <- argument (drop (length c) s) ->
        flattenShown (fromMaybe rule next) inner
    | otherwise -> [(rule, T.pack c)]

-- | Wrapper constructors and the rule their argument belongs to ('Nothing'
-- keeps the current rule).
wrappers :: [(String, Maybe Text)]
wrappers =
  [ ("UtxoFailure", Just "UTXO")
  , ("UtxosFailure", Just "UTXOS")
  , ("ShelleyInAlonzoUtxowPredFailure", Just "UTXOW")
  , ("AlonzoInBabbageUtxoPredFailure", Just "UTXO")
  , ("AlonzoInBabbageUtxowPredFailure", Just "UTXOW")
  ]

-- | The single parenthesised argument of a wrapper constructor.
argument :: String -> Maybe String
argument rest = case dropWhile (== ' ') rest of
  ('(' : more) -> Just (init more)
  _ -> Nothing


-- ---------------------------------------------------------------------
-- Era history

-- | The epoch info and system start of the request's era history, in place
-- of the single-era ones 'globalsArg' derives from the globals. One or two
-- eras are supported.
historyGlobals :: S.EraHistory -> Globals -> Result Globals
historyGlobals h g = do
  eras <- traverse summaryOf (h ^. #eras)
  ei <- case eras of
    [a] -> pure (summaryToEpochInfo (summaryWithExactly (exactlyOne a)))
    [a, b] -> pure (summaryToEpochInfo (summaryWithExactly (exactlyTwo a b)))
    _ -> unsupported "era_history must hold one or two eras"
  pure
    g
      { epochInfo = hoistEpochInfo (first (T.pack . show) . runExcept) ei
      , systemStart = SystemStart (posixSecondsToUTCTime (fromIntegral (h ^. #systemStartUnix)))
      }
  where
    summaryOf e = do
      start <- boundOf (e ^. #startSlot) (e ^. #startEpoch) (e ^. #maybe'startTime)
      slotLen <- rationalArg "slot_length" (e ^. #maybe'slotLength)
      end <- case (e ^. #maybe'endSlot, e ^. #maybe'endEpoch) of
        (Just sl, Just ep) -> EraEnd <$> boundOf sl ep (e ^. #maybe'endTime)
        _ -> pure EraUnbounded
      let params =
            (defaultEraParams (SecurityParam (securityParameter g)) (mkSlotLength (fromRational slotLen)))
              { eraEpochSize = EpochSize (e ^. #epochLength)
              , eraSafeZone = StandardSafeZone (e ^. #safeZone)
              }
      pure (EraSummary start end params)
    boundOf slotNo epochNo time = do
      t <- rationalArg "era time" time
      pure initBound {boundTime = RelativeTime (fromRational t), boundSlot = SlotNo slotNo, boundEpoch = EpochNo epochNo}

