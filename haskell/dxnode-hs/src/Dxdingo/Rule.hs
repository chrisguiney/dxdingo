{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Transaction rules (UTXO, UTXOW), answered by running the real STS rule
-- of cardano-ledger in 'ShelleyBase'.
module Dxdingo.Rule
  ( dispatch
  , implemented
  ) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
  ( AlonzoEraPParams
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
import Cardano.Ledger.Core
import Cardano.Ledger.Babbage (BabbageEra)
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
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Plutus.CostModels (mkCostModel, mkCostModels)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (mkStAnnTx))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition.Extended (STS (..), TRC (..), applySTS)
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
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Rule
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- ---------------------------------------------------------------------
-- Inputs

data RuleEra = RAlonzo | RBabbage | RConway deriving (Eq, Show)

eraArg :: P.Era -> Result RuleEra
eraArg = \case
  P.ERA_ALONZO -> pure RAlonzo
  P.ERA_BABBAGE -> pure RBabbage
  P.ERA_CONWAY -> pure RConway
  e -> unsupported ("transaction rules are modelled for Alonzo, Babbage and Conway, not " <> T.pack (show e))

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

-- | Everything an STS run needs, built once per call.
data Run era = Run
  { runGlobals :: Globals
  , runEnv :: UtxoEnv era
  , runState :: UTxOState era
  , runTx :: StAnnTx TopTx era
  }

prepare ::
  forall era.
  (EraTx era, EraStake era, EraGov era, EraCertState era, ApplyTx era) =>
  Bool ->
  (S.ProtocolParameters -> Result (PParams era)) ->
  S.LedgerContext ->
  S.Globals ->
  S.ProtocolParameters ->
  S.LedgerState ->
  ByteString ->
  Result (Run era)
prepare extend mkPP ctx g p st tx = do
  globals <- globalsArg g (fromIntegral (ctx ^. #networkId))
  pp <- mkPP p
  utxo <- utxoArg st
  t <- txArg tx
  let us = smartUTxOState pp utxo (Coin 0) (Coin 0) emptyGovState (Coin 0)
  let slot = SlotNo (ctx ^. #slot)
      ei = (if extend then unsafeLinearExtendEpochInfo slot else id) (epochInfo globals)
      annotated = mkStAnnTx ei (systemStart globals) pp utxo mempty t
  pure (Run globals (UtxoEnv slot pp def) us annotated)

-- | Run one STS rule and name its failures.
execute ::
  forall s era.
  ( STS s
  , BaseM s ~ ShelleyBase
  , Environment s ~ UtxoEnv era
  , State s ~ UTxOState era
  , Signal s ~ StAnnTx TopTx era
  , Show (PredicateFailure s)
  ) =>
  Text ->
  Run era ->
  [(Text, Text)]
execute top r =
  case runIdentity (runReaderT (applySTS @s (TRC (runEnv r, runState r, runTx r))) (runGlobals r)) of
    Right _ -> []
    Left fs -> sort (nub (concatMap (flattenFailure top . show) (NE.toList fs)))

rejection :: [(Text, Text)] -> Maybe P.Rejection
rejection [] = Nothing
rejection xs =
  Just
    ( defMessage
        & #failures
          .~ [defMessage & #rule .~ r & #name .~ n | (r, n) <- xs]
    )

-- | Walk the showed failure down the wrapper constructors to the innermost
-- one, tracking which STS rule owns it.
flattenFailure :: Text -> String -> [(Text, Text)]
flattenFailure rule s = case words s of
  [] -> []
  (c : _)
    | Just next <- lookup c wrappers
    , Just inner <- argument (drop (length c) s) ->
        flattenFailure (fromMaybe rule next) inner
    | otherwise -> [(rule, T.pack c)]

-- | Wrapper constructors and the rule their argument belongs to ('Nothing'
-- keeps the current rule).
wrappers :: [(String, Maybe Text)]
wrappers =
  [ ("UtxoFailure", Just "UTXO")
  , ("UtxosFailure", Just "UTXOS")
  , ("ShelleyInAlonzoUtxowPredFailure", Just "UTXOW")
  , ("FromAlonzoUtxowFail", Just "UTXOW")
  , ("FromAlonzoUtxoFail", Just "UTXO")
  , ("AlonzoInBabbageUtxoPredFailure", Just "UTXO")
  , ("FromAlonzoUtxosFail", Just "UTXOS")
  , ("AlonzoInBabbageUtxowPredFailure", Just "UTXOW")
  ]

-- | The single parenthesised argument of a wrapper constructor.
argument :: String -> Maybe String
argument rest = case dropWhile (== ' ') rest of
  ('(' : more) -> Just (init more)
  _ -> Nothing

-- ---------------------------------------------------------------------
-- Functions

-- | UTXO with its embedded UTXOS.
ruleUtxo :: RuleUtxoRequest -> Result RuleUtxoResponse
ruleUtxo req = do
  fs <- onEra (req ^. #context) (req ^. #maybe'globals) (req ^. #maybe'pparams) (req ^. #maybe'state) (req ^. #tx) False
  pure (defMessage & #maybe'rejection .~ rejection fs)

-- | UTXOW with its embedded UTXO and UTXOS.
ruleUtxow :: RuleUtxowRequest -> Result RuleUtxowResponse
ruleUtxow req = do
  fs <- onEra (req ^. #context) (req ^. #maybe'globals) (req ^. #maybe'pparams) (req ^. #maybe'state) (req ^. #tx) True
  pure (defMessage & #maybe'rejection .~ rejection fs)

onEra ::
  S.LedgerContext ->
  Maybe S.Globals ->
  Maybe S.ProtocolParameters ->
  Maybe S.LedgerState ->
  ByteString ->
  Bool ->
  Result [(Text, Text)]
onEra ctx mg mp ms tx withWitnesses = do
  era <- eraArg (ctx ^. #era)
  let g = fromMaybe defMessage mg
      p = fromMaybe defMessage mp
      st = fromMaybe defMessage ms
  case era of
    RConway -> do
      r <- prepare False pparamsConway ctx g p st tx
      pure
        ( if withWitnesses
            then execute @(EraRule "UTXOW" ConwayEra) "UTXOW" r
            else execute @(EraRule "UTXO" ConwayEra) "UTXO" r
        )
    RBabbage -> do
      r <- prepare False pparamsBabbage ctx g p st tx
      pure
        ( if withWitnesses
            then execute @(EraRule "UTXOW" BabbageEra) "UTXOW" r
            else execute @(EraRule "UTXO" BabbageEra) "UTXO" r
        )
    RAlonzo -> do
      r <- prepare True pparamsAlonzo ctx g p st tx
      pure
        ( if withWitnesses
            then execute @(EraRule "UTXOW" AlonzoEra) "UTXOW" r
            else execute @(EraRule "UTXO" AlonzoEra) "UTXO" r
        )

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'RuleUtxo q -> Just (respond #ruleUtxo (ruleUtxo q))
  Call'RuleUtxow q -> Just (respond #ruleUtxow (ruleUtxow q))
  _ -> Nothing

implemented :: [Text]
implemented = ["rule_utxo", "rule_utxow"]
