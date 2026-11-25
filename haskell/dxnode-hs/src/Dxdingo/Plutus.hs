{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Plutus functions: what a script sees and what it costs, answered by
-- cardano-ledger's own Plutus context and evaluation code.
module Dxdingo.Plutus
  ( dispatch
  , implemented
  , costModel
  , txInfo
  , scriptPurpose
  , scriptContext
  , evalExUnits
  , collectScripts
  ) where

import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.Address (accountAddressCredentialL)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Plutus.Evaluate (TransactionScriptFailure, collectPlutusScriptsWithContext, evalTxExUnits)
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded (..), resolveNeededPlutusScriptsWithPurpose)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (ProtVer (..), mkVersion, txIxFromIntegral, txIxToInt)
import Cardano.Ledger.Binary (Annotator, DecCBOR, decodeFull', decodeFullAnnotator, decCBOR, serialize')
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Conway.Scripts (ConwayEraScript (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Plutus.CostModels (CostModel, getCostModelParams, mkCostModel, mkCostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..), pwcScriptHash)
import Cardano.Ledger.Plutus.Language (Language (..), LegacyPlutusArgs (..), PlutusArgs (..), SLanguage (..), plutusSLanguage)
import Cardano.Ledger.Plutus (Data)
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.State (EraUTxO (..), UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (RelativeTime (..), SystemStart (..), mkSlotLength)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Convert (rational)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..))
import Ouroboros.Consensus.HardFork.History.EpochInfo (summaryToEpochInfo)
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..), SafeZone (..), pattern NoPerasEnabled)
import Ouroboros.Consensus.HardFork.History.Summary (Bound (..), EraEnd (..), EraSummary (..), Summary (..))
import qualified PlutusCore.Data as PD
import Data.SOP.NonEmpty (NonEmpty (..))
import PlutusTx.IsData.Class (FromData, ToData, fromData, toData)
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Plutus
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- Arguments -----------------------------------------------------------------

language :: P.PlutusVersion -> Result Language
language = \case
  P.PLUTUS_VERSION_V1 -> pure PlutusV1
  P.PLUTUS_VERSION_V2 -> pure PlutusV2
  P.PLUTUS_VERSION_V3 -> pure PlutusV3
  P.PLUTUS_VERSION_V4 -> pure PlutusV4
  v -> unsupported ("no Plutus language " <> T.pack (show v))

-- | The constructor name of a shown value.
constructorName :: Show a => a -> Text
constructorName = T.takeWhile (\c -> c /= ' ' && c /= '(') . T.pack . show

-- | The innermost constructor of a context error: the per-era wrappers
-- (@BabbageContextError (ReferenceInputsNotSupported ...)@) only say which era
-- injected the error.
contextErrorName :: Show a => a -> Text
contextErrorName = go . T.pack . show
  where
    go t
      | (w, rest) <- T.break (== ' ') t
      , "ContextError" `T.isSuffixOf` w
      , Just inner <- T.stripPrefix " (" rest =
          go inner
      | otherwise = T.takeWhile (\c -> c /= ' ' && c /= ')') t

rejectionOf :: Text -> Text -> Text -> P.Rejection
rejectionOf rule name detail =
  defMessage
    & #failures .~ [defMessage & #rule .~ rule & #name .~ name & #detail .~ detail]

-- | 'mkCostModel' on the supplied parameters; the stored parameters are the
-- supplied ones, as 'getCostModelParams' returns them. The reference takes no
-- protocol version.
costModel :: PlutusCostModelRequest -> Result PlutusCostModelResponse
costModel req = do
  lang <- language (req ^. #plutusVersion)
  pure $ case mkCostModel lang (req ^. #params) of
    Right cm -> defMessage & #params .~ getCostModelParams cm
    Left err -> defMessage & #rejection .~ rejectionOf "CostModels" (constructorName err) (T.pack (show err))

-- Era history ----------------------------------------------------------------

-- | The era history as the consensus 'Summary', turned into the 'EpochInfo'
-- the ledger takes.
eraHistory :: S.EraHistory -> Result (EpochInfo (Either Text), SystemStart)
eraHistory h = do
  rows <- traverse summary (h ^. #eras)
  epochInfo <- case rows of
    [] -> unsupported "empty era history"
    _ -> pure (build rows)
  pure (epochInfo, SystemStart (posixSecondsToUTCTime (fromIntegral (h ^. #systemStartUnix))))
  where
    build :: [EraSummary] -> EpochInfo (Either Text)
    build rows = case summaryOf rows of
      SomeSummary s -> hoistEpochInfo (first (T.pack . show) . runExcept) (summaryToEpochInfo s)
    seconds = fmap realToFrac . (rational =<<)
    summary e = do
      start <- maybe (badRequest "era start_time") pure (seconds (e ^. #maybe'startTime))
      slotLen <- maybe (badRequest "era slot_length") pure (seconds (e ^. #maybe'slotLength))
      let bnd t s ep = Bound (RelativeTime t) (SlotNo s) (EpochNo ep) NoPerasEnabled
          endB = case (e ^. #maybe'endSlot, e ^. #maybe'endEpoch, seconds (e ^. #maybe'endTime)) of
            (Just s, Just ep, Just t) -> EraEnd (bnd t s ep)
            _ -> EraUnbounded
      pure $
        EraSummary
          (bnd start (e ^. #startSlot) (e ^. #startEpoch))
          endB
          ( EraParams
              (EpochSize (e ^. #epochLength))
              (mkSlotLength slotLen)
              (StandardSafeZone (e ^. #safeZone))
              (GenesisWindow (e ^. #safeZone))
              NoPerasEnabled
          )

data SomeSummary = forall xs. SomeSummary (Summary xs)

summaryOf :: [EraSummary] -> SomeSummary
summaryOf [] = error "summaryOf: empty"
summaryOf [e] = SomeSummary (Summary (NonEmptyOne e))
summaryOf (e : es) = case summaryOf es of
  SomeSummary (Summary rest) -> SomeSummary (Summary (NonEmptyCons e rest))

-- Eras and the transaction ----------------------------------------------------

type PlutusEra era =
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraPurpose era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  )

-- | The proto form of the purposes only some eras have.
class EraPurpose era where
  extraPurpose :: PlutusPurpose AsIxItem era -> Maybe ScriptPurpose
  extraPurpose _ = Nothing
  extraIx :: PlutusPurpose AsIx era -> Maybe ScriptPurpose
  extraIx _ = Nothing
  extraFromProto :: ScriptPurpose -> Maybe (Result (PlutusPurpose AsIxItem era))
  extraFromProto _ = Nothing

instance EraPurpose AlonzoEra

instance EraPurpose BabbageEra

instance EraPurpose ConwayEra where
  extraPurpose p
    | Just (AsIxItem ix voter) <- toVotingPurpose p =
        Just (defMessage & #tag .~ SCRIPT_PURPOSE_TAG_VOTE & #index .~ ix & #voter .~ voterProto voter)
    | Just (AsIxItem ix proposal) <- toProposingPurpose p =
        Just (defMessage & #tag .~ SCRIPT_PURPOSE_TAG_PROPOSE & #index .~ ix & #proposal .~ serialize' (eraProtVerLow @ConwayEra) proposal)
    | otherwise = Nothing
  extraFromProto p = case p ^. #tag of
    SCRIPT_PURPOSE_TAG_VOTE -> Just $ do
      voter <- voterFromProto (p ^. #voter)
      pure (mkVotingPurpose (AsIxItem (p ^. #index) voter))
    SCRIPT_PURPOSE_TAG_PROPOSE -> Just $ do
      proposal <- decodeBytes @ConwayEra "proposal" (p ^. #proposal)
      pure (mkProposingPurpose (AsIxItem (p ^. #index) proposal))
    _ -> Nothing
  extraIx p
    | Just (AsIx ix) <- toVotingPurpose p = Just (defMessage & #tag .~ SCRIPT_PURPOSE_TAG_VOTE & #index .~ ix)
    | Just (AsIx ix) <- toProposingPurpose p = Just (defMessage & #tag .~ SCRIPT_PURPOSE_TAG_PROPOSE & #index .~ ix)
    | otherwise = Nothing

voterProto :: Voter -> P.Voter
voterProto = \case
  CommitteeVoter c -> defMessage & #role .~ P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE & #credential .~ credProto c
  DRepVoter c -> defMessage & #role .~ P.VOTER_ROLE_DREP & #credential .~ credProto c
  StakePoolVoter (KeyHash h) -> defMessage & #role .~ P.VOTER_ROLE_STAKE_POOL & #poolId .~ hashToBytes h

voterFromProto :: P.Voter -> Result Voter
voterFromProto v = case v ^. #role of
  P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE -> CommitteeVoter <$> credFromProto (v ^. #credential)
  P.VOTER_ROLE_DREP -> DRepVoter <$> credFromProto (v ^. #credential)
  P.VOTER_ROLE_STAKE_POOL -> StakePoolVoter . KeyHash <$> hashArg "pool_id" (v ^. #poolId)
  r -> badRequest ("voter role " <> T.pack (show r))

credFromProto :: P.Credential -> Result (Credential r)
credFromProto c = case c ^. #kind of
  P.CREDENTIAL_KIND_KEY -> KeyHashObj . KeyHash <$> hashArg "credential" (c ^. #hash)
  P.CREDENTIAL_KIND_SCRIPT -> ScriptHashObj . ScriptHash <$> hashArg "credential" (c ^. #hash)
  k -> badRequest ("credential kind " <> T.pack (show k))

hashArg :: HashAlgorithm h => Text -> BS.ByteString -> Result (Hash h a)
hashArg name = maybe (badRequest (name <> " must be 28 bytes")) pure . hashFromBytes

credProto :: Credential r -> P.Credential
credProto = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

data Env era = Env
  { envTx :: Tx TopTx era
  , envUtxo :: UTxO era
  }

txIn :: P.TxIn -> Result TxIn
txIn t = do
  h <- maybe (badRequest "tx_id must be 32 bytes") pure (hashFromBytes (t ^. #txId))
  ix <- maybe (unsupported "input index exceeds Word16") pure (txIxFromIntegral (t ^. #index))
  pure (TxIn (TxId (unsafeMakeSafeHash h)) ix)

decodeEnv :: forall era. PlutusEra era => BS.ByteString -> [S.UtxoEntry] -> Result (Env era)
decodeEnv txBytes entries = do
  tx <- either (\e -> badRequest ("tx: " <> T.pack (show e))) pure $
    decodeFullAnnotator (eraProtVerLow @era) "Tx" decCBOR (BSL.fromStrict txBytes)
  pairs <- traverse entry entries
  pure (Env tx (UTxO (Map.fromList pairs)))
  where
    entry e = do
      i <- txIn (e ^. #txin)
      o <- either (\err -> badRequest ("txout: " <> T.pack (show err))) pure (decodeFull' (eraProtVerLow @era) (e ^. #txout))
      pure (i, o)

protVer :: P.ProtocolVersion -> Result ProtVer
protVer v = do
  major <- maybe (badRequest "protocol version major") pure (mkVersion (toInteger (v ^. #major)))
  pure (ProtVer major (fromIntegral (v ^. #minor)))

-- | Run a handler at the era's type.
withEra :: P.Era -> (forall era. PlutusEra era => Proxy era -> Result r) -> Result r
withEra era k = case era of
  P.ERA_ALONZO -> k (Proxy @AlonzoEra)
  P.ERA_BABBAGE -> k (Proxy @BabbageEra)
  P.ERA_CONWAY -> k (Proxy @ConwayEra)
  e -> unsupported ("no Plutus context for " <> T.pack (show e))

-- plutus_tx_info ---------------------------------------------------------------

-- | 'toPlutusTxInfo' for the language, then the PlutusData of the 'TxInfo'.
-- Outside Dijkstra the purpose is ignored, so a spending one stands in.
txInfoData :: forall l era. (PlutusEra era, EraPlutusTxInfo l era, ToData (PlutusTxInfo l)) => LedgerTxInfo era -> Either (ContextError era) BS.ByteString
txInfoData lti = do
  info <- toPlutusTxInfoForPurpose (Proxy @l) lti (mkSpendingPurpose AsPurpose)
  pure (BSL.toStrict (serialise (toData info :: PD.Data)))

txInfo :: PlutusTxInfoRequest -> Result PlutusTxInfoResponse
txInfo req = do
  lang <- language (req ^. #plutusVersion)
  case (req ^. #context . #era, lang) of
    (P.ERA_ALONZO, PlutusV1) -> txInfoAt @AlonzoEra @'PlutusV1 req
    (P.ERA_BABBAGE, PlutusV1) -> txInfoAt @BabbageEra @'PlutusV1 req
    (P.ERA_BABBAGE, PlutusV2) -> txInfoAt @BabbageEra @'PlutusV2 req
    (P.ERA_CONWAY, PlutusV1) -> txInfoAt @ConwayEra @'PlutusV1 req
    (P.ERA_CONWAY, PlutusV2) -> txInfoAt @ConwayEra @'PlutusV2 req
    (P.ERA_CONWAY, PlutusV3) -> txInfoAt @ConwayEra @'PlutusV3 req
    (e, l) -> unsupported ("no " <> T.pack (show l) <> " in " <> T.pack (show e))

txInfoAt :: forall era l. (PlutusEra era, EraPlutusTxInfo l era, ToData (PlutusTxInfo l)) => PlutusTxInfoRequest -> Result PlutusTxInfoResponse
txInfoAt req = do
  pv <- protVer (req ^. #context . #protocolVersion)
  (epochInfo, systemStart) <- eraHistory (req ^. #eraHistory)
  env <- decodeEnv @era (req ^. #tx) (req ^. #utxo)
  let lti = LedgerTxInfo pv epochInfo systemStart (envUtxo env) (envTx env) mempty
  pure $ case txInfoData @l @era lti of
    Left err -> defMessage & #rejection .~ rejectionOf "ContextError" (contextErrorName err) (T.pack (show err))
    Right bytes -> defMessage & #txInfo .~ bytes

-- plutus_script_purpose ----------------------------------------------------------

-- | Every purpose of the transaction that names a script, as
-- 'getScriptsNeeded' lists them, with the index the redeemer pointer uses.
purposesOf :: forall era. PlutusEra era => Env era -> [ScriptPurpose]
purposesOf env =
  sortOn (\p -> (fromEnum (p ^. #tag), p ^. #index)) $
    [ purposeProto p | (p, _) <- unAlonzoScriptsNeeded (getScriptsNeeded (envUtxo env) (envTx env ^. bodyTxL)) ]

purposeProto :: forall era. PlutusEra era => PlutusPurpose AsIxItem era -> ScriptPurpose
purposeProto p
  | Just (AsIxItem ix i) <- toSpendingPurpose p =
      defMessage & #tag .~ SCRIPT_PURPOSE_TAG_SPEND & #index .~ ix & #spendInput .~ txInProto i
  | Just (AsIxItem ix (PolicyID (ScriptHash h))) <- toMintingPurpose p =
      defMessage & #tag .~ SCRIPT_PURPOSE_TAG_MINT & #index .~ ix & #mintPolicyId .~ hashToBytes h
  | Just (AsIxItem ix c) <- toCertifyingPurpose p =
      defMessage & #tag .~ SCRIPT_PURPOSE_TAG_CERT & #index .~ ix & #certificate .~ serialize' (eraProtVerLow @era) c
  | Just (AsIxItem ix a) <- toWithdrawingPurpose p =
      defMessage & #tag .~ SCRIPT_PURPOSE_TAG_REWARD & #index .~ ix & #rewardAccount .~ credProto (a ^. accountAddressCredentialL)
  | Just q <- extraPurpose p = q
  | otherwise = defMessage

txInProto :: TxIn -> P.TxIn
txInProto (TxIn (TxId h) ix) = defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral (txIxToInt ix)

scriptPurpose :: PlutusScriptPurposeRequest -> Result PlutusScriptPurposeResponse
scriptPurpose req =
  withEra (req ^. #context . #era) $ \(_ :: Proxy era) -> do
    env <- decodeEnv @era (req ^. #tx) (req ^. #utxo)
    pure (defMessage & #purposes .~ purposesOf env)

-- plutus_script_context ----------------------------------------------------------

decodeBytes :: forall era a. (PlutusEra era, DecCBOR a) => Text -> BS.ByteString -> Result a
decodeBytes name = either (\e -> badRequest (name <> ": " <> T.pack (show e))) pure . decodeFull' (eraProtVerLow @era)

decodeAnnotated :: forall era a. (PlutusEra era, DecCBOR (Annotator a)) => Text -> BS.ByteString -> Result a
decodeAnnotated name b =
  either (\e -> badRequest (name <> ": " <> T.pack (show e))) pure $
    decodeFullAnnotator (eraProtVerLow @era) name decCBOR (BSL.fromStrict b)

-- | The redeemer pointer a ScriptPurpose message names.
purposeFromProto :: forall era. PlutusEra era => ScriptPurpose -> Result (PlutusPurpose AsIxItem era)
purposeFromProto p = case p ^. #tag of
  SCRIPT_PURPOSE_TAG_SPEND -> mkSpendingPurpose . AsIxItem ix <$> txIn (p ^. #spendInput)
  SCRIPT_PURPOSE_TAG_MINT -> mkMintingPurpose . AsIxItem ix . PolicyID . ScriptHash <$> hashArg "mint_policy_id" (p ^. #mintPolicyId)
  SCRIPT_PURPOSE_TAG_CERT -> mkCertifyingPurpose . AsIxItem ix <$> decodeBytes @era "certificate" (p ^. #certificate)
  SCRIPT_PURPOSE_TAG_REWARD -> do
    c <- credFromProto (p ^. #rewardAccount)
    pure (mkWithdrawingPurpose (AsIxItem ix (AccountAddress Mainnet (AccountId c))))
  _ -> fromMaybe (badRequest "purpose tag") (extraFromProto p)
  where
    ix = p ^. #index

-- | The arguments a script of the language is applied to.
class ArgsData l where
  argsData :: PlutusArgs l -> [PD.Data]

instance ArgsData 'PlutusV1 where
  argsData = legacyArgs . unPlutusV1Args

instance ArgsData 'PlutusV2 where
  argsData = legacyArgs . unPlutusV2Args

instance ArgsData 'PlutusV3 where
  argsData = pure . toData . unPlutusV3Args

legacyArgs :: ToData (PlutusScriptContext l) => LegacyPlutusArgs l -> [PD.Data]
legacyArgs = \case
  LegacyPlutusArgs2 r c -> [r, toData c]
  LegacyPlutusArgs3 d r c -> [d, r, toData c]

scriptContext :: PlutusScriptContextRequest -> Result PlutusScriptContextResponse
scriptContext req = do
  lang <- language (req ^. #plutusVersion)
  case (req ^. #context . #era, lang) of
    (P.ERA_ALONZO, PlutusV1) -> scriptContextAt @AlonzoEra @'PlutusV1 req
    (P.ERA_BABBAGE, PlutusV1) -> scriptContextAt @BabbageEra @'PlutusV1 req
    (P.ERA_BABBAGE, PlutusV2) -> scriptContextAt @BabbageEra @'PlutusV2 req
    (P.ERA_CONWAY, PlutusV1) -> scriptContextAt @ConwayEra @'PlutusV1 req
    (P.ERA_CONWAY, PlutusV2) -> scriptContextAt @ConwayEra @'PlutusV2 req
    (P.ERA_CONWAY, PlutusV3) -> scriptContextAt @ConwayEra @'PlutusV3 req
    (e, l) -> unsupported ("no " <> T.pack (show l) <> " in " <> T.pack (show e))

-- | 'toPlutusArgs' on a TxInfo decoded from its PlutusData.
scriptContextAt ::
  forall era l.
  (PlutusEra era, EraPlutusTxInfo l era, FromData (PlutusTxInfo l), ArgsData l) =>
  PlutusScriptContextRequest ->
  Result PlutusScriptContextResponse
scriptContextAt req = do
  pv <- protVer (req ^. #context . #protocolVersion)
  info <- do
    d <- either (\e -> badRequest ("tx_info: " <> T.pack (show e))) pure (deserialiseOrFail (BSL.fromStrict (req ^. #txInfo)))
    maybe (badRequest "tx_info is not a TxInfo") pure (fromData (d :: PD.Data))
  purpose <- purposeFromProto @era (req ^. #purpose)
  redeemer <- decodeAnnotated @era @(Data era) "redeemer" (req ^. #redeemer)
  datum <-
    if BS.null (req ^. #datum)
      then pure Nothing
      else Just <$> decodeAnnotated @era @(Data era) "datum" (req ^. #datum)
  pure $ case toPlutusArgs (Proxy @l) pv info purpose datum redeemer of
    Left err -> defMessage & #rejection .~ rejectionOf "ContextError" (contextErrorName err) (T.pack (show err))
    Right args -> defMessage & #args .~ map (BSL.toStrict . serialise) (argsData @l args)

-- plutus_eval_ex_units ---------------------------------------------------------

costModels :: S.ProtocolParameters -> Result (Map.Map Language CostModel)
costModels pp = do
  entries <- traverse one [(PlutusV1, pp ^. #costModelPlutusV1), (PlutusV2, pp ^. #costModelPlutusV2), (PlutusV3, pp ^. #costModelPlutusV3)]
  pure (Map.fromList (concat entries))
  where
    one (_, []) = pure []
    one (l, ps) = either (\e -> badRequest ("cost model: " <> T.pack (show e))) (\c -> pure [(l, c)]) (mkCostModel l ps)

-- | 'evalTxExUnits' on a 'PParams' carrying only what it reads: the protocol
-- version, the cost models and the transaction execution-unit bound.
evalExUnits :: PlutusEvalExUnitsRequest -> Result PlutusEvalExUnitsResponse
evalExUnits req = do
  let ctx = req ^. #context
      pp = req ^. #pparams
  pv <- protVer (ctx ^. #protocolVersion)
  cms <- costModels pp
  (epochInfo, systemStart) <- eraHistory (req ^. #eraHistory)
  withEra (ctx ^. #era) $ \(_ :: Proxy era) -> do
    env <- decodeEnv @era (req ^. #tx) (req ^. #utxo)
    let params :: PParams era
        params =
          emptyPParams
            & ppProtocolVersionL .~ pv
            & ppCostModelsL .~ mkCostModels cms
            & ppMaxTxExUnitsL .~ ExUnits (fromIntegral (pp ^. #maxTxExMem)) (fromIntegral (pp ^. #maxTxExSteps))
        report = evalTxExUnits params (envTx env) (envUtxo env) epochInfo systemStart
    pure (defMessage & #results .~ sortOn (\r -> (fromEnum (r ^. #purpose . #tag), r ^. #purpose . #index)) (map (uncurry (evaluation @era)) (Map.toList report)))

evaluation :: forall era. PlutusEra era => PlutusPurpose AsIx era -> Either (TransactionScriptFailure era) ExUnits -> ScriptEvaluation
evaluation p = \case
  Right (ExUnits mem steps) -> base & #ok .~ True & #exMem .~ fromIntegral mem & #exSteps .~ fromIntegral steps
  Left failure -> base & #ok .~ False & #failure .~ constructorName failure
  where
    base = defMessage & #purpose .~ ixPurpose p

-- | The tag and index of a pointer; the evaluation report carries no item.
ixPurpose :: forall era. PlutusEra era => PlutusPurpose AsIx era -> ScriptPurpose
ixPurpose p
  | Just (AsIx ix) <- toSpendingPurpose p = tagged SCRIPT_PURPOSE_TAG_SPEND ix
  | Just (AsIx ix) <- toMintingPurpose p = tagged SCRIPT_PURPOSE_TAG_MINT ix
  | Just (AsIx ix) <- toCertifyingPurpose p = tagged SCRIPT_PURPOSE_TAG_CERT ix
  | Just (AsIx ix) <- toWithdrawingPurpose p = tagged SCRIPT_PURPOSE_TAG_REWARD ix
  | Just ix <- extraIx p = ix
  | otherwise = defMessage
  where
    tagged t ix = defMessage & #tag .~ t & #index .~ ix

-- plutus_collect_scripts ----------------------------------------------------------

-- | The language, and the arguments a script of it is applied to, of a
-- collected script. Conway has no PlutusV4 script, so that language has none.
argsOf :: PlutusWithContext -> Maybe (P.PlutusVersion, [PD.Data])
argsOf PlutusWithContext {pwcScript = script, pwcArgs = args} = case plutusSLanguage script of
  SPlutusV1 -> Just (P.PLUTUS_VERSION_V1, argsData args)
  SPlutusV2 -> Just (P.PLUTUS_VERSION_V2, argsData args)
  SPlutusV3 -> Just (P.PLUTUS_VERSION_V3, argsData args)
  SPlutusV4 -> Nothing

-- | A collected script as the response has it: a legacy script is applied to
-- the redeemer and the context, with the datum first when it has one; a V3
-- script to the context alone.
collectedProto :: forall era. PlutusEra era => PlutusPurpose AsIxItem era -> PlutusWithContext -> Maybe CollectedScript
collectedProto purpose pwc@PlutusWithContext {pwcExUnits = ExUnits mem steps} = do
  (version, args) <- argsOf pwc
  let enc = BSL.toStrict . serialise
      ScriptHash scriptHash = pwcScriptHash pwc
      base =
        defMessage
          & #purpose .~ purposeProto purpose
          & #scriptHash .~ hashToBytes scriptHash
          & #plutusVersion .~ version
          & #exMem .~ fromIntegral mem
          & #exSteps .~ fromIntegral steps
  case args of
    [c] -> Just (base & #context .~ enc c)
    [r, c] -> Just (base & #redeemer .~ enc r & #context .~ enc c)
    [d, r, c] -> Just (base & #datum .~ enc d & #redeemer .~ enc r & #context .~ enc c)
    _ -> Nothing

collectFailure :: Show e => e -> P.Failure
collectFailure err =
  defMessage & #rule .~ "CollectError" & #name .~ constructorName err & #detail .~ T.pack (show err)

-- | 'collectPlutusScriptsWithContext' on a 'PParams' carrying only what it
-- reads: the protocol version and the cost models. The result lists the
-- scripts in reverse of the order 'resolveNeededPlutusScriptsWithPurpose' has
-- the purposes in, which is how the purpose of each is recovered.
collectScripts :: PlutusCollectScriptsRequest -> Result PlutusCollectScriptsResponse
collectScripts req = do
  let ctx = req ^. #context
      pp = req ^. #pparams
  pv <- protVer (ctx ^. #protocolVersion)
  cms <- costModels pp
  (epochInfo, systemStart) <- eraHistory (req ^. #eraHistory)
  withEra (ctx ^. #era) $ \(_ :: Proxy era) -> do
    env <- decodeEnv @era (req ^. #tx) (req ^. #utxo)
    let params :: PParams era
        params =
          emptyPParams
            & ppProtocolVersionL .~ pv
            & ppCostModelsL .~ mkCostModels cms
        utxo = envUtxo env
        tx = envTx env
        (_, needed) =
          resolveNeededPlutusScriptsWithPurpose
            pv
            (getScriptsProvided utxo tx)
            (getScriptsNeeded utxo (tx ^. bodyTxL))
            mempty
    case collectPlutusScriptsWithContext epochInfo systemStart params tx utxo of
      Left errs -> pure (defMessage & #rejection . #failures .~ map collectFailure (toList errs))
      Right pwcs -> case sequence (zipWith collectedProto (reverse (map fst needed)) pwcs) of
        Just scripts -> pure (defMessage & #scripts .~ sortOn (\c -> (fromEnum (c ^. #purpose . #tag), c ^. #purpose . #index)) scripts)
        Nothing -> unsupported "a collected script has no PlutusV1, V2 or V3 arguments"

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'PlutusCostModel q -> Just (respond #plutusCostModel (costModel q))
  Call'PlutusTxInfo q -> Just (respond #plutusTxInfo (txInfo q))
  Call'PlutusScriptPurpose q -> Just (respond #plutusScriptPurpose (scriptPurpose q))
  Call'PlutusScriptContext q -> Just (respond #plutusScriptContext (scriptContext q))
  Call'PlutusEvalExUnits q -> Just (respond #plutusEvalExUnits (evalExUnits q))
  Call'PlutusCollectScripts q -> Just (respond #plutusCollectScripts (collectScripts q))
  _ -> Nothing

implemented :: [Text]
implemented = ["plutus_cost_model", "plutus_tx_info", "plutus_script_purpose", "plutus_script_context", "plutus_eval_ex_units", "plutus_collect_scripts"]
