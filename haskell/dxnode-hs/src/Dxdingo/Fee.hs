{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Fee functions, answered by cardano-ledger-core and the era packages.
--
-- Eras are Shelley through Conway. Byron has no such rules and Dijkstra is
-- not modelled on either node, so both reply UNSUPPORTED. A result that does
-- not fit a Word64, or reaches the largest one, is UNSUPPORTED on both nodes:
-- dxnode-go calls dingo, which saturates at that value, while the reference
-- computes in unbounded integers.
module Dxdingo.Fee
  ( dispatch
  , implemented
  , txSize
  , minFee
  , refScriptSize
  , refScriptFee
  , scriptFee
  , minUtxo
  , scriptIntegrityHash
  , valueBalance
  ) where

import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, AlonzoEraScript)
import Cardano.Ledger.Alonzo.PParams (CoinPerWord (..), getLanguageView, ppCoinsPerUTxOWordL, ppCostModelsL, ppPricesL)
import Cardano.Ledger.Alonzo.Tx (ScriptIntegrity (..), hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.PParams (ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes (NonNegativeInterval, TxIx (..), unNonZero, unboundRational)
import Cardano.Ledger.Binary (Annotator, Sized, DecCBOR (..), decodeFull', decodeFullAnnotator)
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (ppDRepDepositL, ppGovActionDepositL, ppMinFeeRefScriptCostPerByteL, ppRefScriptCostMultiplierG, ppRefScriptCostStrideG)
import Cardano.Ledger.Conway.Tx (tierRefScriptFee)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Plutus.CostModels (CostModels, mkCostModel, mkCostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..), txscriptfee)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.State (EraUTxO (..), UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (fromShort)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Word (Word16, Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Fee
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- | Run an action at the era's type. Only Shelley through Conway.
withTxEra :: Text -> P.Era -> (forall era. EraTx era => Proxy era -> r) -> Result r
withTxEra fn era k = case era of
  P.ERA_SHELLEY -> pure (k (Proxy @ShelleyEra))
  P.ERA_ALLEGRA -> pure (k (Proxy @AllegraEra))
  P.ERA_MARY -> pure (k (Proxy @MaryEra))
  P.ERA_ALONZO -> pure (k (Proxy @AlonzoEra))
  P.ERA_BABBAGE -> pure (k (Proxy @BabbageEra))
  P.ERA_CONWAY -> pure (k (Proxy @ConwayEra))
  _ -> unsupported (fn <> ": era " <> T.pack (show era) <> " is not modelled")

deserialiseFailure :: Text -> String -> P.Rejection
deserialiseFailure rule err =
  defMessage
    & #failures
      .~ [ defMessage
            & #rule .~ rule
            & #name .~ "DeserialiseFailure"
            & #detail .~ T.pack err
         ]

decodeTx :: forall era. EraTx era => BS.ByteString -> Either String (Tx TopTx era)
decodeTx raw = either (Left . show) Right $
  decodeFullAnnotator (eraProtVerLow @era) "Tx" decCBOR (BSL.fromStrict raw)

-- | The tiered fee loops once per 25600 bytes over a growing rational, so an
-- absurd size would never finish on either node. Both reply UNSUPPORTED above it.
maxRefScriptSize :: Word64
maxRefScriptSize = 2 ^ (28 :: Int)

maxWord64 :: Integer
maxWord64 = toInteger (maxBound :: Word64)

-- | A coin result as a Word64; the largest value is treated as saturated.
word64Result :: Text -> Coin -> Result Word64
word64Result fn (Coin c)
  | c < 0 || c >= maxWord64 = unsupported (fn <> ": result does not fit a Word64")
  | otherwise = pure (fromInteger c)

requiredParam :: Text -> Maybe Word64 -> Result Word64
requiredParam name = maybe (badRequest ("pparams." <> name <> " is required")) pure

nonNegative :: Text -> Maybe P.Rational -> Result NonNegativeInterval
nonNegative = boundedArg

-- | 'sizeTxF', the size every fee and size rule uses.
txSize :: FeeTxSizeRequest -> Result FeeTxSizeResponse
txSize req = do
  r <- withTxEra "fee_tx_size" (req ^. #era) (\(_ :: Proxy era) -> go @era)
  r
  where
    go :: forall era. EraTx era => Result FeeTxSizeResponse
    go = pure $ case decodeTx @era (req ^. #tx) of
      Left err -> defMessage & #rejection .~ deserialiseFailure "tx" err
      Right tx -> defMessage & #size .~ fromIntegral (tx ^. sizeTxF)

-- | 'getMinFeeTx'. Prices apply from Alonzo and the reference-script fee only
-- in Conway; earlier eras ignore them.
minFee :: FeeMinFeeRequest -> Result FeeMinFeeResponse
minFee req = do
  let pp = req ^. #pparams
  a <- requiredParam "min_fee_a" (pp ^. #maybe'minFeeA)
  b <- requiredParam "min_fee_b" (pp ^. #maybe'minFeeB)
  let base :: forall era. EraTx era => PParams era -> PParams era
      base x =
        x
          & ppTxFeePerByteL .~ CoinPerByte (CompactCoin a)
          & ppTxFeeFixedL .~ Coin (toInteger b)
      prices = do
        pm <- nonNegative "price_mem" (pp ^. #maybe'priceMem)
        ps <- nonNegative "price_steps" (pp ^. #maybe'priceSteps)
        pure (Prices pm ps)
  case req ^. #era of
    P.ERA_SHELLEY -> run @ShelleyEra base 0
    P.ERA_ALLEGRA -> run @AllegraEra base 0
    P.ERA_MARY -> run @MaryEra base 0
    P.ERA_ALONZO -> do
      pr <- prices
      run @AlonzoEra (\x -> base x & ppPricesL .~ pr) 0
    P.ERA_BABBAGE -> do
      pr <- prices
      run @BabbageEra (\x -> base x & ppPricesL .~ pr) 0
    P.ERA_CONWAY -> do
      pr <- prices
      cost <- nonNegative "min_fee_ref_script_cost_per_byte" (pp ^. #maybe'minFeeRefScriptCostPerByte)
      size <- refSize
      run @ConwayEra (\x -> base x & ppPricesL .~ pr & ppMinFeeRefScriptCostPerByteL .~ cost) size
    era -> unsupported ("fee_min_fee: era " <> T.pack (show era) <> " is not modelled")
  where
    refSize
      | req ^. #refScriptSize > maxRefScriptSize = unsupported "ref_script_size is above the modelled bound"
      | otherwise = pure (fromIntegral (req ^. #refScriptSize))
    run :: forall era. EraTx era => (PParams era -> PParams era) -> Int -> Result FeeMinFeeResponse
    run set size = case decodeTx @era (req ^. #tx) of
      Left err -> pure (defMessage & #rejection .~ deserialiseFailure "tx" err)
      Right tx -> do
        fee <- word64Result "fee_min_fee" (getMinFeeTx (set emptyPParams) tx size)
        pure (defMessage & #minFee .~ fee)

mkTxIn :: P.TxIn -> Result TxIn
mkTxIn t
  | t ^. #index > fromIntegral (maxBound :: Word16) = unsupported "tx_in index does not fit a Word16"
  | otherwise = case hashFromBytes (t ^. #txId) of
      Nothing -> badRequest "tx_id is not 32 bytes"
      Just h -> pure (TxIn (TxId (unsafeMakeSafeHash h)) (TxIx (fromIntegral (t ^. #index))))


-- | 'txNonDistinctRefScriptsSize' over the request's UTxO. An input in both the
-- spending and reference sets is counted once.
refScriptSize :: FeeRefScriptSizeRequest -> Result FeeRefScriptSizeResponse
refScriptSize req = do
  r <- case req ^. #era of
    P.ERA_BABBAGE -> pure (go @BabbageEra)
    P.ERA_CONWAY -> pure (go @ConwayEra)
    era -> unsupported ("fee_ref_script_size: era " <> T.pack (show era) <> " is not modelled")
  r
  where
    go :: forall era. (EraTx era, BabbageEraTxBody era) => Result FeeRefScriptSizeResponse
    go = do
      entries <- traverse entry (req ^. #utxo)
      let utxo = UTxO (Map.fromList entries) :: UTxO era
      pure $ case decodeTx @era (req ^. #tx) of
        Left err -> defMessage & #rejection .~ deserialiseFailure "tx" err
        Right tx -> defMessage & #size .~ fromIntegral (txNonDistinctRefScriptsSize utxo tx)
    entry :: forall era. EraTx era => S.UtxoEntry -> Result (TxIn, TxOut era)
    entry e = do
      i <- mkTxIn (e ^. #txin)
      case decodeFull' (eraProtVerLow @era) (e ^. #txout) of
        Left err -> badRequest ("utxo txout: " <> T.pack (show err))
        Right o -> pure (i, o)

-- | 'tierRefScriptFee' with Conway's multiplier and stride.
refScriptFee :: FeeRefScriptFeeRequest -> Result FeeRefScriptFeeResponse
refScriptFee req = do
  case req ^. #era of
    P.ERA_CONWAY -> pure ()
    era -> unsupported ("fee_ref_script_fee: era " <> T.pack (show era) <> " is not modelled")
  cost <- nonNegative "cost_per_byte" (req ^. #maybe'costPerByte)
  if req ^. #size > maxRefScriptSize
    then unsupported "size is above the modelled bound"
    else do
      let pp = emptyPParams :: PParams ConwayEra
          mult = unboundRational (pp ^. ppRefScriptCostMultiplierG)
          stride = fromIntegral (unNonZero (pp ^. ppRefScriptCostStrideG))
      fee <- word64Result "fee_ref_script_fee" (tierRefScriptFee mult stride (unboundRational cost) (fromIntegral (req ^. #size)))
      pure (defMessage & #fee .~ fee)

-- | 'txscriptfee'.
scriptFee :: FeeScriptFeeRequest -> Result FeeScriptFeeResponse
scriptFee req = do
  pm <- nonNegative "price_mem" (req ^. #maybe'priceMem)
  ps <- nonNegative "price_steps" (req ^. #maybe'priceSteps)
  fee <- word64Result "fee_script_fee" (txscriptfee (Prices pm ps) (ExUnits (fromIntegral (req ^. #exMem)) (fromIntegral (req ^. #exSteps))))
  pure (defMessage & #fee .~ fee)

-- | 'getMinCoinSizedTxOut' on the output decoded as 'Sized', the way the UTXO
-- rule prices an output: by the size of its encoding on the wire, which for a
-- map-form Babbage output differs from its canonical re-encoding.
minUtxo :: FeeMinUtxoRequest -> Result FeeMinUtxoResponse
minUtxo req = do
  let pp = req ^. #pparams
  case req ^. #era of
    P.ERA_SHELLEY -> do
      v <- requiredParam "min_utxo_value" (pp ^. #maybe'minUtxoValue)
      run @ShelleyEra (ppMinUTxOValueL .~ Coin (toInteger v))
    P.ERA_ALLEGRA -> do
      v <- requiredParam "min_utxo_value" (pp ^. #maybe'minUtxoValue)
      run @AllegraEra (ppMinUTxOValueL .~ Coin (toInteger v))
    P.ERA_MARY -> do
      v <- requiredParam "min_utxo_value" (pp ^. #maybe'minUtxoValue)
      run @MaryEra (ppMinUTxOValueL .~ Coin (toInteger v))
    P.ERA_ALONZO -> do
      v <- requiredParam "coins_per_utxo_word" (pp ^. #maybe'coinsPerUtxoWord)
      run @AlonzoEra (ppCoinsPerUTxOWordL .~ CoinPerWord (Coin (toInteger v)))
    P.ERA_BABBAGE -> do
      v <- requiredParam "coins_per_utxo_byte" (pp ^. #maybe'coinsPerUtxoByte)
      run @BabbageEra (ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin v))
    P.ERA_CONWAY -> do
      v <- requiredParam "coins_per_utxo_byte" (pp ^. #maybe'coinsPerUtxoByte)
      run @ConwayEra (ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin v))
    era -> unsupported ("fee_min_utxo: era " <> T.pack (show era) <> " is not modelled")
  where
    run :: forall era. EraTxOut era => (PParams era -> PParams era) -> Result FeeMinUtxoResponse
    run set = case decodeFull' (eraProtVerLow @era) (req ^. #txout) of
      Left err -> badRequest ("txout: " <> T.pack (show err))
      Right out -> do
        c <- word64Result "fee_min_utxo" (getMinCoinSizedTxOut (set emptyPParams) (out :: Sized (TxOut era)))
        pure (defMessage & #minCoin .~ c)

-- | 'hashScriptIntegrity' over the redeemers and datums as given and the
-- language views of the listed versions. Nothing is hashed when there are no
-- redeemers, datums or languages. Datums without redeemers hash the era's
-- empty redeemers.
scriptIntegrityHash :: FeeScriptIntegrityHashRequest -> Result FeeScriptIntegrityHashResponse
scriptIntegrityHash req = do
  langs <- traverse language (req ^. #languages)
  let costModels =
        mkCostModels . Map.fromList $
          [ (l, cm)
          | l <- langs
          , Right cm <- [mkCostModel l (modelOf l)]
          ]
  case req ^. #era of
    P.ERA_ALONZO -> run @AlonzoEra langs costModels
    P.ERA_BABBAGE -> run @BabbageEra langs costModels
    P.ERA_CONWAY -> run @ConwayEra langs costModels
    era -> unsupported ("fee_script_integrity_hash: era " <> T.pack (show era) <> " is not modelled")
  where
    pp = req ^. #pparams
    modelOf = \case
      PlutusV1 -> pp ^. #costModelPlutusV1
      PlutusV2 -> pp ^. #costModelPlutusV2
      _ -> pp ^. #costModelPlutusV3
    language = \case
      P.PLUTUS_VERSION_V1 -> pure PlutusV1
      P.PLUTUS_VERSION_V2 -> pure PlutusV2
      P.PLUTUS_VERSION_V3 -> pure PlutusV3
      v -> unsupported ("language " <> T.pack (show v) <> " is not modelled")
    run :: forall era. (AlonzoEraPParams era, AlonzoEraScript era) => [Language] -> CostModels -> Result FeeScriptIntegrityHashResponse
    run langs cms
      | BS.null (req ^. #redeemers) && BS.null (req ^. #datums) && null langs = pure defMessage
      | otherwise = do
          let v = eraProtVerLow @era
              decodeBytes :: DecCBOR (Annotator a) => Text -> BS.ByteString -> Result a
              decodeBytes what bytes = case decodeFullAnnotator v what decCBOR (BSL.fromStrict bytes) of
                Left err -> badRequest (what <> ": " <> T.pack (show err))
                Right x -> pure x
          rdmrs <-
            if BS.null (req ^. #redeemers)
              then pure (Redeemers Map.empty :: Redeemers era)
              else decodeBytes "Redeemers" (req ^. #redeemers)
          dats <-
            if BS.null (req ^. #datums)
              then pure (TxDats Map.empty :: TxDats era)
              else decodeBytes "TxDats" (req ^. #datums)
          let ppE = (emptyPParams :: PParams era) & ppCostModelsL .~ cms
              views = Set.fromList (map (getLanguageView ppE) langs)
              h = hashScriptIntegrity (ScriptIntegrity rdmrs dats views)
          pure (defMessage & #hash .~ hashToBytes (extractHash h))

-- | 'getConsumedValue' and 'getProducedValue' with the deposit and refund
-- totals ('getTotalDepositsTxBody', 'getTotalRefundsTxBody'). Conway only. A
-- stake credential's deposit is its account's, and a pool is registered when
-- it is listed as current.
valueBalance :: FeeValueBalanceRequest -> Result FeeValueBalanceResponse
valueBalance req = do
  case req ^. #context . #era of
    P.ERA_CONWAY -> pure ()
    era -> unsupported ("fee_value_balance: era " <> T.pack (show era) <> " is not modelled")
  let pparams = req ^. #pparams
  keyDeposit <- requiredParam "key_deposit" (pparams ^. #maybe'keyDeposit)
  poolDeposit <- requiredParam "pool_deposit" (pparams ^. #maybe'poolDeposit)
  drepDeposit <- requiredParam "drep_deposit" (pparams ^. #maybe'drepDeposit)
  govDeposit <- requiredParam "gov_action_deposit" (pparams ^. #maybe'govActionDeposit)
  let pp =
        (emptyPParams :: PParams ConwayEra)
          & ppKeyDepositL .~ Coin (toInteger keyDeposit)
          & ppPoolDepositL .~ Coin (toInteger poolDeposit)
          & ppDRepDepositL .~ Coin (toInteger drepDeposit)
          & ppGovActionDepositL .~ Coin (toInteger govDeposit)
  entries <- traverse utxoEntry (req ^. #state . #utxo)
  accounts <- traverse account (req ^. #state . #accounts)
  pools <- traverse pool [p | p <- req ^. #state . #pools, p ^. #stage == S.POOL_STAGE_CURRENT]
  case decodeTx @ConwayEra (req ^. #tx) of
    Left err -> pure (defMessage & #rejection .~ deserialiseFailure "tx" err)
    Right tx -> do
      let body = tx ^. bodyTxL
          utxo = UTxO (Map.fromList entries) :: UTxO ConwayEra
          deposits = Map.fromList accounts
          poolSet = Set.fromList pools
          lookupDeposit c = Map.lookup c deposits
          MaryValue consumed consumedAssets = getConsumedValue pp lookupDeposit utxo body
          MaryValue produced producedAssets = getProducedValue pp (`Set.member` poolSet) body
          refunds = getTotalRefundsTxBody pp lookupDeposit body
          deposited = getTotalDepositsTxBody pp (`Set.member` poolSet) body
      cc <- coinResult consumed
      pc <- coinResult produced
      dc <- coinResult deposited
      rc <- coinResult refunds
      ca <- assetsOf consumedAssets
      pa <- assetsOf producedAssets
      pure $
        defMessage
          & #consumedCoin .~ cc
          & #consumedAssets .~ ca
          & #producedCoin .~ pc
          & #producedAssets .~ pa
          & #deposits .~ dc
          & #refunds .~ rc
  where
    utxoEntry :: S.UtxoEntry -> Result (TxIn, TxOut ConwayEra)
    utxoEntry e = do
      i <- mkTxIn (e ^. #txin)
      case decodeFull' (eraProtVerLow @ConwayEra) (e ^. #txout) of
        Left err -> badRequest ("utxo txout: " <> T.pack (show err))
        Right o -> pure (i, o)
    credentialOf :: P.Credential -> Result (Credential Staking)
    credentialOf c = case c ^. #kind of
      P.CREDENTIAL_KIND_KEY -> maybe bad (pure . KeyHashObj . KeyHash) (hashFromBytes (c ^. #hash))
      P.CREDENTIAL_KIND_SCRIPT -> maybe bad (pure . ScriptHashObj . ScriptHash) (hashFromBytes (c ^. #hash))
      _ -> badRequest "credential kind is unspecified"
      where
        bad = badRequest "credential hash is not 28 bytes"
    account a = do
      c <- credentialOf (a ^. #credential)
      pure (c, Coin (toInteger (a ^. #deposit)))
    pool p = case hashFromBytes (p ^. #poolId) of
      Nothing -> badRequest "pool_id is not 28 bytes"
      Just h -> pure (KeyHash h)
    coinResult :: Coin -> Result Word64
    coinResult (Coin c)
      | c < 0 || c > maxWord64 = unsupported "fee_value_balance: coin total does not fit a Word64"
      | otherwise = pure (fromInteger c)
    assetsOf :: MultiAsset -> Result [P.Asset]
    assetsOf (MultiAsset m) =
      traverse asset $
        sortOn (\(p, n, _) -> (p, n))
          [ (hashToBytes h, fromShort (assetNameBytes n), q)
          | (PolicyID (ScriptHash h), names) <- Map.toList m
          , (n, q) <- Map.toList names
          , q /= 0
          ]
    asset (p, n, q)
      | q < toInteger (minBound :: Int64) || q > toInteger (maxBound :: Int64) =
          unsupported "fee_value_balance: asset quantity does not fit an Int64"
      | otherwise = pure (defMessage & #policyId .~ p & #assetName .~ n & #quantity .~ fromInteger q)

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'FeeMinFee q -> Just (respond #feeMinFee (minFee q))
  Call'FeeRefScriptSize q -> Just (respond #feeRefScriptSize (refScriptSize q))
  Call'FeeRefScriptFee q -> Just (respond #feeRefScriptFee (refScriptFee q))
  Call'FeeTxSize q -> Just (respond #feeTxSize (txSize q))
  Call'FeeScriptFee q -> Just (respond #feeScriptFee (scriptFee q))
  Call'FeeMinUtxo q -> Just (respond #feeMinUtxo (minUtxo q))
  Call'FeeScriptIntegrityHash q -> Just (respond #feeScriptIntegrityHash (scriptIntegrityHash q))
  Call'FeeValueBalance q -> Just (respond #feeValueBalance (valueBalance q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "fee_min_fee"
  , "fee_ref_script_size"
  , "fee_ref_script_fee"
  , "fee_tx_size"
  , "fee_script_fee"
  , "fee_min_utxo"
  , "fee_script_integrity_hash"
  , "fee_value_balance"
  ]
