{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Fee (tests) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Alonzo.PParams (LangDepView (..), getLanguageView)
import Cardano.Ledger.BaseTypes (Network (..), boundRational)
import Cardano.Ledger.Binary (decodeFullAnnotator)
import Cardano.Ledger.Binary (DecCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (ppDRepDepositL, ppGovActionDepositL)
import Cardano.Ledger.Conway.TxCert (pattern RegDepositTxCert, pattern UnRegDepositTxCert)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..), txscriptfee)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.State (UTxO (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
import Data.Ratio ((%))
import Data.Char (digitToInt)
import Data.Int (Int64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Data.Int (Int64)
import Data.Word (Word32, Word64, Word8)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..), Reply)
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Test.Util

-- Same cases as dxnode-go (cmd/dxnode-go/fee_test.go). Expected values are
-- derived by hand from the Haskell definitions:
--   sizeTxF: the Shelley transaction bytes; from Alonzo 1 + |body| + |wits| + |aux|.
--   getMinFeeTx: a * size + b + ceil(prMem*mem + prSteps*steps), plus the
--   tiered reference-script fee in Conway.
--   tierRefScriptFee: 25600-byte tiers, price times 6/5 per tier, floor once.
--   getMinCoinTxOut: Shelley the flat parameter, Alonzo coinsPerUTxOWord times
--   entry words, Babbage on (160 + |txout|) * coinsPerUTxOByte.
costModelV1 :: [Int64]
costModelV1 = [0, -1, 0, 1, 1, -1, 0, -1, 0, -1, 1, 1, 0, 1, 0, 0, -1, 0, -1, 0, 0, -1, 1, 0, 1, -1, 0, 1, 1, 0, 1, 1, -1, 1, 0, 1, 1, -1, -1, -1, 1, 1, -1, 0, 0, 1, 1, 0, -1, 1, -1, -1, -1, 0, -1, 1, -1, -1, 1, -1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, -1, -1, 0, -1, -1, 0, -1, 1, 1, -1, 0, 0, 1, 0, 1, 1, -1, -1, 0, -1, -1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, -1, 1, -1, -1, -1, -1, 1, 0, 1, -1, 1, 1, 1, 0, -1, 1, 1, 0, 0, 1, -1, 1, 1, 0, -1, -1, 0, -1, 1, 0, 1, 1, 1, 1, 1, -1, 0, 1, 1, 0, 0, -1, 0, 0, -1, -1, -1, -1, 0, -1, 0, -1, -1, 0, 0, 1, 0, 1]

costModelV2 :: [Int64]
costModelV2 = [-1, 1, -1, -1, -1, 0, 1, 1, 0, -1, -1, 0, -1, 0, 1, 1, 0, 0, -1, 1, -1, 0, -1, 0, 0, -1, 1, -1, 0, 1, 1, 0, -1, -1, -1, -1, 1, 0, -1, -1, 0, -1, 0, -1, 0, 1, 0, 1, -1, 1, -1, 1, -1, 0, -1, 1, -1, -1, -1, 0, -1, -1, 1, 0, -1, -1, -1, -1, 0, 0, -1, 0, 0, 0, 1, 1, -1, -1, 0, 0, 1, 0, 0, -1, 1, 0, 1, 1, -1, 1, 0, 0, 0, 1, -1, -1, -1, 1, 0, 0, -1, -1, 0, 1, 1, -1, 1, -1, -1, -1, -1, -1, -1, 0, 1, -1, 0, -1, 0, -1, 0, 0, 1, 1, 0, -1, 0, 0, -1, 1, 0, 1, 1, 1, -1, 0, -1, -1, -1, 0, 0, -1, 0, 1, 1, 0, 1, 1, 1, 1, 1, -1, 0, 1, -1, 1, 1, 0, -1, 1, 0, 1, 0, 1, -1, -1, 0, -1, -1, 1, 0, 1, -1, -1, -1]

hex :: String -> BS.ByteString
hex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = [] :: [Word8]

-- {0: [[0x01 * 32, 0]], 1: [[addr, 2000000]], 2: 200000, 3: 1000}
body :: BS.ByteString
body = hex "a40081825820010101010101010101010101010101010101010101010101010101010101010100018182581d61111111111111111111111111111111111111111111111111111111111a001e8480021a00030d40031903e8"

shelleyTx, conwayTx :: BS.ByteString
shelleyTx = BS.concat [BS.pack [0x83], body, BS.pack [0xa0, 0xf6]]
conwayTx = BS.concat [BS.pack [0x84], body, BS.pack [0xa0, 0xf5, 0xf6]]

-- Conway body spending 01..:0 and 03..:1, referencing 02..:0 and 03..:1.
refTx :: BS.ByteString
refTx = hex "84a40082825820010101010101010101010101010101010101010101010101010101010101010100825820030303030303030303030303030303030303030303030303030303030303030301018182581d61111111111111111111111111111111111111111111111111111111111a001e8480021a00030d401282825820020202020202020202020202020202020202020202020202020202020202020200825820030303030303030303030303030303030303030303030303030303030303030301a0f5f6"

-- Map-form outputs paying 1000000 (no script) and 5000000 with a Plutus V2
-- script reference of 10, 20 and 7 bytes.
out0, out10, out20, out7, alonzoOut :: BS.ByteString
out0 = hex "a200581d6111111111111111111111111111111111111111111111111111111111011a000f4240"
out10 = hex "a300581d6111111111111111111111111111111111111111111111111111111111011a004c4b4003d8184d82024a00000000000000000000"
out20 = hex "a300581d6111111111111111111111111111111111111111111111111111111111011a004c4b4003d818578202540000000000000000000000000000000000000000"
out7 = hex "a300581d6111111111111111111111111111111111111111111111111111111111011a004c4b4003d8184a82024700000000000000"
alonzoOut = hex "82581d61111111111111111111111111111111111111111111111111111111111a000f4240"


entry :: Word8 -> Word32 -> BS.ByteString -> S.UtxoEntry
entry b i out =
  defMessage
    & #txin .~ (defMessage & #txId .~ BS.replicate 32 b & #index .~ i)
    & #txout .~ out

code :: Reply -> HarnessErrorCode
code r = r ^. #error . #code

sizeOf :: P.Era -> BS.ByteString -> Reply
sizeOf era tx = call (#feeTxSize .~ (defMessage & #era .~ era & #tx .~ tx))

pparams :: S.ProtocolParameters
pparams =
  defMessage
    & #minFeeA .~ 44
    & #minFeeB .~ 155381
    & #priceMem .~ rat ("577", "10000")
    & #priceSteps .~ rat ("721", "10000000")
    & #minFeeRefScriptCostPerByte .~ rat ("15", "1")

minFeeOf :: P.Era -> S.ProtocolParameters -> BS.ByteString -> Word64 -> Reply
minFeeOf era pp tx ref =
  call (#feeMinFee .~ (defMessage & #era .~ era & #pparams .~ pp & #tx .~ tx & #refScriptSize .~ ref))

refFeeOf :: P.Era -> (Text, Text) -> Word64 -> Reply
refFeeOf era cost size =
  call (#feeRefScriptFee .~ (defMessage & #era .~ era & #costPerByte .~ rat cost & #size .~ size))

scriptFeeOf :: (Text, Text) -> (Text, Text) -> Word64 -> Word64 -> Reply
scriptFeeOf pm ps mem steps =
  call (#feeScriptFee .~ (defMessage & #priceMem .~ rat pm & #priceSteps .~ rat ps & #exMem .~ mem & #exSteps .~ steps))

minUtxoOf :: P.Era -> S.ProtocolParameters -> BS.ByteString -> Reply
minUtxoOf era pp out = call (#feeMinUtxo .~ (defMessage & #era .~ era & #pparams .~ pp & #txout .~ out))

integrityOf :: P.Era -> S.ProtocolParameters -> BS.ByteString -> BS.ByteString -> [P.PlutusVersion] -> Reply
integrityOf era pp rdmrs dats langs =
  call (#feeScriptIntegrityHash .~ (defMessage & #era .~ era & #pparams .~ pp
    & #redeemers .~ rdmrs & #datums .~ dats & #languages .~ langs))

refSizeOf :: P.Era -> [S.UtxoEntry] -> Reply
refSizeOf era es = call (#feeRefScriptSize .~ (defMessage & #era .~ era & #tx .~ refTx & #utxo .~ es))

rejectedAs :: Text -> Reply -> Bool
rejectedAs name r = case r ^. #feeTxSize . #rejection . #failures of
  [f] -> f ^. #name == name
  _ -> False

-- Conway transactions for fee_value_balance (see cmd/dxnode-go/fee_value_balance_test.go).
-- First: input 10000000 with assets a:5 b:4, withdrawal 1000000, mint a:+3 b:-2,
-- output 8300000 with a:8 b:2, fee 500000, reg_cert deposit 2000000, donation 200000.
balanceTx, balanceInput, refundTx, refundInput :: BS.ByteString
balanceTx = hex "84a70081825820010101010101010101010101010101010101010101010101010101010101010100018182581d6111111111111111111111111111111111111111111111111111111111821a007ea5e0a2581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1416108581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba1416202021a0007a120048183078200581c333333333333333333333333333333333333333333333333333333331a001e848005a1581de1222222222222222222222222222222222222222222222222222222221a000f424009a2581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1416103581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba1416221161a00030d40a0f5f6"
balanceInput = hex "82581d6111111111111111111111111111111111111111111111111111111111821a00989680a2581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1416105581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba1416204"
-- Input 5000000, unreg_cert refund 2000000, output 6800000, fee 200000.
refundTx = hex "84a40081825820010101010101010101010101010101010101010101010101010101010101010100018182581d61111111111111111111111111111111111111111111111111111111111a0067c280021a00030d40048183088200581c333333333333333333333333333333333333333333333333333333331a001e8480a0f5f6"
refundInput = hex "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40"

conwayPParams :: S.ProtocolParameters
conwayPParams =
  defMessage
    & #keyDeposit .~ 2000000
    & #poolDeposit .~ 500000000
    & #drepDeposit .~ 500000000
    & #govActionDeposit .~ 100000000000

balanceOf :: P.Era -> [S.UtxoEntry] -> [S.Account] -> BS.ByteString -> Reply
balanceOf era utxos accounts tx =
  call (#feeValueBalance .~ (defMessage & #context .~ (defMessage & #era .~ era) & #pparams .~ conwayPParams
    & #state .~ (defMessage & #utxo .~ utxos & #accounts .~ accounts) & #tx .~ tx))

assetIs :: Word8 -> BS.ByteString -> Int64 -> P.Asset -> Bool
assetIs policy name q a = a ^. #policyId == BS.replicate 28 policy && a ^. #assetName == name && a ^. #quantity == q

-- Direct calls to the reference libraries, with no adapter in between. Each
-- has a matching confirmation in cmd/dxnode-go/divergence_fee_test.go.

enterpriseAddr :: Addr
enterpriseAddr = Addr Testnet (KeyHashObj (KeyHash (fromJust (hashFromBytes (BS.replicate 28 0x11))))) StakeRefNull

-- One policy, one one-byte asset name, quantity 5.
maryOut :: TxOut MaryEra
maryOut = mkBasicTxOut enterpriseAddr (MaryValue (Coin 5000000) (MultiAsset (Map.singleton policy (Map.singleton (AssetName (SBS.pack [0x61])) 5))))
  where
    policy = PolicyID (ScriptHash (fromJust (hashFromBytes (BS.replicate 28 0xaa))))

conwayPP :: PParams ConwayEra
conwayPP = emptyPParams & ppKeyDepositL .~ Coin 2000000 & ppDRepDepositL .~ Coin 500000000 & ppGovActionDepositL .~ Coin 100000000000

stakeCred :: Credential Staking
stakeCred = KeyHashObj (KeyHash (fromJust (hashFromBytes (BS.replicate 28 0x33))))

bodyWith :: [TxCert ConwayEra] -> TxBody TopTx ConwayEra
bodyWith cs = mkBasicTxBody & certsTxBodyL .~ SSeq.fromList cs

divergences :: [Check]
divergences =
  [ -- scaledMinDeposit: max mv (quot mv 27 * (27 + size v)); size v is 12 words here.
    ( "divergence: mary min coin scales with the value size"
    , getMinCoinTxOut (emptyPParams & ppMinUTxOValueL .~ Coin 1000000) maryOut == Coin 1444443
    )
  , -- ExUnits are Word64: 2^63 * (1 / 10^9) rounded up.
    ( "divergence: script fee prices units above int64"
    , txscriptfee (Prices (fromJust (boundRational (1 % 1000000000))) (fromJust (boundRational 0))) (ExUnits (2 ^ (63 :: Int)) 0) == Coin 9223372037
    )
  , ( "divergence: reference scripts of absent inputs count nothing"
    , case decodeFullAnnotator (eraProtVerLow @ConwayEra) "Tx" decCBOR (BSL.fromStrict refTx) of
        Right tx -> txNonDistinctRefScriptsSize (UTxO Map.empty :: UTxO ConwayEra) (tx :: Tx TopTx ConwayEra) == 0
        Left _ -> False
    )
  , -- A model the parameters lack is a null view: tag 0x02 and f6.
    ( "divergence: a language without a cost model has a null view"
    , getLanguageView conwayPP PlutusV3 == LangDepView (BS.pack [0x02]) (BS.pack [0xf6])
    )
  , -- The certificate's own amount is not read: keyDeposit is charged, the
    -- account's deposit is refunded, and an unregistered credential refunds nothing.
    ( "divergence: registration deposit is keyDeposit, not the certificate amount"
    , getTotalDepositsTxBody conwayPP (const False) (bodyWith [RegDepositTxCert stakeCred (Coin 1000000)]) == Coin 2000000
    )
  , ( "divergence: deregistration refunds the account deposit, not the certificate amount"
    , getTotalRefundsTxBody conwayPP (const (Just (Coin 2000000))) (bodyWith [UnRegDepositTxCert stakeCred (Coin 1000000)]) == Coin 2000000
    )
  , ( "divergence: deregistering an unregistered credential refunds nothing"
    , getTotalRefundsTxBody conwayPP (const Nothing) (bodyWith [UnRegDepositTxCert stakeCred (Coin 1000000)]) == Coin 0
    )
  ]

tests :: [Check]
tests =
  divergences ++
  [ ("tx size: shelley is the transaction bytes", sizeOf P.ERA_SHELLEY shelleyTx ^. #feeTxSize . #size == fromIntegral (BS.length shelleyTx))
  , ( "tx size: conway omits the IsValid byte"
    , sizeOf P.ERA_CONWAY conwayTx ^. #feeTxSize . #size == fromIntegral (1 + BS.length body + 1 + 1)
    )
  , ("tx size: truncated tx is DeserialiseFailure", rejectedAs "DeserialiseFailure" (sizeOf P.ERA_CONWAY (BS.take 10 conwayTx)))
  , ("tx size: byron is UNSUPPORTED", code (sizeOf P.ERA_BYRON conwayTx) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "min fee: conway size fee"
    , minFeeOf P.ERA_CONWAY pparams conwayTx 0 ^. #feeMinFee . #minFee == 44 * fromIntegral (BS.length body + 3) + 155381
    )
  , ( "min fee: conway reference scripts, first tier"
    , minFeeOf P.ERA_CONWAY pparams conwayTx 30 ^. #feeMinFee . #minFee == 44 * fromIntegral (BS.length body + 3) + 155381 + 450
    )
  , ( "min fee: shelley ignores the reference-script size"
    , minFeeOf P.ERA_SHELLEY pparams shelleyTx 30 ^. #feeMinFee . #minFee == 44 * fromIntegral (BS.length shelleyTx) + 155381
    )
  , ("min fee: missing min_fee_a is BAD_REQUEST", code (minFeeOf P.ERA_CONWAY (defMessage & #minFeeB .~ 1) conwayTx 0) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("min fee: dijkstra is UNSUPPORTED", code (minFeeOf P.ERA_DIJKSTRA pparams conwayTx 0) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "min fee: result over a Word64 is UNSUPPORTED"
    , code (minFeeOf P.ERA_SHELLEY (defMessage & #minFeeA .~ maxBound & #minFeeB .~ 0) shelleyTx 0) == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ("ref script fee: second tier", refFeeOf P.ERA_CONWAY ("15", "1") 30000 ^. #feeRefScriptFee . #fee == 25600 * 15 + 4400 * 18)
  , ("ref script fee: floors once", refFeeOf P.ERA_CONWAY ("1", "3") 25601 ^. #feeRefScriptFee . #fee == 8533)
  , ("ref script fee: empty", refFeeOf P.ERA_CONWAY ("15", "1") 0 ^. #feeRefScriptFee . #fee == 0)
  , ("ref script fee: exact tier", refFeeOf P.ERA_CONWAY ("15", "1") 25600 ^. #feeRefScriptFee . #fee == 384000)
  , ("ref script fee: babbage is UNSUPPORTED", code (refFeeOf P.ERA_BABBAGE ("1", "1") 1) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("ref script fee: negative cost is UNSUPPORTED", code (refFeeOf P.ERA_CONWAY ("-1", "1") 1) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("ref script fee: size past the bound is UNSUPPORTED", code (refFeeOf P.ERA_CONWAY ("1", "1") (2 ^ (29 :: Int))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("ref script fee: zero denominator is BAD_REQUEST", code (refFeeOf P.ERA_CONWAY ("1", "0") 1) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("script fee: priced units", scriptFeeOf ("577", "10000") ("721", "10000000") 14000000 10000000000 ^. #feeScriptFee . #fee == 1528800)
  , ("script fee: ceiling", scriptFeeOf ("1", "3") ("0", "1") 1 0 ^. #feeScriptFee . #fee == 1)
  , ("script fee: one ceiling over the sum", scriptFeeOf ("1", "2") ("1", "2") 1 1 ^. #feeScriptFee . #fee == 1)
  , ("script fee: negative price is UNSUPPORTED", code (scriptFeeOf ("-1", "2") ("1", "2") 0 0) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("min utxo: babbage", minUtxoOf P.ERA_BABBAGE byteCost out0 ^. #feeMinUtxo . #minCoin == (160 + 39) * 4310)
  , ("min utxo: conway", minUtxoOf P.ERA_CONWAY byteCost out0 ^. #feeMinUtxo . #minCoin == (160 + 39) * 4310)
  , ("min utxo: alonzo words", minUtxoOf P.ERA_ALONZO (defMessage & #coinsPerUtxoWord .~ 34482) alonzoOut ^. #feeMinUtxo . #minCoin == 999978)
  , ("min utxo: shelley flat", minUtxoOf P.ERA_SHELLEY flat alonzoOut ^. #feeMinUtxo . #minCoin == 1000000)
  , ("min utxo: allegra flat", minUtxoOf P.ERA_ALLEGRA flat alonzoOut ^. #feeMinUtxo . #minCoin == 1000000)
  , ("min utxo: missing parameter is BAD_REQUEST", code (minUtxoOf P.ERA_BABBAGE defMessage out0) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("min utxo: byron is UNSUPPORTED", code (minUtxoOf P.ERA_BYRON byteCost out0) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "integrity hash: conway, plutus v2"
    , integrityOf P.ERA_CONWAY (defMessage & #costModelPlutusV2 .~ costModelV2) (hex "a18200008200820000") BS.empty [P.PLUTUS_VERSION_V2]
        ^. #feeScriptIntegrityHash . #hash == hex "6e9284e6d25df53370569c54c31a4a5a6bc7c35069638217e149aa0386353ab8"
    )
  , ( "integrity hash: alonzo, plutus v1 with datums"
    , integrityOf P.ERA_ALONZO (defMessage & #costModelPlutusV1 .~ costModelV1) (hex "8184000000820000") (BS.pack [0x81, 0x01]) [P.PLUTUS_VERSION_V1]
        ^. #feeScriptIntegrityHash . #hash == hex "a08dacb8cb9a2c774e5a445ce066887169eb458798aff8bd989308c30bbbf24c"
    )
  , ("integrity hash: nothing to hash", BS.null (integrityOf P.ERA_CONWAY defMessage BS.empty BS.empty [] ^. #feeScriptIntegrityHash . #hash))
  , ("integrity hash: plutus v4 is UNSUPPORTED", code (integrityOf P.ERA_CONWAY defMessage (BS.pack [0xa0]) BS.empty [P.PLUTUS_VERSION_V4]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "ref script size: spending and reference inputs, shared input once"
    , refSizeOf P.ERA_CONWAY [entry 1 0 out10, entry 2 0 out20, entry 3 1 out7] ^. #feeRefScriptSize . #size == 37
    )
  , ( "ref script size: outputs without a script count nothing"
    , refSizeOf P.ERA_BABBAGE [entry 1 0 out0, entry 2 0 out20, entry 3 1 out0] ^. #feeRefScriptSize . #size == 20
    )
  , ("ref script size: alonzo is UNSUPPORTED", code (refSizeOf P.ERA_ALONZO [entry 1 0 out0]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "value balance: coin totals, deposits and refunds"
    , let r = balanceOf P.ERA_CONWAY [entry 1 0 balanceInput] [] balanceTx ^. #feeValueBalance
       in r ^. #consumedCoin == 11000000 && r ^. #producedCoin == 11000000 && r ^. #deposits == 2000000 && r ^. #refunds == 0
    )
  , ( "value balance: consumed assets are inputs plus the positive mint"
    , let r = balanceOf P.ERA_CONWAY [entry 1 0 balanceInput] [] balanceTx ^. #feeValueBalance
       in case r ^. #consumedAssets of
            [a, b] -> assetIs 0xaa "a" 8 a && assetIs 0xbb "b" 4 b
            _ -> False
    )
  , ( "value balance: produced assets are the outputs plus the burn"
    , let r = balanceOf P.ERA_CONWAY [entry 1 0 balanceInput] [] balanceTx ^. #feeValueBalance
       in case r ^. #producedAssets of
            [a, b] -> assetIs 0xaa "a" 8 a && assetIs 0xbb "b" 4 b
            _ -> False
    )
  , ( "value balance: an unresolved input counts nothing"
    , balanceOf P.ERA_CONWAY [] [] balanceTx ^. #feeValueBalance . #consumedCoin == 1000000
    )
  , ( "value balance: refund from the account deposit"
    , let acct = defMessage & #credential .~ (defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ BS.replicate 28 0x33) & #deposit .~ 2000000
          r = balanceOf P.ERA_CONWAY [entry 1 0 refundInput] [acct] refundTx ^. #feeValueBalance
       in r ^. #refunds == 2000000 && r ^. #consumedCoin == 7000000 && r ^. #producedCoin == 7000000
    )
  , ("value balance: babbage is UNSUPPORTED", code (balanceOf P.ERA_BABBAGE [] [] balanceTx) == HARNESS_ERROR_CODE_UNSUPPORTED)
  ]
  where
    byteCost = defMessage & #coinsPerUtxoByte .~ 4310
    flat = defMessage & #minUtxoValue .~ 1000000
