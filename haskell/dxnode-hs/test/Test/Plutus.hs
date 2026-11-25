{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Plutus (tests) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (TransactionScriptFailure (..), evalTxExUnits)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIxItem (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Conway.Scripts (ConwayEraScript (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.BaseTypes (txIxFromIntegral)
import Cardano.Ledger.Binary (DecoderError, decCBOR, decodeFull', decodeFullAnnotator)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Tx, TxLevel (..), emptyPParams, eraProtVerLow)
import Cardano.Ledger.Hashes (ScriptHash (..), unsafeMakeSafeHash)
import Cardano.Ledger.Alonzo.Core (ppCostModelsL, ppMaxTxExUnitsL)
import Cardano.Ledger.Core (ppProtocolVersionL)
import Cardano.Ledger.Plutus.CostModels (mkCostModel, mkCostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust, isJust)
import Data.Proxy (Proxy (..))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.TextFormat (readMessage)
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dxdingo.Dispatch (handle)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call, HarnessErrorCode (..), Reply)
import Proto.Dxdingo.V1.Plutus (ScriptPurposeTag (..))
import Proto.Dxdingo.V1.Types (Era (..), PlutusVersion (..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Util

-- | A corpus case, read from the repository's corpus so both nodes answer
-- the same requests. The cases are mainnet transactions with made-up or real
-- UTxOs, which the tests could not build by hand.
corpus :: FilePath -> Call
corpus name = unsafePerformIO $ do
  t <- TIO.readFile ("../../corpus/" <> name <> ".txtpb")
  either fail pure (readMessage t)
{-# NOINLINE corpus #-}

replyTo :: FilePath -> Reply
replyTo = handle . corpus

tests :: [Check]
tests =
  [ ("cost model: parameters are stored as supplied", costOf PLUTUS_VERSION_V2 (replicate 175 7) ^. #plutusCostModel . #params == replicate 175 7)
  , ("cost model: too few parameters are accepted", costOf PLUTUS_VERSION_V3 (replicate 100 7) ^. #plutusCostModel . #params == replicate 100 7)
  , ("cost model: nothing rejected", null (costOf PLUTUS_VERSION_V1 (replicate 166 7) ^. #plutusCostModel . #rejection . #failures))
  , ("cost model: unspecified language is UNSUPPORTED", costOf PLUTUS_VERSION_UNSPECIFIED [] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "script purpose: the one-shot mint has one mint purpose"
    , let ps = replyTo "plutus_script_purpose_conway_v3_one_shot_mint" ^. #plutusScriptPurpose . #purposes
       in map (^. #tag) ps == [SCRIPT_PURPOSE_TAG_MINT]
            && map (^. #index) ps == [0]
            && map (BS.unpack . (^. #mintPolicyId)) ps == [[0xeb, 0x55, 0xe4, 0x40, 0xda, 0x23, 0x5c, 0x80, 0x97, 0x4c, 0x78, 0x83, 0xb0, 0x92, 0x0c, 0x06, 0xb3, 0x1d, 0xda, 0xc6, 0x6c, 0x12, 0xc2, 0x38, 0x62, 0x62, 0x66, 0xe9]]
    )
  , ( "tx info: the one-shot mint's TxInfo holds its transaction id"
    , txIdIn (replyTo "plutus_tx_info_conway_v3_one_shot_mint_v3" ^. #plutusTxInfo . #txInfo)
    )
  , ( "eval: the one-shot mint's script succeeds"
    , let rs = replyTo "plutus_eval_ex_units_conway_v3_one_shot_mint" ^. #plutusEvalExUnits . #results
       in map (^. #ok) rs == [True] && all (\r -> r ^. #exMem > 0 && r ^. #exSteps > 0) rs
    )
  , ("eval: a failing script is ValidationFailure", evalFailures "plutus_eval_ex_units_babbage_v2_failing_script" == ["ValidationFailure"])
  , ("eval: a missing cost model is NoCostModelInLedgerState", evalFailures "plutus_eval_ex_units_missing_cost_model" == ["NoCostModelInLedgerState"])
  , ("tx info: V1 rejects a reference input in Babbage", rejection "plutus_tx_info_babbage_v2_spend_ref_script_v1" == ["ReferenceInputsNotSupported"])
  , ("tx info: V1 rejects an inline datum", rejection "plutus_tx_info_conway_v3_two_spends_ref_script_v1" == ["InlineDatumsNotSupported"])
  , ("tx info: a missing input", rejection "plutus_tx_info_missing_input" == ["TranslationLogicMissingInput"])
  , ("tx info: a slot past the era history", rejection "plutus_tx_info_past_horizon" == ["TimeTranslationPastHorizon"])
  , ("tx info: a language the era lacks is UNSUPPORTED", all (\n -> replyTo n ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED) ["plutus_tx_info_language_not_in_era", "plutus_tx_info_v2_in_alonzo"])
  , ( "script context: a V1 script is applied to datum, redeemer and context"
    , let a = contextArgs "plutus_script_context_v1_spend" in length a == 3 && take 2 a == [BS.pack [0x18, 0x2a], BS.pack [0xd8, 0x79, 0x80]]
    )
  , ("script context: without a datum, redeemer and context", length (contextArgs "plutus_script_context_v1_spend_no_datum") == 2)
  , ( "script context: a V3 script is applied to the context alone"
    , case contextArgs "plutus_script_context_v3_mint" of
        [c] -> BS.pack [0xd8, 0x79, 0x9f] `BS.isPrefixOf` c
        _ -> False
    )
  , ( "script context: V3 in Babbage is UNSUPPORTED"
    , let c = corpus "plutus_script_context_v3_mint"
          c' = c & #plutusScriptContext . #context . #era .~ ERA_BABBAGE
       in handle c' ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "eval: every redeemer has a result, two failing and one with units"
    , let rs = evalResults "plutus_eval_ex_units_conway_two_failing_one_ok"
       in length rs == 3 && map (^. #purpose . #index) rs == [0, 1, 2] && length (filter (^. #ok) rs) == 1 && all (\r -> r ^. #ok || r ^. #failure == "ValidationFailure") rs
    )
  , ("eval: all redeemers failing", map (^. #failure) (evalResults "plutus_eval_ex_units_conway_all_failing") == replicate 3 "ValidationFailure")
  , ("eval: all redeemers succeeding", let rs = evalResults "plutus_eval_ex_units_conway_three_languages" in length rs == 3 && all (\r -> r ^. #ok && r ^. #exMem > 0 && r ^. #exSteps > 0) rs)
  , ( "collect: three languages, each with its declared units"
    , let ss = collected "plutus_collect_scripts_conway_three_languages"
       in map (^. #purpose . #index) ss == [0, 1, 2]
            && map (^. #exMem) ss == [1000, 1001, 1002]
            && map (^. #exSteps) ss == [2000, 2001, 2002]
            && all (\c -> c ^. #purpose . #tag == SCRIPT_PURPOSE_TAG_MINT && BS.length (c ^. #scriptHash) == 28 && c ^. #scriptHash == c ^. #purpose . #mintPolicyId) ss
            && all (BS.null . (^. #datum)) ss
            && all (\c -> BS.null (c ^. #redeemer) == (c ^. #plutusVersion == PLUTUS_VERSION_V3) && not (BS.null (c ^. #context))) ss
    )
  , ( "collect: a spending script has a datum, a redeemer and a context"
    , case collected "plutus_collect_scripts_alonzo_v1_spend" of
        [c] -> c ^. #plutusVersion == PLUTUS_VERSION_V1 && c ^. #purpose . #tag == SCRIPT_PURPOSE_TAG_SPEND && not (BS.null (c ^. #datum)) && not (BS.null (c ^. #redeemer)) && not (BS.null (c ^. #context))
        _ -> False
    )
  , ("collect: a script with no redeemer is NoRedeemer", collectFailures "plutus_collect_scripts_conway_no_redeemer" == ["NoRedeemer"])
  , ("collect: a language with no cost model is NoCostModel", collectFailures "plutus_collect_scripts_conway_missing_cost_model" == ["NoCostModel"])
  , -- The reference stops applying scripts after the first failure, so two
    -- languages without models give one error.
    ("collect: two languages without cost models are one NoCostModel", collectFailures "plutus_collect_scripts_conway_two_missing_cost_models" == ["NoCostModel"])
  , -- Divergence confirmations call the reference directly, bypassing
    -- dxnode-hs. plutus.cost-model: plutigo refuses CEK step costs below 1
    -- and applies V1 parameters past the 166th (dxnode-go
    -- TestDivergenceZeroCekCostRejected, TestDivergenceExtraParamsAreApplied).
    ("divergence: mkCostModel accepts zero CEK step costs", isRight (mkCostModel PlutusV1 (replicate 166 0)))
  , ("divergence: mkCostModel ignores zero V1 parameters past the 166th", isRight (mkCostModel PlutusV1 (replicate 166 1 <> replicate 100 0)))
  , -- plutus.script-context: toPlutusScriptPurpose answers
    -- PlutusPurposeNotSupported for a voting purpose in V2 (dxnode-go
    -- TestDivergenceVotingPurposeInV2Panics).
    ("divergence: toPlutusScriptPurpose rejects a voting purpose in V2", votingInV2Rejected)
  , -- plutus.eval-ex-units: evalTxExUnits reports MissingScript for a redeemer
    -- whose script is absent; gouroboros' evaluator skips it (dxnode-go
    -- TestDivergenceMissingScriptIsSkipped).
    ("divergence: evalTxExUnits reports MissingScript for the withdrawal redeemer", missingScriptReported)
  , -- plutus.eval-ex-units: with no PlutusV2 cost model, evalTxExUnits fails
    -- only the V2 redeemer; dingo's cost-model rule rejects the whole
    -- transaction (dxnode-go TestDivergenceCostModelRuleIsPerTransaction).
    ("divergence: evalTxExUnits fails only the redeemer of the language with no cost model", missingModelFailsOneRedeemer)
  ]
  where
    costOf :: PlutusVersion -> [Int64] -> Reply
    costOf v ps = call (#plutusCostModel .~ (defMessage & #plutusVersion .~ v & #params .~ ps))
    contextArgs n = replyTo n ^. #plutusScriptContext . #args
    rejection n = map (^. #name) (replyTo n ^. #plutusTxInfo . #rejection . #failures)
    evalResults n = replyTo n ^. #plutusEvalExUnits . #results
    collected n = replyTo n ^. #plutusCollectScripts . #scripts
    collectFailures n = map (^. #name) (replyTo n ^. #plutusCollectScripts . #rejection . #failures)
    evalFailures n = [r ^. #failure | r <- replyTo n ^. #plutusEvalExUnits . #results, not (r ^. #ok)]
    txIdIn bytes = BS.pack [0xde, 0xab, 0x9e, 0xf3, 0xce, 0x74, 0x15, 0xe4, 0x92, 0xdd, 0x6f, 0x1e, 0xb5, 0xff, 0x29, 0x20] `BS.isInfixOf` bytes

-- | 'evalTxExUnits' on the divergence case's transaction and UTxO, decoded
-- with the ledger's own decoders.
missingScriptReported :: Bool
missingScriptReported =
  case (txE, utxoE) of
    (Right tx, Right utxo) ->
      let report = evalTxExUnits (emptyPParams @ConwayEra) tx (UTxO utxo) epochInfo systemStart
       in case Map.toList (Map.filterWithKey (\k _ -> isReward k) report) of
            [(_, Left (MissingScript _ _))] -> True
            _ -> False
    _ -> False
  where
    req = corpus "divergences/plutus_eval_ex_units_conway_v3_spends_and_withdrawal" ^. #plutusEvalExUnits
    txE = decodeFullAnnotator (eraProtVerLow @ConwayEra) "Tx" decCBOR (BSL.fromStrict (req ^. #tx)) :: Either DecoderError (Tx TopTx ConwayEra)
    utxoE = fmap Map.fromList (traverse entry (req ^. #utxo))
    entry e = do
      h <- maybe (Left "hash") Right (hashFromBytes (e ^. #txin . #txId))
      ix <- maybe (Left "ix") Right (txIxFromIntegral (e ^. #txin . #index))
      o <- either (Left . show) Right (decodeFull' (eraProtVerLow @ConwayEra) (e ^. #txout))
      pure (TxIn (TxId (unsafeMakeSafeHash h)) ix, o)
    epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)
    systemStart = SystemStart (posixSecondsToUTCTime 0)
    isReward k = isJust (toWithdrawingPurpose k)

-- | 'toPlutusScriptPurpose' for a voting purpose in the V2 context of Conway.
votingInV2Rejected :: Bool
votingInV2Rejected =
  isLeft $
    toPlutusScriptPurpose (Proxy @'PlutusV2) (ProtVer (natVersion @9) 0) (mkVotingPurpose @ConwayEra (AsIxItem 0 voter))
  where
    voter = DRepVoter (ScriptHashObj (ScriptHash (fromJust (hashFromBytes (BS.replicate 28 0x22)))))

-- | 'evalTxExUnits' on the divergence case's transaction, UTxO and cost
-- models, decoded with the ledger's own decoders and built with 'mkCostModel',
-- bypassing dxnode-hs. The V2 model is the one missing.
missingModelFailsOneRedeemer :: Bool
missingModelFailsOneRedeemer =
  case (txE, utxoE, models) of
    (Right tx, Right utxo, Right cms) ->
      let params =
            emptyPParams @ConwayEra
              & ppProtocolVersionL .~ ProtVer (natVersion @9) 0
              & ppCostModelsL .~ mkCostModels cms
              & ppMaxTxExUnitsL .~ ExUnits 14000000 10000000000
          report = evalTxExUnits params tx (UTxO utxo) epochInfo systemStart
          noModel = [() | Left (NoCostModelInLedgerState _) <- Map.elems report]
          withUnits = [() | Right _ <- Map.elems report]
       in length noModel == 1 && length withUnits == 2
    _ -> False
  where
    req = corpus "divergences/plutus_eval_ex_units_conway_mixed_missing_cost_model" ^. #plutusEvalExUnits
    pp = req ^. #pparams
    txE = decodeFullAnnotator (eraProtVerLow @ConwayEra) "Tx" decCBOR (BSL.fromStrict (req ^. #tx)) :: Either DecoderError (Tx TopTx ConwayEra)
    utxoE = fmap Map.fromList (traverse entry (req ^. #utxo))
    entry e = do
      h <- maybe (Left "hash") Right (hashFromBytes (e ^. #txin . #txId))
      ix <- maybe (Left "ix") Right (txIxFromIntegral (e ^. #txin . #index))
      o <- either (Left . show) Right (decodeFull' (eraProtVerLow @ConwayEra) (e ^. #txout))
      pure (TxIn (TxId (unsafeMakeSafeHash h)) ix, o)
    models = fmap Map.fromList (traverse model [(PlutusV1, pp ^. #costModelPlutusV1), (PlutusV2, pp ^. #costModelPlutusV2), (PlutusV3, pp ^. #costModelPlutusV3)] >>= pure . concat)
    model (_, []) = Right []
    model (l, ps) = either (Left . show) (\c -> Right [(l, c)]) (mkCostModel l ps)
    epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)
    systemStart = SystemStart (posixSecondsToUTCTime 0)
