{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transaction rules. The same cases run in dxnode-go (cmd/dxnode-go/rule_test.go):
-- a mainnet Conway transaction with one synthesised input holding exactly
-- outputs plus fee.
module Test.Rule (tests) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Numeric (readHex)
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Rule (RuleUtxoRequest, RuleUtxosRequest, RuleUtxosResponse, RuleUtxowRequest)
import Proto.Dxdingo.V1.State (EraHistory, LedgerState, ProtocolParameters)
import Proto.Dxdingo.V1.Types (Era (..))
import qualified Test.RuleCerts
import qualified Test.RuleLedger
import Test.Util
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalPlutusScripts)
import Cardano.Ledger.Plutus.Evaluate (ScriptResult (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (plutusScriptsWithContextStAnnTx))
import Cardano.Ledger.Conway (ConwayEra)
import Dxdingo.Rule (Run (..), historyGlobals, pparamsAlonzo, pparamsConway, prepareWith)
import Dxdingo.Domain (Result)
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

txHex, inputId, inputOut :: String
txHex = "84a50081825820876087c685570cd91a6e6ff60120cc55a9105f56576b6b519136850ab66845e401018282583901228ef2e891696d804b705b75660503d8cc1ec4d01a8af20a3697f12d33d1dc483f1c0db883685f10360ba04719f23222d4951e288188e03c1a013de7e78258390162e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de5128c1198de6eba555d96ba1d71435099083ce5c156359525a6407e2e6c1b00000040b4cda07c021a000295c9031a0986ff52081a034e1938a1008182582020fc80626e97534d41073ad5067bbca2eb2b43b4679462c3b69d7f68e7e582b65840aa6d087d15a428279687dbebbae62d3c5f2e6dfeb07ce6684fee69afa685edc28d46a5d126bcafea9e3e1b891431c4d8272bf25ada33bd581cf8f7f75150960bf5f6"
inputId = "876087c685570cd91a6e6ff60120cc55a9105f56576b6b519136850ab66845e4"
inputOut = "a200581d6162e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de512011b00000040b60e1e2c"

unhex :: String -> BS.ByteString
unhex (a : b : rest) = BS.cons (fst (head (readHex [a, b]))) (unhex rest)
unhex _ = BS.empty

slot, ttl :: Word
slot = 159842129
ttl = 159842130

pparams :: ProtocolParameters
pparams =
  defMessage
    & #minFeeA .~ 44 & #minFeeB .~ 155381 & #maxBlockBodySize .~ 90112 & #maxTxSize .~ 16384
    & #maxBlockHeaderSize .~ 1100 & #keyDeposit .~ 2000000 & #poolDeposit .~ 500000000
    & #maxEpoch .~ 18 & #nOpt .~ 500 & #a0 .~ rat ("3", "10") & #rho .~ rat ("3", "1000")
    & #tau .~ rat ("2", "10") & #protocolVersion .~ (defMessage & #major .~ 10)
    & #minPoolCost .~ 170000000 & #priceMem .~ rat ("577", "10000") & #priceSteps .~ rat ("721", "10000000")
    & #maxTxExMem .~ 14000000 & #maxTxExSteps .~ 10000000000
    & #maxBlockExMem .~ 62000000 & #maxBlockExSteps .~ 20000000000
    & #maxValueSize .~ 5000 & #collateralPercentage .~ 150 & #maxCollateralInputs .~ 3
    & #coinsPerUtxoByte .~ 4310

state :: String -> LedgerState
state out =
  defMessage & #utxo .~ [defMessage & #txin .~ (defMessage & #txId .~ unhex inputId & #index .~ 1) & #txout .~ unhex out]

utxow :: (RuleUtxowRequest -> RuleUtxowRequest) -> [Text]
utxow f =
  failures
    ( call
        ( #ruleUtxow
            .~ f
              ( defMessage
                  & #context .~ (defMessage & #era .~ ERA_CONWAY & #slot .~ fromIntegral slot & #networkId .~ 1)
                  & #globals .~ globals
                  & #pparams .~ pparams
                  & #state .~ state inputOut
                  & #tx .~ unhex txHex
              )
        )
        ^. #ruleUtxow . #rejection . #failures
    )
  where
    failures = sort . map (\x -> x ^. #rule <> "/" <> x ^. #name)

globals =
  defMessage
    & #networkMagic .~ 764824073 & #epochLength .~ 432000 & #slotLength .~ rat ("1", "1")
    & #securityParam .~ 2160 & #activeSlotCoeff .~ rat ("1", "20") & #slotsPerKesPeriod .~ 129600
    & #maxKesEvolutions .~ 62 & #maxLovelaceSupply .~ 45000000000000000 & #systemStartUnix .~ 1506203091
    & #quorum .~ 5

replaceOut :: String -> String -> String
replaceOut from to = go inputOut
  where
    go s | take (length from) s == from = to ++ drop (length from) s
    go (c : cs) = c : go cs
    go [] = []

tests :: [Check]
tests =
  Test.RuleCerts.tests ++
  Test.RuleLedger.tests ++
  [ ("utxow: valid transaction has no failures", utxow id == [])
  , ("utxow: last slot before ttl is valid", utxow (#context . #slot .~ fromIntegral (ttl - 1)) == [])
  , ("utxow: slot equal to ttl is outside the interval", utxow (#context . #slot .~ fromIntegral ttl) == ["UTXO/OutsideValidityIntervalUTxO"])
  , ("utxow: before the validity start", utxow (#context . #slot .~ 55449911) == ["UTXO/OutsideValidityIntervalUTxO"])
  , ("utxow: fee too small", utxow (#pparams . #minFeeB .~ 10000000) == ["UTXO/FeeTooSmallUTxO"])
  , ("utxow: max tx size", utxow (#pparams . #maxTxSize .~ 100) == ["UTXO/MaxTxSizeUTxO"])
  , ("utxow: wrong network", utxow (#context . #networkId .~ 0) == ["UTXO/WrongNetwork"])
  , ("utxow: output too small", utxow (#pparams . #coinsPerUtxoByte .~ 1000000000) == ["UTXO/BabbageOutputTooSmallUTxO"])
  , ( "utxow: value not conserved"
    , utxow (#state .~ state (replaceOut "40b60e1e2c" "40b60e1e2d")) == ["UTXO/ValueNotConservedUTxO"]
    )
  , ( "utxow: input key did not sign"
    , utxow (#state .~ state (replaceOut "62e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de512" (concat (replicate 28 "ee"))))
        == ["UTXOW/MissingVKeyWitnessesUTXOW"]
    )
  , ( "utxo: does not run the witness checks"
    , null
        ( call
            ( #ruleUtxo
                .~ ( defMessage
                       & #context .~ (defMessage & #era .~ ERA_CONWAY & #slot .~ fromIntegral slot & #networkId .~ 1)
                       & #globals .~ globals
                       & #pparams .~ pparams
                       & #state .~ state (replaceOut "62e42074c75c5a2067545f21bcef9db30298d05dbf0930899e6de512" (concat (replicate 28 "ee")))
                       & #tx .~ unhex txHex ::
                       RuleUtxoRequest
                   )
            )
            ^. #ruleUtxo . #rejection . #failures
        )
    )
  , ( "utxow: Byron is UNSUPPORTED"
    , call (#ruleUtxow .~ (defMessage & #context .~ (defMessage & #era .~ ERA_BYRON) & #tx .~ unhex txHex :: RuleUtxowRequest))
        ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "utxow: undecodable tx is BAD_REQUEST"
    , call (#ruleUtxow .~ (defMessage & #context .~ (defMessage & #era .~ ERA_CONWAY) & #globals .~ globals & #pparams .~ pparams & #tx .~ BS.take 10 (unhex txHex) :: RuleUtxowRequest))
        ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
    )
  , -- Divergence confirmation, run through the reference's rule: an input
    -- missing from the UTxO yields BadInputsUTxO and ValueNotConservedUTxO
    -- only, because getScriptsNeeded skips unresolved inputs. gouroboros'
    -- required-redeemers rule also fails (dxnode-go
    -- TestDivergenceRequiredRedeemersFailsOnUnresolvedInput).
    ( "divergence: unresolved input yields no MissingRedeemers in the reference"
    , utxow (#state .~ defMessage) == ["UTXO/BadInputsUTxO", "UTXO/ValueNotConservedUTxO"]
    )
  ]
    ++ legacyTests
    ++ utxosTests
    ++ utxosDivergences
    ++ scriptedDivergences

-- ---------------------------------------------------------------------
-- Shelley, Allegra and Mary. The same cases run in dxnode-go
-- (cmd/dxnode-go/rule_eras_test.go): a mainnet transaction per era with its
-- single input synthesised to hold outputs plus fee.

data Legacy = Legacy
  { lgName :: String
  , lgEra :: Era
  , lgMajor :: Word
  , lgTx :: String
  , lgInTxId :: String
  , lgInIndex :: Word
  , lgInOut :: String
  , lgSlot :: Word
  , lgTtl :: Word
  , lgExpiry :: Text
  }

legacies :: [Legacy]
legacies =
  [ Legacy "shelley" ERA_SHELLEY 2
      "83a4008182582080266661df8a01b93e9e20de7232e3b34b609a1ee4a630442db9209485d2293300018282583901491f94fa0985f2c34ccfb94085dcd9763c8eb947737dd380b52e37d8ab93b30d186d41b7b9bc22c336fdbb75dd1ab1ddf58ee9ba906bec821a713fb300825839013ada263c5ccda43559b21a9cd59c1e24f36993d8c579e1d7e8815aae7704a3cd5ae21fed6914ec6fc9d9879ee34008c541fa00d9ae571f5f1b000001748b231e6f021a0002a751031a00f68b55a10081825820e9f3aaf15c651471ae0d5fe633a0e995c52a7d10f1b842ca2ccee768e64c5c0f5840664e923c5ee2cf5bc3aab03c2ccd963c11606014b8934548f65bb449c396fe7aa737c0758ca5c6bbb0efc7b6b3d27f1b3089b7144eff291b641b48ccf0d10e03f6"
      "80266661df8a01b93e9e20de7232e3b34b609a1ee4a630442db9209485d22933" 0
      "82581d613ada263c5ccda43559b21a9cd59c1e24f36993d8c579e1d7e8815aae1b00000174fc6578c0"
      16157525 16157525 "UTXO/ExpiredUTxO"
  , Legacy "allegra" ERA_ALLEGRA 3
      "83a40081825820d53ea8c652a58a8d0b4f62b92acdb5dd71a7b49c68cd3a686bec22e6e088005d01018282584c82d818584283581cf6afb6b5ffa538cba54edb1513a4aa031d280f9448861273b1a1bd6fa101581e581c2b0b011ba3683d15ad5eef2a4a1e16bc63fa2df24653334ea986ae10001a53d31d6b1a10a133c0825839012d1f6eb196eb8c352ad8512c39b2ef980fd9ff11f61f13b327cb3f06aba8304aa2f0dbce3c7b1201659b8872fdcfc2bbbc9144853f4bdace1a0f3efedb021a0002c30a031a01600d72a100818258203c693f0da4e77b94bd0890d82ccd62066662ab7e5a60043854301be914b22b3d584000ef4847489cefcd1c77e70f7d30d2ef98cbd0b5177515add351308b4111c69ade63f5e98565108c3ffa82bebc3a331bdc4583112e8d6738dcf9eb5430673700f6"
      "d53ea8c652a58a8d0b4f62b92acdb5dd71a7b49c68cd3a686bec22e6e088005d" 1
      "82581d612d1f6eb196eb8c352ad8512c39b2ef980fd9ff11f61f13b327cb3f061a1fe2f5a5"
      23072113 23072114 "UTXO/OutsideValidityIntervalUTxO"
  , Legacy "mary" ERA_MARY 4
      "83a50081825820d9557efbf84ffa6dd363661c1b5e7313ed29e50823da16baedc1794f2f616207000181825839015d3c84a4ab34244ca865475c13eabf2e862d5a30c612360f4f0b7b2405fbcf0e74565278f93b2b82ab69bcb9f08706ddd4a00d225d5d678f1a00217dad021a00029d2d031a02613b8a0800a100818258205f565a66dee8a092bd11e6196ce1f974195bc4183cc883699d3f04c87a603b6f58400354c4b89d6d2eacec89eb2b1cea400309edef1ed707b406455f8c405a2a7c7cfda44565c00dfbb8a35c3a9d7a804d6dfbea074ef197d0de7ad5f66c175db105f6"
      "d9557efbf84ffa6dd363661c1b5e7313ed29e50823da16baedc1794f2f616207" 0
      "82581d61a8ab4c5d55e86795f3af683f107d1442ec77762842ce5fd956a69b431a00241ada"
      39926665 39926666 "UTXO/OutsideValidityIntervalUTxO"
  ]

legacyPParams :: Word -> ProtocolParameters
legacyPParams major =
  pparams
    & #protocolVersion .~ (defMessage & #major .~ fromIntegral major)
    & #d .~ rat ("0", "1")
    & #minUtxoValue .~ 1000000

legacyUtxow :: Legacy -> (RuleUtxowRequest -> RuleUtxowRequest) -> [Text]
legacyUtxow l f =
  failures
    ( call
        ( #ruleUtxow
            .~ f
              ( defMessage
                  & #context .~ (defMessage & #era .~ lgEra l & #slot .~ fromIntegral (lgSlot l) & #networkId .~ 1)
                  & #globals .~ globals
                  & #pparams .~ legacyPParams (lgMajor l)
                  & #state .~ legacyState l (lgInOut l)
                  & #tx .~ unhex (lgTx l)
              )
        )
        ^. #ruleUtxow . #rejection . #failures
    )
  where
    failures = sort . map (\x -> x ^. #rule <> "/" <> x ^. #name)

legacyState :: Legacy -> String -> LedgerState
legacyState l out =
  defMessage
    & #utxo .~ [defMessage & #txin .~ (defMessage & #txId .~ unhex (lgInTxId l) & #index .~ fromIntegral (lgInIndex l)) & #txout .~ unhex out]

legacyTests :: [Check]
legacyTests = concatMap checks legacies
  where
    checks l =
      [ (n "valid transaction has no failures", legacyUtxow l id == [])
      , (n "slot past the ttl", legacyUtxow l (#context . #slot .~ fromIntegral (lgTtl l + 1)) == [lgExpiry l])
      , (n "fee too small", legacyUtxow l (#pparams . #minFeeB .~ 10000000) == ["UTXO/FeeTooSmallUTxO"])
      , (n "max tx size", legacyUtxow l (#pparams . #maxTxSize .~ 100) == ["UTXO/MaxTxSizeUTxO"])
      , (n "wrong network", legacyUtxow l (#context . #networkId .~ 0) == ["UTXO/WrongNetwork"])
      , (n "output too small", legacyUtxow l (#pparams . #minUtxoValue .~ 1000000000000) == ["UTXO/OutputTooSmallUTxO"])
      , ( n "value not conserved"
        , legacyUtxow l (#state .~ legacyState l (bumpCoin (lgInOut l))) == ["UTXO/ValueNotConservedUTxO"]
        )
      , ( n "input key did not sign"
        , legacyUtxow l (#state .~ legacyState l (rekey (lgInOut l))) == ["UTXOW/MissingVKeyWitnessesUTXOW"]
        )
      ]
      where
        n what = lgName l ++ " utxow: " ++ what

-- | Raise the coin of an output encoded as @[address, coin]@ by one. All
-- three outputs above end in an unsigned integer of fixed width.
bumpCoin :: String -> String
bumpCoin out = init out ++ [succDigit (last out)]
  where
    succDigit c = case c of
      '0' -> '1'; '1' -> '2'; '2' -> '3'; '3' -> '4'; '4' -> '5'; '5' -> '6'; '6' -> '7'
      '7' -> '8'; '8' -> '9'; '9' -> 'a'; 'a' -> 'b'; 'b' -> 'c'; 'c' -> 'd'; 'd' -> 'e'
      _ -> 'f'

-- | Replace the payment key hash of an enterprise address (bytes 3..30 of
-- the output, after the array header, the bytes header and the address
-- header) with one nobody signed with.
rekey :: String -> String
rekey out = take 8 out ++ concat (replicate 28 "ee") ++ drop (8 + 56) out

-- ---------------------------------------------------------------------
-- UTXOS. The same cases run in dxnode-go (cmd/dxnode-go/rule_utxos_test.go):
-- one input locked by an always-succeeding or always-failing Plutus script.

data Plutus = Plutus Era Word String [(String, String)]

plutusFixtures :: [(String, Plutus)]
plutusFixtures =
  [ ("alonzo_v1_pass", Plutus ERA_ALONZO 6 "84a40081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a100018182581d61111111111111111111111111111111111111111111111111111111111a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b200a3038147460100002224990481d879800581840000d87980821a000f42401a1dcd6500f5f6"
      [("a1", "83581d71fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"), ("b2", "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40")])
  , ("alonzo_v1_fail", Plutus ERA_ALONZO 6 "84a40081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a100018182581d61111111111111111111111111111111111111111111111111111111111a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b200a3038147460100002226010481d879800581840000d87980821a000f42401a1dcd6500f5f6"
      [("a1", "83581d716a09cb22defaf4a96a6be1ef6c07467ac9923d1750a79214a06c503a1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"), ("b2", "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40")])
  , ("conway_v3_pass", Plutus ERA_CONWAY 10 "84a70081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1000181a200581d6111111111111111111111111111111111111111111111111111111111011a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b20010a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480111a002dc6c01281825820c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c300a30481d879800581840000d87980821a000f42401a1dcd6500078146450101002499f5f6"
      [("a1", "a300581d71186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4011a009896800282005820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"), ("b2", "a200581d6111111111111111111111111111111111111111111111111111111111011a004c4b40"), ("c3", "a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480")])
  , ("conway_v3_fail", Plutus ERA_CONWAY 10 "84a70081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1000181a200581d6111111111111111111111111111111111111111111111111111111111011a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b20010a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480111a002dc6c01281825820c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c300a30481d879800581840000d87980821a000f42401a1dcd6500078146450101002601f5f6"
      [("a1", "a300581d71ebb18a12777410738fdeaa77ec0fd582685d677b6b34de9a6e3b6d7e011a009896800282005820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"), ("b2", "a200581d6111111111111111111111111111111111111111111111111111111111011a004c4b40"), ("c3", "a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480")])
  ]

utxosCostModels :: ProtocolParameters -> ProtocolParameters
utxosCostModels p = p & #costModelPlutusV1 .~ mainnetV1 & #costModelPlutusV2 .~ mainnetV2 & #costModelPlutusV3 .~ mainnetV3

utxos :: Plutus -> Bool -> RuleUtxosResponse
utxos p flag = call (#ruleUtxos .~ utxosRequest p flag) ^. #ruleUtxos

utxosRequest :: Plutus -> Bool -> RuleUtxosRequest
utxosRequest (Plutus era major tx outs) flag =
  ( defMessage
               & #context .~ (defMessage & #era .~ era & #slot .~ 100000000 & #networkId .~ 1)
               & #globals .~ globals
               & #pparams .~ (utxosCostModels pparams & #protocolVersion .~ (defMessage & #major .~ fromIntegral major) & #coinsPerUtxoWord .~ 34482 & #d .~ rat ("0", "1"))
               & #state .~ (defMessage & #utxo .~ [defMessage & #txin .~ (defMessage & #txId .~ unhex (concat (replicate 32 i)) & #index .~ 0) & #txout .~ unhex o | (i, o) <- outs])
               & #eraHistory .~ mainnetHistory
               & #tx .~ unhex (if flag then tx else take (length tx - 4) tx ++ "f4f6")
  )

mainnetHistory :: EraHistory
mainnetHistory =
  defMessage
    & #systemStartUnix .~ 1506203091
    & #eras
      .~ [ defMessage & #era .~ ERA_BYRON & #startSlot .~ 0 & #startEpoch .~ 0 & #startTime .~ rat ("0", "1")
             & #endSlot .~ 4492800 & #endEpoch .~ 208 & #endTime .~ rat ("89856000", "1")
             & #epochLength .~ 21600 & #slotLength .~ rat ("20", "1") & #safeZone .~ 4320
         , defMessage & #era .~ ERA_SHELLEY & #startSlot .~ 4492800 & #startEpoch .~ 208 & #startTime .~ rat ("89856000", "1")
             & #epochLength .~ 432000 & #slotLength .~ rat ("1", "1") & #safeZone .~ 129600
         ]

utxosTests :: [Check]
utxosTests =
  [ check "alonzo pass, flagged valid" "alonzo_v1_pass" True [] True 0
  , check "alonzo pass, flagged invalid" "alonzo_v1_pass" False ["UTXOS/ValidationTagMismatch"] True 0
  , check "alonzo fail, flagged valid" "alonzo_v1_fail" True ["UTXOS/ValidationTagMismatch"] False 0
  , check "alonzo fail, flagged invalid" "alonzo_v1_fail" False [] False 5000000
  , check "conway pass, flagged valid" "conway_v3_pass" True [] True 0
  , check "conway pass, flagged invalid" "conway_v3_pass" False ["UTXOS/ValidationTagMismatch"] True 0
  , check "conway fail, flagged valid" "conway_v3_fail" True ["UTXOS/ValidationTagMismatch"] False 0
  , check "conway fail, flagged invalid" "conway_v3_fail" False [] False 3000000
  , ( "utxos: a missing cost model is a collect error"
    , let r = call (#ruleUtxos .~ (utxosRequest alonzoPass True & #pparams . #costModelPlutusV1 .~ [])) ^. #ruleUtxos
       in [f ^. #rule <> "/" <> f ^. #name | f <- r ^. #rejection . #failures] == ["UTXOS/CollectErrors"]
            && not (r ^. #scriptsValid)
    )
  , ( "utxos: Mary is UNSUPPORTED"
    , call (#ruleUtxos .~ (utxosRequest alonzoPass True & #context . #era .~ ERA_MARY)) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  ]
  where
    alonzoPass = fromMaybe (error "alonzo_v1_pass") (lookup "alonzo_v1_pass" plutusFixtures)
    check name fixture flag want ok collateral =
      let r = utxos (fromMaybe (error fixture) (lookup fixture plutusFixtures)) flag
       in ( "utxos: " ++ name
          , sort [f ^. #rule <> "/" <> f ^. #name | f <- r ^. #rejection . #failures] == want
              && r ^. #scriptsValid == ok
              && r ^. #collateralConsumed == collateral
          )

mainnetV1, mainnetV2, mainnetV3 :: [Int64]
mainnetV1 = [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 57996947, 18975, 10]
mainnetV2 = [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 57996947, 18975, 10, 38887044, 32947, 10]
mainnetV3 = [100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1, 1964219, 24520, 3, 607153, 231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7, 1, 232010, 32, 321837444, 25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21, 219951, 9444, 1, 1000, 172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2, 22, 1000, 95933, 1, 1, 11, 1000, 277577, 12, 21]

-- ---------------------------------------------------------------------
-- Divergence: a spending script whose datum is not in the witness set. The
-- reference's getSpendingDatum returns Nothing and the script runs on the
-- remaining arguments, so the collected scripts evaluate and pass; gouroboros
-- refuses with MissingDatumForSpendingScriptError (dxnode-go
-- TestDivergenceSpendingDatumMissing). UTXOW's MissingRequiredDatums rejects
-- such a transaction before UTXOS runs.

noDatumFixtures :: [(String, Plutus)]
noDatumFixtures =
  [ ( "alonzo V1"
    , Plutus ERA_ALONZO 6
        "84a40081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a100018182581d61111111111111111111111111111111111111111111111111111111111a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b200a2038147460100002224990581840000d87980821a000f42401a1dcd6500f5f6"
        [ ("a1", "83581d71fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec")
        , ("b2", "82581d61111111111111111111111111111111111111111111111111111111111a004c4b40")
        ]
    )
  ]

-- | Collect and evaluate the scripts of an Alonzo request with the ledger's
-- own functions, without the rule or its adapter.
alonzoScriptsPass :: Plutus -> Maybe Bool
alonzoScriptsPass p = case run of
  Left _ -> Nothing
  Right r -> case plutusScriptsWithContextStAnnTx (runTx r) of
    Left _ -> Just False
    Right l -> Just (case evalPlutusScripts l of Passes _ -> True; Fails _ _ -> False)
  where
    q = utxosRequest p True
    run :: Result (Run AlonzoEra)
    run =
      prepareWith False (historyGlobals (q ^. #eraHistory)) pparamsAlonzo
        (q ^. #context) (q ^. #globals) (q ^. #pparams) (q ^. #state) (q ^. #tx)

utxosDivergences :: [Check]
utxosDivergences =
  [ ( "divergence: " ++ name ++ " spending datum missing: the reference runs the script on the remaining arguments"
    , alonzoScriptsPass p == Just True
    )
  | (name, p) <- noDatumFixtures
  ]

-- ---------------------------------------------------------------------
-- Divergences found by the scripted corpus.
--
-- A spending redeemer for a key-locked input with no Plutus script: the
-- reference reports ExtraRedeemers and no missing script, as none is needed;
-- gouroboros' redeemer-and-script-witnesses rule also fails (dxnode-go
-- TestDivergenceRedeemerForKeyLockedInput).
--
-- An input missing from the UTxO: the reference collects the scripts without
-- it and UTXOS reports nothing; gouroboros' script run fails to resolve it
-- (dxnode-go TestDivergenceUnresolvedInputFailsScriptRun).

keyLockedRedeemer :: Plutus
keyLockedRedeemer =
  Plutus ERA_CONWAY 10
    "84a60081825820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1000181a200581d6111111111111111111111111111111111111111111111111111111111011a00958940021a00030d400d81825820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b20010a200581d6111111111111111111111111111111111111111111111111111111111011a001e8480111a002dc6c0a20481d879800581840000d87980821a000f42401a1dcd6500f5f6"
    [ ("a1", "a200581d6111111111111111111111111111111111111111111111111111111111011a00989680")
    , ("b2", "a200581d6111111111111111111111111111111111111111111111111111111111011a004c4b40")
    ]

conwayPass :: Plutus
conwayPass = fromMaybe (error "conway_v3_pass") (lookup "conway_v3_pass" plutusFixtures)

-- | The scripts the ledger collects for a Conway request, without the rule.
conwayCollects :: Plutus -> Maybe Bool
conwayCollects p = case run of
  Left _ -> Nothing
  Right r -> Just (either (const False) (const True) (plutusScriptsWithContextStAnnTx (runTx r)))
  where
    q = utxosRequest p True
    run :: Result (Run ConwayEra)
    run =
      prepareWith False (historyGlobals (q ^. #eraHistory)) pparamsConway
        (q ^. #context) (q ^. #globals) (q ^. #pparams) (q ^. #state) (q ^. #tx)

scriptedDivergences :: [Check]
scriptedDivergences =
  [ ( "divergence: redeemer for a key-locked input is ExtraRedeemers, no missing script witness"
    , let fs = [f ^. #rule <> "/" <> f ^. #name | f <- r ^. #rejection . #failures]
          r = call (#ruleUtxow .~ utxowOf keyLockedRedeemer) ^. #ruleUtxow
       in "UTXOW/ExtraRedeemers" `elem` fs && "UTXOW/MissingScriptWitnessesUTXOW" `notElem` fs
    )
  , ( "divergence: unresolved input leaves the collected scripts empty, not a collect error"
    , conwayCollects (case conwayPass of Plutus e m t _ -> Plutus e m t []) == Just True
    )
  ]
  where
    utxowOf p =
      let q = utxosRequest p True
       in (defMessage :: RuleUtxowRequest)
            & #context .~ (q ^. #context)
            & #globals .~ (q ^. #globals)
            & #pparams .~ (q ^. #pparams)
            & #state .~ (q ^. #state)
            & #tx .~ (q ^. #tx)
