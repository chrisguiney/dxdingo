{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transaction rules. The same cases run in dxnode-go (cmd/dxnode-go/rule_test.go):
-- a mainnet Conway transaction with one synthesised input holding exactly
-- outputs plus fee.
module Test.Rule (tests) where

import qualified Data.ByteString as BS
import Data.List (sort)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Numeric (readHex)
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Rule (RuleUtxoRequest, RuleUtxowRequest)
import Proto.Dxdingo.V1.State (LedgerState, ProtocolParameters)
import Proto.Dxdingo.V1.Types (Era (..))
import Test.Util

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
  , ( "utxow: Shelley is UNSUPPORTED"
    , call (#ruleUtxow .~ (defMessage & #context .~ (defMessage & #era .~ ERA_SHELLEY) & #tx .~ unhex txHex :: RuleUtxowRequest))
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
