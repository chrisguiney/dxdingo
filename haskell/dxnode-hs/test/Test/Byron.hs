{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Byron (tests) where

import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Byron
import qualified Proto.Dxdingo.V1.State as S
import Test.Util

-- The same cases as dxnode-go's byron_test.go, with the transactions from
-- internal/byronfixture written out in hex: a 10 ADA output at key 1's
-- address spent by one witnessed transaction.
tests :: [Check]
tests =
  [ ("min fee: mainnet 200 bytes is ceil(8.7892)", fee ("155381", "1") mainnetB 200 == Just 155390)
  , ("min fee: zero size is the summand", fee ("155381", "1") mainnetB 0 == Just 155381)
  , ("min fee: exact product is not rounded up", fee ("0", "1") ("1", "2") 10 == Just 5)
  , ("min fee: half a lovelace rounds up", fee ("0", "1") ("1", "2") 11 == Just 6)
  , ("min fee: zero multiplier", fee ("7", "1") ("0", "1") 1000000 == Just 7)
  , ("min fee: summand at the Lovelace bound", fee ("45000000000000000", "1") ("0", "1") 5 == Just 45000000000000000)
  , ("min fee: overflow is rejected", failures (minFee ("45000000000000000", "1") ("1", "1") 1) == ["LovelaceOverflow"])
  , ("tx: valid", txValid == (Nothing, 1000000, 1, 1))
  , ("tx: fee below the minimum", txName "fee" == ["TxValidationFeeTooSmall"])
  , ("tx: outputs exceed inputs", txName "exceed" == ["TxValidationLovelaceError"])
  , ("tx: witness signs another transaction", txName "badsig" == ["TxValidationWitnessWrongSignature"])
  , ("tx: witness key does not own the input", txName "wrongkey" == ["TxValidationWitnessWrongKey"])
  , ("tx: output on another network", txName "network" == ["TxValidationNetworkMagicMismatch"])
  , ("tx: input not in the UTxO", txName "missing" == ["UTxOMissingInput"])
  , ("tx: over the size limit", failures (txResp "valid" (#maxTxSize .~ 100)) == ["TxValidationTxTooLarge"])
  , ("tx: undecodable", failures (runTx (BS.pack [0xff]) id) == ["DeserialiseFailure"])
  ]
  where
    mainnetB = ("43946", "1000000")
    minFee a b size =
      call
        ( #byronMinFee
            .~ (defMessage & #txFeeA .~ rat a & #txFeeB .~ rat b & #size .~ size)
        )
        ^. #byronMinFee
    fee a b size =
      let r = minFee a b size in if null (failures r) then Just (r ^. #fee) else Nothing
    failures r = [f ^. #name | f <- r ^. #rejection . #failures]
    pparams :: S.ByronProtocolParameters
    pparams =
      defMessage
        & #txFeeA .~ rat ("155381", "1")
        & #txFeeB .~ rat ("43946", "1000000")
        & #maxTxSize .~ 4096
        & #maxBlockSize .~ 2000000
        & #maxHeaderSize .~ 2000000
    utxoEntry =
      defMessage
        & #txin .~ (defMessage & #txId .~ BS.pack (0x11 : replicate 31 0) & #index .~ 0)
        & #txout .~ unhex "8282d818582183581cec0e23340c62b5e0d64ed01278769291f3ee83e721ea3375d8830d7ca0001aed599b891a00989680"
    runTx tx adj =
      call
        ( #byronTxValidate
            .~ ( defMessage
                  & #protocolMagic .~ 764824073
                  & #pparams .~ adj pparams
                  & #utxo .~ [utxoEntry]
                  & #tx .~ tx
               )
        )
        ^. #byronTxValidate
    txResp name adj = runTx (unhex (txHex name)) adj
    txName name = failures (txResp name id)
    txValid =
      let r = txResp "valid" id
       in ( if null (failures r) then Nothing else Just (failures r)
          , r ^. #fee
          , length (r ^. #utxoAdded)
          , length (r ^. #utxoRemoved)
          )

unhex :: String -> BS.ByteString
unhex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []

txHex :: String -> String
txHex name = case name of
  "valid" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840d2c7222bf8a3587ffd93f1197e34a1e3bf1b9a5bd6f6c7ed81e6a764defc10b7a32eedff530f90c87daa928882d5880bcc77355ca11b1e1ee8acc76a1245a206"
  "fee" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00989298ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840ded369471ebe409d1c6b4bb8dd7943ffa5f4b364599b380864a736f120fe56c28f253d63ce47c43ba31f3b1cca01e15fad52c50fb686f68b7e1148ab38077203"
  "exceed" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00989681ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840ee17dfbe0df1eb3d0abcc5506e7c9105c879f5181bf3d64c4bb28fae61a7426cba51c6794d0fdc9ba89f96f19c6bac381d8fdd6c1a9c595f92435d537b1de30b"
  "badsig" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b584012b5036022288e1e752761cd8db9bafe1d27a436c6c579503c368d2206736f60fa2506f97e88890f754bf439b0c1cfcf3b9e5e9ee676111cb7a37cb9ed2fcb0d"
  "wrongkey" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b394585858585858585858585858585858585858585858585858585858585858585858409aedfda9055a7ce6c070d5e1d7ee51399f11a1507208285b444573eb1d2124f1538bbcc24295da770a0d937117b5733f8a8bb88fcd7c066e933817020e4c1003"
  "network" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582583581cb48bb6d3e2be6deda4c4cdbe535f406bdb0129f072ba16a0fb1cb30aa10242182a001a7a66285c1a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840864ab158f3f635d8cc9053a60825617f2e22274010701cc993eb269d87555de1ce9d96656c90b92d037a359ea0b9d549690ebc80d78a90454abded2a8e35e601"
  _ -> "82839f8200d8185824825820990000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b58407c34fd86ace87e2ca05e7e89695228fe430f929ac086a0afd2333a4412e36397dbd5faf027c74ce17188a47728e9a2fca37bbc3e07f3aa9a70ae2d8caf492b06"
