{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Dxdingo.Dispatch (handle, handleBytes)
import Dxdingo.Wire
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call, HarnessErrorCode (..), Reply)
import System.Exit (exitFailure)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do
        putStrLn ("FAIL: " <> name)
        modifyIORef' failures (+ 1)

  check "header is big-endian uint32 length" $
    encodeFrame "abc" == BS.pack [0, 0, 0, 3, 97, 98, 99]
  check "consecutive frames round-trip" $
    let buf = encodeFrame "one" <> encodeFrame "" <> encodeFrame "three"
        go b = case decodeFrame b of
          Right (Just (p, rest)) -> p : go rest
          _ -> []
     in go buf == ["one", "", "three"]
  check "empty buffer is clean end of stream" $ decodeFrame "" == Right Nothing
  check "truncated header" $ decodeFrame (BS.pack [0, 0]) == Left Truncated
  check "truncated body" $ decodeFrame (BS.pack [0, 0, 0, 5, 97]) == Left Truncated
  check "oversized length rejected" $
    decodeFrame (BS.pack [255, 255, 255, 255]) == Left (TooLarge 4294967295)

  let feeCall =
        (defMessage :: Call)
          & #id .~ 42
          & #feeScriptFee .~ (defMessage & #exMem .~ 7)
      unimpl = handle feeCall
  check "unhandled function replies UNIMPLEMENTED with the call id" $
    unimpl ^. #id == 42
      && unimpl ^. #error . #code == HARNESS_ERROR_CODE_UNIMPLEMENTED
  check "meta_info is answered" $
    let r = handle ((defMessage :: Call) & #id .~ 1 & #metaInfo .~ defMessage)
     in r ^. #metaInfo . #implementation == "cardano-ledger"
  check "call without request is BAD_REQUEST" $
    handle ((defMessage :: Call) & #id .~ 9) ^. #error . #code
      == HARNESS_ERROR_CODE_BAD_REQUEST
  check "undecodable bytes reply BAD_REQUEST" $
    case decodeMessage (handleBytes (BS.pack [255, 255, 255])) of
      Right (r :: Reply) -> r ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
      Left _ -> False
  check "handleBytes round-trips a call" $
    case decodeMessage (handleBytes (encodeMessage feeCall)) of
      Right (r :: Reply) -> r ^. #id == 42
      Left _ -> False

  let memberCall reward cost (mn, md) member pool total =
        (defMessage :: Call)
          & #id .~ 7
          & #rewardsMemberReward
            .~ ( defMessage
                  & #poolReward .~ reward
                  & #cost .~ cost
                  & #margin .~ (defMessage & #numerator .~ mn & #denominator .~ md)
                  & #memberStake .~ member
                  & #poolStake .~ pool
                  & #totalStake .~ total
               )
      rewardOf c = handle c ^. #rewardsMemberReward . #reward
      codeOf c = handle c ^. #error . #code
  -- Same cases as dxnode-go's TestMemberReward.
  check "member reward: half the pool, 1% margin" $
    rewardOf (memberCall 1000000000 340000000 ("1", "100") 25 50 1000) == 326700000
  check "member reward: reward below cost is zero" $
    rewardOf (memberCall 300000000 340000000 ("0", "1") 25 50 1000) == 0
  check "member reward: floors" $
    rewardOf (memberCall 10 0 ("0", "1") 1 3 3) == 3
  check "member reward: zero pool stake is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "2") 1 0 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: zero total stake is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "2") 1 2 0) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: margin above one is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("3", "2") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: negative margin is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("-1", "2") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: margin over 64 bits is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "18446744073709551617") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: malformed margin is BAD_REQUEST" $
    codeOf (memberCall 1000 1 ("x", "2") 1 2 4) == HARNESS_ERROR_CODE_BAD_REQUEST

  n <- readIORef failures
  unless (n == 0) exitFailure
  putStrLn "all tests passed"
