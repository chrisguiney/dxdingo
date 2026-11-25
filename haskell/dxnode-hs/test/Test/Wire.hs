{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Wire (tests) where

import qualified Data.ByteString as BS
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Dxdingo.Dispatch (handle, handleBytes, handleWith)
import Dxdingo.Wire
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call, HarnessErrorCode (..), Reply)
import Test.Util (Check)

tests :: [Check]
tests =
  [ ("header is big-endian uint32 length", encodeFrame "abc" == BS.pack [0, 0, 0, 3, 97, 98, 99])
  , ( "consecutive frames round-trip"
    , let buf = encodeFrame "one" <> encodeFrame "" <> encodeFrame "three"
          go b = case decodeFrame b of
            Right (Just (p, rest)) -> p : go rest
            _ -> []
       in go buf == ["one", "", "three"]
    )
  , ("empty buffer is clean end of stream", decodeFrame "" == Right Nothing)
  , ("truncated header", decodeFrame (BS.pack [0, 0]) == Left Truncated)
  , ("truncated body", decodeFrame (BS.pack [0, 0, 0, 5, 97]) == Left Truncated)
  , ("oversized length rejected", decodeFrame (BS.pack [255, 255, 255, 255]) == Left (TooLarge 4294967295))
  , ( "a function no area answers replies UNIMPLEMENTED with the call id"
    , let r = handleWith [] feeCall
       in r ^. #id == 42 && r ^. #error . #code == HARNESS_ERROR_CODE_UNIMPLEMENTED
    )
  , ("meta_info is answered", handle ((defMessage :: Call) & #id .~ 1 & #metaInfo .~ defMessage) ^. #metaInfo . #implementation == "cardano-ledger")
  , ("call without request is BAD_REQUEST", handle ((defMessage :: Call) & #id .~ 9) ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ( "undecodable bytes reply BAD_REQUEST"
    , case decodeMessage (handleBytes (BS.pack [255, 255, 255])) of
        Right (r :: Reply) -> r ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
        Left _ -> False
    )
  , ( "handleBytes round-trips a call id"
    , case decodeMessage (handleBytes (encodeMessage feeCall)) of
        Right (r :: Reply) -> r ^. #id == 42
        Left _ -> False
    )
  ]
  where
    feeCall = (defMessage :: Call) & #id .~ 42 & #feeScriptFee .~ (defMessage & #exMem .~ 7)
