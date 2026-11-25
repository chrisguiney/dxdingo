{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Routes envelope calls to reference implementations. Functions without
-- a handler reply UNIMPLEMENTED.
module Dxdingo.Dispatch
  ( handle
  , handleBytes
  , implemented
  ) where

import Data.ByteString (ByteString)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope
  ( Call
  , Call'Request (..)
  , HarnessErrorCode (..)
  , Reply
  )
import Dxdingo.Rewards (memberReward)
import Proto.Dxdingo.V1.Meta (MetaInfoResponse)

-- | Envelope field names this node handles.
implemented :: [Text]
implemented = ["meta_info", "rewards_member_reward"]

handle :: Call -> Reply
handle call = case call ^. #maybe'request of
  Nothing -> harnessError HARNESS_ERROR_CODE_BAD_REQUEST "call has no request"
  Just (Call'MetaInfo _) -> base & #metaInfo .~ metaInfo
  Just (Call'RewardsMemberReward req) ->
    either (uncurry harnessError) (\r -> base & #rewardsMemberReward .~ r) (memberReward req)
  Just _ -> harnessError HARNESS_ERROR_CODE_UNIMPLEMENTED "not implemented by dxnode-hs"
  where
    base :: Reply
    base = defMessage & #id .~ (call ^. #id)
    harnessError code msg =
      base & #error .~ (defMessage & #code .~ code & #message .~ msg)

metaInfo :: MetaInfoResponse
metaInfo =
  defMessage
    & #implementation .~ "cardano-ledger"
    & #implemented .~ implemented

-- | Decode a call, evaluate it, encode the reply. An undecodable call gets a
-- BAD_REQUEST reply with id 0.
handleBytes :: ByteString -> ByteString
handleBytes bs = encodeMessage $ case decodeMessage bs of
  Left err ->
    (defMessage :: Reply)
      & #error
        .~ ( defMessage
              & #code .~ HARNESS_ERROR_CODE_BAD_REQUEST
              & #message .~ T.pack err
           )
  Right call -> handle call
