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
import Lens.Micro (ASetter', (&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope
  ( Call
  , Call'Request (..)
  , HarnessErrorCode (..)
  , Reply
  )
import qualified Dxdingo.Crypto as Crypto
import qualified Dxdingo.Praos as Praos
import qualified Dxdingo.Rewards as Rewards
import Proto.Dxdingo.V1.Meta (MetaInfoResponse)

-- | Envelope field names this node handles.
implemented :: [Text]
implemented =
  [ "meta_info"
  , "rewards_member_reward"
  , "rewards_leader_reward"
  , "rewards_max_pool"
  , "rewards_apparent_performance"
  , "crypto_vkey_witness"
  , "crypto_vrf_verify"
  , "crypto_kes_verify"
  , "crypto_address_decode"
  , "praos_leader_check"
  , "praos_nonce_epoch"
  , "tpraos_overlay_schedule"
  ]

handle :: Call -> Reply
handle call = case call ^. #maybe'request of
  Nothing -> harnessError HARNESS_ERROR_CODE_BAD_REQUEST "call has no request"
  Just (Call'MetaInfo _) -> base & #metaInfo .~ metaInfo
  Just (Call'RewardsMemberReward q) -> answer #rewardsMemberReward (Rewards.memberReward q)
  Just (Call'RewardsLeaderReward q) -> answer #rewardsLeaderReward (Rewards.leaderReward q)
  Just (Call'RewardsMaxPool q) -> answer #rewardsMaxPool (Rewards.maxPool q)
  Just (Call'RewardsApparentPerformance q) -> answer #rewardsApparentPerformance (Rewards.apparentPerformance q)
  Just (Call'CryptoVkeyWitness q) -> answer #cryptoVkeyWitness (Crypto.vkeyWitness q)
  Just (Call'CryptoVrfVerify q) -> answer #cryptoVrfVerify (Crypto.vrfVerify q)
  Just (Call'CryptoKesVerify q) -> answer #cryptoKesVerify (Crypto.kesVerify q)
  Just (Call'CryptoAddressDecode q) -> answer #cryptoAddressDecode (Crypto.addressDecode q)
  Just (Call'PraosLeaderCheck q) -> answer #praosLeaderCheck (Praos.leaderCheck q)
  Just (Call'PraosNonceEpoch q) -> answer #praosNonceEpoch (Praos.nonceEpoch q)
  Just (Call'TpraosOverlaySchedule q) -> answer #tpraosOverlaySchedule (Praos.overlaySchedule q)
  Just _ -> harnessError HARNESS_ERROR_CODE_UNIMPLEMENTED "not implemented by dxnode-hs"
  where
    base :: Reply
    base = defMessage & #id .~ (call ^. #id)
    harnessError code msg =
      base & #error .~ (defMessage & #code .~ code & #message .~ msg)
    answer :: ASetter' Reply a -> Either (HarnessErrorCode, Text) a -> Reply
    answer field = either (uncurry harnessError) (\r -> base & field .~ r)

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
