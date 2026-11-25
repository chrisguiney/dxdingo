{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Praos (tests) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.BaseTypes (Nonce (..), (⭒))
import qualified Data.ByteString as BS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Praos (OverlaySlotKind (..))
import Proto.Dxdingo.V1.Types (Era (..))
import Test.Util

tests :: [Check]
tests =
  [ ("leader check: praos lowest value leads", leader ERA_BABBAGE (BS.replicate 32 0) ^. #praosLeaderCheck . #isLeader)
  , ("leader check: praos highest value does not", not (leader ERA_BABBAGE (BS.replicate 32 255) ^. #praosLeaderCheck . #isLeader))
  , ("leader check: tpraos lowest value leads", leader ERA_SHELLEY (BS.replicate 64 0) ^. #praosLeaderCheck . #isLeader)
  , ("leader check: 64-byte praos value is UNSUPPORTED", leader ERA_BABBAGE (BS.replicate 64 0) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("nonce epoch: candidate only", nonceOf a "" "" == a)
  , ("nonce epoch: neutral candidate returns the lab", nonceOf "" b "" == b)
  , ("nonce epoch: lab and extra entropy mix the same way", nonceOf a b "" == nonceOf a "" b && BS.length (nonceOf a b "") == 32)
  , ( "overlay: active slot names genesis key 0"
    , overlay 1000 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_ACTIVE
        && overlay 1000 ^. #tpraosOverlaySchedule . #genesisKeyHash == BS.replicate 28 1
    )
  , ("overlay: praos slot", overlay 1001 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_PRAOS)
  , ("overlay: non-active slot", overlay 1002 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_NON_ACTIVE)
  , ("overlay: slot before epoch is UNSUPPORTED", overlay 999 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , -- Divergence confirmations call the reference directly, bypassing
    -- dxnode-hs, so a reported mismatch cannot come from request
    -- translation. praos.nonce-epoch, neutral candidate: dingo encodes it as
    -- 32 zero bytes and hashes it when a lab is present
    -- (dxnode-go TestDivergenceNeutralCandidateIsHashed).
    ("divergence: NeutralNonce is the identity of the nonce operator", (NeutralNonce ⭒ labN) ⭒ NeutralNonce == labN)
  , ("divergence: a real all-zero nonce is not neutral in the reference", zeroN /= NeutralNonce && (zeroN ⭒ labN) /= labN)
  ]
  where
    a = BS.replicate 32 0xaa
    b = BS.replicate 32 0xbb
    leader era v =
      call (#praosLeaderCheck .~ (defMessage & #era .~ era & #leaderValue .~ v
        & #poolStake .~ 1 & #totalStake .~ 2 & #activeSlotCoeff .~ rat ("1", "20")))
    nonceOf c l e =
      call (#praosNonceEpoch .~ (defMessage & #era .~ ERA_SHELLEY
        & #candidate . #hash .~ c & #previousLab . #hash .~ l & #extraEntropy . #hash .~ e))
        ^. #praosNonceEpoch . #epochNonce . #hash
    keys = [defMessage & #genesisKeyHash .~ BS.replicate 28 i | i <- [1 .. 7]]
    overlay slot =
      call (#tpraosOverlaySchedule .~ (defMessage & #slot .~ slot & #epochFirstSlot .~ 1000
        & #d .~ rat ("1", "2") & #activeSlotCoeff .~ rat ("1", "20") & #genesisDelegations .~ keys))
    nonceFrom = maybe NeutralNonce Nonce . hashFromBytes
    labN = nonceFrom (BS.replicate 32 0xbb)
    zeroN = nonceFrom (BS.replicate 32 0)
