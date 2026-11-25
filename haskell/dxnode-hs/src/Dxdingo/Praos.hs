{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Leader eligibility, epoch nonce and the TPraos overlay schedule,
-- answered by cardano-protocol-tpraos and cardano-ledger-core.
module Dxdingo.Praos
  ( leaderCheck
  , nonceEpoch
  , overlaySchedule
  ) where

import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.BaseTypes (Nonce (..), mkActiveSlotCoeff, (⭒))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Slot (isOverlaySlot)
import Cardano.Protocol.TPraos.BHeader (assertBoundedNatural, checkLeaderNatValue)
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot (..), classifyOverlaySlot)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as T
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Praos
import qualified Proto.Dxdingo.V1.Types as P

-- | 'checkLeaderNatValue' on the certified natural, big-endian: 64 bytes
-- under TPraos (Shelley to Alonzo) and 32 under Praos (Babbage on), bounded
-- by 2^(8 * size) as the reference bounds each.
leaderCheck :: PraosLeaderCheckRequest -> Result PraosLeaderCheckResponse
leaderCheck req = do
  f <- positiveUnitIntervalArg "active_slot_coeff" (req ^. #maybe'activeSlotCoeff)
  total <- nonzero "total_stake" (req ^. #totalStake)
  let pool = toInteger (req ^. #poolStake)
  if pool > total then unsupported "pool_stake exceeds total_stake" else pure ()
  size <- case req ^. #era of
    e | e `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO] -> pure 64
    e | e `elem` [P.ERA_BABBAGE, P.ERA_CONWAY] -> pure 32
    e -> unsupported ("no Praos leader check in " <> T.pack (show e))
  let v = req ^. #leaderValue
  if BS.length v /= size
    then unsupported ("leader value must be " <> T.pack (show size) <> " bytes")
    else
      pure $
        defMessage
          & #isLeader
            .~ checkLeaderNatValue
              (assertBoundedNatural (2 ^ (8 * size)) (bytesToNatural v))
              (pool % total)
              (mkActiveSlotCoeff f)
  where
    bytesToNatural = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | (candidate ⭒ previous lab) ⭒ extra entropy; an empty hash is the
-- neutral nonce.
nonceEpoch :: PraosNonceEpochRequest -> Result PraosNonceEpochResponse
nonceEpoch req = do
  c <- nonce "candidate" (req ^. #candidate . #hash)
  l <- nonce "previous_lab" (req ^. #previousLab . #hash)
  e <- nonce "extra_entropy" (req ^. #extraEntropy . #hash)
  let out = case (c ⭒ l) ⭒ e of
        NeutralNonce -> BS.empty
        Nonce h -> hashToBytes h
  pure (defMessage & #epochNonce .~ (defMessage & #hash .~ out))
  where
    nonce name bs
      | BS.null bs = pure NeutralNonce
      | otherwise = maybe (badRequest (name <> " must be empty (neutral) or 32 bytes")) (pure . Nonce) (hashFromBytes bs)

-- | 'isOverlaySlot', then 'classifyOverlaySlot' for overlay slots. The
-- genesis keys form a Set, so they are indexed in hash order.
overlaySchedule :: TpraosOverlayScheduleRequest -> Result TpraosOverlayScheduleResponse
overlaySchedule req = do
  d <- unitIntervalArg "d" (req ^. #maybe'd)
  f <- positiveUnitIntervalArg "active_slot_coeff" (req ^. #maybe'activeSlotCoeff)
  keys <-
    traverse
      (\g -> maybe (badRequest "genesis key hash must be 28 bytes") (pure . KeyHash) (hashFromBytes (g ^. #genesisKeyHash)))
      (req ^. #genesisDelegations)
  if null keys then unsupported "no genesis keys" else pure ()
  let first = req ^. #epochFirstSlot
      slot = req ^. #slot
  if slot < first then unsupported "slot precedes the epoch's first slot" else pure ()
  pure $
    if not (isOverlaySlot (SlotNo first) d (SlotNo slot))
      then defMessage & #kind .~ OVERLAY_SLOT_KIND_PRAOS
      else case classifyOverlaySlot (SlotNo first) (Set.fromList keys) d (mkActiveSlotCoeff f) (SlotNo slot) of
        ActiveSlot (KeyHash h) ->
          defMessage & #kind .~ OVERLAY_SLOT_KIND_ACTIVE & #genesisKeyHash .~ hashToBytes h
        NonActiveSlot -> defMessage & #kind .~ OVERLAY_SLOT_KIND_NON_ACTIVE
