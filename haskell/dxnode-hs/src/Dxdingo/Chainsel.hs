{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Chain selection, answered by ouroboros-consensus: 'preferCandidate' on a
-- 'SelectView' for the block-number-then-tiebreaker order, and
-- 'preferCandidate' on a 'PraosTiebreakerView' alone for the tiebreaker.
module Dxdingo.Chainsel
  ( dispatch
  , implemented
  , compareTips
  , vrfTiebreak
  , genesisDensity
  , mkHeader
  ) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, SignedDSIGN (..), rawDeserialiseSigDSIGN, rawDeserialiseVerKeyDSIGN)
import Cardano.Crypto.Hash (Blake2b_256, castHash, hashWith)
import Cardano.Crypto.KES (SignedKES (..), rawDeserialiseSigKES, rawDeserialiseVerKeyKES)
import Cardano.Crypto.VRF (CertifiedVRF (..), mkTestOutputVRF, rawDeserialiseCertVRF, rawDeserialiseVerKeyVRF)
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion, unsafeNonZero)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Protocol.Praos.BlockHeader (HeaderBody (..), Header (..))
import Cardano.Protocol.TPraos.BlockHeader (HashHeader (..), PrevHash (..))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCert (..))
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (At))
import Cardano.Slotting.Time (RelativeTime (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (SJust))
import qualified Data.Set as Set
import Data.Word (Word64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Genesis.Governor (densityDisconnect)
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (ChainSyncState (..))
import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..))
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock, ShelleyHash (..), mkShelleyHeader)
import qualified Ouroboros.Consensus.Block.Abstract as B
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainOrder (..)
  , SelectView (..)
  , ShouldSwitch (..)
  )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosTiebreakerView (..)
  , VRFTiebreakerFlavor (..)
  )
import Proto.Dxdingo.V1.Chainsel
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.Types as P
import Prelude hiding (Ordering)

type Tiebreaker = PraosTiebreakerView StandardCrypto

-- | The tiebreaker view of one tip. An issuer key is 32 bytes and a VRF
-- output 64; any other length is not representable and is UNSUPPORTED, as
-- on dxnode-go.
tiebreaker :: Text -> Maybe ChainTip -> Result (ChainTip, Tiebreaker)
tiebreaker name = \case
  Nothing -> badRequest (name <> ": missing tip")
  Just t -> do
    vk <- case rawDeserialiseVerKeyDSIGN @Ed25519DSIGN (t ^. #issuerVkey) of
      Just k | BS.length (t ^. #issuerVkey) == 32 -> pure (VKey k)
      _ -> unsupported (name <> ": issuer_vkey must be 32 bytes")
    let v = t ^. #tiebreakVrf
    if BS.length v /= 64 then unsupported (name <> ": tiebreak_vrf must be 64 bytes") else pure ()
    pure
      ( t
      , PraosTiebreakerView
          { ptvSlotNo = SlotNo (t ^. #slot)
          , ptvIssuer = vk
          , ptvIssueNo = t ^. #issueNo
          , ptvTieBreakVRF = mkTestOutputVRF (BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 v)
          }
      )

flavor :: Maybe Word -> VRFTiebreakerFlavor
flavor = maybe UnrestrictedVRFTiebreaker (RestrictedVRFTiebreaker . SlotNo . fromIntegral)

-- | 'ShouldSwitch' is the candidate (b) winning; 'ShouldNotSwitch' carries
-- the ordering of ours (a) against the candidate.
orderOf :: ShouldSwitch r -> (P.Ordering, Bool)
orderOf = \case
  ShouldSwitch _ -> (P.ORDERING_LT, True)
  ShouldNotSwitch o -> (case o of LT -> P.ORDERING_LT; EQ -> P.ORDERING_EQ; GT -> P.ORDERING_GT, False)

-- | 'preferCandidate' on 'SelectView' (block number, then the tiebreaker).
compareTips :: ChainselCompareRequest -> Result ChainselCompareResponse
compareTips req = do
  if req ^. #era `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO, P.ERA_BABBAGE, P.ERA_CONWAY, P.ERA_DIJKSTRA]
    then pure ()
    else unsupported ("no Praos chain selection in " <> T.pack (show (req ^. #era)))
  (a, ta) <- tiebreaker "a" (req ^. #maybe'a)
  (b, tb) <- tiebreaker "b" (req ^. #maybe'b)
  let sv t tv = SelectView @(Praos StandardCrypto) (BlockNo (t ^. #blockNo)) tv
      (o, switch) = orderOf (preferCandidate (flavor (fromIntegral <$> req ^. #maybe'restrictedVrfWindow)) (sv a ta) (sv b tb))
  pure (defMessage & #order .~ o & #preferB .~ switch)

-- | 'preferCandidate' on 'PraosTiebreakerView', without block numbers.
vrfTiebreak :: ChainselVrfTiebreakRequest -> Result ChainselVrfTiebreakResponse
vrfTiebreak req = do
  (_, ta) <- tiebreaker "a" (req ^. #maybe'a)
  (_, tb) <- tiebreaker "b" (req ^. #maybe'b)
  pure (defMessage & #order .~ fst (orderOf (preferCandidate (flavor (fromIntegral <$> req ^. #maybe'restrictedVrfWindow)) ta tb)))

-- | The block type 'densityDisconnect' is run at: the Conway block, whose
-- 'HasHeader' and 'LedgerSupportsProtocol' instances the reference supplies.
type Blk = ShelleyBlock (Praos StandardCrypto) ConwayEra

-- | A header at the given slot and block number. @salt@ makes the block body
-- hash, and so the header hash, unique per block; every other field is
-- filler 'densityDisconnect' does not read.
mkHeader :: BS.ByteString -> Word64 -> Word64 -> PrevHash -> B.Header Blk
mkHeader salt blockNo slot prev = mkShelleyHeader (Header body sig)
 where
  must :: String -> Maybe a -> a
  must what = fromMaybe (error ("density filler: " ++ what))
  zeros n = BS.replicate n 0
  body =
    HeaderBody
      { hbBlockNo = BlockNo blockNo
      , hbSlotNo = SlotNo slot
      , hbPrev = prev
      , hbVk = VKey (must "issuer key" (rawDeserialiseVerKeyDSIGN (zeros 32)))
      , hbVrfVk = must "vrf key" (rawDeserialiseVerKeyVRF (zeros 32))
      , hbVrfRes = CertifiedVRF (mkTestOutputVRF 0) (must "vrf proof" (rawDeserialiseCertVRF (zeros 80)))
      , hbBodySize = 0
      , hbBodyHash = castHash (hashWith @Blake2b_256 id salt)
      , hbOCert =
          OCert
            { ocertVkHot = must "kes key" (rawDeserialiseVerKeyKES (zeros 32))
            , ocertN = 0
            , ocertKESPeriod = KESPeriod 0
            , ocertSigma = SignedDSIGN (must "opcert signature" (rawDeserialiseSigDSIGN (zeros 64)))
            }
      , hbProtVer = ProtVer (natVersion @10) 0
      }
  sig = SignedKES (must "kes signature" (rawDeserialiseSigKES (zeros 448)))

-- | 'densityDisconnect' on candidates that fork right after the shared
-- intersection at loe_slot. The request carries slots only, so each block's
-- hash is unique to its peer and position. dxnode-go compares peers pairwise
-- at their own intersection; the reference compares them all at the shared
-- one, and these inputs have the same intersection throughout.
genesisDensity :: ChainselGenesisDensityRequest -> Result ChainselGenesisDensityResponse
genesisDensity req = do
  window <- nonzero "genesis_window" (req ^. #genesisWindow)
  k <- nonzero "security_param" (req ^. #securityParam)
  let loe = req ^. #loeSlot
      maxSlot = toInteger (maxBound :: Int) :: Integer
  if window > maxSlot || toInteger loe > maxSlot - window
    then unsupported "loe_slot + genesis_window exceeds the slot range"
    else pure ()
  mapM_ validate (req ^. #candidates)
  let peers = map (^. #peer) (req ^. #candidates)
  if Set.size (Set.fromList peers) /= length peers then unsupported "duplicate candidate" else pure ()
  let loeNo = 1000
      loeHash = castHash (hashWith @Blake2b_256 id "intersection")
      anchor = AF.Anchor (SlotNo loe) (ShelleyHash loeHash) (BlockNo loeNo)
      loeFrag = AF.Empty anchor
      build c = go (AF.Empty anchor) (BlockHash (HashHeader loeHash)) (loeNo + 1) (zip [0 :: Int ..] (c ^. #slots))
       where
        go frag _ _ [] = frag
        go frag prev n ((i, s) : rest) =
          let h = mkHeader (BC.pack (show (c ^. #peer)) <> BC.pack ('/' : show i)) n s prev
              next = BlockHash (HashHeader (unShelleyHash (B.blockHash h)))
           in go (AF.addBlock (HeaderWithTime h (RelativeTime 0)) frag) next (n + 1) rest
      suffixes = [(c ^. #peer, build c) | c <- req ^. #candidates]
      states =
        Map.fromList
          [ ( c ^. #peer
            , ChainSyncState
                { csCandidate = build c
                , csIdling = c ^. #idling
                , csLatestSlot = SJust (At (SlotNo (c ^. #tipSlot)))
                }
            )
          | c <- req ^. #candidates
          ]
      (losers, _) = densityDisconnect (GenesisWindow (fromInteger window)) (SecurityParam (unsafeNonZero (fromInteger k))) states suffixes loeFrag
  pure (defMessage & #disconnect .~ sort losers)
 where
  validate c = do
    if T.null (c ^. #peer) then unsupported "candidate with no peer name" else pure ()
    let slots = c ^. #slots
        loe = req ^. #loeSlot
        ascending = and (zipWith (<) (loe : slots) slots)
        headSlot = if null slots then loe else last slots
    if not ascending
      then unsupported (c ^. #peer <> ": slots must ascend strictly after loe_slot")
      else pure ()
    if c ^. #tipSlot < headSlot
      then unsupported (c ^. #peer <> ": tip_slot is behind the candidate's headers")
      else pure ()

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'ChainselCompare q -> Just (respond #chainselCompare (compareTips q))
  Call'ChainselVrfTiebreak q -> Just (respond #chainselVrfTiebreak (vrfTiebreak q))
  Call'ChainselGenesisDensity q -> Just (respond #chainselGenesisDensity (genesisDensity q))
  _ -> Nothing

implemented :: [Text]
implemented = ["chainsel_compare", "chainsel_vrf_tiebreak", "chainsel_genesis_density"]
