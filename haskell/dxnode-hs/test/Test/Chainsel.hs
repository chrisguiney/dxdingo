{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Chainsel (tests) where

import Cardano.Crypto.Hash (Blake2b_256, castHash, hashWith)
import Cardano.Protocol.TPraos.BlockHeader (HashHeader (..), PrevHash (..))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (At))
import Cardano.Slotting.Time (RelativeTime (..))
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust))
import Dxdingo.Chainsel (mkHeader)
import Ouroboros.Consensus.Block.Abstract (GenesisWindow (..), blockHash)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Genesis.Governor (densityDisconnect)
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (ChainSyncState (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import Cardano.Ledger.BaseTypes (unsafeNonZero)
import qualified Data.ByteString as BS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Chainsel (ChainTip, DensityCandidate)
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Types (Era (..), Ordering (..))
import Test.Util
import Prelude hiding (Ordering)

-- | Hand-derived from comparePraos and the SelectView instance: block number
-- first, then the opcert issue number for the same issuer and slot, then the
-- lower VRF output; the restricted flavour arms the VRF only inside the window.
tests :: [Check]
tests =
  [ ("compare: higher block number wins", cmp Nothing (t 10 5 1 0 9) (t 9 5 2 0 1) == (ORDERING_GT, False))
  , ("compare: lower block number loses", cmp Nothing (t 9 5 1 0 1) (t 10 5 2 0 9) == (ORDERING_LT, True))
  , ("compare: same issuer and slot, higher issue number wins", cmp Nothing (t 7 5 1 1 1) (t 7 5 1 2 9) == (ORDERING_LT, True))
  , ("compare: issue number outranks the VRF", cmp Nothing (t 7 5 1 2 9) (t 7 5 1 1 1) == (ORDERING_GT, False))
  , ("compare: different issuers ignore issue numbers", cmp Nothing (t 7 5 1 9 9) (t 7 5 2 0 1) == (ORDERING_LT, True))
  , ("compare: lower VRF wins, unrestricted", cmp Nothing (t 7 5 1 0 1) (t 7 500 2 0 2) == (ORDERING_GT, False))
  , ("compare: candidate lower VRF wins, unrestricted", cmp Nothing (t 7 5 1 0 2) (t 7 500 2 0 1) == (ORDERING_LT, True))
  , ("compare: equal VRF ties", cmp Nothing (t 7 5 1 0 3) (t 7 6 2 0 3) == (ORDERING_EQ, False))
  , ("compare: restricted, distance equal to window arms VRF", cmp (Just 5) (t 7 10 1 0 2) (t 7 15 2 0 1) == (ORDERING_LT, True))
  , ("compare: restricted, distance past window disarms VRF", cmp (Just 5) (t 7 10 1 0 2) (t 7 16 2 0 1) == (ORDERING_EQ, False))
  , ("compare: restricted, distance is symmetric", cmp (Just 5) (t 7 16 1 0 2) (t 7 10 2 0 1) == (ORDERING_EQ, False))
  , ("compare: restricted, window zero needs the same slot", cmp (Just 0) (t 7 10 1 0 2) (t 7 11 2 0 1) == (ORDERING_EQ, False))
  , ("compare: restricted, issue number still arms", cmp (Just 0) (t 7 10 1 1 2) (t 7 10 1 2 9) == (ORDERING_LT, True))
  , ("compare: identical tips tie", cmp Nothing (t 7 5 1 2 3) (t 7 5 1 2 3) == (ORDERING_EQ, False))
  , ("vrf tiebreak: block numbers are ignored", tie Nothing (t 1 5 1 0 1) (t 99 5 2 0 2) == ORDERING_GT)
  , ("vrf tiebreak: candidate lower VRF", tie Nothing (t 1 5 1 0 2) (t 1 6 2 0 1) == ORDERING_LT)
  , ("vrf tiebreak: restricted window disarms", tie (Just 1) (t 1 5 1 0 2) (t 1 7 2 0 1) == ORDERING_EQ)
  , ("vrf tiebreak: issue number beats VRF", tie Nothing (t 1 5 1 2 9) (t 1 5 1 1 1) == ORDERING_GT)
  , ("density: denser peer beats a sparser one", dens 2 [cand "a" False 110 full, cand "b" False 105 [105]] == ["b"])
  , ("density: idling peer is compared at its delivered density", dens 2 [cand "a" False 104 [101, 102, 103, 104], cand "b" True 105 [105]] == ["b"])
  , ("density: a rival with k or fewer headers does not qualify", dens 20 [cand "a" False 104 [101, 102, 103, 104], cand "b" True 105 [105]] == [])
  , ("density: a peer with no headers is not compared", dens 2 [cand "a" False 110 full, cand "b" False 100 []] == [])
  , ("density: a lone candidate is never disconnected", dens 2 [cand "a" False 105 [105]] == [])
  , ("density: a rival meeting the bound exactly dominates", dens 2 [cand "a" False 103 [101, 102, 103], cand "b" False 110 full] == ["a"])
  , ("density: zero window is UNSUPPORTED", code (densCall 0 2 [cand "a" False 105 [105]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("density: zero security parameter is UNSUPPORTED", code (densCall 10 0 [cand "a" False 105 [105]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("density: slots at or before the loe are UNSUPPORTED", code (densCall 10 2 [cand "a" False 105 [100, 105]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("density: slots out of order are UNSUPPORTED", code (densCall 10 2 [cand "a" False 105 [105, 101]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("density: tip slot behind the headers is UNSUPPORTED", code (densCall 10 2 [cand "a" False 104 [101, 105]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("density: duplicate peer is UNSUPPORTED", code (densCall 10 2 [cand "a" False 105 [105], cand "a" False 105 [105]]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , -- Divergences from dxnode-go's chainselection.FindGenesisDensityLosers,
    -- confirmed by calling the reference's densityDisconnect directly
    -- (dxnode-go TestDivergenceDensity*). Window 101..110, k = 2.
    ("divergence: equal density disconnects both peers", direct [("a", False, 110, [101, 105, 110]), ("b", False, 110, [102, 108, 110])] == ["a", "b"])
  , ("divergence: an idling peer with no headers is disconnected", direct [("a", False, 110, [101 .. 110]), ("b", True, 100, [])] == ["b"])
  , ("divergence: a latest slot past the window closes the upper bound", direct [("a", False, 104, [101 .. 104]), ("b", False, 120, [105])] == ["b"])
  , ("divergence control: without the latest slot b can still reach 6 headers", direct [("a", False, 104, [101 .. 104]), ("b", False, 105, [105])] == [])
  , ("compare: byron is UNSUPPORTED", code (compareCall ERA_BYRON Nothing (t 1 1 1 0 1) (t 1 1 2 0 1)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("compare: short VRF output is UNSUPPORTED", code (compareCall ERA_CONWAY Nothing (shortVrf (t 1 1 1 0 1)) (t 1 1 2 0 1)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("compare: missing tip is BAD_REQUEST", code (call (#chainselCompare .~ (defMessage & #era .~ ERA_CONWAY & #a .~ t 1 1 1 0 1))) == HARNESS_ERROR_CODE_BAD_REQUEST)
  ]
  where
    t :: Word64 -> Word64 -> Word8 -> Word64 -> Word8 -> ChainTip
    t b s i n v =
      defMessage & #blockNo .~ b & #slot .~ s & #issuerVkey .~ BS.replicate 32 i
        & #issueNo .~ n & #tiebreakVrf .~ BS.replicate 64 v
    shortVrf x = x & #tiebreakVrf .~ BS.replicate 63 1
    withWindow w m = maybe m (\x -> m & #restrictedVrfWindow .~ x) w
    compareCall era w a b =
      call (#chainselCompare .~ (defMessage & #era .~ era & #a .~ a & #b .~ b & withWindow w))
    cmp w a b =
      let r = compareCall ERA_CONWAY w a b ^. #chainselCompare
       in (r ^. #order, r ^. #preferB)
    tie w a b =
      call (#chainselVrfTiebreak .~ (defMessage & #a .~ a & #b .~ b & withWindow w))
        ^. #chainselVrfTiebreak . #order
    code r = r ^. #error . #code
    full = [101 .. 110]
    -- densityDisconnect on hand-built fragments forking at slot 100.
    direct :: [(Text, Bool, Word64, [Word64])] -> [Text]
    direct peers =
      let loeHash = castHash (hashWith @Blake2b_256 id "intersection")
          anchor = AF.Anchor (SlotNo 100) (ShelleyHash loeHash) (BlockNo 1000)
          frag name slots = go (AF.Empty anchor) (BlockHash (HashHeader loeHash)) 1001 (zip [0 :: Int ..] slots)
            where
              go f _ _ [] = f
              go f prev n ((i, sl) : rest) =
                let h = mkHeader (BC.pack (show name ++ "/" ++ show i)) n sl prev
                 in go (AF.addBlock (HeaderWithTime h (RelativeTime 0)) f) (BlockHash (HashHeader (unShelleyHash (blockHash h)))) (n + 1) rest
          suffixes = [(n, frag n sl) | (n, _, _, sl) <- peers]
          states = Map.fromList [(n, ChainSyncState (frag n sl) idle (SJust (At (SlotNo tip)))) | (n, idle, tip, sl) <- peers]
       in sort (fst (densityDisconnect (GenesisWindow 10) (SecurityParam (unsafeNonZero 2)) states suffixes (AF.Empty anchor)))
    cand :: Text -> Bool -> Word64 -> [Word64] -> DensityCandidate
    cand p idle tip slots = defMessage & #peer .~ p & #idling .~ idle & #tipSlot .~ tip & #slots .~ slots
    densCall w k cs =
      call (#chainselGenesisDensity .~ (defMessage & #genesisWindow .~ w & #securityParam .~ k & #loeSlot .~ 100 & #candidates .~ cs))
    dens k cs = densCall 10 k cs ^. #chainselGenesisDensity . #disconnect
