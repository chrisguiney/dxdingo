{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Header validity and nonce evolution, answered by
-- ouroboros-consensus-protocol (Praos), cardano-protocol-tpraos and
-- cardano-ledger-core.
module Dxdingo.Praos
  ( dispatch
  , implemented
  , leaderCheck
  , nonceEpoch
  , overlaySchedule
  , headerEnvelope
  , vrfCheck
  , kesCheck
  , opcertCheck
  , vrfKeyRegistered
  , nonceRolling
  , nonceCandidate
  ) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (Hash, hashFromBytes, hashToBytes)
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff
  , BoundedRational (boundRational, unboundRational)
  , Globals (..)
  , Network (..)
  , Nonce (..)
  , PositiveUnitInterval
  , ProtVer (..)
  , UnitInterval
  , Version
  , mkActiveSlotCoeff
  , mkNonceFromOutputVRF
  , natVersion
  , unsafeNonZero
  , (⭒)
  )
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..), HASH, HashHeader (..), originalBytesSize)
import Cardano.Ledger.Keys (KeyHash (..), VKey (..), coerceKeyRole, hashKey, toVRFVerKeyHash)
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Shelley.StabilityWindow (computeRandomnessStabilisationWindow, computeStabilityWindow)
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (StandardCrypto, VRF)
import Cardano.Protocol.Praos.BlockHeader (Header (..), HeaderBody (..), headerSize)
import Cardano.Protocol.Praos.VRF (InputVRF, vrfLeaderValue, vrfNonceValue)
import Cardano.Protocol.TPraos.BlockHeader (BHBody (..), BHeader, BoundedNatural (bvValue), PrevHash (..), bhbody)
import qualified Cardano.Protocol.TPraos.Rules.OCert as OC
import qualified Cardano.Protocol.TPraos.Rules.Overlay as OV
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCert (..))
import Cardano.Protocol.TPraos.Rules.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad (foldM, when)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.Functor (void)
import Data.Functor.Identity (runIdentity)
import GHC.Exts (fromList)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import Numeric.Natural (Natural)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Cardano.Slotting.Block (BlockNo (..))
import Ouroboros.Consensus.HeaderValidation
  ( BasicEnvelopeValidation (expectedFirstBlockNo, expectedNextBlockNo, minimumNextSlotNo, minimumPossibleSlotNo)
  )
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (reupdateChainDepState))
import Ouroboros.Consensus.Protocol.TPraos ()
import Ouroboros.Consensus.Shelley.HFEras (StandardConwayBlock, StandardShelleyBlock)
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , PraosParams (..)
  , PraosState (..)
  , PraosValidationErr (..)
  , Ticked (..)
  , doValidateKESSignature
  , doValidateVRFSignature
  )
import qualified Ouroboros.Consensus.Protocol.Praos as PraosP
import Ouroboros.Consensus.Protocol.Praos.Common (MaxMajorProtVer (..))
import Ouroboros.Consensus.Protocol.Praos.Views (HeaderView (..), PraosLedgerView (..))
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks (EnvelopeError (..), EnvelopeHeaderView (..), envelopeCheck)
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
import Dxdingo.Answer (Answer, respond)
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Data.Text (Text)
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

-- | The one crypto the Praos eras use.
type C = StandardCrypto

-- | A rejection of one failure under a rule.
rejection :: Text -> Text -> Text -> P.Rejection
rejection rule name detail =
  defMessage & #failures .~ [defMessage & #rule .~ rule & #name .~ name & #detail .~ detail]

-- | The header format of a Praos era, as the protocol version its CBOR is
-- decoded at: Babbage 7, Conway 9. Earlier eras run TPraos and Dijkstra is not
-- modelled.
praosVersion :: P.Era -> Result Version
praosVersion = \case
  P.ERA_BABBAGE -> pure (natVersion @7)
  P.ERA_CONWAY -> pure (natVersion @9)
  e -> unsupported ("no Praos header check in " <> T.pack (show e))

-- | Decode a block header as it appears in a block. A decode failure is the
-- rejection DeserialiseFailure.
decodeHeader :: P.Era -> BS.ByteString -> Result (Either P.Rejection (Header C))
decodeHeader era raw = do
  v <- praosVersion era
  pure $ case decodeFullAnnotator v "Header" decCBOR (BL.fromStrict raw) of
    Left err -> Left (rejection "header" "DeserialiseFailure" (T.pack (show err)))
    Right h -> Right h

-- | The fields Praos reads from a header, as the consensus layer builds them.
headerView :: Header C -> HeaderView C
headerView (Header body sig) =
  HeaderView
    { hvPrevHash = hbPrev body
    , hvVK = hbVk body
    , hvVrfVK = hbVrfVk body
    , hvVrfRes = hbVrfRes body
    , hvOCert = hbOCert body
    , hvSlotNo = hbSlotNo body
    , hvSigned = body
    , hvSignature = sig
    }

poolKeyHash :: HeaderView C -> KeyHash Keys.StakePool
poolKeyHash = coerceKeyRole . hashKey . hvVK

-- | The failure name of a Praos validation error.
praosErrName :: PraosValidationErr c -> Text
praosErrName = \case
  VRFKeyUnknown {} -> "VRFKeyUnknown"
  VRFKeyWrongVRFKey {} -> "VRFKeyWrongVRFKey"
  VRFKeyBadProof {} -> "VRFKeyBadProof"
  VRFLeaderValueTooBig {} -> "VRFLeaderValueTooBig"
  KESBeforeStartOCERT {} -> "KESBeforeStartOCERT"
  KESAfterEndOCERT {} -> "KESAfterEndOCERT"
  CounterTooSmallOCERT {} -> "CounterTooSmallOCERT"
  CounterOverIncrementedOCERT {} -> "CounterOverIncrementedOCERT"
  InvalidSignatureOCERT {} -> "InvalidSignatureOCERT"
  InvalidKesSignatureOCERT {} -> "InvalidKesSignatureOCERT"
  NoCounterForKeyHashOCERT {} -> "NoCounterForKeyHashOCERT"

praosRejection :: PraosValidationErr C -> P.Rejection
praosRejection e = rejection "Praos" (praosErrName e) (T.pack (show e))

-- | An active slot coefficient of one, for which 'checkLeaderNatValue'
-- always succeeds, so the VRF and KES checks run without a leader threshold.
alwaysLeader :: Result ActiveSlotCoeff
alwaysLeader = maybe (internal "unit interval one") (pure . mkActiveSlotCoeff) (boundRational 1)

hashArg :: Text -> Int -> BS.ByteString -> Result BS.ByteString
hashArg name n b
  | BS.length b == n = pure b
  | otherwise = badRequest (name <> " must be " <> T.pack (show n) <> " bytes")

-- | The pool distribution entry the VRF check reads: the pool's registered
-- VRF key hash; the stake is irrelevant when the coefficient is one.
poolEntry :: BS.ByteString -> Result SL.IndividualPoolStake
poolEntry vrfHash = do
  h <- maybe (badRequest "vrf key hash must be 32 bytes") pure (hashFromBytes vrfHash :: Maybe (Hash HASH (VRF.VerKeyVRF (VRF C))))
  pure (SL.IndividualPoolStake 1 (CompactCoin 0) (toVRFVerKeyHash h))

-- | The eras whose headers run TPraos.
isTPraos :: P.Era -> Bool
isTPraos e = e `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO]

-- | The protocol version a TPraos era's header is decoded at.
tpraosVersion :: P.Era -> Result Version
tpraosVersion = \case
  P.ERA_SHELLEY -> pure (natVersion @2)
  P.ERA_ALLEGRA -> pure (natVersion @3)
  P.ERA_MARY -> pure (natVersion @4)
  P.ERA_ALONZO -> pure (natVersion @5)
  e -> unsupported ("no TPraos header check in " <> T.pack (show e))

-- | Decode a TPraos block header as it appears in a block. A decode failure is
-- the rejection DeserialiseFailure.
decodeBHeader :: P.Era -> BS.ByteString -> Result (Either P.Rejection (BHeader C))
decodeBHeader era raw = do
  v <- tpraosVersion era
  pure $ case decodeFullAnnotator v "Header" decCBOR (BL.fromStrict raw) of
    Left err -> Left (rejection "header" "DeserialiseFailure" (T.pack (show err)))
    Right h -> Right h

-- | The protocol version major of an operational-certificate request names the
-- header format: 2 Shelley, 3 Allegra, 4 Mary, 5 and 6 Alonzo.
tpraosEraOfMajor :: Word64 -> Maybe P.Era
tpraosEraOfMajor = \case
  2 -> Just P.ERA_SHELLEY
  3 -> Just P.ERA_ALLEGRA
  4 -> Just P.ERA_MARY
  5 -> Just P.ERA_ALONZO
  6 -> Just P.ERA_ALONZO
  _ -> Nothing

-- | A predicate failure as (rule, name, detail).
type Failure = (Text, Text, Text)

failuresReply :: [Failure] -> P.Rejection
failuresReply fs = defMessage & #failures .~ [defMessage & #rule .~ r & #name .~ n & #detail .~ d | (r, n, d) <- fs]

ocertFailure :: OC.OcertPredicateFailure -> Failure
ocertFailure f = ("OCERT", name, T.pack (show f))
  where
    name = case f of
      OC.KESBeforeStartOCERT {} -> "KESBeforeStartOCERT"
      OC.KESAfterEndOCERT {} -> "KESAfterEndOCERT"
      OC.CounterTooSmallOCERT {} -> "CounterTooSmallOCERT"
      OC.InvalidSignatureOCERT {} -> "InvalidSignatureOCERT"
      OC.InvalidKesSignatureOCERT {} -> "InvalidKesSignatureOCERT"
      OC.NoCounterForKeyHashOCERT {} -> "NoCounterForKeyHashOCERT"

overlayFailure :: OV.OverlayPredicateFailure C -> Failure
overlayFailure = \case
  OV.OcertFailure f -> ocertFailure f
  f -> ("OVERLAY", name f, T.pack (show f))
  where
    name = \case
      OV.VRFKeyUnknown {} -> "VRFKeyUnknown"
      OV.VRFKeyWrongVRFKey {} -> "VRFKeyWrongVRFKey"
      OV.VRFKeyBadNonce {} -> "VRFKeyBadNonce"
      OV.VRFKeyBadLeaderValue {} -> "VRFKeyBadLeaderValue"
      OV.VRFLeaderValueTooBig {} -> "VRFLeaderValueTooBig"
      OV.NotActiveSlotOVERLAY {} -> "NotActiveSlotOVERLAY"
      OV.WrongGenesisColdKeyOVERLAY {} -> "WrongGenesisColdKeyOVERLAY"
      OV.WrongGenesisVRFKeyOVERLAY {} -> "WrongGenesisVRFKeyOVERLAY"
      OV.UnknownGenesisKeyOVERLAY {} -> "UnknownGenesisKeyOVERLAY"
      OV.OcertFailure {} -> "OcertFailure"

-- | Globals for the OVERLAY and OCERT rules: the epoch length that fixes the
-- first slot of the header's epoch, the active slot coefficient, and the KES
-- parameters.
ruleGlobals :: Word64 -> ActiveSlotCoeff -> Word64 -> Word64 -> Globals
ruleGlobals epochLength asc spkp maxEvo =
  (tpraosGlobals (fixedEpochInfo (EpochSize epochLength) (mkSlotLength 1)) 1 asc 0 0)
    { slotsPerKESPeriod = spkp
    , maxKESEvo = maxEvo
    }

type Counters = Map.Map (KeyHash Keys.BlockIssuer) Word64

-- | The OVERLAY rule over one header.
runOverlay :: Globals -> OV.OverlayEnv -> Counters -> BHeader C -> [OV.OverlayPredicateFailure C]
runOverlay g env cs bh =
  either NE.toList (const []) (void <$> runIdentity (runReaderT (applySTS @(OV.OVERLAY C) (TRC (env, cs, bh))) g))

-- | The OCERT rule over one header.
runOcert :: Globals -> OC.OCertEnv -> Counters -> BHeader C -> [OC.OcertPredicateFailure]
runOcert g env cs bh =
  either NE.toList (const []) (void <$> runIdentity (runReaderT (applySTS @(OC.OCERT C) (TRC (env, cs, bh))) g))

issuerKeyHash :: BHBody C -> KeyHash Keys.BlockIssuer
issuerKeyHash = hashKey . bheaderVk

-- | The overlay environment of a TPraos VRF check: without one no slot is an
-- overlay slot (d = 0, f = 1).
data Overlay = Overlay
  { ovD :: UnitInterval
  , ovF :: PositiveUnitInterval
  , ovLength :: Word64
  , ovDelegs :: Map.Map (KeyHash Keys.GenesisRole) GenDelegPair
  }

overlayArg :: Maybe TpraosOverlayEnv -> Result Overlay
overlayArg Nothing = do
  d <- maybe (internal "unit interval zero") pure (boundRational 0)
  f <- maybe (internal "unit interval one") pure (boundRational 1)
  pure (Overlay d f 1 Map.empty)
overlayArg (Just o) = do
  d <- unitIntervalArg "overlay.d" (o ^. #maybe'd)
  f <- positiveUnitIntervalArg "overlay.active_slot_coeff" (o ^. #maybe'activeSlotCoeff)
  len <- nonzero' "overlay.epoch_length" (o ^. #epochLength)
  pairs <- traverse pair (o ^. #genesisDelegations)
  if Map.size (Map.fromList pairs) /= length pairs then unsupported "duplicate genesis key" else pure ()
  if unboundRational d > 0 && null pairs then unsupported "no genesis keys" else pure ()
  pure (Overlay d f len (Map.fromList pairs))
  where
    pair g = do
      gk <- hashArg "genesis_key_hash" 28 (g ^. #genesisKeyHash)
      dk <- hashArg "delegate_key_hash" 28 (g ^. #delegateKeyHash)
      vk <- hashArg "vrf_key_hash" 32 (g ^. #vrfKeyHash)
      gkh <- maybe (badRequest "genesis_key_hash must be 28 bytes") (pure . KeyHash) (hashFromBytes gk)
      dkh <- maybe (badRequest "delegate_key_hash must be 28 bytes") (pure . KeyHash) (hashFromBytes dk)
      vh <- maybe (badRequest "vrf_key_hash must be 32 bytes") pure (hashFromBytes vk :: Maybe (Hash HASH (VRF.VerKeyVRF (VRF C))))
      pure (gkh, GenDelegPair dkh (toVRFVerKeyHash vh))

-- | The VRF checks of OVERLAY on a TPraos header, the failures of its OCERT
-- sub-rule left out. The header's issuer is registered under the given VRF key
-- hash outside the overlay schedule.
tpraosVrfFailures :: Overlay -> Nonce -> BS.ByteString -> BHeader C -> Result [Failure]
tpraosVrfFailures ov eta0 keyHash bh = do
  entry <- poolEntry keyHash
  let body = bhbody bh
      slot@(SlotNo s) = bheaderSlotNo body
      first = SlotNo (s - s `mod` ovLength ov)
      pd = SL.PoolDistr (Map.singleton (coerceKeyRole (issuerKeyHash body)) entry) (unsafeNonZero (Coin 1))
      asc = mkActiveSlotCoeff (ovF ov)
      g = ruleGlobals (ovLength ov) asc 129600 62
      cs = Map.singleton (issuerKeyHash body) 0
      env = OV.OverlayEnv (ovD ov) pd (GenDelegs (ovDelegs ov)) eta0
  -- Outside the overlay schedule the leader threshold applies, with the
  -- issuer holding all the stake; it cannot fail only when f = 1.
  when (not (isOverlaySlot first (ovD ov) slot) && unboundRational (ovF ov) /= 1) $
    unsupported "a slot outside the overlay schedule needs active_slot_coeff 1: the leader threshold is not part of the request"
  pure [overlayFailure f | f <- runOverlay g env cs bh, notOcert f]
  where
    notOcert = \case
      OV.OcertFailure {} -> False
      _ -> True

-- | The TPraos branch of the VRF check.
tpraosVrfCheck :: PraosVrfCheckRequest -> Result PraosVrfCheckResponse
tpraosVrfCheck req = do
  let era = req ^. #era
  _ <- tpraosVersion era
  eta0 <- case req ^. #epochNonce . #hash of
    bs
      | BS.null bs -> unsupported "neutral epoch nonce"
      | otherwise -> maybe (badRequest "epoch_nonce must be 32 bytes") (pure . Nonce) (hashFromBytes bs)
  keyHash <- hashArg "pool_vrf_key_hash" 32 (req ^. #poolVrfKeyHash)
  ov <- overlayArg (req ^. #maybe'overlay)
  decoded <- decodeBHeader era (req ^. #header)
  case decoded of
    Left rej -> pure (defMessage & #rejection .~ rej)
    Right bh -> do
      fs <- tpraosVrfFailures ov eta0 keyHash bh
      let body = bhbody bh
      pure $
        if null fs
          then
            defMessage
              & #leaderValue .~ VRF.getOutputVRFBytes (VRF.certifiedOutput (bheaderL body))
              & #nonceValue .~ nonceBytes (mkNonceFromOutputVRF (VRF.certifiedOutput (bheaderEta body)))
          else defMessage & #rejection .~ failuresReply fs

-- | The TPraos branch of the KES check: OCERT with a counter that passes.
tpraosKesCheck :: PraosKesCheckRequest -> Result PraosKesCheckResponse
tpraosKesCheck req = do
  _ <- tpraosVersion (req ^. #era)
  spkp <- nonzero' "slots_per_kes_period" (req ^. #slotsPerKesPeriod)
  maxEvo <- nonzero' "max_kes_evolutions" (req ^. #maxKesEvolutions)
  decoded <- decodeBHeader (req ^. #era) (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right bh ->
      let body = bhbody bh
          OCert _ n (KESPeriod c0) _ = bheaderOCert body
          SlotNo slot = bheaderSlotNo body
          cs = Map.singleton (issuerKeyHash body) n
          g = ruleGlobals 1 (alwaysLeaderCoeff) spkp maxEvo
       in case runOcert g (OC.OCertEnv mempty mempty) cs bh of
            [] -> defMessage & #kesPeriod .~ (slot `div` spkp - fromIntegral c0)
            fs -> defMessage & #rejection .~ failuresReply (map ocertFailure fs)

-- | The TPraos branch of the operational certificate check: OCERT with the
-- recorded counter, or the issuer registered as a pool or genesis delegate.
tpraosOpcertCheck :: P.Era -> PraosOpCertCheckRequest -> Result PraosOpCertCheckResponse
tpraosOpcertCheck era req = do
  spkp <- nonzero' "slots_per_kes_period" (req ^. #slotsPerKesPeriod)
  maxEvo <- nonzero' "max_kes_evolutions" (req ^. #maxKesEvolutions)
  decoded <- decodeBHeader era (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right bh ->
      let body = bhbody bh
          OCert _ n _ _ = bheaderOCert body
          hk = issuerKeyHash body
          cs = maybe Map.empty (Map.singleton hk) (req ^. #maybe'lastCounter)
          pools = if req ^. #poolRegistered then Set.singleton (coerceKeyRole hk) else Set.empty
          genesis = if req ^. #issuerIsGenesisDelegate then Set.singleton (coerceKeyRole hk) else Set.empty
          g = ruleGlobals 1 alwaysLeaderCoeff spkp maxEvo
       in case runOcert g (OC.OCertEnv pools genesis) cs bh of
            [] -> defMessage & #newCounter .~ n
            fs -> defMessage & #rejection .~ failuresReply (map ocertFailure fs)

-- | The TPraos branch of the VRF key registration check: the pool and key hash
-- lookups of OVERLAY under the neutral epoch nonce and no overlay slots.
tpraosVrfKeyRegistered :: P.Era -> [(KeyHash Keys.StakePool, SL.IndividualPoolStake)] -> BS.ByteString -> Result PraosVrfKeyRegisteredResponse
tpraosVrfKeyRegistered era entries raw = do
  ov <- overlayArg Nothing
  decoded <- decodeBHeader era raw
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right bh ->
      let body = bhbody bh
          pool = coerceKeyRole (issuerKeyHash body) :: KeyHash Keys.StakePool
          pd = SL.PoolDistr (Map.fromList entries) (unsafeNonZero (Coin 1))
          g = ruleGlobals 1 (mkActiveSlotCoeff (ovF ov)) 129600 62
          env = OV.OverlayEnv (ovD ov) pd (GenDelegs Map.empty) NeutralNonce
          cs = Map.singleton (issuerKeyHash body) 0
          registration = [overlayFailure f | f <- runOverlay g env cs bh, isRegistration f]
       in case registration of
            [] -> defMessage & #poolId .~ (let KeyHash h = pool in hashToBytes h)
            fs -> defMessage & #rejection .~ failuresReply fs
  where
    isRegistration = \case
      OV.VRFKeyUnknown {} -> True
      OV.VRFKeyWrongVRFKey {} -> True
      _ -> False

-- | An active slot coefficient of one for rules whose leader threshold is not
-- under test.
alwaysLeaderCoeff :: ActiveSlotCoeff
alwaysLeaderCoeff = mkActiveSlotCoeff one
  where
    one = case boundRational 1 of
      Just u -> u
      Nothing -> error "unit interval one"

-- | The position checks of 'validateEnvelope', then 'envelopeCheck': the
-- header's encoded size against the maximum header size, then the body size
-- it declares against the maximum body size. The obsolete-node bound is set to
-- the header's own version, so it never fires.
headerEnvelope :: PraosHeaderEnvelopeRequest -> Result PraosHeaderEnvelopeResponse
headerEnvelope req = do
  let era = req ^. #era
  v <- if isTPraos era then tpraosVersion era else praosVersion era
  pparams <- maybe (badRequest "pparams is required") pure (req ^. #maybe'pparams)
  maxHeader <- limit "max_block_header_size" (pparams ^. #maybe'maxBlockHeaderSize)
  maxBody <- limit "max_block_body_size" (pparams ^. #maybe'maxBlockBodySize)
  if maxHeader > fromIntegral (maxBound :: Word16) then unsupported "max_block_header_size does not fit a Word16" else pure ()
  parent <- parentArg req
  if isTPraos era
    then do
      decoded <- decodeBHeader era (req ^. #header)
      pure $ case decoded of
        Left rej -> defMessage & #rejection .~ rej
        Right hdr ->
          let body = bhbody hdr
              ProtVer m _ = bprotver body
              ccd = ChainChecksPParams {ccMaxBHSize = fromIntegral maxHeader, ccMaxBBSize = maxBody, ccProtocolVersion = ProtVer m 0}
              view = EnvelopeHeaderView {ehvProtVer = m, ehvHeaderSize = originalBytesSize hdr, ehvBodySize = bsize body}
           in envelopeReply
                (positionFailure (Proxy @StandardShelleyBlock) parent (bheaderBlockNo body) (bheaderSlotNo body) (bheaderPrev body))
                (runExcept (envelopeCheck m ccd view))
    else do
      decoded <- decodeHeader era (req ^. #header)
      pure $ case decoded of
        Left rej -> defMessage & #rejection .~ rej
        Right hdr@(Header body _) ->
          let ccd =
                ChainChecksPParams
                  { ccMaxBHSize = fromIntegral maxHeader
                  , ccMaxBBSize = maxBody
                  , ccProtocolVersion = ProtVer v 0
                  }
              view =
                EnvelopeHeaderView
                  { ehvProtVer = v
                  , ehvHeaderSize = headerSize hdr
                  , ehvBodySize = hbBodySize body
                  }
           in envelopeReply
                (positionFailure (Proxy @StandardConwayBlock) parent (hbBlockNo body) (hbSlotNo body) (hbPrev body))
                (runExcept (envelopeCheck v ccd view))
  where
    limit name = maybe (badRequest (name <> " is required")) pure
    envelopeReply position sizes = case (position, sizes) of
      (Just (name, detail), _) -> defMessage & #rejection .~ rejection "Envelope" name detail
      (Nothing, Left e) -> defMessage & #rejection .~ rejection "Envelope" (envelopeErrName e) (T.pack (show e))
      (Nothing, Right ()) -> defMessage

-- | The tip a header is checked against.
data Parent
  = -- | No position check.
    NoParent
  | -- | The header extends the empty chain.
    AtOrigin
  | Tip Word64 Word64 PrevHash

parentArg :: PraosHeaderEnvelopeRequest -> Result Parent
parentArg req = case req ^. #maybe'parent of
  Just t -> do
    h <- hashArg "parent.hash" 32 (t ^. #hash)
    hh <- maybe (badRequest "parent.hash must be 32 bytes") (pure . HashHeader) (hashFromBytes h)
    if t ^. #blockNo == maxBound || t ^. #slot == maxBound
      then unsupported "parent block number or slot is the largest Word64: its successor is not representable"
      else pure (Tip (t ^. #blockNo) (t ^. #slot) (BlockHash hh))
  Nothing
    | req ^. #origin -> pure AtOrigin
    | otherwise -> pure NoParent

-- | The three position checks of 'validateEnvelope' in its order: block
-- number, slot, then the previous hash. The expected block number and the
-- minimum slot are the 'BasicEnvelopeValidation' methods of the block type, not
-- restated here.
positionFailure ::
  forall blk.
  BasicEnvelopeValidation blk =>
  Proxy blk ->
  Parent ->
  BlockNo ->
  SlotNo ->
  PrevHash ->
  Maybe (Text, Text)
positionFailure p parent no slot prev = case parent of
  NoParent -> Nothing
  AtOrigin -> go (expectedFirstBlockNo p) (minimumPossibleSlotNo p) (prev == GenesisHash)
  Tip pno pslot h ->
    go
      (expectedNextBlockNo p (error "tip info") (error "tip info") (BlockNo pno))
      (minimumNextSlotNo p (error "tip info") (error "tip info") (SlotNo pslot))
      (prev == h)
  where
    go expectedNo minSlot hashOk
      | no /= expectedNo = Just ("UnexpectedBlockNo", T.pack (show (expectedNo, no)))
      | slot < minSlot = Just ("UnexpectedSlotNo", T.pack (show (minSlot, slot)))
      | not hashOk = Just ("UnexpectedPrevHash", T.pack (show prev))
      | otherwise = Nothing

envelopeErrName :: EnvelopeError -> Text
envelopeErrName = \case
  ObsoleteNode {} -> "ObsoleteNode"
  HeaderSizeTooLarge {} -> "HeaderSizeTooLarge"
  BlockSizeTooLarge {} -> "BlockSizeTooLarge"

-- | 'doValidateVRFSignature' with the header's issuer registered under the
-- given VRF key hash: key hash first, then the proof over the Praos VRF input.
-- The leader and nonce values are 'vrfLeaderValue' and 'vrfNonceValue'.
vrfCheck :: PraosVrfCheckRequest -> Result PraosVrfCheckResponse
vrfCheck req | isTPraos (req ^. #era) = tpraosVrfCheck req
vrfCheck req = do
  _ <- praosVersion (req ^. #era)
  eta0 <- case req ^. #epochNonce . #hash of
    bs
      | BS.null bs -> unsupported "neutral epoch nonce"
      | otherwise -> maybe (badRequest "epoch_nonce must be 32 bytes") (pure . Nonce) (hashFromBytes bs)
  _ <- hashArg "pool_vrf_key_hash" 32 (req ^. #poolVrfKeyHash)
  entry <- poolEntry (req ^. #poolVrfKeyHash)
  f <- alwaysLeader
  decoded <- decodeHeader (req ^. #era) (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right hdr ->
      let view = headerView hdr
          pd = Map.singleton (poolKeyHash view) entry
       in case runExcept (doValidateVRFSignature eta0 pd f view) of
            Left e -> defMessage & #rejection .~ praosRejection e
            Right () ->
              defMessage
                & #leaderValue .~ naturalBytes 32 (bvValue (vrfLeaderValue (Proxy @C) (hvVrfRes view)))
                & #nonceValue .~ nonceBytes (vrfNonceValue (Proxy @C) (hvVrfRes view))

naturalBytes :: Int -> Natural -> BS.ByteString
naturalBytes n x = BS.pack (reverse (take n (go x ++ repeat 0)))
  where
    go 0 = []
    go k = fromIntegral (k `mod` 256) : go (k `div` 256)

nonceBytes :: Nonce -> BS.ByteString
nonceBytes = \case
  NeutralNonce -> BS.empty
  Nonce h -> hashToBytes h

-- | 'doValidateKESSignature' with a counter that passes: the KES period
-- window, the operational certificate's cold signature, then the KES
-- signature. The reported kes_period is the evolution, the header's KES period
-- less the certificate's start period.
kesCheck :: PraosKesCheckRequest -> Result PraosKesCheckResponse
kesCheck req | isTPraos (req ^. #era) = tpraosKesCheck req
kesCheck req = do
  _ <- praosVersion (req ^. #era)
  spkp <- nonzero' "slots_per_kes_period" (req ^. #slotsPerKesPeriod)
  maxEvo <- nonzero' "max_kes_evolutions" (req ^. #maxKesEvolutions)
  decoded <- decodeHeader (req ^. #era) (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right hdr ->
      let view = headerView hdr
          OCert _ n (KESPeriod c0) _ = hvOCert view
          counters = Map.singleton (hashKey (hvVK view)) n
          SlotNo slot = hvSlotNo view
       in case runExcept (doValidateKESSignature maxEvo spkp Map.empty counters view) of
            Left e -> defMessage & #rejection .~ praosRejection e
            Right () -> defMessage & #kesPeriod .~ (slot `div` spkp - fromIntegral c0)

nonzero' :: Text -> Word64 -> Result Word64
nonzero' name x = fromIntegral <$> nonzero name x

-- | 'doValidateKESSignature' with the recorded counter, or the pool in the
-- stake distribution when none is recorded: after the period window and
-- signatures it checks counter m <= n <= m + 1.
opcertCheck :: PraosOpCertCheckRequest -> Result PraosOpCertCheckResponse
opcertCheck req
  | Just era <- tpraosEraOfMajor (fromIntegral (req ^. #protocolVersion . #major)) = tpraosOpcertCheck era req
opcertCheck req = do
  era <- case req ^. #protocolVersion . #major of
    m | m >= 9 -> pure P.ERA_CONWAY
    m | m >= 7 -> pure P.ERA_BABBAGE
    m -> unsupported ("no header format at protocol version " <> T.pack (show m))
  spkp <- nonzero' "slots_per_kes_period" (req ^. #slotsPerKesPeriod)
  maxEvo <- nonzero' "max_kes_evolutions" (req ^. #maxKesEvolutions)
  entry <- poolEntry (BS.replicate 32 0)
  decoded <- decodeHeader era (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right hdr ->
      let view = headerView hdr
          OCert _ n _ _ = hvOCert view
          hk = hashKey (hvVK view)
          counters = maybe Map.empty (Map.singleton hk) (req ^. #maybe'lastCounter)
          stake = if req ^. #poolRegistered then Map.singleton (poolKeyHash view) entry else Map.empty
       in case runExcept (doValidateKESSignature maxEvo spkp stake counters view) of
            Left e -> defMessage & #rejection .~ praosRejection e
            Right () -> defMessage & #newCounter .~ n

-- | 'doValidateVRFSignature' under the neutral epoch nonce: the pool lookup
-- and the registered VRF key hash come before the proof, so a bad proof still
-- means the key check passed.
vrfKeyRegistered :: PraosVrfKeyRegisteredRequest -> Result PraosVrfKeyRegisteredResponse
vrfKeyRegistered req = do
  entries <- traverse entry (req ^. #poolDistribution)
  if Map.size (Map.fromList entries) /= length entries then unsupported "duplicate pool in the distribution" else pure ()
  let era = if req ^. #era == P.ERA_UNSPECIFIED then P.ERA_CONWAY else req ^. #era
  if isTPraos era then tpraosVrfKeyRegistered era entries (req ^. #header) else praosVrfKeyRegistered req era entries
  where
    entry p = do
      pid <- hashArg "pool_id" 28 (p ^. #poolId)
      kh <- maybe (badRequest "pool_id must be 28 bytes") (pure . KeyHash) (hashFromBytes pid)
      e <- poolEntry =<< hashArg "vrf_key_hash" 32 (p ^. #vrfKeyHash)
      pure (kh, e)

praosVrfKeyRegistered :: PraosVrfKeyRegisteredRequest -> P.Era -> [(KeyHash Keys.StakePool, SL.IndividualPoolStake)] -> Result PraosVrfKeyRegisteredResponse
praosVrfKeyRegistered req era entries = do
  f <- alwaysLeader
  decoded <- decodeHeader era (req ^. #header)
  pure $ case decoded of
    Left rej -> defMessage & #rejection .~ rej
    Right hdr ->
      let view = headerView hdr
          pool = poolKeyHash view
          registered = defMessage & #poolId .~ (let KeyHash h = pool in hashToBytes h)
       in case runExcept (doValidateVRFSignature NeutralNonce (Map.fromList entries) f view) of
            Left e@(VRFKeyUnknown {}) -> defMessage & #rejection .~ praosRejection e
            Left e@(VRFKeyWrongVRFKey {}) -> defMessage & #rejection .~ praosRejection e
            Left _ -> registered
            Right () -> registered

-- | The era's nonce contribution of a block's VRF output: TPraos
-- 'mkNonceFromOutputVRF', Praos 'vrfNonceValue'.
contribution :: P.Era -> BS.ByteString -> Result Nonce
contribution era out = do
  _ <- hashArg "vrf_output" 64 out
  cert <- certified out
  case era of
    e | e `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO] -> pure (mkNonceFromOutputVRF (VRF.certifiedOutput cert))
    e | e `elem` [P.ERA_BABBAGE, P.ERA_CONWAY] -> pure (vrfNonceValue (Proxy @C) cert)
    e -> unsupported ("no nonce evolution in " <> T.pack (show e))

-- | A certified VRF output with a placeholder proof: nonce derivation reads
-- the output only.
certified :: BS.ByteString -> Result (VRF.CertifiedVRF (VRF C) InputVRF)
certified out =
  maybe
    (internal "placeholder VRF proof")
    (pure . VRF.CertifiedVRF (VRF.OutputVRF (fromList (BS.unpack out))))
    (VRF.rawDeserialiseCertVRF (BS.replicate 80 0))

-- | An evolving nonce argument; an empty hash is the neutral nonce.
nonceArg :: Text -> BS.ByteString -> Result Nonce
nonceArg name bs
  | BS.null bs = pure NeutralNonce
  | otherwise = maybe (badRequest (name <> " must be empty (neutral) or 32 bytes")) (pure . Nonce) (hashFromBytes bs)

-- | evolving ⭒ the block's nonce contribution.
nonceRolling :: PraosNonceRollingRequest -> Result PraosNonceRollingResponse
nonceRolling req = do
  c <- contribution (req ^. #era) (req ^. #vrfOutput)
  ev <- nonceArg "evolving" (req ^. #evolving . #hash)
  pure (defMessage & #evolving . #hash .~ nonceBytes (ev ⭒ c))

-- | The candidate nonce over a run of blocks within one epoch. TPraos eras
-- apply the UPDN rule to each block under Globals carrying the era's
-- stability window; Praos eras apply 'reupdateChainDepState' to a header view
-- built around the block's VRF output, under a config carrying the window the
-- Cardano node gives the era: 3k/f through Babbage, 4k/f from Conway. The
-- epoch has a fixed length, so its first slot must be a multiple of it.
nonceCandidate :: PraosNonceCandidateRequest -> Result PraosNonceCandidateResponse
nonceCandidate req = do
  let era = req ^. #era
  f <- positiveUnitIntervalArg "active_slot_coeff" (req ^. #maybe'activeSlotCoeff)
  k <- nonzero' "security_param" (req ^. #securityParam)
  let first = req ^. #epochFirstSlot
      next = req ^. #nextEpochFirstSlot
  if next <= first then unsupported "next_epoch_first_slot must follow epoch_first_slot" else pure ()
  if first `mod` (next - first) /= 0 then unsupported "epoch_first_slot is not a multiple of the epoch length" else pure ()
  ev0 <- nonceArg "evolving" (req ^. #evolving . #hash)
  cand0 <- nonceArg "candidate" (req ^. #candidate . #hash)
  blocks <- traverse (block first next) (req ^. #blocks)
  let asc = mkActiveSlotCoeff f
      stability = computeStabilityWindow k asc
      randomness = computeRandomnessStabilisationWindow k asc
      epochSize = EpochSize (next - first)
  (ev, cand) <- case era of
    e | e `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO] -> do
      let globals = tpraosGlobals (fixedEpochInfo epochSize (mkSlotLength 1)) k asc stability randomness
      foldM (tpraosStep globals) (ev0, cand0) blocks
    e | e `elem` [P.ERA_BABBAGE, P.ERA_CONWAY] -> do
      let window = if e == P.ERA_BABBAGE then stability else randomness
      foldM (praosStep k asc window (fixedEpochInfo epochSize (mkSlotLength 1))) (ev0, cand0) blocks
    e -> unsupported ("no nonce evolution in " <> T.pack (show e))
  pure (defMessage & #evolving . #hash .~ nonceBytes ev & #candidate . #hash .~ nonceBytes cand)
  where
    block first next b = do
      let slot = b ^. #slot
      if slot < first || slot >= next then unsupported "block slot outside the epoch" else pure ()
      c <- contribution (req ^. #era) (b ^. #vrfOutput)
      cert <- certified (b ^. #vrfOutput)
      pure (SlotNo slot, c, cert)

tpraosGlobals :: EpochInfo (Either Text) -> Word64 -> ActiveSlotCoeff -> Word64 -> Word64 -> Globals
tpraosGlobals ei k asc stability randomness =
  Globals
    { epochInfo = ei
    , slotsPerKESPeriod = 129600
    , stabilityWindow = stability
    , randomnessStabilisationWindow = randomness
    , securityParameter = unsafeNonZero k
    , maxKESEvo = 62
    , quorum = 5
    , maxLovelaceSupply = 45000000000000000
    , activeSlotCoeff = asc
    , networkId = Mainnet
    , systemStart = SystemStart (read "1970-01-01 00:00:00 UTC")
    }

tpraosStep :: Globals -> (Nonce, Nonce) -> (SlotNo, Nonce, c) -> Result (Nonce, Nonce)
tpraosStep globals (ev, cand) (slot, eta, _) =
  case runIdentity (runReaderT (applySTS @(UPDN C) (TRC (UpdnEnv eta, UpdnState ev cand, slot))) globals) of
    Right (UpdnState ev' cand') -> pure (ev', cand')
    Left _ -> internal "UPDN has no predicate failures"

praosStep ::
  Word64 -> ActiveSlotCoeff -> Word64 -> EpochInfo (Except History.PastHorizonException) ->
  (Nonce, Nonce) -> (SlotNo, Nonce, VRF.CertifiedVRF (VRF C) InputVRF) -> Result (Nonce, Nonce)
praosStep k asc window ei (ev, cand) (slot, _, cert) = do
  view <- placeholderView slot cert
  let cfg =
        PraosConfig
          { praosParams =
              PraosParams
                { praosSlotsPerKESPeriod = 129600
                , praosLeaderF = asc
                , praosSecurityParam = SecurityParam (unsafeNonZero k)
                , praosMaxKESEvo = 62
                , praosMaxMajorPV = MaxMajorProtVer (natVersion @10)
                , praosRandomnessStabilisationWindow = window
                }
          , praosEpochInfo = ei
          }
      st =
        PraosState
          { praosStateLastSlot = Origin
          , praosStateOCertCounters = Map.empty
          , praosStateEvolvingNonce = ev
          , praosStateCandidateNonce = cand
          , praosStateEpochNonce = NeutralNonce
          , praosStatePreviousEpochNonce = NeutralNonce
          , praosStateLabNonce = NeutralNonce
          , praosStateLastEpochBlockNonce = NeutralNonce
          }
      lv =
        PraosLedgerView
          { plvPoolDistr = SL.PoolDistr Map.empty (unsafeNonZero (Coin 1))
          , plvMaxHeaderSize = 0
          , plvMaxBodySize = 0
          , plvProtocolVersion = ProtVer (natVersion @10) 0
          }
      st' = reupdateChainDepState cfg view slot (TickedPraosState st lv)
  pure (praosStateEvolvingNonce st', praosStateCandidateNonce st')

-- | A header view whose slot and VRF result are the block's. The other fields
-- are placeholders: 'reupdateChainDepState' reads the previous-hash nonce, the
-- certificate's counter and the issuer key besides them, and no check runs.
placeholderView :: SlotNo -> VRF.CertifiedVRF (VRF C) InputVRF -> Result (HeaderView C)
placeholderView slot cert = do
  vk <- maybe (internal "placeholder issuer key") (pure . VKey) (DSIGN.rawDeserialiseVerKeyDSIGN (BS.replicate 32 0))
  vrfVk <- maybe (internal "placeholder VRF key") pure (VRF.rawDeserialiseVerKeyVRF (BS.replicate 32 0))
  hot <- maybe (internal "placeholder KES key") pure (KES.rawDeserialiseVerKeyKES (BS.replicate 32 0))
  cold <- maybe (internal "placeholder cold signature") (pure . DSIGN.SignedDSIGN) (DSIGN.rawDeserialiseSigDSIGN (BS.replicate 64 0))
  sig <- maybe (internal "placeholder KES signature") (pure . KES.SignedKES) (KES.rawDeserialiseSigKES (BS.replicate 448 0))
  bodyHash <- maybe (internal "placeholder body hash") pure (hashFromBytes (BS.replicate 32 0))
  let ocert = OCert hot 0 (KESPeriod 0) cold
      body =
        HeaderBody
          { hbBlockNo = 0
          , hbSlotNo = slot
          , hbPrev = GenesisHash
          , hbVk = vk
          , hbVrfVk = vrfVk
          , hbVrfRes = cert
          , hbBodySize = 0
          , hbBodyHash = bodyHash
          , hbOCert = ocert
          , hbProtVer = ProtVer (natVersion @10) 0
          }
  pure
    HeaderView
      { hvPrevHash = GenesisHash
      , hvVK = vk
      , hvVrfVK = vrfVk
      , hvVrfRes = cert
      , hvOCert = ocert
      , hvSlotNo = slot
      , hvSigned = body
      , hvSignature = sig
      }

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'PraosLeaderCheck q -> Just (respond #praosLeaderCheck (leaderCheck q))
  Call'PraosNonceEpoch q -> Just (respond #praosNonceEpoch (nonceEpoch q))
  Call'TpraosOverlaySchedule q -> Just (respond #tpraosOverlaySchedule (overlaySchedule q))
  Call'PraosHeaderEnvelope q -> Just (respond #praosHeaderEnvelope (headerEnvelope q))
  Call'PraosVrfCheck q -> Just (respond #praosVrfCheck (vrfCheck q))
  Call'PraosKesCheck q -> Just (respond #praosKesCheck (kesCheck q))
  Call'PraosOpcertCheck q -> Just (respond #praosOpcertCheck (opcertCheck q))
  Call'PraosVrfKeyRegistered q -> Just (respond #praosVrfKeyRegistered (vrfKeyRegistered q))
  Call'PraosNonceRolling q -> Just (respond #praosNonceRolling (nonceRolling q))
  Call'PraosNonceCandidate q -> Just (respond #praosNonceCandidate (nonceCandidate q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "praos_leader_check"
  , "praos_nonce_epoch"
  , "tpraos_overlay_schedule"
  , "praos_header_envelope"
  , "praos_vrf_check"
  , "praos_kes_check"
  , "praos_opcert_check"
  , "praos_vrf_key_registered"
  , "praos_nonce_rolling"
  , "praos_nonce_candidate"
  ]
