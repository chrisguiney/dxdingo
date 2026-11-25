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
  , BoundedRational (boundRational)
  , Globals (..)
  , Network (..)
  , Nonce (..)
  , ProtVer (..)
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
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys (KeyHash (..), VKey (..), coerceKeyRole, hashKey, toVRFVerKeyHash)
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Shelley.StabilityWindow (computeRandomnessStabilisationWindow, computeStabilityWindow)
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (StandardCrypto, VRF)
import Cardano.Protocol.Praos.BlockHeader (Header (..), HeaderBody (..), headerSize)
import Cardano.Protocol.Praos.VRF (InputVRF, vrfLeaderValue, vrfNonceValue)
import Cardano.Protocol.TPraos.BlockHeader (BoundedNatural (bvValue), PrevHash (..))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCert (..))
import Cardano.Protocol.TPraos.Rules.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad (foldM)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.Functor.Identity (runIdentity)
import GHC.Exts (fromList)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import Numeric.Natural (Natural)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (reupdateChainDepState))
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
import Ouroboros.Consensus.Protocol.TPraos ()
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

-- | 'envelopeCheck': the header's encoded size against the maximum header
-- size, then the body size it declares against the maximum body size. The
-- obsolete-node bound is set to the header's own version, so it never fires.
headerEnvelope :: PraosHeaderEnvelopeRequest -> Result PraosHeaderEnvelopeResponse
headerEnvelope req = do
  v <- praosVersion (req ^. #era)
  pparams <- maybe (badRequest "pparams is required") pure (req ^. #maybe'pparams)
  maxHeader <- limit "max_block_header_size" (pparams ^. #maybe'maxBlockHeaderSize)
  maxBody <- limit "max_block_body_size" (pparams ^. #maybe'maxBlockBodySize)
  if maxHeader > fromIntegral (maxBound :: Word16) then unsupported "max_block_header_size does not fit a Word16" else pure ()
  decoded <- decodeHeader (req ^. #era) (req ^. #header)
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
       in case runExcept (envelopeCheck v ccd view) of
            Right () -> defMessage
            Left e -> defMessage & #rejection .~ rejection "Envelope" (envelopeErrName e) (T.pack (show e))
  where
    limit name = maybe (badRequest (name <> " is required")) pure

envelopeErrName :: EnvelopeError -> Text
envelopeErrName = \case
  ObsoleteNode {} -> "ObsoleteNode"
  HeaderSizeTooLarge {} -> "HeaderSizeTooLarge"
  BlockSizeTooLarge {} -> "BlockSizeTooLarge"

-- | 'doValidateVRFSignature' with the header's issuer registered under the
-- given VRF key hash: key hash first, then the proof over the Praos VRF input.
-- The leader and nonce values are 'vrfLeaderValue' and 'vrfNonceValue'.
vrfCheck :: PraosVrfCheckRequest -> Result PraosVrfCheckResponse
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
opcertCheck req = do
  era <- case req ^. #protocolVersion . #major of
    m | m >= 9 -> pure P.ERA_CONWAY
    m | m >= 7 -> pure P.ERA_BABBAGE
    m -> unsupported ("protocol version " <> T.pack (show m) <> " runs TPraos")
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
  f <- alwaysLeader
  decoded <- decodeHeader P.ERA_CONWAY (req ^. #header)
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
  where
    entry p = do
      pid <- hashArg "pool_id" 28 (p ^. #poolId)
      kh <- maybe (badRequest "pool_id must be 28 bytes") (pure . KeyHash) (hashFromBytes pid)
      e <- poolEntry =<< hashArg "vrf_key_hash" 32 (p ^. #vrfKeyHash)
      pure (kh, e)

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
