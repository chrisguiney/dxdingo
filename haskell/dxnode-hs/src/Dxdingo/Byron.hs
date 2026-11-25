{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Byron functions, answered by cardano-ledger-byron.
module Dxdingo.Byron
  ( dispatch
  , implemented
  , byronMinFee
  , byronTxValidate
  , byronHeaderValidate
  , byronChainUpdate
  , byronUpdateProposal
  , fullParamsArg
  , upiStateArg
  , delegationPairs
  ) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Interface as DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Endorsement as Endorsement
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Common (BlockCount (..), KeyHash (..), LovelacePortion, lovelacePortionToRational)
import Cardano.Chain.Slotting (slotNumberEpoch)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64, Word8)
import Cardano.Chain.Common (hashKey)
import Cardano.Chain.Slotting (EpochSlots (..), SlotNumber (..))
import Cardano.Crypto.DSIGN (SignedDSIGN (..), rawDeserialiseVerKeyDSIGN)
import Cardano.Ledger.BaseTypes (unsafeNonZero)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Bimap as Bimap
import Ouroboros.Consensus.Byron.Crypto.DSIGN (ByronDSIGN, SigDSIGN (..), VerKeyDSIGN (..))
import Ouroboros.Consensus.Byron.Ledger.PBFT (toPBftLedgerView)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (updateChainDepState))
import Ouroboros.Consensus.Protocol.PBFT
  ( ConsensusConfig (..)
  , PBftFields (..)
  , PBftParams (..)
  , PBftSignatureThreshold (..)
  , PBftValidateView (..)
  , PBftValidationErr (..)
  , Ticked (..)
  )
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import Cardano.Chain.Common
  ( LovelaceError (..)
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , calculateTxSizeLinear
  , mkLovelace
  , unsafeGetLovelace
  , rationalToLovelacePortion
  )
import Cardano.Chain.Slotting (EpochNumber (..))
import Cardano.Chain.UTxO
  ( ATxAux (..)
  , TxIn (..)
  , TxOut (..)
  , UTxO
  , UTxOValidationError (..)
  , TxValidationError (..)
  , UTxOError (..)
  , TxValidationMode (..)
  , defaultUTxOConfiguration

  )
import qualified Cardano.Chain.UTxO as UTxO
import Cardano.Chain.Update (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Chain.Block (BlockValidationMode (..))
import Cardano.Chain.ValidationMode (ValidationMode (..))
import Cardano.Crypto (AProtocolMagic (..), RequiresNetworkMagic (..), ProtocolMagicId (..), abstractHashFromBytes, abstractHashToBytes, hashRaw)
import Cardano.Ledger.Binary
  ( Annotated (..)
  , DecoderError
  , byronProtVer
  , decodeFull
  , decodeFullAnnotatedBytes
  , serialize'
  , decCBOR
  )
import Control.Monad (forM, when)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Byron
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

maxLovelace :: Integer
maxLovelace = 45000000000000000

rejection :: Text -> Text -> Text -> P.Rejection
rejection rule name detail =
  defMessage
    & #failures
      .~ [ defMessage
            & #rule .~ rule
            & #name .~ name
            & #detail .~ detail
         ]

-- | TxSizeLinear a b: a must be a Lovelace integer and b a non-negative
-- multiple of 1e-9 (Nano, the reference's CBOR form); anything else is
-- UNSUPPORTED, as in dxnode-go.
feePolicyArg :: Maybe P.Rational -> Maybe P.Rational -> Result TxSizeLinear
feePolicyArg ma mb = do
  a <- rationalArg "tx_fee_a" ma
  b <- rationalArg "tx_fee_b" mb
  when (denominator a /= 1 || numerator a < 0 || numerator a > maxLovelace) $
    unsupported "tx_fee_a is not a Lovelace integer"
  let nano = b * 1000000000
  when (b < 0 || denominator nano /= 1) $
    unsupported "tx_fee_b is not a non-negative Nano"
  case mkLovelace (fromInteger (numerator a)) of
    Left e -> internal (T.pack (show e))
    Right l -> pure (TxSizeLinear l b)

lovelaceErrorName :: LovelaceError -> Text
lovelaceErrorName = \case
  LovelaceOverflow _ -> "LovelaceOverflow"
  LovelaceTooLarge _ -> "LovelaceTooLarge"
  LovelaceTooSmall _ -> "LovelaceTooSmall"
  LovelaceUnderflow _ _ -> "LovelaceUnderflow"

-- | 'calculateTxSizeLinear'. A failure is the 'LovelaceError' constructor.
byronMinFee :: ByronMinFeeRequest -> Result ByronMinFeeResponse
byronMinFee req = do
  policy <- feePolicyArg (req ^. #maybe'txFeeA) (req ^. #maybe'txFeeB)
  pure $ case calculateTxSizeLinear policy (fromIntegral (req ^. #size)) of
    Left e -> defMessage & #rejection .~ rejection "byron-fee" (lovelaceErrorName e) (T.pack (show e))
    Right l -> defMessage & #fee .~ unsafeGetLovelace l

-- | Protocol parameters as the transaction rules read them: the size limit and
-- the fee policy. The rest are unread by 'UTxO.updateUTxOTxWitness'.
paramsArg :: Maybe S.ByronProtocolParameters -> Result ProtocolParameters
paramsArg Nothing = badRequest "pparams missing"
paramsArg (Just pp) = do
  policy <- feePolicyArg (pp ^. #maybe'txFeeA) (pp ^. #maybe'txFeeB)
  let zero = rationalToLovelacePortion 0
  pure
    ProtocolParameters
      { ppScriptVersion = 0
      , ppSlotDuration = 0
      , ppMaxBlockSize = fromIntegral (pp ^. #maxBlockSize)
      , ppMaxHeaderSize = fromIntegral (pp ^. #maxHeaderSize)
      , ppMaxTxSize = fromIntegral (pp ^. #maxTxSize)
      , ppMaxProposalSize = 0
      , ppMpcThd = zero
      , ppHeavyDelThd = zero
      , ppUpdateVoteThd = zero
      , ppUpdateProposalThd = zero
      , ppUpdateProposalTTL = SlotNumber 0
      , ppSoftforkRule = SoftforkRule zero zero zero
      , ppTxFeePolicy = TxFeePolicyTxSizeLinear policy
      , ppUnlockStakeEpoch = EpochNumber 0
      }

txInProto :: TxIn -> P.TxIn
txInProto (TxInUtxo h ix) =
  defMessage & #txId .~ abstractHashToBytes h & #index .~ fromIntegral ix

-- | 'UTxO.updateUTxOTxWitness' under full validation: the witnesses, then
-- 'UTxO.validateTxAux', then 'UTxO.validateTx'. The reference stops at its
-- first failure, which is what the response reports. The UTxO delta is the
-- difference between the input and the updated UTxO; the fee is the balance
-- of the spent entries minus that of the new ones.
byronTxValidate :: ByronTxValidateRequest -> Result ByronTxValidateResponse
byronTxValidate req = do
  pp <- paramsArg (req ^. #maybe'pparams)
  entries <- forM (zip [0 :: Int ..] (req ^. #utxo)) $ \(i, e) -> do
    let label = "utxo[" <> T.pack (show i) <> "]"
    txid <- maybe (badRequest (label <> ": tx_id is not 32 bytes")) pure (abstractHashFromBytes (e ^. #txin . #txId))
    out <- either (const (badRequest (label <> ": txout does not decode"))) pure
      (decodeFull byronProtVer (BSL.fromStrict (e ^. #txout)) :: Either DecoderError TxOut)
    pure (TxInUtxo txid (fromIntegral (e ^. #txin . #index)), out)
  let utxo = UTxO.fromList entries
      pm = ProtocolMagicId (req ^. #protocolMagic)
      env =
        UTxO.Environment
          { UTxO.protocolMagic =
              AProtocolMagic (Annotated pm (serialize' byronProtVer pm)) (if pm == ProtocolMagicId 764824073 then RequiresNoMagic else RequiresMagic)
          , UTxO.protocolParameters = pp
          , UTxO.utxoConfiguration = defaultUTxOConfiguration
          }
  case decodeFullAnnotatedBytes byronProtVer "ATxAux" decCBOR (BSL.fromStrict (req ^. #tx)) :: Either DecoderError (ATxAux ByteString) of
    Left e -> pure (defMessage & #rejection .~ rejection "Byron.Tx" "DeserialiseFailure" (T.pack (show e)))
    Right aux ->
      case runExcept (runReaderT (UTxO.updateUTxOTxWitness env utxo aux) (ValidationMode BlockValidation TxValidation)) of
        Left e -> pure (defMessage & #rejection .~ rejection "Byron.Tx" (failureName e) (T.pack (show e)))
        Right utxo' -> pure (delta utxo utxo')
  where
    failureName = \case
      UTxOValidationTxValidationError e -> case e of
        TxValidationLovelaceError _ _ -> "TxValidationLovelaceError"
        TxValidationFeeTooSmall {} -> "TxValidationFeeTooSmall"
        TxValidationWitnessWrongSignature {} -> "TxValidationWitnessWrongSignature"
        TxValidationWitnessWrongKey {} -> "TxValidationWitnessWrongKey"
        TxValidationMissingInput _ -> "TxValidationMissingInput"
        TxValidationNetworkMagicMismatch {} -> "TxValidationNetworkMagicMismatch"
        TxValidationTxTooLarge {} -> "TxValidationTxTooLarge"
        TxValidationUnknownAddressAttributes -> "TxValidationUnknownAddressAttributes"
        TxValidationUnknownAttributes -> "TxValidationUnknownAttributes"
      UTxOValidationUTxOError e -> case e of
        UTxOMissingInput _ -> "UTxOMissingInput"
        UTxOOverlappingUnion -> "UTxOOverlappingUnion"
    key (TxInUtxo h ix) = (abstractHashToBytes h, ix)
    delta :: UTxO -> UTxO -> ByronTxValidateResponse
    delta old new =
      let oldL = UTxO.toList old
          newL = UTxO.toList new
          removed = [(i, o) | (i, o) <- oldL, not (UTxO.member i new)]
          added = [(i, o) | (i, o) <- newL, not (UTxO.member i old)]
          total = sum . map (toInteger . unsafeGetLovelace . txOutValue . snd)
          entry (i, o) = defMessage & #txin .~ txInProto i & #txout .~ serialize' byronProtVer o
       in defMessage
            & #utxoRemoved .~ map (txInProto . fst) (sortOn (key . fst) removed)
            & #utxoAdded .~ map entry (sortOn (key . fst) added)
            & #fee .~ fromInteger (total removed - total added)

-- | The PBFT checks of 'updateChainDepState' for a header with no earlier
-- signers: the delegate's signature over the signed header bytes, then that the
-- signer is an active delegate (the first failure is reported, in that order).
-- The PBftValidateView is assembled from the header the way
-- 'Ouroboros.Consensus.Byron.Ledger.PBFT.validateView' does. The window is the
-- chain state's signers with their slots; recent_signers or a previous slot is
-- UNSUPPORTED, as in dxnode-go.
byronHeaderValidate :: ByronHeaderValidateRequest -> Result ByronHeaderValidateResponse
byronHeaderValidate req = do
  st <- maybe (badRequest "chain_state missing") pure (req ^. #maybe'chainState)
  when (not (null (st ^. #recentSigners)) || st ^. #maybe'prevSlot /= Nothing) $
    unsupported "recent_signers and a previous slot are not modelled; the window is signers"
  when (req ^. #securityParam == 0) $ unsupported "security_param must be nonzero"
  when (toInteger (length (st ^. #signers)) > toInteger (req ^. #securityParam)) $
    unsupported "more signers than the window holds"
  when (null (st ^. #delegations)) $ unsupported "no delegations"
  pairs <- forM (zip [0 :: Int ..] (st ^. #delegations)) $ \(i, d) -> do
    let label = "delegations[" <> T.pack (show i) <> "]"
    iss <- vkey (label <> ".issuer_vkey") (d ^. #issuerVkey)
    del <- vkey (label <> ".delegate_vkey") (d ^. #delegateVkey)
    pure (hashKey iss, hashKey del)
  let issuers = map fst pairs
      delegates = map snd pairs
  when (Set.size (Set.fromList issuers) /= length issuers || Set.size (Set.fromList delegates) /= length delegates) $
    unsupported "a delegation repeats an issuer or delegate"
  -- The window: each signer's genesis key with the slot it signed, oldest
  -- first. The reference's fromList needs the slots not to decrease.
  signers <- forM (zip [0 :: Int ..] (st ^. #signers)) $ \(i, sg) -> do
    kh <- keyHashArg ("signers[" <> T.pack (show i) <> "].key") (sg ^. #key)
    pure (sg ^. #slot, PBftState.PBftSigner (SlotNo (sg ^. #slot)) kh)
  let signerSlots = map fst signers
  when (or (zipWith (>) signerSlots (drop 1 signerSlots))) $
    unsupported "signers are not in slot order"
  let k = req ^. #securityParam
      epochSlots = EpochSlots (10 * k)
      pm = ProtocolMagicId (req ^. #protocolMagic)
      ledgerView = toPBftLedgerView (Delegation.Map (Bimap.fromList pairs))
      cfg =
        PBftConfig
          { pbftParams =
              PBftParams
                { pbftSecurityParam = SecurityParam (unsafeNonZero k)
                , pbftNumNodes = NumCoreNodes (fromIntegral (length pairs))
                , pbftSignatureThreshold = PBftSignatureThreshold 0.22
                }
          }
  pure $ case decodeFullAnnotatedBytes byronProtVer "Header" (CC.decCBORAHeader epochSlots) (BSL.fromStrict (req ^. #header)) of
    Left e -> defMessage & #rejection .~ rejection "PBFT" "DeserialiseFailure" (T.pack (show e))
    Right hdr ->
      let genKey = VerKeyByronDSIGN (CC.headerGenesisKey hdr)
          fields =
            PBftFields
              { pbftIssuer = VerKeyByronDSIGN (Delegation.delegateVK (CC.delegationCertificate (CC.headerSignature hdr)))
              , pbftGenKey = genKey
              , pbftSignature = SignedDSIGN (SigByronDSIGN (CC.signature (CC.headerSignature hdr)))
              }
          view = PBftValidateRegular fields (CC.recoverSignedBytes epochSlots hdr) (pm, genKey)
          SlotNumber slot = CC.headerSlot hdr
       in case runExcept (updateChainDepState cfg view (SlotNo slot) (TickedPBftState ledgerView (PBftState.fromList (map snd signers)))) of
            Right _ -> defMessage
            Left e -> defMessage & #rejection .~ rejection "PBFT" (pbftName e) (T.pack (show e))
  where
    vkey label bs = case rawDeserialiseVerKeyDSIGN @ByronDSIGN bs of
      Just (VerKeyByronDSIGN vk) -> pure vk
      Nothing -> badRequest (label <> " is not a 64-byte extended key")
    pbftName = \case
      PBftInvalidSignature _ -> "PBftInvalidSignature"
      PBftNotGenesisDelegate {} -> "PBftNotGenesisDelegate"
      PBftExceededSignThreshold {} -> "PBftExceededSignThreshold"
      PBftInvalidSlot -> "PBftInvalidSlot"

-- | A LovelacePortion given as a rational in [0, 1] that is a multiple of
-- 1e-15; an absent value is zero, and anything else is UNSUPPORTED, as in
-- dxnode-go.
portionArg :: Text -> Maybe P.Rational -> Result LovelacePortion
portionArg _ Nothing = pure (rationalToLovelacePortion 0)
portionArg name r = do
  x <- rationalArg name r
  let scaled = x * 1000000000000000
  when (x < 0 || x > 1 || denominator scaled /= 1) $
    unsupported (name <> " is not a LovelacePortion")
  pure (rationalToLovelacePortion x)

portionProto :: LovelacePortion -> P.Rational
portionProto = rationalProto . lovelacePortionToRational

-- | Every Byron protocol parameter, as dxnode-go's byronFullParams reads
-- them. The adoption threshold is the softfork rule's minimum.
fullParamsArg :: Maybe S.ByronProtocolParameters -> Result ProtocolParameters
fullParamsArg Nothing = badRequest "pparams missing"
fullParamsArg (Just pp) = do
  policy <- feePolicyArg (pp ^. #maybe'txFeeA) (pp ^. #maybe'txFeeB)
  when (pp ^. #scriptVersion > 0xffff) $ unsupported "script_version does not fit Word16"
  mpc <- portionArg "mpc_threshold" (pp ^. #maybe'mpcThreshold)
  heavy <- portionArg "heavy_delegation_threshold" (pp ^. #maybe'heavyDelegationThreshold)
  vote <- portionArg "update_vote_threshold" (pp ^. #maybe'updateVoteThreshold)
  prop <- portionArg "update_proposal_threshold" (pp ^. #maybe'updateProposalThreshold)
  initThd <- portionArg "softfork_init_threshold" (pp ^. #maybe'softforkInitThreshold)
  minThd <- portionArg "update_adoption_threshold" (pp ^. #maybe'updateAdoptionThreshold)
  decrement <- portionArg "softfork_decrement" (pp ^. #maybe'softforkDecrement)
  pure
    ProtocolParameters
      { ppScriptVersion = fromIntegral (pp ^. #scriptVersion)
      , ppSlotDuration = fromIntegral (pp ^. #slotDuration)
      , ppMaxBlockSize = fromIntegral (pp ^. #maxBlockSize)
      , ppMaxHeaderSize = fromIntegral (pp ^. #maxHeaderSize)
      , ppMaxTxSize = fromIntegral (pp ^. #maxTxSize)
      , ppMaxProposalSize = fromIntegral (pp ^. #maxProposalSize)
      , ppMpcThd = mpc
      , ppHeavyDelThd = heavy
      , ppUpdateVoteThd = vote
      , ppUpdateProposalThd = prop
      , ppUpdateProposalTTL = SlotNumber (pp ^. #updateProposalTtl)
      , ppSoftforkRule = SoftforkRule initThd minThd decrement
      , ppTxFeePolicy = TxFeePolicyTxSizeLinear policy
      , ppUnlockStakeEpoch = EpochNumber (pp ^. #unlockStakeEpoch)
      }

paramsProto :: ProtocolParameters -> S.ByronProtocolParameters
paramsProto pp =
  let TxFeePolicyTxSizeLinear (TxSizeLinear a b) = ppTxFeePolicy pp
      SoftforkRule initThd minThd decrement = ppSoftforkRule pp
   in defMessage
        & #txFeeA .~ rationalProto (toRational (unsafeGetLovelace a))
        & #txFeeB .~ rationalProto b
        & #maxTxSize .~ fromIntegral (ppMaxTxSize pp)
        & #maxBlockSize .~ fromIntegral (ppMaxBlockSize pp)
        & #maxHeaderSize .~ fromIntegral (ppMaxHeaderSize pp)
        & #updateAdoptionThreshold .~ portionProto minThd
        & #updateProposalTtl .~ unSlotNumber' (ppUpdateProposalTTL pp)
        & #updateVoteThreshold .~ portionProto (ppUpdateVoteThd pp)
        & #scriptVersion .~ fromIntegral (ppScriptVersion pp)
        & #slotDuration .~ fromIntegral (ppSlotDuration pp)
        & #maxProposalSize .~ fromIntegral (ppMaxProposalSize pp)
        & #mpcThreshold .~ portionProto (ppMpcThd pp)
        & #heavyDelegationThreshold .~ portionProto (ppHeavyDelThd pp)
        & #updateProposalThreshold .~ portionProto (ppUpdateProposalThd pp)
        & #softforkInitThreshold .~ portionProto initThd
        & #softforkDecrement .~ portionProto decrement
        & #unlockStakeEpoch .~ getEpochNumber (ppUnlockStakeEpoch pp)

unSlotNumber' :: SlotNumber -> Word64
unSlotNumber' (SlotNumber n) = n

keyHashArg :: Text -> ByteString -> Result KeyHash
keyHashArg name b = maybe (badRequest (name <> " is not 28 bytes")) (pure . KeyHash) (abstractHashFromBytes b)

khBytes :: KeyHash -> ByteString
khBytes = abstractHashToBytes . unKeyHash

versionArg :: Text -> Maybe P.ProtocolVersion -> Result Update.ProtocolVersion
versionArg name mv =
  let v = maybe (defMessage :: P.ProtocolVersion) id mv
   in if v ^. #major > 0xffff || v ^. #minor > 0xffff || v ^. #alt > 0xff
        then unsupported (name <> " does not fit a Byron ProtocolVersion")
        else pure (Update.ProtocolVersion (fromIntegral (v ^. #major)) (fromIntegral (v ^. #minor)) (fromIntegral (v ^. #alt)))

versionProto :: Update.ProtocolVersion -> P.ProtocolVersion
versionProto (Update.ProtocolVersion a b c) =
  defMessage & #major .~ fromIntegral a & #minor .~ fromIntegral b & #alt .~ fromIntegral c

decodeProposal :: ByteString -> Either Text (Update.AProposal ByteString)
decodeProposal raw =
  either (Left . T.pack . show) Right $
    decodeFullAnnotatedBytes byronProtVer "Update.Proposal" decCBOR (BSL.fromStrict raw)

-- | The update interface state of a request. @fallback@ supplies the adopted
-- parameters when the state carries none; @epoch@ is the state's current epoch.
upiStateArg :: Maybe ByronUpdateState -> Maybe S.ByronProtocolParameters -> EpochNumber -> Result UPI.State
upiStateArg mst fallback epoch = do
  let st = maybe (defMessage :: ByronUpdateState) id mst
  adopted <- fullParamsArg (maybe fallback Just (st ^. #maybe'pparams))
  version <- versionArg "adopted" (st ^. #maybe'adopted)
  proposals <- forM (zip [0 :: Int ..] (st ^. #proposals)) $ \(i, p) -> do
    let label = "proposals[" <> T.pack (show i) <> "]"
    prop <- either (\e -> badRequest (label <> ": " <> e)) pure (decodeProposal (p ^. #proposal))
    let upId = Update.recoverUpId prop
        body = Update.body prop
    when (abstractHashToBytes upId /= p ^. #proposalId) $
      badRequest (label <> ": proposal_id is not the hash of the proposal")
    when (not (p ^. #protocol) && not (p ^. #software)) $
      badRequest (label <> ": neither protocol nor software")
    pure (p, upId, body)
  let upIds = [u | (_, u, _) <- proposals]
  when (length (Set.toList (Set.fromList upIds)) /= length upIds) $
    badRequest "proposals: repeated proposal"
  let protocolProposals =
        Map.fromList
          [ ( u
            , Registration.ProtocolUpdateProposal
                (Update.protocolVersion b)
                (Update.apply (Update.protocolParametersUpdate b) adopted)
            )
          | (p, u, b) <- proposals
          , p ^. #protocol
          ]
      softwareProposals =
        Map.fromList
          [ (u, Registration.SoftwareUpdateProposal (Update.softwareVersion b) (Update.metadata b))
          | (p, u, b) <- proposals
          , p ^. #software
          ]
      regSlots = Map.fromList [(u, SlotNumber (p ^. #registeredSlot)) | (p, u, _) <- proposals]
      confirmed = Map.fromList [(u, SlotNumber s) | (p, u, _) <- proposals, Just s <- [p ^. #maybe'confirmedSlot]]
  votes <- forM (zip [0 :: Int ..] (st ^. #votes)) $ \(i, v) -> do
    let label = "votes[" <> T.pack (show i) <> "]"
    upId <- maybe (badRequest (label <> ": proposal_id is not 32 bytes")) pure (abstractHashFromBytes (v ^. #proposalId))
    when (not (Map.member upId regSlots)) $ badRequest (label <> ": proposal is not in the state")
    voter <- keyHashArg (label <> ".voter_key_hash") (v ^. #voterKeyHash)
    pure (upId, Set.singleton voter)
  endorsements <- forM (zip [0 :: Int ..] (st ^. #endorsements)) $ \(i, e) -> do
    pv <- versionArg "endorsements" (e ^. #maybe'protocolVersion)
    kh <- keyHashArg ("endorsements[" <> T.pack (show i) <> "].key_hash") (e ^. #keyHash)
    pure (Endorsement.Endorsement pv kh)
  candidates <- forM (zip [0 :: Int ..] (st ^. #candidates)) $ \(i, c) -> do
    pv <- versionArg "candidates" (c ^. #maybe'protocolVersion)
    pp <- fullParamsArg (c ^. #maybe'pparams)
    let _ = i
    pure (Endorsement.CandidateProtocolUpdate (SlotNumber (c ^. #slot)) pv pp)
  pure
    UPI.State
      { UPI.currentEpoch = epoch
      , UPI.adoptedProtocolVersion = version
      , UPI.adoptedProtocolParameters = adopted
      , UPI.candidateProtocolUpdates = candidates
      , UPI.appVersions =
          Map.fromList
            [ (Update.ApplicationName (a ^. #name), Registration.ApplicationVersion (a ^. #version) (SlotNumber (a ^. #slot)) Map.empty)
            | a <- st ^. #appVersions
            ]
      , UPI.registeredProtocolUpdateProposals = protocolProposals
      , UPI.registeredSoftwareUpdateProposals = softwareProposals
      , UPI.confirmedProposals = confirmed
      , UPI.proposalVotes = Map.fromListWith Set.union votes
      , UPI.registeredEndorsements = Set.fromList endorsements
      , UPI.proposalRegistrationSlot = regSlots
      }

-- | The canonical response form of an update state: sorted, proposals
-- identified by id alone.
upiStateProto :: UPI.State -> ByronUpdateState
upiStateProto st =
  let protocolIds = Map.keysSet (UPI.registeredProtocolUpdateProposals st)
      softwareIds = Map.keysSet (UPI.registeredSoftwareUpdateProposals st)
      proposal u =
        defMessage
          & #proposalId .~ abstractHashToBytes u
          & #registeredSlot .~ maybe 0 unSlotNumber' (Map.lookup u (UPI.proposalRegistrationSlot st))
          & #maybe'confirmedSlot .~ fmap unSlotNumber' (Map.lookup u (UPI.confirmedProposals st))
          & #protocol .~ Set.member u protocolIds
          & #software .~ Set.member u softwareIds
      endorsement (Endorsement.Endorsement pv kh) =
        defMessage & #protocolVersion .~ versionProto pv & #keyHash .~ khBytes kh
      candidate c =
        defMessage
          & #slot .~ unSlotNumber' (Endorsement.cpuSlot c)
          & #protocolVersion .~ versionProto (Endorsement.cpuProtocolVersion c)
          & #pparams .~ paramsProto (Endorsement.cpuProtocolParameters c)
   in defMessage
        & #adopted .~ versionProto (UPI.adoptedProtocolVersion st)
        & #pparams .~ paramsProto (UPI.adoptedProtocolParameters st)
        & #proposals .~ map proposal (sortOn abstractHashToBytes (Set.toList (Set.union protocolIds softwareIds)))
        & #votes
          .~ [ defMessage & #proposalId .~ abstractHashToBytes u & #voterKeyHash .~ khBytes v & #accept .~ True
             | (u, vs) <- sortOn (abstractHashToBytes . fst) (Map.toList (UPI.proposalVotes st))
             , v <- sortOn khBytes (Set.toList vs)
             ]
        & #endorsements
          .~ map endorsement (sortOn (\(Endorsement.Endorsement pv kh) -> (versionKey pv, khBytes kh)) (Set.toList (UPI.registeredEndorsements st)))
        & #candidates .~ map candidate (UPI.candidateProtocolUpdates st)
        & #appVersions
          .~ [ defMessage & #name .~ Update.unApplicationName n & #version .~ Registration.avNumSoftwareVersion a & #slot .~ unSlotNumber' (Registration.avSlotNumber a)
             | (n, a) <- sortOn (Update.unApplicationName . fst) (Map.toList (UPI.appVersions st))
             ]
  where
    versionKey (Update.ProtocolVersion a b c) = (a, b, c)

-- | An update-interface failure as dxnode-go names it: the interface error,
-- its sub-rule and the sub-rule's constructor.
upiFailures :: UPI.Error -> [P.Failure]
upiFailures e = case e of
  UPI.Registration r -> two "Registration" (registrationName r)
  UPI.Voting v -> two "Voting" (votingName v)
  UPI.Endorsement (Endorsement.MultipleProposalsForProtocolVersion _) ->
    two "Endorsement" "MultipleProposalsForProtocolVersion"
  UPI.NumberOfGenesisKeysTooLarge _ ->
    [failure "Byron.UPI" "NumberOfGenesisKeysTooLarge"]
  where
    detail = T.pack (show e)
    failure rule name = defMessage & #rule .~ rule & #name .~ name & #detail .~ detail
    two kind name = [failure "Byron.UPI" kind, failure ("Byron.UPI." <> kind) name]
    registrationName = \case
      Registration.DuplicateProtocolVersion _ -> "DuplicateProtocolVersion"
      Registration.DuplicateSoftwareVersion _ -> "DuplicateSoftwareVersion"
      Registration.InvalidProposer _ -> "InvalidProposer"
      Registration.InvalidProtocolVersion _ _ -> "InvalidProtocolVersion"
      Registration.InvalidScriptVersion _ _ -> "InvalidScriptVersion"
      Registration.InvalidSignature -> "InvalidSignature"
      Registration.InvalidSoftwareVersion _ _ -> "InvalidSoftwareVersion"
      Registration.MaxBlockSizeTooLarge _ -> "MaxBlockSizeTooLarge"
      Registration.MaxTxSizeTooLarge _ -> "MaxTxSizeTooLarge"
      Registration.ProposalAttributesUnknown -> "ProposalAttributesUnknown"
      Registration.ProposalTooLarge _ -> "ProposalTooLarge"
      Registration.SoftwareVersionError _ -> "SoftwareVersionError"
      Registration.SystemTagError _ -> "SystemTagError"
      Registration.NullUpdateProposal -> "NullUpdateProposal"
    votingName = \case
      Voting.VotingInvalidSignature -> "VotingInvalidSignature"
      Voting.VotingProposalNotRegistered _ -> "VotingProposalNotRegistered"
      Voting.VotingVoterNotDelegate _ -> "VotingVoterNotDelegate"
      Voting.VotingVoteAlreadyCast _ -> "VotingVoteAlreadyCast"

-- | The active delegations as the reference's delegation map reads them: the
-- issuer (genesis key) and the delegate that signs for it. A repeated issuer
-- or delegate is UNSUPPORTED, since the map is a Bimap.
delegationPairs :: [ByronDelegation] -> Result [(KeyHash, KeyHash)]
delegationPairs ds = do
  pairs <- forM (zip [0 :: Int ..] ds) $ \(i, d) -> do
    let label = "delegations[" <> T.pack (show i) <> "]"
    iss <- vkey (label <> ".issuer_vkey") (d ^. #issuerVkey)
    del <- vkey (label <> ".delegate_vkey") (d ^. #delegateVkey)
    pure (hashKey iss, hashKey del)
  let issuers = map fst pairs
      delegates = map snd pairs
  when (Set.size (Set.fromList issuers) /= length issuers || Set.size (Set.fromList delegates) /= length delegates) $
    unsupported "a delegation repeats an issuer or delegate"
  pure pairs
  where
    vkey label bs = case rawDeserialiseVerKeyDSIGN @ByronDSIGN bs of
      Just (VerKeyByronDSIGN vk) -> pure vk
      Nothing -> badRequest (label <> " is not a 64-byte extended key")

-- | 'UPI.registerUpdate': the block's proposal, its votes in order, then the
-- issuer's endorsement of the block's protocol version.
byronUpdateProposal :: ByronUpdateProposalRequest -> Result ByronUpdateProposalResponse
byronUpdateProposal req = do
  st0 <- maybe (badRequest "state missing") pure (req ^. #maybe'state)
  when (req ^. #securityParam == 0) $ unsupported "security_param must be nonzero"
  st <- upiStateArg (Just st0) Nothing (EpochNumber 0)
  when (req ^. #numGenesisKeys > 255) $ unsupported "num_genesis_keys does not fit Word8"
  pairs <- delegationPairs (req ^. #delegations)
  version <- versionArg "block_version" (req ^. #maybe'blockVersion)
  issuerKey <- case rawDeserialiseVerKeyDSIGN @ByronDSIGN (req ^. #issuerVkey) of
    Just (VerKeyByronDSIGN vk) -> pure vk
    Nothing -> badRequest "issuer_vkey is not a 64-byte extended key"
  let pm = ProtocolMagicId (req ^. #protocolMagic)
      env =
        UPI.Environment
          { UPI.protocolMagic = Annotated pm (serialize' byronProtVer pm)
          , UPI.k = BlockCount (req ^. #securityParam)
          , UPI.currentSlot = SlotNumber (req ^. #slot)
          , UPI.numGenKeys = fromIntegral (req ^. #numGenesisKeys) :: Word8
          , UPI.delegationMap = Delegation.fromList pairs
          }
  pure $ case decodeFullAnnotatedBytes byronProtVer "Update.Payload" decCBOR (BSL.fromStrict (req ^. #updatePayload)) :: Either DecoderError (Update.APayload ByteString) of
    Left e -> defMessage & #rejection .~ rejection "Byron.UPI" "DeserialiseFailure" (T.pack (show e))
    Right payload ->
      let signal = UPI.Signal (Update.payloadProposal payload) (Update.payloadVotes payload) (Endorsement.Endorsement version (hashKey issuerKey))
       in case runExcept (UPI.registerUpdate env st signal) of
            Left e -> defMessage & #rejection .~ (defMessage & #failures .~ upiFailures e)
            Right st' -> defMessage & #state .~ upiStateProto st'

utxoErrorName :: UTxOValidationError -> Text
utxoErrorName = \case
  UTxOValidationTxValidationError e -> case e of
    TxValidationLovelaceError _ _ -> "TxValidationLovelaceError"
    TxValidationFeeTooSmall {} -> "TxValidationFeeTooSmall"
    TxValidationWitnessWrongSignature {} -> "TxValidationWitnessWrongSignature"
    TxValidationWitnessWrongKey {} -> "TxValidationWitnessWrongKey"
    TxValidationMissingInput _ -> "TxValidationMissingInput"
    TxValidationNetworkMagicMismatch {} -> "TxValidationNetworkMagicMismatch"
    TxValidationTxTooLarge {} -> "TxValidationTxTooLarge"
    TxValidationUnknownAddressAttributes -> "TxValidationUnknownAddressAttributes"
    TxValidationUnknownAttributes -> "TxValidationUnknownAttributes"
  UTxOValidationUTxOError e -> case e of
    UTxOMissingInput _ -> "UTxOMissingInput"
    UTxOOverlappingUnion -> "UTxOOverlappingUnion"

-- | A chain validation failure as dxnode-go names it: the constructor, and for
-- the nested update and UTxO failures the failure inside.
chainFailures :: CC.ChainValidationError -> [P.Failure]
chainFailures e = case e of
  CC.ChainValidationBoundaryTooLarge -> [chain "ChainValidationBoundaryTooLarge"]
  CC.ChainValidationBlockTooLarge {} -> [chain "ChainValidationBlockTooLarge"]
  CC.ChainValidationHeaderTooLarge {} -> [chain "ChainValidationHeaderTooLarge"]
  CC.ChainValidationProofValidationError _ -> [chain "ChainValidationProofValidationError"]
  CC.ChainValidationDelegationSchedulingError _ -> [chain "ChainValidationDelegationSchedulingError"]
  CC.ChainValidationInvalidHash {} -> [chain "ChainValidationInvalidHash"]
  CC.ChainValidationGenesisHashMismatch {} -> [chain "ChainValidationGenesisHashMismatch"]
  CC.ChainValidationExpectedGenesisHash {} -> [chain "ChainValidationExpectedGenesisHash"]
  CC.ChainValidationExpectedHeaderHash {} -> [chain "ChainValidationExpectedHeaderHash"]
  CC.ChainValidationProtocolMagicMismatch {} -> [chain "ChainValidationProtocolMagicMismatch"]
  CC.ChainValidationUpdateError _ u -> chain "ChainValidationUpdateError" : upiFailures u
  CC.ChainValidationUTxOValidationError u ->
    [ chain "ChainValidationUTxOValidationError"
    , defMessage & #rule .~ "Byron.Tx" & #name .~ utxoErrorName u & #detail .~ T.pack (show u)
    ]
  _ -> [chain (T.takeWhile (/= ' ') (T.pack (show e)))]
  where
    chain name = defMessage & #rule .~ "Byron.Chain" & #name .~ name & #detail .~ T.pack (show e)

-- | 'CC.updateChainBlockOrBoundary' over a chain validation state assembled
-- from the request. The genesis configuration is the minimum the rules read:
-- the genesis keys (the issuers), k, the protocol magic and the UTxO
-- configuration. The PBFT signature and window are the consensus layer's, so
-- a request carrying a window is UNSUPPORTED, as in dxnode-go.
byronChainUpdate :: ByronChainUpdateRequest -> Result ByronChainUpdateResponse
byronChainUpdate req = do
  st <- maybe (badRequest "chain_state missing") pure (req ^. #maybe'chainState)
  when (req ^. #securityParam == 0) $ unsupported "security_param must be nonzero"
  when (null (st ^. #delegations)) $ unsupported "no delegations"
  pairs <- delegationPairs (st ^. #delegations)
  when (length pairs > 255) $ unsupported "more than 255 genesis keys"
  when (not (null (st ^. #recentSigners)) || not (null (st ^. #signers))) $
    unsupported "a PBFT signature window is not part of the ledger-level update"
  let k = req ^. #securityParam
      epochSlots = EpochSlots (10 * k)
      lastSlot = SlotNumber (st ^. #prevSlot)
  adoptedState <- upiStateArg (st ^. #maybe'update) (req ^. #maybe'pparams) (slotNumberEpoch epochSlots lastSlot)
  entries <- forM (zip [0 :: Int ..] (req ^. #utxo)) $ \(i, e) -> do
    let label = "utxo[" <> T.pack (show i) <> "]"
    txid <- maybe (badRequest (label <> ": tx_id is not 32 bytes")) pure (abstractHashFromBytes (e ^. #txin . #txId))
    out <- either (const (badRequest (label <> ": txout does not decode"))) pure
      (decodeFull byronProtVer (BSL.fromStrict (e ^. #txout)) :: Either DecoderError TxOut)
    pure (TxInUtxo txid (fromIntegral (e ^. #txin . #index)), out)
  prevHash <-
    if BS.null (st ^. #prevHash)
      then pure Nothing
      else maybe (badRequest "prev_hash is not 32 bytes") (pure . Just) (abstractHashFromBytes (st ^. #prevHash) :: Maybe CC.HeaderHash)
  let utxo = UTxO.fromList entries
      pm = ProtocolMagicId (req ^. #protocolMagic)
      activation =
        Activation.State
          { Activation.delegationMap = Delegation.fromList pairs
          , Activation.delegationSlots = Map.fromList [(i, SlotNumber 0) | (i, _) <- pairs]
          }
      delegationState = DI.State (Scheduling.State Seq.empty Set.empty) activation
      configWith genesisHash =
        Genesis.Config
          { Genesis.configGenesisData =
              Genesis.GenesisData
                { Genesis.gdGenesisKeyHashes = Genesis.GenesisKeyHashes (Set.fromList (map fst pairs))
                , Genesis.gdHeavyDelegation = Genesis.UnsafeGenesisDelegation Map.empty
                , Genesis.gdStartTime = posixSecondsToUTCTime 0
                , Genesis.gdNonAvvmBalances = Genesis.GenesisNonAvvmBalances Map.empty
                , Genesis.gdProtocolParameters = UPI.adoptedProtocolParameters adoptedState
                , Genesis.gdK = BlockCount k
                , Genesis.gdProtocolMagicId = pm
                , Genesis.gdAvvmDistr = Genesis.GenesisAvvmBalances Map.empty
                }
          , Genesis.configGenesisHash = genesisHash
          , Genesis.configReqNetMagic = if pm == ProtocolMagicId 764824073 then RequiresNoMagic else RequiresMagic
          , Genesis.configUTxOConfiguration = defaultUTxOConfiguration
          }
      stateWith prev =
        CC.ChainValidationState
          { CC.cvsLastSlot = lastSlot
          , CC.cvsPreviousHash = prev
          , CC.cvsUtxo = utxo
          , CC.cvsUpdateState = adoptedState
          , CC.cvsDelegationState = delegationState
          }
      zeroGenesis = Genesis.GenesisHash (hashRaw BSL.empty)
  pure $ case decodeFullAnnotatedBytes byronProtVer "ABlockOrBoundary" (CC.decCBORABlockOrBoundary epochSlots) (BSL.fromStrict (req ^. #block)) :: Either DecoderError (CC.ABlockOrBoundary ByteString) of
    Left e -> defMessage & #rejection .~ rejection "Byron.Chain" "DeserialiseFailure" (T.pack (show e))
    Right blk ->
      let -- With no previous hash, the chain is at its start: a boundary block
          -- that opens it carries the genesis hash, which is taken from the
          -- block itself since the request carries none.
          genesisHash = case blk of
            CC.ABOBBoundary b | Left g <- CC.boundaryPrevHash (CC.boundaryHeader b) -> g
            _ -> zeroGenesis
          prev = maybe (Left genesisHash) Right prevHash
       in case runExcept (runReaderT (CC.updateChainBlockOrBoundary (configWith genesisHash) (stateWith prev) blk) (ValidationMode BlockValidation TxValidation)) of
            Left e -> defMessage & #rejection .~ (defMessage & #failures .~ chainFailures e)
            Right cvs -> chainResponse utxo cvs

chainResponse :: UTxO -> CC.ChainValidationState -> ByronChainUpdateResponse
chainResponse old cvs =
  let new = CC.cvsUtxo cvs
      oldL = UTxO.toList old
      newL = UTxO.toList new
      removed = [(i, o) | (i, o) <- oldL, not (UTxO.member i new)]
      added = [(i, o) | (i, o) <- newL, not (UTxO.member i old)]
      key (TxInUtxo h ix) = (abstractHashToBytes h, ix)
      entry (i, o) = defMessage & #txin .~ txInProto i & #txout .~ serialize' byronProtVer o
      dlg (i, d) = defMessage & #issuerKeyHash .~ khBytes i & #delegateKeyHash .~ khBytes d
      prevHash = either (const BS.empty) abstractHashToBytes (CC.cvsPreviousHash cvs)
      SlotNumber lastSlot = CC.cvsLastSlot cvs
   in defMessage
        & #chainState
          .~ ( defMessage
                & #prevHash .~ prevHash
                & #prevSlot .~ lastSlot
                & #delegations .~ map dlg (sortOn (khBytes . fst) (Bimap.toList (Delegation.unMap (DI.delegationMap (CC.cvsDelegationState cvs)))))
                & #update .~ upiStateProto (CC.cvsUpdateState cvs)
             )
        & #utxoRemoved .~ map (txInProto . fst) (sortOn (key . fst) removed)
        & #utxoAdded .~ map entry (sortOn (key . fst) added)

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'ByronMinFee q -> Just (respond #byronMinFee (byronMinFee q))
  Call'ByronTxValidate q -> Just (respond #byronTxValidate (byronTxValidate q))
  Call'ByronHeaderValidate q -> Just (respond #byronHeaderValidate (byronHeaderValidate q))
  Call'ByronChainUpdate q -> Just (respond #byronChainUpdate (byronChainUpdate q))
  Call'ByronUpdateProposal q -> Just (respond #byronUpdateProposal (byronUpdateProposal q))
  _ -> Nothing

implemented :: [Text]
implemented = ["byron_min_fee", "byron_tx_validate", "byron_header_validate", "byron_chain_update", "byron_update_proposal"]
