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
  ) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Common (hashKey)
import Cardano.Chain.Slotting (EpochSlots (..), SlotNumber (..))
import Cardano.Crypto.DSIGN (SignedDSIGN (..), rawDeserialiseVerKeyDSIGN)
import Cardano.Ledger.BaseTypes (unsafeNonZero)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Bimap as Bimap
import Ouroboros.Consensus.Byron.Crypto.DSIGN (ByronDSIGN, SigDSIGN (..), VerKeyDSIGN (..))
import Ouroboros.Consensus.Byron.Ledger.PBFT (toPBftLedgerView)
import Ouroboros.Consensus.Byron.Protocol (PBftByronCrypto)
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
  , subLovelace
  )
import Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
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
import qualified Cardano.Chain.UTxO.UTxO as UTxO
import qualified Cardano.Chain.UTxO.Validation as UTxO
import Cardano.Chain.Update (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Chain.Block (BlockValidationMode (..))
import Cardano.Chain.ValidationMode (ValidationMode (..))
import Cardano.Crypto (AProtocolMagic (..), RequiresNetworkMagic (..), ProtocolMagicId (..), abstractHashFromBytes, abstractHashToBytes)
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
import Numeric.Natural (Natural)
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
-- 'Ouroboros.Consensus.Byron.Ledger.PBFT.validateView' does. A chain state
-- with a signature window or previous slot is UNSUPPORTED, as in dxnode-go.
byronHeaderValidate :: ByronHeaderValidateRequest -> Result ByronHeaderValidateResponse
byronHeaderValidate req = do
  st <- maybe (badRequest "chain_state missing") pure (req ^. #maybe'chainState)
  when (not (null (st ^. #recentSigners)) || st ^. #maybe'prevSlot /= Nothing) $
    unsupported "a PBFT signature window and previous slot are not modelled"
  when (req ^. #securityParam == 0) $ unsupported "security_param must be nonzero"
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
       in case runExcept (updateChainDepState cfg view (SlotNo slot) (TickedPBftState ledgerView PBftState.empty)) of
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

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'ByronMinFee q -> Just (respond #byronMinFee (byronMinFee q))
  Call'ByronTxValidate q -> Just (respond #byronTxValidate (byronTxValidate q))
  Call'ByronHeaderValidate q -> Just (respond #byronHeaderValidate (byronHeaderValidate q))
  _ -> Nothing

implemented :: [Text]
implemented = ["byron_min_fee", "byron_tx_validate", "byron_header_validate"]
