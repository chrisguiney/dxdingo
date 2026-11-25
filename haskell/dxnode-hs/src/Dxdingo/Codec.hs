{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Codec functions: decode with the reference's own decoders, re-encode,
-- identify. A decode failure is @Rejection{codec, DeserialiseFailure}@.
module Dxdingo.Codec
  ( dispatch
  , implemented
  , codecTx
  , codecTxOutput
  , codecCertificate
  , codecGovernance
  , codecPlutusData
  , codecBlockHeader
  , codecBlockBody
  , codecByronBlock
  , codecHfcBlock
  , codecLedgerState
  , codecPParamsUpdate
  ) where

import qualified Cardano.Chain.Block as Byron
import Cardano.Chain.Slotting (EpochAndSlotCount (..), EpochNumber (..), EpochSlots (..), SlotCount (..), fromSlotNumber)
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hashing as ByronHash
import qualified Cardano.Chain.Common as ByronCommon
import qualified Cardano.Chain.UTxO as ByronUTxO
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Block (Block (..))
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (Anchor (..), BlocksMade (..), TxIx (..), unNonZero, urlToText)
import Cardano.Ledger.Conway.Governance
  ( Committee (..)
  , ConwayEraGov
  , Constitution (..)
  , ConwayGovState (..)
  , GovActionId (..)
  , GovActionIx (..)
  , GovActionState (..)
  , GovPurposeId (..)
  , GovRelation (..)
  , Proposals
  , govStatePrevGovActionIds
  , proposalsActions
  )
import Cardano.Ledger.Conway.State (CommitteeAuthorization (..), CommitteeState (..), ConwayEraAccounts, ConwayEraCertState, VState (..), certVStateL, dRepDelegationAccountStateL)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (ScriptHash (..), VRFVerKeyHash (..), extractHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..)
  , LedgerState (..)
  , NewEpochState (..)
  , UTxOState (..)
  )
import Cardano.Ledger.State
  ( ActiveStake (..)
  , ChainAccountState (..)
  , DState (..)
  , EraAccounts (..)
  , EraCertState (..)
  , PState (..)
  , SnapShot (..)
  , SnapShots (..)
  , StakePoolParams (..)
  , StakePoolState (..)
  , StakeWithDelegation (..)
  , UTxO (..)
  )
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.MemPack as MemPack
import qualified Data.VMap as VMap
import Data.Foldable (toList)
import Data.List (sortOn)
import Ouroboros.Consensus.Block (blockHash)
import Ouroboros.Consensus.Byron.Ledger.Config (CodecConfig (ByronCodecConfig))
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , CodecConfig (CardanoCodecConfig)
  , pattern BlockAllegra
  , pattern BlockAlonzo
  , pattern BlockBabbage
  , pattern BlockByron
  , pattern BlockConway
  , pattern BlockDijkstra
  , pattern BlockMary
  , pattern BlockShelley
  )
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (SerialiseHFC (..))
import Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (ShelleyCodecConfig))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, CoinPerWord (..), ppCoinsPerUTxOWordL, ppuCoinsPerUTxOWordL, ppuCollateralPercentageL, ppuCostModelsL, ppuMaxBlockExUnitsL, ppuMaxCollateralInputsL, ppuMaxTxExUnitsL, ppuMaxValSizeL, ppuPricesL)
import Cardano.Ledger.Babbage.PParams (BabbageEraPParams, ppuCoinsPerUTxOByteL)
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL, ConwayEraPParams, DRepVotingThresholds (..), PoolVotingThresholds (..), ppuCommitteeMaxTermLengthL, ppuCommitteeMinSizeL, ppuDRepActivityL, ppuDRepDepositL, ppuDRepVotingThresholdsL, ppuGovActionDepositL, ppuGovActionLifetimeL, ppuMinFeeRefScriptCostPerByteL, ppuPoolVotingThresholdsL)
import Cardano.Ledger.Plutus.CostModels (costModelsValid, getCostModelParams)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational), EpochInterval (..), Nonce (..), StrictMaybe, strictMaybeToMaybe)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Binary (Annotator, Decoded (..), EncCBOR, EncCBORGroup, byronProtVer, decodeFullAnnotatedBytes, getVersion)
import Data.Typeable (Typeable)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as TPraos
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCert (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Cardano.Slotting.Block (BlockNo (..))
import Data.Word (Word64)
import Cardano.Ledger.Address (serialiseAddr)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, IsPhase2Valid (..), isPhase2ValidTxL)
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import Cardano.Ledger.Binary
  ( DecoderError
  , DecCBOR (..)
  , Version
  , decodeFull
  , decodeFullAnnotator
  , mkVersion
  , serialize'
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Conway.Governance
  ( GovAction (..)
  , ProposalProcedure (..)
  , VotingProcedures
  )
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..), DijkstraTxCert (..))
import Cardano.Ledger.Conway.TxCert
  ( ConwayEraTxCert
  , ConwayGovCert (..)
  , Delegatee (..)
  , pattern AuthCommitteeHotKeyTxCert
  , pattern DelegTxCert
  , pattern RegDRepTxCert
  , pattern RegDepositDelegTxCert
  , pattern RegDepositTxCert
  , pattern ResignCommitteeColdTxCert
  , pattern UnRegDRepTxCert
  , pattern UnRegDepositTxCert
  , pattern UpdateDRepTxCert
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data, Datum (..), binaryDataToData)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.TxCert
  ( ShelleyEraTxCert
  , pattern DelegStakeTxCert
  , pattern GenesisDelegTxCert
  , pattern MirTxCert
  , pattern RegTxCert
  , pattern UnRegTxCert
  )
import Cardano.Ledger.TxIn (TxId (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.), to)
import qualified Data.Set as Set
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Coin (unCompactCoin)
import Proto.Dxdingo.V1.Codec
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- * Shared

decodeFailure :: DecoderError -> P.Rejection
decodeFailure e =
  defMessage
    & #failures
      .~ [defMessage & #rule .~ "codec" & #name .~ "DeserialiseFailure" & #detail .~ T.pack (show e)]

-- | A request version outside the era's range (eraProtVerLow to
-- eraProtVerHigh) is UNSUPPORTED; an unset one means the era's own.
versionArg :: forall era. Era era => Maybe P.ProtocolVersion -> Result Version
versionArg mpv = case maybe 0 (^. #major) mpv of
  0 -> pure (eraProtVerLow @era)
  m -> do
    v <- maybe (unsupported "protocol version") pure (mkVersion m)
    if v >= eraProtVerLow @era && v <= eraProtVerHigh @era
      then pure v
      else unsupported "protocol version outside the era"

-- * codec_tx

data TxEra = forall era. EraTx era => TxEra (Proxy era) (Tx TopTx era -> Bool)

txEra :: P.Era -> Result TxEra
txEra = \case
  P.ERA_SHELLEY -> pure (TxEra (Proxy @ShelleyEra) (const True))
  P.ERA_ALLEGRA -> pure (TxEra (Proxy @AllegraEra) (const True))
  P.ERA_MARY -> pure (TxEra (Proxy @MaryEra) (const True))
  P.ERA_ALONZO -> pure (TxEra (Proxy @AlonzoEra) phase2)
  P.ERA_BABBAGE -> pure (TxEra (Proxy @BabbageEra) phase2)
  P.ERA_CONWAY -> pure (TxEra (Proxy @ConwayEra) phase2)
  P.ERA_DIJKSTRA -> pure (TxEra (Proxy @DijkstraEra) phase2)
  e -> unsupported ("no codec for " <> T.pack (show e))
  where
    phase2 :: AlonzoEraTx era => Tx TopTx era -> Bool
    phase2 tx = tx ^. isPhase2ValidTxL == Phase2Valid

-- | Byron protocol versions are 0 and 1; an unset one is the era's own.
byronVersionArg :: Maybe P.ProtocolVersion -> Result ()
byronVersionArg mpv
  | maybe 0 (^. #major) mpv > 1 = unsupported "protocol version outside Byron"
  | otherwise = pure ()

-- | A submitted Byron transaction, [tx, [witness...]]. The id is the hash of
-- the transaction's wire bytes and the size that of the whole wire form, which
-- the fee is computed from; the encoding is rebuilt from the decoded value.
codecByronTx :: CodecTxRequest -> Result CodecTxResponse
codecByronTx req = do
  byronVersionArg (req ^. #maybe'protocolVersion)
  pure $
    case decodeFullAnnotatedBytes byronProtVer "ATxAux" decCBOR (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (ByronUTxO.ATxAux BS.ByteString) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right aux ->
        defMessage
          & #cbor .~ serialize' byronProtVer (void aux)
          & #txId .~ ByronHash.abstractHashToBytes (ByronHash.hashDecoded (ByronUTxO.aTaTx aux))
          & #size .~ fromIntegral (BS.length (recoverBytes aux))
          & #isValid .~ True

-- | A Byron output, [address, lovelace]; the address in its wire form.
codecByronTxOutput :: CodecTxOutputRequest -> Result CodecTxOutputResponse
codecByronTxOutput req = do
  byronVersionArg (req ^. #maybe'protocolVersion)
  pure $
    case decodeFull byronProtVer (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError ByronUTxO.TxOut of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right o ->
        defMessage
          & #cbor .~ serialize' byronProtVer o
          & #address .~ serialize' byronProtVer (ByronUTxO.txOutAddress o)
          & #coin .~ fromIntegral (ByronCommon.unsafeGetLovelace (ByronUTxO.txOutValue o))

codecTx :: CodecTxRequest -> Result CodecTxResponse
codecTx req
  | req ^. #era == P.ERA_BYRON = codecByronTx req
  | otherwise = codecTxEra req

codecTxEra :: CodecTxRequest -> Result CodecTxResponse
codecTxEra req = do
  TxEra (_ :: Proxy era) valid <- txEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFullAnnotator v "Tx" decCBOR (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (Tx TopTx era) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right tx ->
        defMessage
          & #cbor .~ serialize' v tx
          & #txId .~ hashToBytes (extractHash (unTxId (txIdTx tx)))
          & #size .~ fromIntegral (tx ^. sizeTxF)
          & #isValid .~ valid tx

-- * codec_tx_output

data OutInfo = OutInfo
  { oiAssets :: [P.Asset]
  , oiDatumHash :: BS.ByteString
  , oiInlineDatum :: BS.ByteString
  , oiScriptRef :: BS.ByteString
  }

noInfo :: OutInfo
noInfo = OutInfo [] BS.empty BS.empty BS.empty

data OutEra = forall era. EraTxOut era => OutEra (Proxy era) (TxOut era -> OutInfo)

outEra :: P.Era -> Result OutEra
outEra = \case
  P.ERA_SHELLEY -> pure (OutEra (Proxy @ShelleyEra) (const noInfo))
  P.ERA_ALLEGRA -> pure (OutEra (Proxy @AllegraEra) (const noInfo))
  P.ERA_MARY -> pure (OutEra (Proxy @MaryEra) maryInfo)
  P.ERA_ALONZO -> pure (OutEra (Proxy @AlonzoEra) (\o -> (maryInfo o) {oiDatumHash = dataHashOf o}))
  P.ERA_BABBAGE -> pure (OutEra (Proxy @BabbageEra) babbageInfo)
  P.ERA_CONWAY -> pure (OutEra (Proxy @ConwayEra) babbageInfo)
  P.ERA_DIJKSTRA -> pure (OutEra (Proxy @DijkstraEra) babbageInfo)
  e -> unsupported ("no codec for " <> T.pack (show e))

maryInfo :: (EraTxOut era, Value era ~ MaryValue) => TxOut era -> OutInfo
maryInfo o =
  noInfo
    { oiAssets =
        [ defMessage
            & #policyId .~ (let ScriptHash h = pol in hashToBytes h)
            & #assetName .~ SBS.fromShort nm
            & #quantity .~ fromIntegral q
        | (PolicyID pol, m) <- Map.toAscList ma
        , (AssetName nm, q) <- Map.toAscList m
        ]
    }
  where
    MaryValue _ (MultiAsset ma) = o ^. valueTxOutL

dataHashOf :: AlonzoEraTxOut era => TxOut era -> BS.ByteString
dataHashOf o = case o ^. dataHashTxOutL of
  SJust h -> hashToBytes (extractHash h)
  SNothing -> BS.empty

babbageInfo :: (BabbageEraTxOut era, Value era ~ MaryValue) => TxOut era -> OutInfo
babbageInfo o =
  (maryInfo o)
    { oiDatumHash = case o ^. datumTxOutF of
        DatumHash h -> hashToBytes (extractHash h)
        _ -> BS.empty
    , oiInlineDatum = case o ^. datumTxOutF of
        Datum bd -> originalBytes (binaryDataToData bd)
        _ -> BS.empty
    , oiScriptRef = case o ^. referenceScriptTxOutL of
        SJust s -> let ScriptHash h = hashScript s in hashToBytes h
        SNothing -> BS.empty
    }

codecTxOutput :: CodecTxOutputRequest -> Result CodecTxOutputResponse
codecTxOutput req
  | req ^. #era == P.ERA_BYRON = codecByronTxOutput req
  | otherwise = codecTxOutputEra req

codecTxOutputEra :: CodecTxOutputRequest -> Result CodecTxOutputResponse
codecTxOutputEra req = do
  OutEra (_ :: Proxy era) info <- outEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFull v (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (TxOut era) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right o ->
        let i = info o
         in defMessage
              & #cbor .~ serialize' v o
              & #address .~ serialiseAddr (o ^. addrTxOutL)
              & #coin .~ fromIntegral (unCoin (o ^. coinTxOutL))
              & #assets .~ oiAssets i
              & #datumHash .~ oiDatumHash i
              & #inlineDatum .~ oiInlineDatum i
              & #scriptRef .~ oiScriptRef i

-- * codec_certificate

shelleyKind :: ShelleyEraTxCert era => TxCert era -> Maybe Text
shelleyKind = \case
  RegTxCert _ -> Just "RegTxCert"
  UnRegTxCert _ -> Just "UnRegTxCert"
  DelegStakeTxCert _ _ -> Just "DelegStakeTxCert"
  RegPoolTxCert _ -> Just "RegPoolTxCert"
  RetirePoolTxCert _ _ -> Just "RetirePoolTxCert"
  _ -> Nothing

-- | Genesis delegation and MIR certificates end with Babbage.
preConwayKind :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Text
preConwayKind c = case c of
  GenesisDelegTxCert {} -> "GenesisDelegTxCert"
  MirTxCert _ -> "MirTxCert"
  _ -> fromMaybe "UnknownTxCert" (shelleyKind c)

-- | Conway: the Shelley-shaped forms first (tag 2 matches both
-- DelegStakeTxCert and DelegTxCert), then the Conway-only constructors.
conwayKind :: (ShelleyEraTxCert era, ConwayEraTxCert era) => TxCert era -> Text
conwayKind c = case shelleyKind c of
  Just k | k `elem` ["RegTxCert", "UnRegTxCert", "DelegStakeTxCert"] -> k
  _ -> case c of
    RegDepositTxCert {} -> "RegDepositTxCert"
    UnRegDepositTxCert {} -> "UnRegDepositTxCert"
    RegDepositDelegTxCert {} -> "RegDepositDelegTxCert"
    DelegTxCert {} -> "DelegTxCert"
    AuthCommitteeHotKeyTxCert {} -> "AuthCommitteeHotKeyTxCert"
    ResignCommitteeColdTxCert {} -> "ResignCommitteeColdTxCert"
    RegDRepTxCert {} -> "RegDRepTxCert"
    UnRegDRepTxCert {} -> "UnRegDRepTxCert"
    UpdateDRepTxCert {} -> "UpdateDRepTxCert"
    _ -> fromMaybe "UnknownTxCert" (shelleyKind c)

-- | Dijkstra removes the Shelley-shaped forms, genesis delegation and MIR, so
-- its kinds follow the constructors: the stake-delegation tag 2 keeps its
-- Shelley name, the deposit forms are the Conway ones.
dijkstraKind :: TxCert DijkstraEra -> Text
dijkstraKind = \case
  DijkstraTxCertDeleg d -> case d of
    DijkstraRegCert {} -> "RegDepositTxCert"
    DijkstraUnRegCert {} -> "UnRegDepositTxCert"
    DijkstraDelegCert _ (DelegStake _) -> "DelegStakeTxCert"
    DijkstraDelegCert {} -> "DelegTxCert"
    DijkstraRegDelegCert {} -> "RegDepositDelegTxCert"
  DijkstraTxCertPool (RegPool _) -> "RegPoolTxCert"
  DijkstraTxCertPool (RetirePool _ _) -> "RetirePoolTxCert"
  DijkstraTxCertGov g -> case g of
    ConwayRegDRep {} -> "RegDRepTxCert"
    ConwayUnRegDRep {} -> "UnRegDRepTxCert"
    ConwayUpdateDRep {} -> "UpdateDRepTxCert"
    ConwayAuthCommitteeHotKey {} -> "AuthCommitteeHotKeyTxCert"
    ConwayResignCommitteeColdKey {} -> "ResignCommitteeColdTxCert"

data CertEra = forall era. EraTxCert era => CertEra (Proxy era) (TxCert era -> Text)

certEra :: P.Era -> Result CertEra
certEra = \case
  P.ERA_SHELLEY -> pure (CertEra (Proxy @ShelleyEra) pre)
  P.ERA_ALLEGRA -> pure (CertEra (Proxy @AllegraEra) pre)
  P.ERA_MARY -> pure (CertEra (Proxy @MaryEra) pre)
  P.ERA_ALONZO -> pure (CertEra (Proxy @AlonzoEra) pre)
  P.ERA_BABBAGE -> pure (CertEra (Proxy @BabbageEra) pre)
  P.ERA_CONWAY -> pure (CertEra (Proxy @ConwayEra) conwayKind)
  P.ERA_DIJKSTRA -> pure (CertEra (Proxy @DijkstraEra) dijkstraKind)
  e -> unsupported ("no codec for " <> T.pack (show e))
  where
    pre :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Text
    pre = preConwayKind

codecCertificate :: CodecCertificateRequest -> Result CodecCertificateResponse
codecCertificate req = do
  CertEra (_ :: Proxy era) kind <- certEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFull v (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (TxCert era) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right c -> defMessage & #cbor .~ serialize' v c & #certKind .~ kind c

-- * codec_governance

data GovEra
  = forall era.
    ( EraPParams era
    , DecCBOR (ProposalProcedure era)
    , EncCBOR (ProposalProcedure era)
    , DecCBOR (VotingProcedures era)
    , EncCBOR (VotingProcedures era)
    ) =>
    GovEra (Proxy era)

-- | Governance items exist from Conway.
govEra :: P.Era -> Result GovEra
govEra = \case
  P.ERA_CONWAY -> pure (GovEra (Proxy @ConwayEra))
  P.ERA_DIJKSTRA -> pure (GovEra (Proxy @DijkstraEra))
  _ -> unsupported "governance items exist from Conway"

codecGovernance :: CodecGovernanceRequest -> Result CodecGovernanceResponse
codecGovernance req = do
  GovEra (_ :: Proxy era) <- govEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  let bs = BSL.fromStrict (req ^. #cbor)
  case req ^. #kind of
    GOVERNANCE_ITEM_KIND_PROPOSAL ->
      pure $ case decodeFull v bs :: Either DecoderError (ProposalProcedure era) of
        Left e -> defMessage & #rejection .~ decodeFailure e
        Right p -> defMessage & #cbor .~ serialize' v p & #actionType .~ actionType (pProcGovAction p)
    GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES ->
      pure $ case decodeFull v bs :: Either DecoderError (VotingProcedures era) of
        Left e -> defMessage & #rejection .~ decodeFailure e
        Right p -> defMessage & #cbor .~ serialize' v p
    _ -> badRequest "governance kind unset"

actionType :: GovAction era -> P.GovActionType
actionType = \case
  ParameterChange {} -> P.GOV_ACTION_TYPE_PARAMETER_CHANGE
  HardForkInitiation {} -> P.GOV_ACTION_TYPE_HARD_FORK_INITIATION
  TreasuryWithdrawals {} -> P.GOV_ACTION_TYPE_TREASURY_WITHDRAWALS
  NoConfidence {} -> P.GOV_ACTION_TYPE_NO_CONFIDENCE
  UpdateCommittee {} -> P.GOV_ACTION_TYPE_UPDATE_COMMITTEE
  NewConstitution {} -> P.GOV_ACTION_TYPE_NEW_CONSTITUTION
  InfoAction -> P.GOV_ACTION_TYPE_INFO

-- * codec_plutus_data

-- | The ledger's @Data@: annotator-decoded, re-encoded as its original bytes.
codecPlutusData :: CodecPlutusDataRequest -> Result CodecPlutusDataResponse
codecPlutusData req =
  pure $
    case decodeFullAnnotator (eraProtVerLow @ConwayEra) "Data" decCBOR (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (Data ConwayEra) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right d -> defMessage & #cbor .~ originalBytes d

-- * Block headers and blocks

-- | The header fields the codec reports, read from either header type.
data HdrView = HdrView
  { hvHash :: BS.ByteString
  , hvSlot :: Word64
  , hvBlockNo :: Word64
  , hvPrev :: BS.ByteString
  , hvIssuer :: BS.ByteString
  , hvVrf :: BS.ByteString
  , hvBodySize :: Word64
  , hvBodyHash :: BS.ByteString
  , hvCounter :: Word64
  , hvKes :: Word64
  , hvProtVer :: ProtVer
  }

prevBytes :: TPraos.PrevHash -> BS.ByteString
prevBytes = \case
  TPraos.GenesisHash -> BS.empty
  TPraos.BlockHash (TPraos.HashHeader h) -> hashToBytes h

tpraosView :: TPraos.BHeader StandardCrypto -> HdrView
tpraosView h =
  HdrView
    { hvHash = let TPraos.HashHeader x = TPraos.bhHash h in hashToBytes x
    , hvSlot = unSlotNo (TPraos.bheaderSlotNo b)
    , hvBlockNo = unBlockNo (TPraos.bheaderBlockNo b)
    , hvPrev = prevBytes (TPraos.bheaderPrev b)
    , hvIssuer = DSIGN.rawSerialiseVerKeyDSIGN (unVKey (TPraos.bheaderVk b))
    , hvVrf = VRF.rawSerialiseVerKeyVRF (TPraos.bheaderVrfVk b)
    , hvBodySize = fromIntegral (TPraos.bsize b)
    , hvBodyHash = hashToBytes (TPraos.bhash b)
    , hvCounter = ocertN oc
    , hvKes = let KESPeriod k = ocertKESPeriod oc in fromIntegral k
    , hvProtVer = TPraos.bprotver b
    }
  where
    b = TPraos.bhbody h
    oc = TPraos.bheaderOCert b

praosView :: Praos.Header StandardCrypto -> HdrView
praosView h =
  HdrView
    { hvHash = hashToBytes (Praos.headerHash h)
    , hvSlot = unSlotNo (Praos.hbSlotNo b)
    , hvBlockNo = unBlockNo (Praos.hbBlockNo b)
    , hvPrev = prevBytes (Praos.hbPrev b)
    , hvIssuer = DSIGN.rawSerialiseVerKeyDSIGN (unVKey (Praos.hbVk b))
    , hvVrf = VRF.rawSerialiseVerKeyVRF (Praos.hbVrfVk b)
    , hvBodySize = fromIntegral (Praos.hbBodySize b)
    , hvBodyHash = hashToBytes (Praos.hbBodyHash b)
    , hvCounter = ocertN oc
    , hvKes = let KESPeriod k = ocertKESPeriod oc in fromIntegral k
    , hvProtVer = Praos.hbProtVer b
    }
  where
    b = Praos.headerBody h
    oc = Praos.hbOCert b

type BlockCtx h era =
  (EraBlockBody era, EncCBORGroup (BlockBody era), DecCBOR (Annotator h), EncCBOR h, Typeable h)

-- | The era's header type (TPraos to Alonzo, Praos from Babbage), its block
-- type, and the header view.
data BlkEra = forall h era. BlockCtx h era => BlkEra (Proxy h) (Proxy era) (h -> HdrView)

blkEra :: P.Era -> Result BlkEra
blkEra = \case
  P.ERA_SHELLEY -> pure (tp (Proxy @ShelleyEra))
  P.ERA_ALLEGRA -> pure (tp (Proxy @AllegraEra))
  P.ERA_MARY -> pure (tp (Proxy @MaryEra))
  P.ERA_ALONZO -> pure (tp (Proxy @AlonzoEra))
  P.ERA_BABBAGE -> pure (pr (Proxy @BabbageEra))
  P.ERA_CONWAY -> pure (pr (Proxy @ConwayEra))
  P.ERA_DIJKSTRA -> pure (pr (Proxy @DijkstraEra))
  e -> unsupported ("no codec for " <> T.pack (show e))
  where
    tp :: (EraBlockBody era, EncCBORGroup (BlockBody era)) => Proxy era -> BlkEra
    tp p = BlkEra (Proxy @(TPraos.BHeader StandardCrypto)) p tpraosView
    pr :: (EraBlockBody era, EncCBORGroup (BlockBody era)) => Proxy era -> BlkEra
    pr p = BlkEra (Proxy @(Praos.Header StandardCrypto)) p praosView

protVerProto :: ProtVer -> P.ProtocolVersion
protVerProto (ProtVer maj mnr) =
  defMessage & #major .~ getVersion maj & #minor .~ fromIntegral mnr

codecBlockHeader :: CodecBlockHeaderRequest -> Result CodecBlockHeaderResponse
codecBlockHeader req = do
  BlkEra (_ :: Proxy h) (_ :: Proxy era) view <- blkEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFullAnnotator v "Header" decCBOR (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError h of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right h ->
        let i = view h
         in defMessage
              & #cbor .~ serialize' v h
              & #headerHash .~ hvHash i
              & #slot .~ hvSlot i
              & #blockNo .~ hvBlockNo i
              & #prevHash .~ hvPrev i
              & #issuerVkey .~ hvIssuer i
              & #vrfVkey .~ hvVrf i
              & #bodySize .~ hvBodySize i
              & #bodyHash .~ hvBodyHash i
              & #opcertCounter .~ hvCounter i
              & #kesPeriod .~ hvKes i
              & #protocolVersion .~ protVerProto (hvProtVer i)

-- | A whole era block. The body hash is the one the header commits to; the
-- body itself is not hashed here, the BBODY rule does that.
codecBlockBody :: CodecBlockBodyRequest -> Result CodecBlockBodyResponse
codecBlockBody req = do
  BlkEra (_ :: Proxy h) (_ :: Proxy era) view <- blkEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFullAnnotator v "Block" decCBOR (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (Block h era) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right blk ->
        defMessage
          & #cbor .~ serialize' v blk
          & #txCount .~ fromIntegral (length (blockBody blk ^. txSeqBlockBodyL))
          & #bodyHash .~ hvBodyHash (view (blockHeader blk))

-- | The ABlockOrBoundary wire form, [tag, block], under mainnet's epoch
-- length. Re-encoded from the decoded block.
codecByronBlock :: CodecByronBlockRequest -> Result CodecByronBlockResponse
codecByronBlock req =
  pure $
    case decodeFullAnnotatedBytes byronProtVer "ABlockOrBoundary" (Byron.decCBORABlockOrBoundary epochSlots) (BSL.fromStrict (req ^. #cbor)) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right b ->
        let enc = serialize' byronProtVer (Byron.encCBORABlockOrBoundary (ProtocolMagicId (req ^. #protocolMagic)) epochSlots b)
         in case b of
              Byron.ABOBBlock blk ->
                let EpochAndSlotCount (EpochNumber ep) (SlotCount sl) = fromSlotNumber epochSlots (Byron.blockSlot blk)
                 in defMessage
                      & #cbor .~ enc
                      & #isBoundary .~ False
                      & #epoch .~ ep
                      & #slot .~ sl
                      & #headerHash .~ ByronHash.abstractHashToBytes (Byron.blockHashAnnotated blk)
              Byron.ABOBBoundary blk ->
                defMessage
                  & #cbor .~ enc
                  & #isBoundary .~ True
                  & #epoch .~ Byron.boundaryEpoch (Byron.boundaryHeader blk)
                  & #headerHash .~ ByronHash.abstractHashToBytes (Byron.boundaryHashAnnotated blk)
  where
    epochSlots = EpochSlots 21600

-- * codec_hfc_block

-- | The consensus codec configuration of the Cardano chain under mainnet's
-- Byron epoch length.
hfcCodecConfig :: CodecConfig (CardanoBlock StandardCrypto)
hfcCodecConfig =
  CardanoCodecConfig
    (ByronCodecConfig (EpochSlots 21600))
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig

hfcEra :: CardanoBlock StandardCrypto -> P.Era
hfcEra = \case
  BlockByron _ -> P.ERA_BYRON
  BlockShelley _ -> P.ERA_SHELLEY
  BlockAllegra _ -> P.ERA_ALLEGRA
  BlockMary _ -> P.ERA_MARY
  BlockAlonzo _ -> P.ERA_ALONZO
  BlockBabbage _ -> P.ERA_BABBAGE
  BlockConway _ -> P.ERA_CONWAY
  BlockDijkstra _ -> P.ERA_DIJKSTRA

rejectionOf :: Show e => e -> P.Rejection
rejectionOf e =
  defMessage
    & #failures
      .~ [defMessage & #rule .~ "codec" & #name .~ "DeserialiseFailure" & #detail .~ T.pack (show e)]

-- | A Cardano hard-fork-combinator block. Version 0 is the disk form of
-- 'encodeDiskHfcBlock'; a non-zero node-to-node version is the node-to-node
-- form, that encoding wrapped in CBOR tag 24 (the reference ignores the
-- version otherwise).
codecHfcBlock :: CodecHfcBlockRequest -> Result CodecHfcBlockResponse
codecHfcBlock req =
  pure $
    case decoded of
      Left e -> defMessage & #rejection .~ rejectionOf e
      Right blk ->
        defMessage
          & #cbor .~ BSL.toStrict (CBOR.toLazyByteString (encode blk))
          & #era .~ hfcEra blk
          & #headerHash .~ toRawHash (Proxy @(CardanoBlock StandardCrypto)) (blockHash blk)
  where
    bytes = BSL.fromStrict (req ^. #cbor)
    n2n = req ^. #nodeToNodeVersion /= 0
    decoded :: Either String (CardanoBlock StandardCrypto)
    decoded
      | n2n = case CBOR.deserialiseFromBytes (unwrapCBORinCBOR (decodeDiskHfcBlock hfcCodecConfig)) bytes of
          Left e -> Left (show e)
          Right (rest, blk) -> finish rest (Right blk)
      | otherwise = case CBOR.deserialiseFromBytes (decodeDiskHfcBlock hfcCodecConfig) bytes of
          Left e -> Left (show e)
          Right (rest, f) -> finish rest (either (Left . show) Right (f bytes))
    finish rest r
      | not (BSL.null rest) = Left "trailing bytes"
      | otherwise = r
    encode blk
      | n2n = wrapCBORinCBOR (encodeDiskHfcBlock hfcCodecConfig) blk
      | otherwise = encodeDiskHfcBlock hfcCodecConfig blk

-- * codec_ledger_state

-- | The era's ledger-state readers: the MemPack TxOut re-encoder and the
-- NewEpochState view.
data LsEra
  = forall era.
    (EraTxOut era, DecCBOR (NewEpochState era), MemPack.MemPack (TxOut era)) =>
    LsEra (Proxy era) (Version -> NewEpochState era -> CodecLedgerStateResponse)

lsEra :: P.Era -> Result LsEra
lsEra = \case
  P.ERA_SHELLEY -> pure (LsEra (Proxy @ShelleyEra) (viewPre (\pp -> ppBase pp & #maybe'd .~ Just (rationalProto (unboundRational (pp ^. ppDL))) & #maybe'extraEntropy .~ Just (nonceOf (pp ^. ppExtraEntropyL)) & #maybe'minUtxoValue .~ Just (coinW (pp ^. ppMinUTxOValueL)))))
  P.ERA_ALLEGRA -> pure (LsEra (Proxy @AllegraEra) (viewPre (\pp -> ppBase pp & #maybe'd .~ Just (rationalProto (unboundRational (pp ^. ppDL))) & #maybe'extraEntropy .~ Just (nonceOf (pp ^. ppExtraEntropyL)) & #maybe'minUtxoValue .~ Just (coinW (pp ^. ppMinUTxOValueL)))))
  P.ERA_MARY -> pure (LsEra (Proxy @MaryEra) (viewPre (\pp -> ppBase pp & #maybe'd .~ Just (rationalProto (unboundRational (pp ^. ppDL))) & #maybe'extraEntropy .~ Just (nonceOf (pp ^. ppExtraEntropyL)) & #maybe'minUtxoValue .~ Just (coinW (pp ^. ppMinUTxOValueL)))))
  P.ERA_ALONZO -> pure (LsEra (Proxy @AlonzoEra) (viewPre (\pp -> ppAlonzo pp & #maybe'd .~ Just (rationalProto (unboundRational (pp ^. ppDL))) & #maybe'extraEntropy .~ Just (nonceOf (pp ^. ppExtraEntropyL)) & #maybe'coinsPerUtxoWord .~ Just (coinW (unCoinPerWord (pp ^. ppCoinsPerUTxOWordL))))))
  P.ERA_BABBAGE -> pure (LsEra (Proxy @BabbageEra) (viewPre (\pp -> ppAlonzo pp & #maybe'coinsPerUtxoByte .~ Just (coinW (fromCompact (unCoinPerByte (pp ^. ppCoinsPerUTxOByteL)))))))
  P.ERA_CONWAY -> pure (LsEra (Proxy @ConwayEra) viewNewEpochState)
  P.ERA_DIJKSTRA -> pure (LsEra (Proxy @DijkstraEra) viewNewEpochState)
  e -> unsupported ("no ledger state codec for " <> T.pack (show e))
  where
    nonceOf n = defMessage & #hash .~ (case n of NeutralNonce -> BS.empty; Nonce h -> hashToBytes h)

-- | Ledger-state encodings: Shelley through Dijkstra. MemPack TxOut rows are
-- read with the era's TxOut; a NewEpochState with the era's own decoder.
codecLedgerState :: CodecLedgerStateRequest -> Result CodecLedgerStateResponse
codecLedgerState req = do
  LsEra (_ :: Proxy era) view <- lsEra (req ^. #era)
  let v = eraProtVerLow @era
      bytes = req ^. #cbor
  case req ^. #format of
    LEDGER_STATE_FORMAT_MEMPACK_TXOUT ->
      pure $ case MemPack.unpackMonadFail bytes :: Maybe (TxOut era) of
        Nothing -> defMessage & #rejection .~ rejectionOf ("MemPack TxOut" :: String)
        Just out -> defMessage & #cbor .~ serialize' v out
    LEDGER_STATE_FORMAT_NEW_EPOCH_STATE ->
      pure $ case decodeFull v (BSL.fromStrict bytes) :: Either DecoderError (NewEpochState era) of
        Left e -> defMessage & #rejection .~ decodeFailure e
        Right nes -> view v nes
    _ -> badRequest "ledger state format"

-- | The canonical order of credentials: kind (key before script), then hash.
credKey :: P.Credential -> (Int, BS.ByteString)
credKey c = (fromEnum (c ^. #kind), c ^. #hash)

cred :: Credential r -> P.Credential
cred = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

poolHash :: KeyHash r -> BS.ByteString
poolHash (KeyHash h) = hashToBytes h

drepProto :: DRep -> P.DRep
drepProto = \case
  DRepCredential c -> defMessage & #credential .~ cred c
  DRepAlwaysAbstain -> defMessage & #alwaysAbstain .~ True
  DRepAlwaysNoConfidence -> defMessage & #alwaysNoConfidence .~ True

anchorFields :: Anchor -> (T.Text, BS.ByteString)
anchorFields a = (urlToText (anchorUrl a), hashToBytes (extractHash (anchorDataHash a)))

govId :: GovActionId -> P.GovActionId
govId (GovActionId (TxId h) (GovActionIx ix)) = defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral ix

-- | The view every era shares: epoch, pots, UTxO, accounts, pools, snapshots
-- and blocks made. The era-specific account fields and the parameters are
-- added by the caller.
baseView ::
  forall era.
  (EraTxOut era, EraGov era, EraCertState era, EraAccounts era) =>
  (AccountState era -> S.Account -> S.Account) ->
  Version ->
  NewEpochState era ->
  CodecLedgerStateResponse
baseView acctExtra v nes =
  defMessage
    & #epoch .~ unEpochNo (nesEL nes)
    & #state .~ state
    & #snapshots .~ sortOn (\e -> (fromEnum (e ^. #snapshot), credKey (e ^. #credential))) (concat [snap S.SNAPSHOT_KIND_MARK (ssStakeMark ss), snap S.SNAPSHOT_KIND_SET (ssStakeSet ss), snap S.SNAPSHOT_KIND_GO (ssStakeGo ss)])
    & #blocksCurrent .~ sortOn (^. #poolId) (blocks (nesBcur nes))
    & #blocksPrevious .~ sortOn (^. #poolId) (blocks (nesBprev nes))
  where
    es = nesEs nes
    ss = esSnapshots es
    ls = esLState es
    us = lsUTxOState ls
    cs = lsCertState ls
    ps = cs ^. certPStateL
    accounts = cs ^. certDStateL . to dsAccounts . accountsMapL
    blocks (BlocksMade m) = [defMessage & #poolId .~ poolHash k & #count .~ fromIntegral n | (k, n) <- Map.toList m]
    snap kind s =
      [ defMessage & #snapshot .~ kind & #credential .~ cred c & #amount .~ fromIntegral (unCompactCoin (unNonZero (swdStake d))) & #poolId .~ poolHash (swdDelegation d)
      | (c, d) <- VMap.toAscList (unActiveStake (ssActiveStake s))
      ]
    state =
      defMessage
        & #utxo .~ sortOn (\e -> (e ^. #txin . #txId, e ^. #txin . #index)) [defMessage & #txin .~ txIn i & #txout .~ serialize' v o | (i, o) <- Map.toList (unUTxO (utxosUtxo us))]
        & #accounts .~ sortOn (credKey . (^. #credential)) [account c a | (c, a) <- Map.toList accounts]
        & #pools .~ sortOn (\e -> (e ^. #poolId, fromEnum (e ^. #stage))) (concat [pool k p | (k, p) <- Map.toList (psStakePools ps)] ++ [futurePool k p | (k, p) <- Map.toList (psFutureStakePoolParams ps)])
        & #pots .~ pots
    pots =
      defMessage
        & #reserves .~ coinW (casReserves (esChainAccountState es))
        & #treasury .~ coinW (casTreasury (esChainAccountState es))
        & #fees .~ coinW (utxosFees us)
        & #donations .~ coinW (utxosDonation us)
    txIn (TxIn (TxId h) (TxIx ix)) = defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral ix
    account c a =
      acctExtra a $
        defMessage
          & #credential .~ cred c
          & #deposit .~ fromIntegral (unCompactCoin (a ^. depositAccountStateL))
          & #balance .~ fromIntegral (unCompactCoin (a ^. balanceAccountStateL))
          & #poolId .~ maybe BS.empty poolHash (a ^. stakePoolDelegationAccountStateL)
    pool k p =
      [ defMessage
          & #poolId .~ poolHash k
          & #stage .~ S.POOL_STAGE_CURRENT
          & #vrfKeyHash .~ hashToBytes (unVRFVerKeyHash (spsVrf p))
          & #pledge .~ coinW (spsPledge p)
          & #cost .~ coinW (spsCost p)
          & #margin .~ rationalProto (unboundRational (spsMargin p))
          & #rewardAccount .~ cred (unAccountId (spsAccountId p))
          & #owners .~ map poolHash (Set.toAscList (spsOwners p))
          & #deposit .~ fromIntegral (unCompactCoin (spsDeposit p))
          & #maybe'retiringEpoch .~ (unEpochNo <$> Map.lookup k (psRetiring ps))
      ]
    futurePool k p =
      defMessage
        & #poolId .~ poolHash k
        & #stage .~ S.POOL_STAGE_FUTURE
        & #vrfKeyHash .~ hashToBytes (unVRFVerKeyHash (sppVrf p))
        & #pledge .~ coinW (sppPledge p)
        & #cost .~ coinW (sppCost p)
        & #margin .~ rationalProto (unboundRational (sppMargin p))
        & #rewardAccount .~ cred (unAccountId (aaId (sppAccountAddress p)))
        & #owners .~ map poolHash (Set.toAscList (sppOwners p))
        & #deposit .~ maybe 0 (fromIntegral . unCompactCoin . spsDeposit) (Map.lookup k (psStakePools ps))

-- | A pre-Conway state: no DReps, committee or proposals, and the era's own
-- parameters.
viewPre ::
  (EraTxOut era, EraGov era, EraCertState era, EraAccounts era) =>
  (PParams era -> S.ProtocolParameters) ->
  Version ->
  NewEpochState era ->
  CodecLedgerStateResponse
viewPre ppv v nes =
  baseView (\_ -> id) v nes
    & #pparams .~ ppv (utxosGovState (lsUTxOState (esLState (nesEs nes))) ^. curPParamsGovStateL)

type ConwayLs era =
  ( EraTxOut era
  , ConwayEraGov era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , ConwayEraAccounts era
  , GovState era ~ ConwayGovState era
  )

-- | A Conway or Dijkstra state: the shared view with the delegations,
-- DReps, committee, proposals and enact state.
viewNewEpochState :: forall era. ConwayLs era => Version -> NewEpochState era -> CodecLedgerStateResponse
viewNewEpochState v nes =
  baseView (\a -> #maybe'drep .~ (drepProto <$> (a ^. dRepDelegationAccountStateL))) v nes
    & #pparams .~ ppProto (cgsCurPParams gs)
    & #state . #dreps .~ sortOn (credKey . (^. #credential)) [drep c d | (c, d) <- Map.toList (vsDReps vs)]
    & #state . #maybe'committee .~ (committee vs <$> strictMaybeToMaybe (cgsCommittee gs))
    & #state . #proposals .~ sortOn (\e -> (e ^. #id . #txId, e ^. #id . #index)) (map proposal (toList (proposalsActions (cgsProposals gs))))
    & #state . #enactState .~ enact gs
  where
    us = lsUTxOState (esLState (nesEs nes))
    cs = lsCertState (esLState (nesEs nes))
    gs = utxosGovState us
    vs = cs ^. certVStateL
    drep c d =
      let (url, h) = anchorFields' (drepAnchor d)
       in defMessage
            & #credential .~ cred c
            & #deposit .~ fromIntegral (unCompactCoin (drepDeposit d))
            & #expiry .~ unEpochNo (drepExpiry d)
            & #anchorUrl .~ url
            & #anchorHash .~ h
    anchorFields' = \case
      SNothing -> ("", BS.empty)
      SJust a -> anchorFields a
    committee vst c =
      let auths = csCommitteeCreds (vsCommitteeState vst)
          member (cold, expiry) =
            let auth = Map.lookup cold auths
             in defMessage
                  & #cold .~ cred cold
                  & #maybe'hot .~ (case auth of Just (CommitteeHotCredential h) -> Just (cred h); _ -> Nothing)
                  & #expiry .~ unEpochNo expiry
                  & #resigned .~ (case auth of Just (CommitteeMemberResigned _) -> True; _ -> False)
       in defMessage
            & #members .~ sortOn (credKey . (^. #cold)) (map member (Map.toList (committeeMembers c)))
            & #threshold .~ rationalProto (unboundRational (committeeThreshold c))
    proposal g =
      let pp = gasProposalProcedure g
          action = pProcGovAction pp
          (aType, parent) = actionInfo action
       in defMessage
            & #id .~ govId (gasId g)
            & #actionType .~ aType
            & #action .~ serialize' v action
            & #maybe'parent .~ parent
            & #proposedEpoch .~ unEpochNo (gasProposedIn g)
            & #expiresEpoch .~ unEpochNo (gasExpiresAfter g)
            & #deposit .~ coinW (pProcDeposit pp)
            & #returnAccount .~ cred (unAccountId (aaId (pProcReturnAddr pp)))
    actionInfo = \case
      ParameterChange p _ _ -> (P.GOV_ACTION_TYPE_PARAMETER_CHANGE, govId . unGovPurposeId <$> strictMaybeToMaybe p)
      HardForkInitiation p _ -> (P.GOV_ACTION_TYPE_HARD_FORK_INITIATION, govId . unGovPurposeId <$> strictMaybeToMaybe p)
      TreasuryWithdrawals _ _ -> (P.GOV_ACTION_TYPE_TREASURY_WITHDRAWALS, Nothing)
      NoConfidence p -> (P.GOV_ACTION_TYPE_NO_CONFIDENCE, govId . unGovPurposeId <$> strictMaybeToMaybe p)
      UpdateCommittee p _ _ _ -> (P.GOV_ACTION_TYPE_UPDATE_COMMITTEE, govId . unGovPurposeId <$> strictMaybeToMaybe p)
      NewConstitution p _ -> (P.GOV_ACTION_TYPE_NEW_CONSTITUTION, govId . unGovPurposeId <$> strictMaybeToMaybe p)
      InfoAction -> (P.GOV_ACTION_TYPE_INFO, Nothing)
    enact :: ConwayGovState era -> S.EnactState
    enact g =
      let roots = govStatePrevGovActionIds g
          c = cgsConstitution g
          (url, h) = anchorFields (constitutionAnchor c)
       in defMessage
            & #maybe'prevPparamUpdate .~ (govId . unGovPurposeId <$> strictMaybeToMaybe (grPParamUpdate roots))
            & #maybe'prevHardFork .~ (govId . unGovPurposeId <$> strictMaybeToMaybe (grHardFork roots))
            & #maybe'prevCommittee .~ (govId . unGovPurposeId <$> strictMaybeToMaybe (grCommittee roots))
            & #maybe'prevConstitution .~ (govId . unGovPurposeId <$> strictMaybeToMaybe (grConstitution roots))
            & #constitution
              .~ ( defMessage
                     & #anchorUrl .~ url
                     & #anchorHash .~ h
                     & #guardrailScriptHash .~ maybe BS.empty (\(ScriptHash sh) -> hashToBytes sh) (strictMaybeToMaybe (constitutionGuardrailsScriptHash c))
                 )


-- | The parameters every era has, as the NewEpochState view reads them.
ppBase :: EraPParams era => PParams era -> S.ProtocolParameters
ppBase pp =
  defMessage
    & #maybe'minFeeA .~ Just (coinW (pp ^. ppMinFeeAL))
    & #maybe'minFeeB .~ Just (coinW (pp ^. ppMinFeeBL))
    & #maybe'maxBlockBodySize .~ Just (fromIntegral (pp ^. ppMaxBBSizeL))
    & #maybe'maxTxSize .~ Just (fromIntegral (pp ^. ppMaxTxSizeL))
    & #maybe'maxBlockHeaderSize .~ Just (fromIntegral (pp ^. ppMaxBHSizeL))
    & #maybe'keyDeposit .~ Just (coinW (pp ^. ppKeyDepositL))
    & #maybe'poolDeposit .~ Just (coinW (pp ^. ppPoolDepositL))
    & #maybe'maxEpoch .~ Just (fromIntegral (unEpochInterval (pp ^. ppEMaxL)))
    & #maybe'nOpt .~ Just (fromIntegral (pp ^. ppNOptL))
    & #a0 .~ rationalProto (unboundRational (pp ^. ppA0L))
    & #rho .~ rationalProto (unboundRational (pp ^. ppRhoL))
    & #tau .~ rationalProto (unboundRational (pp ^. ppTauL))
    & #protocolVersion .~ protVerOf (pp ^. ppProtocolVersionL)
    & #maybe'minPoolCost .~ Just (coinW (pp ^. ppMinPoolCostL))

-- | Alonzo and Babbage add the Plutus parameters.
ppAlonzo :: AlonzoEraPParams era => PParams era -> S.ProtocolParameters
ppAlonzo pp =
  ppBase pp
    & #costModelPlutusV1 .~ models PlutusV1
    & #costModelPlutusV2 .~ models PlutusV2
    & #priceMem .~ rationalProto (unboundRational (prMem prices))
    & #priceSteps .~ rationalProto (unboundRational (prSteps prices))
    & #maybe'maxTxExMem .~ Just (fromIntegral (exUnitsMem (pp ^. ppMaxTxExUnitsL)))
    & #maybe'maxTxExSteps .~ Just (fromIntegral (exUnitsSteps (pp ^. ppMaxTxExUnitsL)))
    & #maybe'maxBlockExMem .~ Just (fromIntegral (exUnitsMem (pp ^. ppMaxBlockExUnitsL)))
    & #maybe'maxBlockExSteps .~ Just (fromIntegral (exUnitsSteps (pp ^. ppMaxBlockExUnitsL)))
    & #maybe'maxValueSize .~ Just (fromIntegral (pp ^. ppMaxValSizeL))
    & #maybe'collateralPercentage .~ Just (fromIntegral (pp ^. ppCollateralPercentageL))
    & #maybe'maxCollateralInputs .~ Just (fromIntegral (pp ^. ppMaxCollateralInputsL))
  where
    prices = pp ^. ppPricesL
    models l = maybe [] getCostModelParams (Map.lookup l (costModelsValid (pp ^. ppCostModelsL)))

-- | Every parameter of a Conway PParams, as the update view reads an update.
ppProto :: ConwayEraPParams era => PParams era -> S.ProtocolParameters
ppProto pp =
  defMessage
    & #maybe'minFeeA .~ Just (coinW (pp ^. ppMinFeeAL))
    & #maybe'minFeeB .~ Just (coinW (pp ^. ppMinFeeBL))
    & #maybe'maxBlockBodySize .~ Just (fromIntegral (pp ^. ppMaxBBSizeL))
    & #maybe'maxTxSize .~ Just (fromIntegral (pp ^. ppMaxTxSizeL))
    & #maybe'maxBlockHeaderSize .~ Just (fromIntegral (pp ^. ppMaxBHSizeL))
    & #maybe'keyDeposit .~ Just (coinW (pp ^. ppKeyDepositL))
    & #maybe'poolDeposit .~ Just (coinW (pp ^. ppPoolDepositL))
    & #maybe'maxEpoch .~ Just (fromIntegral (unEpochInterval (pp ^. ppEMaxL)))
    & #maybe'nOpt .~ Just (fromIntegral (pp ^. ppNOptL))
    & #a0 .~ rationalProto (unboundRational (pp ^. ppA0L))
    & #rho .~ rationalProto (unboundRational (pp ^. ppRhoL))
    & #tau .~ rationalProto (unboundRational (pp ^. ppTauL))
    & #protocolVersion .~ protVerOf (pp ^. ppProtocolVersionL)
    & #maybe'minPoolCost .~ Just (coinW (pp ^. ppMinPoolCostL))
    & #maybe'coinsPerUtxoByte .~ Just (coinW (fromCompact (unCoinPerByte (pp ^. ppCoinsPerUTxOByteL))))
    & #costModelPlutusV1 .~ models PlutusV1
    & #costModelPlutusV2 .~ models PlutusV2
    & #costModelPlutusV3 .~ models PlutusV3
    & #costModelPlutusV4 .~ models PlutusV4
    & #priceMem .~ rationalProto (unboundRational (prMem prices))
    & #priceSteps .~ rationalProto (unboundRational (prSteps prices))
    & #maybe'maxTxExMem .~ Just (fromIntegral (exUnitsMem (pp ^. ppMaxTxExUnitsL)))
    & #maybe'maxTxExSteps .~ Just (fromIntegral (exUnitsSteps (pp ^. ppMaxTxExUnitsL)))
    & #maybe'maxBlockExMem .~ Just (fromIntegral (exUnitsMem (pp ^. ppMaxBlockExUnitsL)))
    & #maybe'maxBlockExSteps .~ Just (fromIntegral (exUnitsSteps (pp ^. ppMaxBlockExUnitsL)))
    & #maybe'maxValueSize .~ Just (fromIntegral (pp ^. ppMaxValSizeL))
    & #maybe'collateralPercentage .~ Just (fromIntegral (pp ^. ppCollateralPercentageL))
    & #maybe'maxCollateralInputs .~ Just (fromIntegral (pp ^. ppMaxCollateralInputsL))
    & #poolVotingThresholds .~ poolT (pp ^. ppPoolVotingThresholdsL)
    & #drepVotingThresholds .~ drepT (pp ^. ppDRepVotingThresholdsL)
    & #maybe'committeeMinSize .~ Just (fromIntegral (pp ^. ppCommitteeMinSizeL))
    & #maybe'committeeMaxTermLength .~ Just (fromIntegral (unEpochInterval (pp ^. ppCommitteeMaxTermLengthL)))
    & #maybe'govActionLifetime .~ Just (fromIntegral (unEpochInterval (pp ^. ppGovActionLifetimeL)))
    & #maybe'govActionDeposit .~ Just (coinW (pp ^. ppGovActionDepositL))
    & #maybe'drepDeposit .~ Just (coinW (pp ^. ppDRepDepositL))
    & #maybe'drepActivity .~ Just (fromIntegral (unEpochInterval (pp ^. ppDRepActivityL)))
    & #minFeeRefScriptCostPerByte .~ rationalProto (unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL))
  where
    prices = pp ^. ppPricesL
    models l = maybe [] getCostModelParams (Map.lookup l (costModelsValid (pp ^. ppCostModelsL)))
    r = rationalProto . unboundRational
    poolT t =
      defMessage
        & #motionNoConfidence .~ r (pvtMotionNoConfidence t)
        & #committeeNormal .~ r (pvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (pvtCommitteeNoConfidence t)
        & #hardForkInitiation .~ r (pvtHardForkInitiation t)
        & #ppSecurityGroup .~ r (pvtPPSecurityGroup t)
    drepT t =
      defMessage
        & #motionNoConfidence .~ r (dvtMotionNoConfidence t)
        & #committeeNormal .~ r (dvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (dvtCommitteeNoConfidence t)
        & #updateToConstitution .~ r (dvtUpdateToConstitution t)
        & #hardForkInitiation .~ r (dvtHardForkInitiation t)
        & #ppNetworkGroup .~ r (dvtPPNetworkGroup t)
        & #ppEconomicGroup .~ r (dvtPPEconomicGroup t)
        & #ppTechnicalGroup .~ r (dvtPPTechnicalGroup t)
        & #ppGovGroup .~ r (dvtPPGovGroup t)
        & #treasuryWithdrawal .~ r (dvtTreasuryWithdrawal t)

-- * codec_pparams_update

mb :: StrictMaybe a -> Maybe a
mb = strictMaybeToMaybe

coinW :: Coin -> Word64
coinW = fromIntegral . unCoin

-- | The parameters every era's update can set.
baseUpdate :: EraPParams era => PParamsUpdate era -> S.ProtocolParameters
baseUpdate u =
  defMessage
    & #maybe'minFeeA .~ (coinW <$> mb (u ^. ppuMinFeeAL))
    & #maybe'minFeeB .~ (coinW <$> mb (u ^. ppuMinFeeBL))
    & #maybe'maxBlockBodySize .~ (fromIntegral <$> mb (u ^. ppuMaxBBSizeL))
    & #maybe'maxTxSize .~ (fromIntegral <$> mb (u ^. ppuMaxTxSizeL))
    & #maybe'maxBlockHeaderSize .~ (fromIntegral <$> mb (u ^. ppuMaxBHSizeL))
    & #maybe'keyDeposit .~ (coinW <$> mb (u ^. ppuKeyDepositL))
    & #maybe'poolDeposit .~ (coinW <$> mb (u ^. ppuPoolDepositL))
    & #maybe'maxEpoch .~ (fromIntegral . unEpochInterval <$> mb (u ^. ppuEMaxL))
    & #maybe'nOpt .~ (fromIntegral <$> mb (u ^. ppuNOptL))
    & #maybe'a0 .~ (rationalProto . unboundRational <$> mb (u ^. ppuA0L))
    & #maybe'rho .~ (rationalProto . unboundRational <$> mb (u ^. ppuRhoL))
    & #maybe'tau .~ (rationalProto . unboundRational <$> mb (u ^. ppuTauL))
    & #maybe'minPoolCost .~ (coinW <$> mb (u ^. ppuMinPoolCostL))

protVerOf :: ProtVer -> P.ProtocolVersion
protVerOf = protVerProto

-- | Shelley through Alonzo: decentralisation and extra entropy.
shelleyUpdate :: (EraPParams era, AtMostEra "Alonzo" era, AtMostEra "Babbage" era) => PParamsUpdate era -> S.ProtocolParameters -> S.ProtocolParameters
shelleyUpdate u p =
  p
    & #maybe'd .~ (rationalProto . unboundRational <$> mb (u ^. ppuDL))
    & #maybe'extraEntropy .~ (nonceProto <$> mb (u ^. ppuExtraEntropyL))
    & #maybe'protocolVersion .~ (protVerOf <$> mb (u ^. ppuProtocolVersionL))
  where
    nonceProto n = defMessage & #hash .~ (case n of NeutralNonce -> BS.empty; Nonce h -> hashToBytes h)

alonzoUpdate :: AlonzoEraPParams era => PParamsUpdate era -> S.ProtocolParameters -> S.ProtocolParameters
alonzoUpdate u p =
  p
    & #costModelPlutusV1 .~ models PlutusV1
    & #costModelPlutusV2 .~ models PlutusV2
    & #costModelPlutusV3 .~ models PlutusV3
    & #costModelPlutusV4 .~ models PlutusV4
    & #maybe'priceMem .~ (rationalProto . unboundRational . prMem <$> prices)
    & #maybe'priceSteps .~ (rationalProto . unboundRational . prSteps <$> prices)
    & #maybe'maxTxExMem .~ (fromIntegral . exUnitsMem <$> mb (u ^. ppuMaxTxExUnitsL))
    & #maybe'maxTxExSteps .~ (fromIntegral . exUnitsSteps <$> mb (u ^. ppuMaxTxExUnitsL))
    & #maybe'maxBlockExMem .~ (fromIntegral . exUnitsMem <$> mb (u ^. ppuMaxBlockExUnitsL))
    & #maybe'maxBlockExSteps .~ (fromIntegral . exUnitsSteps <$> mb (u ^. ppuMaxBlockExUnitsL))
    & #maybe'maxValueSize .~ (fromIntegral <$> mb (u ^. ppuMaxValSizeL))
    & #maybe'collateralPercentage .~ (fromIntegral <$> mb (u ^. ppuCollateralPercentageL))
    & #maybe'maxCollateralInputs .~ (fromIntegral <$> mb (u ^. ppuMaxCollateralInputsL))
  where
    prices = mb (u ^. ppuPricesL)
    models l = maybe [] (maybe [] getCostModelParams . Map.lookup l . costModelsValid) (mb (u ^. ppuCostModelsL))

conwayUpdate :: ConwayEraPParams era => PParamsUpdate era -> S.ProtocolParameters -> S.ProtocolParameters
conwayUpdate u p =
  p
    & #maybe'poolVotingThresholds .~ (pool <$> mb (u ^. ppuPoolVotingThresholdsL))
    & #maybe'drepVotingThresholds .~ (drep <$> mb (u ^. ppuDRepVotingThresholdsL))
    & #maybe'committeeMinSize .~ (fromIntegral <$> mb (u ^. ppuCommitteeMinSizeL))
    & #maybe'committeeMaxTermLength .~ (fromIntegral . unEpochInterval <$> mb (u ^. ppuCommitteeMaxTermLengthL))
    & #maybe'govActionLifetime .~ (fromIntegral . unEpochInterval <$> mb (u ^. ppuGovActionLifetimeL))
    & #maybe'govActionDeposit .~ (coinW <$> mb (u ^. ppuGovActionDepositL))
    & #maybe'drepDeposit .~ (coinW <$> mb (u ^. ppuDRepDepositL))
    & #maybe'drepActivity .~ (fromIntegral . unEpochInterval <$> mb (u ^. ppuDRepActivityL))
    & #maybe'minFeeRefScriptCostPerByte .~ (rationalProto . unboundRational <$> mb (u ^. ppuMinFeeRefScriptCostPerByteL))
  where
    r = rationalProto . unboundRational
    pool t =
      defMessage
        & #motionNoConfidence .~ r (pvtMotionNoConfidence t)
        & #committeeNormal .~ r (pvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (pvtCommitteeNoConfidence t)
        & #hardForkInitiation .~ r (pvtHardForkInitiation t)
        & #ppSecurityGroup .~ r (pvtPPSecurityGroup t)
    drep t =
      defMessage
        & #motionNoConfidence .~ r (dvtMotionNoConfidence t)
        & #committeeNormal .~ r (dvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (dvtCommitteeNoConfidence t)
        & #updateToConstitution .~ r (dvtUpdateToConstitution t)
        & #hardForkInitiation .~ r (dvtHardForkInitiation t)
        & #ppNetworkGroup .~ r (dvtPPNetworkGroup t)
        & #ppEconomicGroup .~ r (dvtPPEconomicGroup t)
        & #ppTechnicalGroup .~ r (dvtPPTechnicalGroup t)
        & #ppGovGroup .~ r (dvtPPGovGroup t)
        & #treasuryWithdrawal .~ r (dvtTreasuryWithdrawal t)

data PPUEra = forall era. EraPParams era => PPUEra (Proxy era) (PParamsUpdate era -> S.ProtocolParameters)

ppuEra :: P.Era -> Result PPUEra
ppuEra = \case
  P.ERA_SHELLEY -> pure (PPUEra (Proxy @ShelleyEra) (\u -> shelleyUpdate u (baseUpdate u) & #maybe'minUtxoValue .~ (coinW <$> mb (u ^. ppuMinUTxOValueL))))
  P.ERA_ALLEGRA -> pure (PPUEra (Proxy @AllegraEra) (\u -> shelleyUpdate u (baseUpdate u) & #maybe'minUtxoValue .~ (coinW <$> mb (u ^. ppuMinUTxOValueL))))
  P.ERA_MARY -> pure (PPUEra (Proxy @MaryEra) (\u -> shelleyUpdate u (baseUpdate u) & #maybe'minUtxoValue .~ (coinW <$> mb (u ^. ppuMinUTxOValueL))))
  P.ERA_ALONZO ->
    pure
      ( PPUEra (Proxy @AlonzoEra) $ \u ->
          alonzoUpdate u (shelleyUpdate u (baseUpdate u))
            & #maybe'coinsPerUtxoWord .~ (coinW . unCoinPerWord <$> mb (u ^. ppuCoinsPerUTxOWordL))
      )
  P.ERA_BABBAGE -> pure (PPUEra (Proxy @BabbageEra) (\u -> babbageUpdate u (alonzoUpdate u (baseUpdate u)) & #maybe'protocolVersion .~ (protVerOf <$> mb (u ^. ppuProtocolVersionL))))
  P.ERA_CONWAY -> pure (PPUEra (Proxy @ConwayEra) (\u -> conwayUpdate u (babbageUpdate u (alonzoUpdate u (baseUpdate u)))))
  P.ERA_DIJKSTRA -> pure (PPUEra (Proxy @DijkstraEra) (\u -> conwayUpdate u (babbageUpdate u (alonzoUpdate u (baseUpdate u)))))
  e -> unsupported ("no codec for " <> T.pack (show e))
  where
    babbageUpdate :: (BabbageEraPParams era) => PParamsUpdate era -> S.ProtocolParameters -> S.ProtocolParameters
    babbageUpdate u p =
      p
        & #maybe'coinsPerUtxoByte .~ (coinW . fromCompact . unCoinPerByte <$> mb (u ^. ppuCoinsPerUTxOByteL))

codecPParamsUpdate :: CodecPParamsUpdateRequest -> Result CodecPParamsUpdateResponse
codecPParamsUpdate req = do
  PPUEra (_ :: Proxy era) view <- ppuEra (req ^. #era)
  v <- versionArg @era (req ^. #maybe'protocolVersion)
  pure $
    case decodeFull v (BSL.fromStrict (req ^. #cbor)) :: Either DecoderError (PParamsUpdate era) of
      Left e -> defMessage & #rejection .~ decodeFailure e
      Right u -> defMessage & #cbor .~ serialize' v u & #update .~ view u

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'CodecTx q -> Just (respond #codecTx (codecTx q))
  Call'CodecTxOutput q -> Just (respond #codecTxOutput (codecTxOutput q))
  Call'CodecCertificate q -> Just (respond #codecCertificate (codecCertificate q))
  Call'CodecGovernance q -> Just (respond #codecGovernance (codecGovernance q))
  Call'CodecPlutusData q -> Just (respond #codecPlutusData (codecPlutusData q))
  Call'CodecBlockHeader q -> Just (respond #codecBlockHeader (codecBlockHeader q))
  Call'CodecBlockBody q -> Just (respond #codecBlockBody (codecBlockBody q))
  Call'CodecByronBlock q -> Just (respond #codecByronBlock (codecByronBlock q))
  Call'CodecHfcBlock q -> Just (respond #codecHfcBlock (codecHfcBlock q))
  Call'CodecLedgerState q -> Just (respond #codecLedgerState (codecLedgerState q))
  Call'CodecPparamsUpdate q -> Just (respond #codecPparamsUpdate (codecPParamsUpdate q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "codec_tx"
  , "codec_tx_output"
  , "codec_certificate"
  , "codec_governance"
  , "codec_plutus_data"
  , "codec_block_header"
  , "codec_block_body"
  , "codec_byron_block"
  , "codec_hfc_block"
  , "codec_ledger_state"
  , "codec_pparams_update"
  ]
