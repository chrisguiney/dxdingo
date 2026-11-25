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
  , codecPParamsUpdate
  ) where

import qualified Cardano.Chain.Block as Byron
import Cardano.Chain.Slotting (EpochAndSlotCount (..), EpochNumber (..), EpochSlots (..), SlotCount (..), fromSlotNumber)
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hashing as ByronHash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, CoinPerWord (..), ppuCoinsPerUTxOWordL, ppuCollateralPercentageL, ppuCostModelsL, ppuMaxBlockExUnitsL, ppuMaxCollateralInputsL, ppuMaxTxExUnitsL, ppuMaxValSizeL, ppuPricesL)
import Cardano.Ledger.Babbage.PParams (BabbageEraPParams, ppuCoinsPerUTxOByteL)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, DRepVotingThresholds (..), PoolVotingThresholds (..), ppuCommitteeMaxTermLengthL, ppuCommitteeMinSizeL, ppuDRepActivityL, ppuDRepDepositL, ppuDRepVotingThresholdsL, ppuGovActionDepositL, ppuGovActionLifetimeL, ppuMinFeeRefScriptCostPerByteL, ppuPoolVotingThresholdsL)
import Cardano.Ledger.Plutus.CostModels (costModelsValid, getCostModelParams)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational), EpochInterval (..), Nonce (..), StrictMaybe, strictMaybeToMaybe)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Binary (Annotator, EncCBOR, EncCBORGroup, byronProtVer, decodeFullAnnotatedBytes, getVersion)
import Data.Typeable (Typeable)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as TPraos
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCert (..))
import Cardano.Slotting.Slot (SlotNo (..))
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
import Cardano.Ledger.Conway.Governance
  ( GovAction (..)
  , ProposalProcedure (..)
  , VotingProcedures
  )
import Cardano.Ledger.Conway.TxCert
  ( ConwayEraTxCert
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
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
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
  e -> unsupported ("no codec for " <> T.pack (show e))
  where
    phase2 :: AlonzoEraTx era => Tx TopTx era -> Bool
    phase2 tx = tx ^. isPhase2ValidTxL == Phase2Valid

codecTx :: CodecTxRequest -> Result CodecTxResponse
codecTx req = do
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
codecTxOutput req = do
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

data CertEra = forall era. EraTxCert era => CertEra (Proxy era) (TxCert era -> Text)

certEra :: P.Era -> Result CertEra
certEra = \case
  P.ERA_SHELLEY -> pure (CertEra (Proxy @ShelleyEra) pre)
  P.ERA_ALLEGRA -> pure (CertEra (Proxy @AllegraEra) pre)
  P.ERA_MARY -> pure (CertEra (Proxy @MaryEra) pre)
  P.ERA_ALONZO -> pure (CertEra (Proxy @AlonzoEra) pre)
  P.ERA_BABBAGE -> pure (CertEra (Proxy @BabbageEra) pre)
  P.ERA_CONWAY -> pure (CertEra (Proxy @ConwayEra) conwayKind)
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

codecGovernance :: CodecGovernanceRequest -> Result CodecGovernanceResponse
codecGovernance req = do
  if req ^. #era /= P.ERA_CONWAY
    then unsupported "governance items exist from Conway"
    else pure ()
  v <- versionArg @ConwayEra (req ^. #maybe'protocolVersion)
  let bs = BSL.fromStrict (req ^. #cbor)
  case req ^. #kind of
    GOVERNANCE_ITEM_KIND_PROPOSAL ->
      pure $ case decodeFull v bs :: Either DecoderError (ProposalProcedure ConwayEra) of
        Left e -> defMessage & #rejection .~ decodeFailure e
        Right p -> defMessage & #cbor .~ serialize' v p & #actionType .~ actionType (pProcGovAction p)
    GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES ->
      pure $ case decodeFull v bs :: Either DecoderError (VotingProcedures ConwayEra) of
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
  , "codec_pparams_update"
  ]
