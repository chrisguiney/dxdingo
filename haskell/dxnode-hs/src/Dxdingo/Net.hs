{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Mini-protocol codecs, the mux SDU header and protocol agency, answered
-- by the ouroboros-network protocol codecs and the ouroboros-consensus
-- serialisation of the Cardano block. A request names the protocol state the
-- receiver is in and carries one wire message; the codec decodes per state
-- token, so a message that is legal elsewhere fails to decode.
module Dxdingo.Net
  ( dispatch
  , implemented
    -- * The Cardano wiring, for the direct library checks in the tests
  , Blk
  , codecConfig
  , Fam (..)
  , family
  , bfCodec
  , csN2NCodec
  , ltmCodec
  , decodeQueryBytes
  ) where

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Network.NodeToClient.Version
  ( NodeToClientVersion (..)
  , nodeToClientVersionDataCodec
  )
import Cardano.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , nodeToNodeVersionDataCodec
  )
import qualified Cardano.Network.NodeToClient.Version as N2CV
import qualified Cardano.Network.NodeToNode.Version as N2NV
import Cardano.Network.Protocol.Handshake.Codec
  ( nodeToClientHandshakeCodec
  , nodeToNodeHandshakeCodec
  )
import qualified Cardano.Network.Protocol.LocalStateQuery.Codec as LSQCodec
import qualified Cardano.Network.Protocol.LocalTxMonitor.Codec as LTMCodec
import Cardano.Protocol.Crypto (StandardCrypto)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.ProtoLens (Message, defMessage)
import Data.ProtoLens.Field (HasField)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Network.Mux.Codec (decodeSDU)
import Network.Mux.Types
  ( MiniProtocolDir (..)
  , MiniProtocolNum (..)
  , RemoteClockModel (..)
  , SDUHeader (..)
  , msHeader
  )
import Network.Socket (SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import Network.TypedProtocol.Codec (Codec, CodecF (..), DecodeStep (..), SomeMessage (..))
import qualified Network.TypedProtocol.Core as TP
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import Ouroboros.Consensus.Block (ConvertRawHash (..), Header, Point)
import Ouroboros.Consensus.Block (decodeRawHash, encodeRawHash)
import Ouroboros.Consensus.Byron.Ledger (CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CodecConfig (CardanoCodecConfig))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Query
  ( Query
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( BlockNodeToClientVersion
  , BlockNodeToNodeVersion
  , SupportedNetworkProtocolVersion (..)
  )
import Ouroboros.Consensus.Node.Serialisation
  ( SerialiseNodeToClient (..)
  , SerialiseNodeToNode (..)
  , SerialiseResult (..)
  )
import Ouroboros.Consensus.Shelley.Ledger (CodecConfig (ShelleyCodecConfig))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Util (SomeSecond (..))
import Ouroboros.Network.Block
  ( BlockNo (..)
  , SlotNo (..)
  , Tip (..)
  , decodePoint
  , decodeTip
  , encodePoint
  , encodeTip
  , pattern BlockPoint
  , pattern GenesisPoint
  )
import Ouroboros.Network.CodecCBORTerm (decodeData)
import Ouroboros.Network.DiffusionMode (DiffusionMode (..))
import Ouroboros.Network.Magic (unNetworkMagic)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import qualified Ouroboros.Network.Protocol.BlockFetch.Codec as BFCodec
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BF
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as CSCodec
import qualified Ouroboros.Network.Protocol.ChainSync.Type as CS
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Ouroboros.Network.Protocol.KeepAlive.Codec as KACodec
import qualified Ouroboros.Network.Protocol.KeepAlive.Type as KA
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Codec as LSQBase
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Codec as LTSCodec
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import qualified Ouroboros.Network.Protocol.PeerSharing.Codec as PSCodec
import qualified Ouroboros.Network.Protocol.PeerSharing.Type as PS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Codec as TXCodec
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TX
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Net
import qualified Proto.Dxdingo.V1.Types as P

------------------------------------------------------------------------------
-- The Cardano block and the negotiated version domain

type Blk = CardanoBlock StandardCrypto

codecConfig :: CodecConfig Blk
codecConfig =
  CardanoCodecConfig
    (ByronCodecConfig mainnetEpochSlots)
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig

-- | node-to-node 14 to 16 and node-to-client 16 to 23, the latter in wire
-- form with the high bit set. Anything else is UNSUPPORTED.
data Fam
  = N2N NodeToNodeVersion (BlockNodeToNodeVersion Blk)
  | N2C NodeToClientVersion (BlockNodeToClientVersion Blk)

ntnVersions :: [(Word32, NodeToNodeVersion)]
ntnVersions = [(14, NodeToNodeV_14), (15, NodeToNodeV_15), (16, NodeToNodeV_16)]

ntcVersions :: [(Word32, NodeToClientVersion)]
ntcVersions =
  zip
    [16 ..]
    [ NodeToClientV_16
    , NodeToClientV_17
    , NodeToClientV_18
    , NodeToClientV_19
    , NodeToClientV_20
    , NodeToClientV_21
    , NodeToClientV_22
    , NodeToClientV_23
    ]

family :: Bool -> Bool -> NetCodecRequest -> Result Fam
family allowN2N allowN2C c
  | v >= 0x8000 =
      case (allowN2C, lookup (v - 0x8000) ntcVersions) of
        (True, Just nv)
          | Just bv <- Map.lookup nv (supportedNodeToClientVersions proxyBlk) -> pure (N2C nv bv)
        _ -> unsupported ("node-to-client version " <> tshow v)
  | otherwise =
      case (allowN2N, lookup v ntnVersions) of
        (True, Just nv)
          | Just bv <- Map.lookup nv (supportedNodeToNodeVersions proxyBlk) -> pure (N2N nv bv)
        _ -> unsupported ("node-to-node version " <> tshow v)
  where
    v = c ^. #version

proxyBlk :: Proxy Blk
proxyBlk = Proxy

tshow :: Show a => a -> Text
tshow = T.pack . show

codecArg :: Maybe NetCodecRequest -> Result NetCodecRequest
codecArg = maybe (badRequest "codec request missing") pure

------------------------------------------------------------------------------
-- Running a codec

-- | A protocol state, with or without agency.
data Tok ps
  = forall (st :: ps). (TP.StateTokenI st, TP.ActiveState st) => Tok (TP.StateToken st)
  | Terminal

rejection :: Text -> Text -> P.Rejection
rejection name detail =
  defMessage
    & #failures
      .~ [ defMessage
            & #rule .~ "net"
            & #name .~ name
            & #detail .~ detail
         ]

deserialiseFailure :: Text -> P.Rejection
deserialiseFailure = rejection "DeserialiseFailure"

-- | Feed the whole message and then end of input. Trailing bytes are a
-- failure, which 'runDecoder' would drop.
drive :: Monad m => BL.ByteString -> DecodeStep BL.ByteString DeserialiseFailure m a -> m (Either Text a)
drive input = go (Just input)
  where
    go _ (DecodeFail e) = pure (Left (tshow e))
    go _ (DecodeDone a rest)
      | maybe True BL.null rest = pure (Right a)
      | otherwise = pure (Left "trailing bytes")
    go i (DecodePartial k) = k i >>= go Nothing

-- | Decode one message in a state and give the continuation the message and
-- its re-encoding by the same codec.
runCodec ::
  forall ps r.
  (forall s. Codec ps DeserialiseFailure (ST s) BL.ByteString) ->
  Tok ps ->
  BS.ByteString ->
  (forall st st'. Message' ps st st' -> BL.ByteString -> r) ->
  Either P.Rejection r
runCodec _ Terminal _ _ = Left (deserialiseFailure "state has no agency")
runCodec codec (Tok stok) bytes k = runST $ do
  step <- decode codec stok
  res <- drive (BL.fromStrict bytes) step
  pure $ case res of
    Left e -> Left (deserialiseFailure e)
    Right (SomeMessage m) -> Right (k m (encode codec m))

-- | Alias keeping 'runCodec' signatures short.
type Message' ps st st' = TP.Message ps st st'

stateTok :: Text -> [(Text, Tok ps)] -> Result (Tok ps)
stateTok name tbl = maybe (badRequest ("unknown state " <> name)) pure (lookup name tbl)

-- | Every response carries its rejection in field 1; a rejection leaves the
-- rest unset.
finish :: (Message resp, HasField resp "rejection" P.Rejection) => Either P.Rejection resp -> resp
finish = either (\rej -> defMessage & #rejection .~ rej) id

serialise :: (a -> Encoding) -> a -> BS.ByteString
serialise enc = BL.toStrict . CBOR.toLazyByteString . enc

------------------------------------------------------------------------------
-- Wire parts shared by the protocols

pointFields :: Point Blk -> (Maybe Word64, BS.ByteString)
pointFields = \case
  GenesisPoint -> (Nothing, BS.empty)
  BlockPoint (SlotNo s) h -> (Just s, toRawHash proxyBlk h)

tipFields :: Tip Blk -> (Maybe Word64, Maybe Word64)
tipFields = \case
  TipGenesis -> (Nothing, Nothing)
  Tip (SlotNo s) _ (BlockNo b) -> (Just s, Just b)

n2nEnc :: SerialiseNodeToNode Blk a => BlockNodeToNodeVersion Blk -> a -> Encoding
n2nEnc = encodeNodeToNode codecConfig

n2nDec :: SerialiseNodeToNode Blk a => BlockNodeToNodeVersion Blk -> forall s. Decoder s a
n2nDec = decodeNodeToNode codecConfig

n2cEnc :: SerialiseNodeToClient Blk a => BlockNodeToClientVersion Blk -> a -> Encoding
n2cEnc = encodeNodeToClient codecConfig

n2cDec :: SerialiseNodeToClient Blk a => BlockNodeToClientVersion Blk -> forall s. Decoder s a
n2cDec = decodeNodeToClient codecConfig

------------------------------------------------------------------------------
-- ChainSync

type CSN2N = CS.ChainSync (Header Blk) (Point Blk) (Tip Blk)

type CSN2C = CS.ChainSync Blk (Point Blk) (Tip Blk)

csN2NCodec :: MonadST m => BlockNodeToNodeVersion Blk -> Codec CSN2N DeserialiseFailure m BL.ByteString
csN2NCodec bv =
  CSCodec.codecChainSync (n2nEnc bv) (n2nDec bv) (n2nEnc bv) (n2nDec bv) (n2nEnc bv) (n2nDec bv)

csN2CCodec :: MonadST m => BlockNodeToClientVersion Blk -> Codec CSN2C DeserialiseFailure m BL.ByteString
csN2CCodec bv =
  CSCodec.codecChainSync
    (n2cEnc bv)
    (n2cDec bv)
    (encodePoint (encodeRawHash proxyBlk))
    (decodePoint (decodeRawHash proxyBlk))
    (encodeTip (encodeRawHash proxyBlk))
    (decodeTip (decodeRawHash proxyBlk))

csStates :: [(Text, Tok (CS.ChainSync h p t))]
csStates =
  [ ("StIdle", Tok CS.SingIdle)
  , ("StCanAwait", Tok (CS.SingNext CS.SingCanAwait))
  , ("StMustReply", Tok (CS.SingNext CS.SingMustReply))
  , ("StIntersect", Tok CS.SingIntersect)
  , ("StDone", Terminal)
  ]

-- | The constructor name and the fields of a ChainSync message.
csResponse ::
  (h -> BS.ByteString) ->
  CS.Message (CS.ChainSync h (Point Blk) (Tip Blk)) st st' ->
  BL.ByteString ->
  NetCodecChainSyncResponse
csResponse payload msg enc = case msg of
  CS.MsgRequestNext -> base "MsgRequestNext"
  CS.MsgAwaitReply -> base "MsgAwaitReply"
  CS.MsgRollForward h tip -> withTip tip (base "MsgRollForward" & #payload .~ payload h)
  CS.MsgRollBackward p tip -> withTip tip (withPoint p (base "MsgRollBackward"))
  CS.MsgFindIntersect _ -> base "MsgFindIntersect"
  CS.MsgIntersectFound p tip -> withTip tip (withPoint p (base "MsgIntersectFound"))
  CS.MsgIntersectNotFound tip -> withTip tip (base "MsgIntersectNotFound")
  CS.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecChainSyncResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc
    withPoint p r =
      let (s, h) = pointFields p
       in r & #maybe'pointSlot .~ s & #pointHash .~ h
    withTip tip r =
      let (s, b) = tipFields tip
       in r & #maybe'tipSlot .~ s & #maybe'tipBlockNo .~ b

netCodecChainSync :: NetCodecChainSyncRequest -> Result NetCodecChainSyncResponse
netCodecChainSync req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True True c
  let msg = c ^. #message
  case fam of
    N2N _ bv -> do
      tok <- stateTok (c ^. #state) csStates
      pure (finish (runCodec (csN2NCodec bv) tok msg (\m e -> csResponse (serialise (n2nEnc bv)) m e)))
    N2C _ bv -> do
      tok <- stateTok (c ^. #state) csStates
      pure (finish (runCodec (csN2CCodec bv) tok msg (\m e -> csResponse (serialise (n2cEnc bv)) m e)))


------------------------------------------------------------------------------
-- BlockFetch

type BFBlk = BF.BlockFetch Blk (Point Blk)

bfCodec :: MonadST m => BlockNodeToNodeVersion Blk -> Codec BFBlk DeserialiseFailure m BL.ByteString
bfCodec bv = BFCodec.codecBlockFetch (n2nEnc bv) (n2nDec bv) (n2nEnc bv) (n2nDec bv)

bfStates :: [(Text, Tok (BF.BlockFetch b p))]
bfStates =
  [ ("BFIdle", Tok BF.SingBFIdle)
  , ("BFBusy", Tok BF.SingBFBusy)
  , ("BFStreaming", Tok BF.SingBFStreaming)
  , ("BFDone", Terminal)
  ]

netCodecBlockFetch :: NetCodecBlockFetchRequest -> Result NetCodecBlockFetchResponse
netCodecBlockFetch req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True False c
  tok <- stateTok (c ^. #state) bfStates
  case fam of
    N2N _ bv -> pure (finish (runCodec (bfCodec bv) tok (c ^. #message) (bfAnswer (serialise (n2nEnc bv)))))
    N2C {} -> unsupported "block fetch runs node-to-node only"

bfAnswer ::
  (Blk -> BS.ByteString) ->
  TP.Message BFBlk st st' ->
  BL.ByteString ->
  NetCodecBlockFetchResponse
bfAnswer payload msg enc = case msg of
  BF.MsgRequestRange (BF.ChainRange from to) ->
    base "MsgRequestRange" & #maybe'fromSlot .~ fst (pointFields from) & #maybe'toSlot .~ fst (pointFields to)
  BF.MsgClientDone -> base "MsgClientDone"
  BF.MsgStartBatch -> base "MsgStartBatch"
  BF.MsgNoBlocks -> base "MsgNoBlocks"
  BF.MsgBlock b -> base "MsgBlock" & #payload .~ payload b
  BF.MsgBatchDone -> base "MsgBatchDone"
  where
    base :: Text -> NetCodecBlockFetchResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- TxSubmission2

type TXBlk = TX.TxSubmission2 (GenTxId Blk) (GenTx Blk)

txCodec :: MonadST m => BlockNodeToNodeVersion Blk -> Codec TXBlk DeserialiseFailure m BL.ByteString
txCodec bv = TXCodec.codecTxSubmission2 (n2nEnc bv) (n2nDec bv) (n2nEnc bv) (n2nDec bv)

txStates :: [(Text, Tok (TX.TxSubmission2 i t))]
txStates =
  [ ("StInit", Tok TX.SingInit)
  , ("StIdle", Tok TX.SingIdle)
  , ("StTxIdsBlocking", Tok (TX.SingTxIds TX.SingBlocking))
  , ("StTxIdsNonBlocking", Tok (TX.SingTxIds TX.SingNonBlocking))
  , ("StTxs", Tok TX.SingTxs)
  , ("StDone", Terminal)
  ]

netCodecTxSubmission2 :: NetCodecTxSubmission2Request -> Result NetCodecTxSubmission2Response
netCodecTxSubmission2 req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True False c
  tok <- stateTok (c ^. #state) txStates
  case fam of
    N2N _ bv -> pure (finish (runCodec (txCodec bv) tok (c ^. #message) (txAnswer bv)))
    N2C {} -> unsupported "tx submission 2 runs node-to-node only"

txAnswer ::
  BlockNodeToNodeVersion Blk ->
  TP.Message TXBlk st st' ->
  BL.ByteString ->
  NetCodecTxSubmission2Response
txAnswer bv msg enc = case msg of
  TX.MsgInit -> base "MsgInit"
  TX.MsgRequestTxIds blocking ack req ->
    base "MsgRequestTxIds"
      & #maybe'ack .~ Just (fromIntegral (TX.getNumTxIdsToAck ack))
      & #maybe'req .~ Just (fromIntegral (TX.getNumTxIdsToReq req))
      & #maybe'blocking .~ Just (case blocking of TX.SingBlocking -> True; TX.SingNonBlocking -> False)
  TX.MsgReplyTxIds (TX.BlockingReply ids) ->
    base "MsgReplyTxIds" & #txIds .~ map (serialise (n2nEnc bv) . fst) (NE.toList ids)
  TX.MsgReplyTxIds (TX.NonBlockingReply ids) ->
    base "MsgReplyTxIds" & #txIds .~ map (serialise (n2nEnc bv) . fst) ids
  TX.MsgRequestTxs ids -> base "MsgRequestTxs" & #txIds .~ map (serialise (n2nEnc bv)) ids
  TX.MsgReplyTxs txs -> base "MsgReplyTxs" & #txs .~ map (serialise (n2nEnc bv)) txs
  TX.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecTxSubmission2Response
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- LocalStateQuery

type LSQBlk = LSQ.LocalStateQuery Blk (Point Blk) (Query Blk)

lsqCodec ::
  MonadST m =>
  NodeToClientVersion ->
  BlockNodeToClientVersion Blk ->
  Stateful.Codec LSQBlk DeserialiseFailure LSQ.State m BL.ByteString
lsqCodec nv bv =
  LSQCodec.codecLocalStateQuery
    nv
    (encodePoint (encodeRawHash proxyBlk))
    (decodePoint (decodeRawHash proxyBlk))
    (queryEncodeNodeToClient codecConfig qv bv . SomeSecond)
    ((\(SomeSecond q) -> LSQBase.Some q) <$> queryDecodeNodeToClient codecConfig qv bv)
    (encodeResult codecConfig bv)
    (decodeResult codecConfig bv)
  where
    qv = nodeToClientVersionToQueryVersion nv

-- | The query a MsgResult answers, decoded from its own CBOR. Every other
-- state ignores it.
decodeQueryBytes ::
  NodeToClientVersion ->
  BlockNodeToClientVersion Blk ->
  BS.ByteString ->
  Either Text (LSQBase.Some (Query Blk))
decodeQueryBytes nv bv bytes =
  case deserialiseFromBytes
    ((\(SomeSecond q) -> LSQBase.Some q) <$> queryDecodeNodeToClient codecConfig (nodeToClientVersionToQueryVersion nv) bv)
    (BL.fromStrict bytes) of
    Left e -> Left (tshow e)
    Right (rest, x)
      | BL.null rest -> Right x
      | otherwise -> Left "trailing bytes after the query"

-- | The constructor path of a query, from the derived 'Show': peel the era
-- walk of QueryIfCurrent, keep the hard-fork and anytime sub-queries apart.
queryPath :: LSQBase.Some (Query Blk) -> Text
queryPath (LSQBase.Some q) = case words (show (SomeSecond q)) of
  ("Query" : "BlockQuery" : rest) -> blockPath (stripEra (map (filter (`notElem` ("()" :: String))) rest))
  ["Query", name] -> T.pack name
  _ -> "Query"
  where
    stripEra = dropWhile (`elem` ["QueryIfCurrent", "QS", "QZ", ""])
    blockPath = \case
      ("QueryHardFork" : n : _) -> "BlockQuery.QueryHardFork." <> T.pack n
      ("QueryAnytime" : n : _) -> "BlockQuery.QueryAnytime." <> T.pack n
      (n : _) -> "BlockQuery." <> T.pack n
      [] -> "BlockQuery"

-- | A state of the stateful codec with its state value; StQuerying carries
-- the query and is built from the request.
data LTok = forall (st :: LSQBlk). (TP.StateTokenI st, TP.ActiveState st) => LTok (TP.StateToken st) (LSQ.State st)

lsqStates :: [(Text, Maybe LTok)]
lsqStates =
  [ ("StIdle", Just (LTok LSQ.SingIdle LSQ.StateIdle))
  , ("StAcquiring", Just (LTok LSQ.SingAcquiring LSQ.StateAcquiring))
  , ("StAcquired", Just (LTok LSQ.SingAcquired LSQ.StateAcquired))
  , ("StDone", Nothing)
  ]

netCodecLocalStateQuery :: NetCodecLocalStateQueryRequest -> Result NetCodecLocalStateQueryResponse
netCodecLocalStateQuery req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family False True c
  case fam of
    N2N {} -> unsupported "local state query runs node-to-client only"
    N2C nv bv
      | c ^. #state == "StQuerying" ->
          if BS.null (req ^. #query)
            then badRequest "StQuerying needs the query in flight"
            else pure . finish $ case decodeQueryBytes nv bv (req ^. #query) of
              Left e -> Left (deserialiseFailure e)
              Right (LSQBase.Some q) ->
                runStateful (lsqCodec nv bv) LSQ.SingQuerying (LSQ.StateQuerying q) (c ^. #message) (lsqAnswer bv)
      | otherwise -> case lookup (c ^. #state) lsqStates of
          Nothing -> badRequest ("unknown state " <> c ^. #state)
          Just Nothing -> pure (finish (Left (deserialiseFailure "state has no agency")))
          Just (Just (LTok stok st)) ->
            pure (finish (runStateful (lsqCodec nv bv) stok st (c ^. #message) (lsqAnswer bv)))

runStateful ::
  forall ps st f r.
  (TP.StateTokenI st, TP.ActiveState st) =>
  (forall s. Stateful.Codec ps DeserialiseFailure f (ST s) BL.ByteString) ->
  TP.StateToken st ->
  f st ->
  BS.ByteString ->
  (forall st'. f st -> TP.Message ps st st' -> BL.ByteString -> r) ->
  Either P.Rejection r
runStateful codec stok st bytes k = runST $ do
  step <- Stateful.decode codec stok st
  res <- drive (BL.fromStrict bytes) step
  pure $ case res of
    Left e -> Left (deserialiseFailure e)
    Right (SomeMessage m) -> Right (k st m (Stateful.encode codec st m))

lsqAnswer ::
  BlockNodeToClientVersion Blk ->
  LSQ.State st ->
  TP.Message LSQBlk st st' ->
  BL.ByteString ->
  NetCodecLocalStateQueryResponse
lsqAnswer bv st msg enc = case msg of
  LSQ.MsgAcquire _ -> base "MsgAcquire"
  LSQ.MsgAcquired -> base "MsgAcquired"
  LSQ.MsgFailure _ -> base "MsgFailure"
  LSQ.MsgQuery q -> base "MsgQuery" & #query .~ queryPath (LSQBase.Some q)
  LSQ.MsgResult r -> case st of
    LSQ.StateQuerying q -> base "MsgResult" & #result .~ serialise (encodeResult codecConfig bv q) r
  LSQ.MsgRelease -> base "MsgRelease"
  LSQ.MsgReAcquire _ -> base "MsgReAcquire"
  LSQ.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecLocalStateQueryResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- LocalTxSubmission

type LTSBlk = LTS.LocalTxSubmission (GenTx Blk) (ApplyTxErr Blk)

ltsCodec :: MonadST m => BlockNodeToClientVersion Blk -> Codec LTSBlk DeserialiseFailure m BL.ByteString
ltsCodec bv = LTSCodec.codecLocalTxSubmission (n2cEnc bv) (n2cDec bv) (n2cEnc bv) (n2cDec bv)

ltsStates :: [(Text, Tok (LTS.LocalTxSubmission t r))]
ltsStates =
  [ ("StIdle", Tok LTS.SingIdle)
  , ("StBusy", Tok LTS.SingBusy)
  , ("StDone", Terminal)
  ]

netCodecLocalTxSubmission :: NetCodecLocalTxSubmissionRequest -> Result NetCodecLocalTxSubmissionResponse
netCodecLocalTxSubmission req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family False True c
  tok <- stateTok (c ^. #state) ltsStates
  case fam of
    N2C _ bv -> pure (finish (runCodec (ltsCodec bv) tok (c ^. #message) (ltsAnswer bv)))
    N2N {} -> unsupported "local tx submission runs node-to-client only"

ltsAnswer ::
  BlockNodeToClientVersion Blk ->
  TP.Message LTSBlk st st' ->
  BL.ByteString ->
  NetCodecLocalTxSubmissionResponse
ltsAnswer bv msg enc = case msg of
  LTS.MsgSubmitTx tx -> base "MsgSubmitTx" & #tx .~ serialise (n2cEnc bv) tx
  LTS.MsgAcceptTx -> base "MsgAcceptTx"
  LTS.MsgRejectTx _ -> base "MsgRejectTx"
  LTS.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecLocalTxSubmissionResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- LocalTxMonitor

type LTMBlk = LTM.LocalTxMonitor (GenTxId Blk) (GenTx Blk) SlotNo

ltmCodec ::
  MonadST m =>
  NodeToClientVersion ->
  BlockNodeToClientVersion Blk ->
  Codec LTMBlk DeserialiseFailure m BL.ByteString
ltmCodec nv bv =
  LTMCodec.codecLocalTxMonitor nv (n2cEnc bv) (n2cDec bv) (n2cEnc bv) (n2cDec bv) (n2cEnc bv) (n2cDec bv)

ltmStates :: [(Text, Tok (LTM.LocalTxMonitor i t s))]
ltmStates =
  [ ("StIdle", Tok LTM.SingIdle)
  , ("StAcquiring", Tok LTM.SingAcquiring)
  , ("StAcquired", Tok LTM.SingAcquired)
  , ("StBusyNextTx", Tok (LTM.SingBusy LTM.SingNextTx))
  , ("StBusyHasTx", Tok (LTM.SingBusy LTM.SingHasTx))
  , ("StBusyGetSizes", Tok (LTM.SingBusy LTM.SingGetSizes))
  , ("StBusyGetMeasures", Tok (LTM.SingBusy LTM.SingGetMeasures))
  , ("StDone", Terminal)
  ]

netCodecLocalTxMonitor :: NetCodecLocalTxMonitorRequest -> Result NetCodecLocalTxMonitorResponse
netCodecLocalTxMonitor req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family False True c
  tok <- stateTok (c ^. #state) ltmStates
  case fam of
    N2C nv bv -> pure (finish (runCodec (ltmCodec nv bv) tok (c ^. #message) (ltmAnswer bv)))
    N2N {} -> unsupported "local tx monitor runs node-to-client only"

ltmAnswer ::
  BlockNodeToClientVersion Blk ->
  TP.Message LTMBlk st st' ->
  BL.ByteString ->
  NetCodecLocalTxMonitorResponse
ltmAnswer bv msg enc = case msg of
  LTM.MsgDone -> base "MsgDone"
  LTM.MsgAcquire -> base "MsgAcquire"
  LTM.MsgAcquired _ -> base "MsgAcquired"
  LTM.MsgAwaitAcquire -> base "MsgAwaitAcquire"
  LTM.MsgRelease -> base "MsgRelease"
  LTM.MsgNextTx -> base "MsgNextTx"
  LTM.MsgReplyNextTx mtx -> base "MsgReplyNextTx" & #payload .~ maybe BS.empty (serialise (n2cEnc bv)) mtx
  LTM.MsgHasTx txid -> base "MsgHasTx" & #payload .~ serialise (n2cEnc bv) txid
  LTM.MsgReplyHasTx _ -> base "MsgReplyHasTx"
  LTM.MsgGetSizes -> base "MsgGetSizes"
  LTM.MsgReplyGetSizes _ -> base "MsgReplyGetSizes"
  LTM.MsgGetMeasures -> base "MsgGetMeasures"
  LTM.MsgReplyGetMeasures _ -> base "MsgReplyGetMeasures"
  where
    base :: Text -> NetCodecLocalTxMonitorResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- KeepAlive

kaStates :: [(Text, Tok KA.KeepAlive)]
kaStates =
  [ ("StClient", Tok KA.SingClient)
  , ("StServer", Tok KA.SingServer)
  , ("StDone", Terminal)
  ]

netCodecKeepAlive :: NetCodecKeepAliveRequest -> Result NetCodecKeepAliveResponse
netCodecKeepAlive req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True False c
  tok <- stateTok (c ^. #state) kaStates
  case fam of
    N2N {} -> pure (finish (runCodec KACodec.codecKeepAlive_v2 tok (c ^. #message) kaAnswer))
    N2C {} -> unsupported "keep alive runs node-to-node only"

kaAnswer :: TP.Message KA.KeepAlive st st' -> BL.ByteString -> NetCodecKeepAliveResponse
kaAnswer msg enc = case msg of
  KA.MsgKeepAlive cookie -> base "MsgKeepAlive" & #maybe'cookie .~ Just (fromIntegral (KA.unCookie cookie))
  KA.MsgKeepAliveResponse cookie -> base "MsgKeepAliveResponse" & #maybe'cookie .~ Just (fromIntegral (KA.unCookie cookie))
  KA.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecKeepAliveResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- PeerSharing

psCodec :: MonadST m => Codec (PS.PeerSharing SockAddr) DeserialiseFailure m BL.ByteString
psCodec = PSCodec.codecPeerSharing (encodeRemoteAddress ()) (decodeRemoteAddress ())

psStates :: [(Text, Tok (PS.PeerSharing a))]
psStates =
  [ ("StIdle", Tok PS.SingIdle)
  , ("StBusy", Tok PS.SingBusy)
  , ("StDone", Terminal)
  ]

netCodecPeerSharing :: NetCodecPeerSharingRequest -> Result NetCodecPeerSharingResponse
netCodecPeerSharing req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True False c
  tok <- stateTok (c ^. #state) psStates
  case fam of
    N2N {} -> pure (finish (runCodec psCodec tok (c ^. #message) psAnswer))
    N2C {} -> unsupported "peer sharing runs node-to-node only"

-- | Dotted quad, or eight lowercase hex groups in brackets, then the port.
renderAddr :: SockAddr -> Text
renderAddr = \case
  SockAddrInet port host ->
    let (a, b, c, d) = hostAddressToTuple host
     in T.intercalate "." (map tshow [a, b, c, d]) <> ":" <> tshow port
  SockAddrInet6 port _ host _ ->
    let (a, b, c, d, e, f, g, h) = hostAddress6ToTuple host
     in "[" <> T.intercalate ":" (map hex [a, b, c, d, e, f, g, h]) <> "]:" <> tshow port
  other -> tshow other
  where
    hex n = T.pack (showHex' (fromIntegral n :: Int))
    showHex' 0 = "0"
    showHex' n = reverse (go n)
    go 0 = ""
    go n = "0123456789abcdef" !! (n `mod` 16) : go (n `div` 16)

psAnswer :: TP.Message (PS.PeerSharing SockAddr) st st' -> BL.ByteString -> NetCodecPeerSharingResponse
psAnswer msg enc = case msg of
  PS.MsgShareRequest amount -> base "MsgShareRequest" & #maybe'amount .~ Just (fromIntegral (PS.getAmount amount))
  PS.MsgSharePeers peers -> base "MsgSharePeers" & #peers .~ map renderAddr peers
  PS.MsgDone -> base "MsgDone"
  where
    base :: Text -> NetCodecPeerSharingResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc

------------------------------------------------------------------------------
-- Handshake

hsStates :: [(Text, Tok (HS.Handshake v p))]
hsStates =
  [ ("StPropose", Tok HS.SingPropose)
  , ("StConfirm", Tok HS.SingConfirm)
  , ("StDone", Terminal)
  ]

netCodecHandshake :: NetCodecHandshakeRequest -> Result NetCodecHandshakeResponse
netCodecHandshake req = do
  c <- codecArg (req ^. #maybe'codec)
  fam <- family True True c
  case fam of
    N2N _ _ -> do
      tok <- stateTok (c ^. #state) hsStates
      pure (finish (runCodec nodeToNodeHandshakeCodec tok (c ^. #message) hsN2N >>= id))
    N2C _ _ -> do
      tok <- stateTok (c ^. #state) hsStates
      pure (finish (runCodec nodeToClientHandshakeCodec tok (c ^. #message) hsN2C >>= id))

-- | Version data of a recognised version that does not decode is a decode
-- failure.
hsN2N ::
  TP.Message (HS.Handshake NodeToNodeVersion CBOR.Term) st st' ->
  BL.ByteString ->
  Either P.Rejection NetCodecHandshakeResponse
hsN2N msg enc = case msg of
  HS.MsgProposeVersions m -> versions "MsgProposeVersions" (Map.toList m)
  HS.MsgReplyVersions m -> versions "MsgReplyVersions" (Map.toList m)
  HS.MsgQueryReply m -> versions "MsgQueryReply" (Map.toList m)
  HS.MsgAcceptVersion v t -> versions "MsgAcceptVersion" [(v, t)]
  HS.MsgRefuse _ -> Right (base "MsgRefuse")
  where
    base :: Text -> NetCodecHandshakeResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc
    versions name vs = do
      hvs <- traverse one vs
      Right (base name & #versions .~ hvs)
    one (v, t) = case decodeData nodeToNodeVersionDataCodec v t of
      Left e -> Left (deserialiseFailure e)
      Right d ->
        Right $
          defMessage
            & #version .~ maybe 0 fst (L.find ((== v) . snd) ntnVersions)
            & #networkMagic .~ unNetworkMagic (N2NV.networkMagic d)
            & #initiatorOnly .~ (N2NV.diffusionMode d == InitiatorOnlyDiffusionMode)
            & #peerSharing .~ (case N2NV.peerSharing d of PeerSharingEnabled -> 1; PeerSharingDisabled -> 0)
            & #query .~ N2NV.query d

hsN2C ::
  TP.Message (HS.Handshake NodeToClientVersion CBOR.Term) st st' ->
  BL.ByteString ->
  Either P.Rejection NetCodecHandshakeResponse
hsN2C msg enc = case msg of
  HS.MsgProposeVersions m -> versions "MsgProposeVersions" (Map.toList m)
  HS.MsgReplyVersions m -> versions "MsgReplyVersions" (Map.toList m)
  HS.MsgQueryReply m -> versions "MsgQueryReply" (Map.toList m)
  HS.MsgAcceptVersion v t -> versions "MsgAcceptVersion" [(v, t)]
  HS.MsgRefuse _ -> Right (base "MsgRefuse")
  where
    base :: Text -> NetCodecHandshakeResponse
    base name = defMessage & #messageType .~ name & #cbor .~ BL.toStrict enc
    versions name vs = do
      hvs <- traverse one vs
      Right (base name & #versions .~ hvs)
    one (v, t) = case decodeData nodeToClientVersionDataCodec v t of
      Left e -> Left (deserialiseFailure e)
      Right d ->
        Right $
          defMessage
            & #version .~ maybe 0 ((+ 0x8000) . fst) (L.find ((== v) . snd) ntcVersions)
            & #networkMagic .~ unNetworkMagic (N2CV.networkMagic d)
            & #query .~ N2CV.query d

------------------------------------------------------------------------------
-- Mux SDU header

netCodecMuxSdu :: NetCodecMuxSduRequest -> Result NetCodecMuxSduResponse
netCodecMuxSdu req = pure $ case decodeSDU (BL.fromStrict (req ^. #header)) of
  Left e -> defMessage & #rejection .~ deserialiseFailure (tshow e)
  Right sdu ->
    let h = msHeader sdu
        MiniProtocolNum num = mhNum h
     in defMessage
          & #timestamp .~ unRemoteClockModel (mhTimestamp h)
          & #mode .~ (case mhDir h of InitiatorDir -> MUX_MODE_INITIATOR; ResponderDir -> MUX_MODE_RESPONDER)
          & #protocol .~ fromIntegral num
          & #length .~ fromIntegral (mhLength h)

------------------------------------------------------------------------------
-- Agency

-- | The value of a type-level agency, from the protocol's own 'StateAgency'.
class AgencyOf (a :: TP.Agency) where
  agencyOf :: Agency

instance AgencyOf 'TP.ClientAgency where agencyOf = AGENCY_CLIENT

instance AgencyOf 'TP.ServerAgency where agencyOf = AGENCY_SERVER

instance AgencyOf 'TP.NobodyAgency where agencyOf = AGENCY_NOBODY

-- | One transition: from state, message constructor, to state, agency of the
-- state reached. The message argument fixes both states by type.
data Tr = Tr Text Text Text Agency

tr ::
  forall ps st st'.
  AgencyOf (TP.StateAgency st') =>
  Text -> Text -> Text -> TP.Message ps st st' -> Tr
tr from m to _ = Tr from m to (agencyOf @(TP.StateAgency st'))

none :: a
none = error "constructor argument is never evaluated"

type CS' = CS.ChainSync () () ()

csTransitions :: [Tr]
csTransitions =
  [ tr "StIdle" "MsgRequestNext" "StCanAwait" (CS.MsgRequestNext :: TP.Message CS' CS.StIdle (CS.StNext CS.StCanAwait))
  , tr "StIdle" "MsgFindIntersect" "StIntersect" (CS.MsgFindIntersect none :: TP.Message CS' CS.StIdle CS.StIntersect)
  , tr "StIdle" "MsgDone" "StDone" (CS.MsgDone :: TP.Message CS' CS.StIdle CS.StDone)
  , tr "StCanAwait" "MsgAwaitReply" "StMustReply" (CS.MsgAwaitReply :: TP.Message CS' (CS.StNext CS.StCanAwait) (CS.StNext CS.StMustReply))
  , tr "StCanAwait" "MsgRollForward" "StIdle" (CS.MsgRollForward none none :: TP.Message CS' (CS.StNext CS.StCanAwait) CS.StIdle)
  , tr "StCanAwait" "MsgRollBackward" "StIdle" (CS.MsgRollBackward none none :: TP.Message CS' (CS.StNext CS.StCanAwait) CS.StIdle)
  , tr "StMustReply" "MsgRollForward" "StIdle" (CS.MsgRollForward none none :: TP.Message CS' (CS.StNext CS.StMustReply) CS.StIdle)
  , tr "StMustReply" "MsgRollBackward" "StIdle" (CS.MsgRollBackward none none :: TP.Message CS' (CS.StNext CS.StMustReply) CS.StIdle)
  , tr "StIntersect" "MsgIntersectFound" "StIdle" (CS.MsgIntersectFound none none :: TP.Message CS' CS.StIntersect CS.StIdle)
  , tr "StIntersect" "MsgIntersectNotFound" "StIdle" (CS.MsgIntersectNotFound none :: TP.Message CS' CS.StIntersect CS.StIdle)
  ]

type BF' = BF.BlockFetch () ()

bfTransitions :: [Tr]
bfTransitions =
  [ tr "BFIdle" "MsgRequestRange" "BFBusy" (BF.MsgRequestRange none :: TP.Message BF' BF.BFIdle BF.BFBusy)
  , tr "BFIdle" "MsgClientDone" "BFDone" (BF.MsgClientDone :: TP.Message BF' BF.BFIdle BF.BFDone)
  , tr "BFBusy" "MsgStartBatch" "BFStreaming" (BF.MsgStartBatch :: TP.Message BF' BF.BFBusy BF.BFStreaming)
  , tr "BFBusy" "MsgNoBlocks" "BFIdle" (BF.MsgNoBlocks :: TP.Message BF' BF.BFBusy BF.BFIdle)
  , tr "BFStreaming" "MsgBlock" "BFStreaming" (BF.MsgBlock none :: TP.Message BF' BF.BFStreaming BF.BFStreaming)
  , tr "BFStreaming" "MsgBatchDone" "BFIdle" (BF.MsgBatchDone :: TP.Message BF' BF.BFStreaming BF.BFIdle)
  ]

type TX' = TX.TxSubmission2 () ()

txTransitions :: [Tr]
txTransitions =
  [ tr "StInit" "MsgInit" "StIdle" (TX.MsgInit :: TP.Message TX' TX.StInit TX.StIdle)
  , tr "StIdle" "MsgRequestTxIdsBlocking" "StTxIdsBlocking" (TX.MsgRequestTxIds TX.SingBlocking none none :: TP.Message TX' TX.StIdle (TX.StTxIds TX.StBlocking))
  , tr "StIdle" "MsgRequestTxIdsNonBlocking" "StTxIdsNonBlocking" (TX.MsgRequestTxIds TX.SingNonBlocking none none :: TP.Message TX' TX.StIdle (TX.StTxIds TX.StNonBlocking))
  , tr "StIdle" "MsgRequestTxs" "StTxs" (TX.MsgRequestTxs none :: TP.Message TX' TX.StIdle TX.StTxs)
  , tr "StTxIdsBlocking" "MsgReplyTxIds" "StIdle" (TX.MsgReplyTxIds none :: TP.Message TX' (TX.StTxIds TX.StBlocking) TX.StIdle)
  , tr "StTxIdsNonBlocking" "MsgReplyTxIds" "StIdle" (TX.MsgReplyTxIds none :: TP.Message TX' (TX.StTxIds TX.StNonBlocking) TX.StIdle)
  , tr "StTxIdsBlocking" "MsgDone" "StDone" (TX.MsgDone :: TP.Message TX' (TX.StTxIds TX.StBlocking) TX.StDone)
  , tr "StTxs" "MsgReplyTxs" "StIdle" (TX.MsgReplyTxs none :: TP.Message TX' TX.StTxs TX.StIdle)
  ]

data NoQuery r

type LSQ' = LSQ.LocalStateQuery () () NoQuery

lsqTransitions :: [Tr]
lsqTransitions =
  [ tr "StIdle" "MsgAcquire" "StAcquiring" (LSQ.MsgAcquire none :: TP.Message LSQ' LSQ.StIdle LSQ.StAcquiring)
  , tr "StIdle" "MsgDone" "StDone" (LSQ.MsgDone :: TP.Message LSQ' LSQ.StIdle LSQ.StDone)
  , tr "StAcquiring" "MsgAcquired" "StAcquired" (LSQ.MsgAcquired :: TP.Message LSQ' LSQ.StAcquiring LSQ.StAcquired)
  , tr "StAcquiring" "MsgFailure" "StIdle" (LSQ.MsgFailure none :: TP.Message LSQ' LSQ.StAcquiring LSQ.StIdle)
  , tr "StAcquired" "MsgQuery" "StQuerying" (LSQ.MsgQuery (none :: NoQuery ()) :: TP.Message LSQ' LSQ.StAcquired (LSQ.StQuerying ()))
  , tr "StAcquired" "MsgReAcquire" "StAcquiring" (LSQ.MsgReAcquire none :: TP.Message LSQ' LSQ.StAcquired LSQ.StAcquiring)
  , tr "StAcquired" "MsgRelease" "StIdle" (LSQ.MsgRelease :: TP.Message LSQ' LSQ.StAcquired LSQ.StIdle)
  , tr "StQuerying" "MsgResult" "StAcquired" (LSQ.MsgResult none :: TP.Message LSQ' (LSQ.StQuerying ()) LSQ.StAcquired)
  ]

type LTS' = LTS.LocalTxSubmission () ()

ltsTransitions :: [Tr]
ltsTransitions =
  [ tr "StIdle" "MsgSubmitTx" "StBusy" (LTS.MsgSubmitTx none :: TP.Message LTS' LTS.StIdle LTS.StBusy)
  , tr "StBusy" "MsgAcceptTx" "StIdle" (LTS.MsgAcceptTx :: TP.Message LTS' LTS.StBusy LTS.StIdle)
  , tr "StBusy" "MsgRejectTx" "StIdle" (LTS.MsgRejectTx none :: TP.Message LTS' LTS.StBusy LTS.StIdle)
  , tr "StIdle" "MsgDone" "StDone" (LTS.MsgDone :: TP.Message LTS' LTS.StIdle LTS.StDone)
  ]

type LTM' = LTM.LocalTxMonitor () () ()

ltmTransitions :: [Tr]
ltmTransitions =
  [ tr "StIdle" "MsgAcquire" "StAcquiring" (LTM.MsgAcquire :: TP.Message LTM' LTM.StIdle LTM.StAcquiring)
  , tr "StIdle" "MsgDone" "StDone" (LTM.MsgDone :: TP.Message LTM' LTM.StIdle LTM.StDone)
  , tr "StAcquiring" "MsgAcquired" "StAcquired" (LTM.MsgAcquired none :: TP.Message LTM' LTM.StAcquiring LTM.StAcquired)
  , tr "StAcquired" "MsgAwaitAcquire" "StAcquiring" (LTM.MsgAwaitAcquire :: TP.Message LTM' LTM.StAcquired LTM.StAcquiring)
  , tr "StAcquired" "MsgRelease" "StIdle" (LTM.MsgRelease :: TP.Message LTM' LTM.StAcquired LTM.StIdle)
  , tr "StAcquired" "MsgNextTx" "StBusyNextTx" (LTM.MsgNextTx :: TP.Message LTM' LTM.StAcquired (LTM.StBusy LTM.NextTx))
  , tr "StAcquired" "MsgHasTx" "StBusyHasTx" (LTM.MsgHasTx none :: TP.Message LTM' LTM.StAcquired (LTM.StBusy LTM.HasTx))
  , tr "StAcquired" "MsgGetSizes" "StBusyGetSizes" (LTM.MsgGetSizes :: TP.Message LTM' LTM.StAcquired (LTM.StBusy LTM.GetSizes))
  , tr "StAcquired" "MsgGetMeasures" "StBusyGetMeasures" (LTM.MsgGetMeasures :: TP.Message LTM' LTM.StAcquired (LTM.StBusy LTM.GetMeasures))
  , tr "StBusyNextTx" "MsgReplyNextTx" "StAcquired" (LTM.MsgReplyNextTx none :: TP.Message LTM' (LTM.StBusy LTM.NextTx) LTM.StAcquired)
  , tr "StBusyHasTx" "MsgReplyHasTx" "StAcquired" (LTM.MsgReplyHasTx none :: TP.Message LTM' (LTM.StBusy LTM.HasTx) LTM.StAcquired)
  , tr "StBusyGetSizes" "MsgReplyGetSizes" "StAcquired" (LTM.MsgReplyGetSizes none :: TP.Message LTM' (LTM.StBusy LTM.GetSizes) LTM.StAcquired)
  , tr "StBusyGetMeasures" "MsgReplyGetMeasures" "StAcquired" (LTM.MsgReplyGetMeasures none :: TP.Message LTM' (LTM.StBusy LTM.GetMeasures) LTM.StAcquired)
  ]

type HS' = HS.Handshake NodeToNodeVersion CBOR.Term

hsTransitions :: [Tr]
hsTransitions =
  [ tr "StPropose" "MsgProposeVersions" "StConfirm" (HS.MsgProposeVersions none :: TP.Message HS' HS.StPropose HS.StConfirm)
  , tr "StConfirm" "MsgReplyVersions" "StDone" (HS.MsgReplyVersions none :: TP.Message HS' HS.StConfirm HS.StDone)
  , tr "StConfirm" "MsgAcceptVersion" "StDone" (HS.MsgAcceptVersion none none :: TP.Message HS' HS.StConfirm HS.StDone)
  , tr "StConfirm" "MsgRefuse" "StDone" (HS.MsgRefuse none :: TP.Message HS' HS.StConfirm HS.StDone)
  , tr "StConfirm" "MsgQueryReply" "StDone" (HS.MsgQueryReply none :: TP.Message HS' HS.StConfirm HS.StDone)
  ]

kaTransitions :: [Tr]
kaTransitions =
  [ tr "StClient" "MsgKeepAlive" "StServer" (KA.MsgKeepAlive none :: TP.Message KA.KeepAlive KA.StClient KA.StServer)
  , tr "StServer" "MsgKeepAliveResponse" "StClient" (KA.MsgKeepAliveResponse none :: TP.Message KA.KeepAlive KA.StServer KA.StClient)
  , tr "StClient" "MsgDone" "StDone" (KA.MsgDone :: TP.Message KA.KeepAlive KA.StClient KA.StDone)
  ]

type PS' = PS.PeerSharing ()

psTransitions :: [Tr]
psTransitions =
  [ tr "StIdle" "MsgShareRequest" "StBusy" (PS.MsgShareRequest none :: TP.Message PS' PS.StIdle PS.StBusy)
  , tr "StBusy" "MsgSharePeers" "StIdle" (PS.MsgSharePeers none :: TP.Message PS' PS.StBusy PS.StIdle)
  , tr "StIdle" "MsgDone" "StDone" (PS.MsgDone :: TP.Message PS' PS.StIdle PS.StDone)
  ]

transitionsOf :: MiniProtocol -> Maybe ([Text], [Tr])
transitionsOf = \case
  MINI_PROTOCOL_CHAIN_SYNC -> Just (names csStates, csTransitions)
  MINI_PROTOCOL_BLOCK_FETCH -> Just (names bfStates, bfTransitions)
  MINI_PROTOCOL_TX_SUBMISSION2 -> Just (names txStates, txTransitions)
  MINI_PROTOCOL_LOCAL_STATE_QUERY -> Just (map fst lsqStates ++ ["StQuerying"], lsqTransitions)
  MINI_PROTOCOL_LOCAL_TX_SUBMISSION -> Just (names ltsStates, ltsTransitions)
  MINI_PROTOCOL_LOCAL_TX_MONITOR -> Just (names ltmStates, ltmTransitions)
  MINI_PROTOCOL_HANDSHAKE -> Just (names hsStates, hsTransitions)
  MINI_PROTOCOL_KEEP_ALIVE -> Just (names kaStates, kaTransitions)
  MINI_PROTOCOL_PEER_SHARING -> Just (names psStates, psTransitions)
  _ -> Nothing
  where
    names :: [(Text, a)] -> [Text]
    names = map fst

-- | An unknown state is BAD_REQUEST; a state or message with no transition,
-- including an unknown message name, is the rejection IllegalTransition.
netAgency :: NetAgencyRequest -> Result NetAgencyResponse
netAgency req = do
  (states, trs) <- maybe (badRequest "protocol unspecified") pure (transitionsOf (req ^. #protocol))
  if (req ^. #state) `notElem` states
    then badRequest ("unknown state " <> req ^. #state)
    else
      pure $
        case [t | t@(Tr from m _ _) <- trs, from == req ^. #state, m == req ^. #messageType] of
          Tr _ _ to ag : _ -> defMessage & #nextState .~ to & #agency .~ ag
          [] ->
            defMessage
              & #rejection
                .~ rejection "IllegalTransition" (req ^. #messageType <> " in " <> req ^. #state)

------------------------------------------------------------------------------

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'NetCodecChainSync q -> Just (respond #netCodecChainSync (netCodecChainSync q))
  Call'NetCodecBlockFetch q -> Just (respond #netCodecBlockFetch (netCodecBlockFetch q))
  Call'NetCodecTxSubmission2 q -> Just (respond #netCodecTxSubmission2 (netCodecTxSubmission2 q))
  Call'NetCodecLocalStateQuery q -> Just (respond #netCodecLocalStateQuery (netCodecLocalStateQuery q))
  Call'NetCodecLocalTxSubmission q -> Just (respond #netCodecLocalTxSubmission (netCodecLocalTxSubmission q))
  Call'NetCodecLocalTxMonitor q -> Just (respond #netCodecLocalTxMonitor (netCodecLocalTxMonitor q))
  Call'NetCodecHandshake q -> Just (respond #netCodecHandshake (netCodecHandshake q))
  Call'NetCodecKeepAlive q -> Just (respond #netCodecKeepAlive (netCodecKeepAlive q))
  Call'NetCodecPeerSharing q -> Just (respond #netCodecPeerSharing (netCodecPeerSharing q))
  Call'NetCodecMuxSdu q -> Just (respond #netCodecMuxSdu (netCodecMuxSdu q))
  Call'NetAgency q -> Just (respond #netAgency (netAgency q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "net_codec_chain_sync"
  , "net_codec_block_fetch"
  , "net_codec_tx_submission2"
  , "net_codec_local_state_query"
  , "net_codec_local_tx_submission"
  , "net_codec_local_tx_monitor"
  , "net_codec_handshake"
  , "net_codec_keep_alive"
  , "net_codec_peer_sharing"
  , "net_codec_mux_sdu"
  , "net_agency"
  ]
