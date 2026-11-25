{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | Cases mirror cmd/dxnode-go/net_test.go; both sides hand-encode the same
-- CBOR from the ouroboros-network specs.
module Test.Net (tests) where

import Cardano.Network.NodeToClient.Version (NodeToClientVersion (..))
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..), nodeToNodeVersionDataCodec)
import Cardano.Network.Protocol.Handshake.Codec (nodeToNodeHandshakeCodec)
import qualified Cardano.Network.Protocol.LocalTxMonitor.Codec as LTMCodec
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Codec.CBOR.Term as T
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64, Word8)
import Dxdingo.Net (Fam (..), bfCodec, csN2NCodec, decodeQueryBytes, family, ltmCodec)
import Network.TypedProtocol.Codec (Codec, CodecF (..), runDecoder)
import qualified Network.TypedProtocol.Core as TP
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import Ouroboros.Network.CodecCBORTerm (decodeData)
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BF
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as CSCodec
import qualified Ouroboros.Network.Protocol.ChainSync.Type as CS
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Codec as LSQBase
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.PeerSharing.Codec as PSCodec
import qualified Ouroboros.Network.Protocol.PeerSharing.Type as PS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Codec as TXCodec
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TX
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Net
import Test.Util

hash32 :: Word8 -> [Word8]
hash32 b = 0x58 : 0x20 : replicate 32 b

codecReq :: Word -> Text -> [Word8] -> NetCodecRequest
codecReq v st msg =
  defMessage & #version .~ fromIntegral v & #state .~ st & #message .~ BS.pack msg

chainSync :: Word -> Text -> [Word8] -> NetCodecChainSyncResponse
chainSync v st msg =
  call (#netCodecChainSync .~ (defMessage & #codec .~ codecReq v st msg)) ^. #netCodecChainSync

keepAlive :: Text -> [Word8] -> NetCodecKeepAliveResponse
keepAlive st msg =
  call (#netCodecKeepAlive .~ (defMessage & #codec .~ codecReq 14 st msg)) ^. #netCodecKeepAlive

keepAliveErr :: Word -> Text -> HarnessErrorCode
keepAliveErr v st =
  call (#netCodecKeepAlive .~ (defMessage & #codec .~ codecReq v st [0x81, 0x02])) ^. #error . #code

muxSdu :: [Word8] -> NetCodecMuxSduResponse
muxSdu h = call (#netCodecMuxSdu .~ (defMessage & #header .~ BS.pack h)) ^. #netCodecMuxSdu

agency :: MiniProtocol -> Text -> Text -> NetAgencyResponse
agency p st m =
  call (#netAgency .~ (defMessage & #protocol .~ p & #state .~ st & #messageType .~ m)) ^. #netAgency

tests :: [Check]
tests =
  [ ("net chainsync: RequestNext", reqNext ^. #messageType == "MsgRequestNext" && reqNext ^. #cbor == BS.pack [0x81, 0x00])
  , ( "net chainsync: RollBackward"
    , back ^. #messageType == "MsgRollBackward" && back ^. #maybe'pointSlot == Just 5
        && back ^. #pointHash == BS.replicate 32 0xaa
        && back ^. #maybe'tipSlot == Just 10 && back ^. #maybe'tipBlockNo == Just 7
    )
  , ( "net chainsync: IntersectFound at origin has no slots"
    , origin ^. #messageType == "MsgIntersectFound" && origin ^. #maybe'pointSlot == Nothing
        && origin ^. #maybe'tipSlot == Nothing && origin ^. #maybe'tipBlockNo == Nothing
    )
  , ("net chainsync: RequestNext is not decodable in StMustReply", rejectedName wrongState "DeserialiseFailure")
  , ("net chainsync: StDone has no agency", rejectedName (chainSync 14 "StDone" [0x81, 0x07]) "DeserialiseFailure")
  , ("net chainsync: trailing byte", rejectedName (chainSync 14 "StIdle" [0x81, 0x00, 0x00]) "DeserialiseFailure")
  , ( "net domain: bad states and versions are harness errors"
    , all
        (== HARNESS_ERROR_CODE_UNSUPPORTED)
        [keepAliveErr 13 "StClient", keepAliveErr 0 "StClient", keepAliveErr (32784 + 100) "StClient", keepAliveErr 32784 "StClient"]
        && keepAliveErr 14 "StNowhere" == HARNESS_ERROR_CODE_BAD_REQUEST
        && keepAliveErr 14 "BFIdle" == HARNESS_ERROR_CODE_BAD_REQUEST
    )
  , ("net keepalive: MsgKeepAlive", ka ^. #messageType == "MsgKeepAlive" && ka ^. #maybe'cookie == Just 42 && ka ^. #cbor == BS.pack [0x82, 0x00, 0x18, 0x2a])
  , ("net keepalive: response cookie 256", kr ^. #messageType == "MsgKeepAliveResponse" && kr ^. #maybe'cookie == Just 256)
  , ("net keepalive: MsgKeepAlive in StServer", rejectedName (keepAlive "StServer" [0x82, 0x00, 0x01]) "DeserialiseFailure")
  , ("net keepalive: cookie is a Word16", rejectedName (keepAlive "StClient" [0x82, 0x00, 0x1a, 0x00, 0x01, 0x00, 0x00]) "DeserialiseFailure")
  , ("net mux: initiator", mi ^. #timestamp == 0x01020304 && mi ^. #mode == MUX_MODE_INITIATOR && mi ^. #protocol == 2 && mi ^. #length == 513)
  , ("net mux: responder", mr ^. #mode == MUX_MODE_RESPONDER && mr ^. #protocol == 10 && mr ^. #length == 1)
  , ("net mux: zero length is refused", rejectedName (muxSdu [0, 0, 0, 7, 0, 2, 0, 0]) "DeserialiseFailure")
  , ("net mux: short header", rejectedName (muxSdu [1, 2, 3]) "DeserialiseFailure")
  , ("net agency: RequestNext", a1 ^. #nextState == "StCanAwait" && a1 ^. #agency == AGENCY_SERVER)
  , ("net agency: Done", a2 ^. #nextState == "StDone" && a2 ^. #agency == AGENCY_NOBODY)
  , ("net agency: KeepAlive", a3 ^. #nextState == "StServer" && a3 ^. #agency == AGENCY_SERVER)
  , ("net agency: AwaitReply in StIdle", illegal (agency MINI_PROTOCOL_CHAIN_SYNC "StIdle" "MsgAwaitReply"))
  , ("net agency: Done in StDone", illegal (agency MINI_PROTOCOL_CHAIN_SYNC "StDone" "MsgDone"))
  ]
    ++ divergences
  where
    reqNext = chainSync 14 "StIdle" [0x81, 0x00]
    back =
      chainSync 14 "StCanAwait" $
        [0x83, 0x03, 0x82, 0x05] ++ hash32 0xaa ++ [0x82, 0x82, 0x0a] ++ hash32 0xbb ++ [0x07]
    origin = chainSync 14 "StIntersect" [0x83, 0x05, 0x80, 0x82, 0x80, 0x00]
    wrongState = chainSync 14 "StMustReply" [0x81, 0x00]
    ka = keepAlive "StClient" [0x82, 0x00, 0x18, 0x2a]
    kr = keepAlive "StServer" [0x82, 0x01, 0x19, 0x01, 0x00]
    hdr ts p l = [fromIntegral (ts `div` 16777216), fromIntegral (ts `div` 65536), fromIntegral (ts `div` 256), fromIntegral ts, fromIntegral (p `div` 256), fromIntegral p, fromIntegral (l `div` 256), fromIntegral l] :: [Word8]
    mi = muxSdu (hdr (0x01020304 :: Word) (2 :: Word) (513 :: Word))
    mr = muxSdu (hdr (7 :: Word) (0x800a :: Word) (1 :: Word))
    a1 = agency MINI_PROTOCOL_CHAIN_SYNC "StIdle" "MsgRequestNext"
    a2 = agency MINI_PROTOCOL_CHAIN_SYNC "StIdle" "MsgDone"
    a3 = agency MINI_PROTOCOL_KEEP_ALIVE "StClient" "MsgKeepAlive"
    illegal r = map (^. #name) (r ^. #rejection . #failures) == ["IllegalTransition"] && map (^. #rule) (r ^. #rejection . #failures) == ["net"]
    rejectedName r n = map (^. #name) (r ^. #rejection . #failures) == [n] && map (^. #rule) (r ^. #rejection . #failures) == ["net"]

------------------------------------------------------------------------------
-- Direct checks of the reference libraries for the cases in
-- corpus/divergences, bypassing the adapters. cmd/dxnode-go/divergence_net_test.go
-- holds the gouroboros side on the same bytes.

-- | Does the codec decode the whole message in the given state?
decodes ::
  forall ps (st :: ps).
  (TP.StateTokenI st, TP.ActiveState st) =>
  (forall s. Codec ps DeserialiseFailure (ST s) BL.ByteString) ->
  TP.StateToken st ->
  [Word8] ->
  Bool
decodes codec stok bytes = runST $ do
  step <- decode codec stok
  r <- runDecoder [BL.pack bytes] step
  pure (either (const False) (const True) r)

decodesStateful ::
  forall ps f (st :: ps).
  (TP.StateTokenI st, TP.ActiveState st) =>
  (forall s. Stateful.Codec ps DeserialiseFailure f (ST s) BL.ByteString) ->
  TP.StateToken st ->
  f st ->
  [Word8] ->
  Bool
decodesStateful codec stok st bytes = runST $ do
  step <- Stateful.decode codec stok st
  r <- runDecoder [BL.pack bytes] step
  pure (either (const False) (const True) r)

-- | Codecs over plain words, to show how the reference writes and reads lists.
wordChainSync :: Codec (CS.ChainSync () Word64 Word64) DeserialiseFailure IO BL.ByteString
wordChainSync = CSCodec.codecChainSync (const mempty) (fail "no headers") E.encodeWord64 D.decodeWord64 E.encodeWord64 D.decodeWord64

wordPeerSharing :: Codec (PS.PeerSharing Word32) DeserialiseFailure IO BL.ByteString
wordPeerSharing = PSCodec.codecPeerSharing E.encodeWord32 D.decodeWord32

wordTxSubmission :: Codec (TX.TxSubmission2 Word32 Word32) DeserialiseFailure (ST s) BL.ByteString
wordTxSubmission = TXCodec.codecTxSubmission2 E.encodeWord32 D.decodeWord32 E.encodeWord32 D.decodeWord32

unitTxMonitor :: Codec (LTM.LocalTxMonitor () () ()) DeserialiseFailure (ST s) BL.ByteString
unitTxMonitor =
  LTMCodec.codecLocalTxMonitor NodeToClientV_23 (const mempty) (fail "no txids") (const mempty) (fail "no txs") (const mempty) (fail "no slots")

data NoQuery r

deriving instance Show (NoQuery r)

instance LSQ.ShowQuery NoQuery where
  showResult _ _ = "no queries"

noQueryCodec :: Stateful.Codec (LSQ.LocalStateQuery () Word64 NoQuery) DeserialiseFailure LSQ.State (ST s) BL.ByteString
noQueryCodec =
  LSQBase.codecLocalStateQuery
    LSQBase.LocalStateQuery_V2
    E.encodeWord64
    D.decodeWord64
    (const mempty)
    (fail "no queries")
    (\_ _ -> mempty)
    (\_ -> fail "no results")

divergences :: [Check]
divergences =
  [ ( "divergence: the reference writes a non-empty point list indefinite"
    , take 3 (BL.unpack (encode wordChainSync (CS.MsgFindIntersect [1]))) == [0x82, 0x04, 0x9f]
    )
  , ( "divergence: the reference writes a non-empty peer list indefinite"
    , take 3 (BL.unpack (encode wordPeerSharing (PS.MsgSharePeers [7]))) == [0x82, 0x01, 0x9f]
    )
  , ( "divergence: handshake versions must ascend"
    , not (decodes nodeToNodeHandshakeCodec HS.SingPropose [0x82, 0x00, 0xa2, 0x0f, 0x84, 0x01, 0xf4, 0x00, 0xf4, 0x0e, 0x84, 0x01, 0xf4, 0x00, 0xf4])
        && decodes nodeToNodeHandshakeCodec HS.SingPropose [0x82, 0x00, 0xa2, 0x0e, 0x84, 0x01, 0xf4, 0x00, 0xf4, 0x0f, 0x84, 0x01, 0xf4, 0x00, 0xf4]
    )
  , ( "divergence: peer sharing is 0 or 1"
    , isLeft' (decodeData nodeToNodeVersionDataCodec NodeToNodeV_14 (T.TList [T.TInt 1, T.TBool False, T.TInt 2, T.TBool False]))
        && not (isLeft' (decodeData nodeToNodeVersionDataCodec NodeToNodeV_14 (T.TList [T.TInt 1, T.TBool False, T.TInt 1, T.TBool False])))
    )
  , ( "divergence: MsgReplyVersions is decodable in the confirm state"
    , decodes nodeToNodeHandshakeCodec HS.SingConfirm [0x82, 0x00, 0xa1, 0x0e, 0x84, 0x01, 0xf4, 0x00, 0xf4]
    )
  , ( "divergence: acquire failure codes are 0 and 1"
    , not (decodesStateful noQueryCodec LSQ.SingAcquiring LSQ.StateAcquiring [0x82, 0x02, 0x02])
        && decodesStateful noQueryCodec LSQ.SingAcquiring LSQ.StateAcquiring [0x82, 0x02, 0x01]
    )
  , ( "divergence: tx submission lists are indefinite and a blocking reply is not empty"
    , not (decodes wordTxSubmission (TX.SingTxIds TX.SingNonBlocking) [0x82, 0x01, 0x81, 0x82, 0x07, 0x18, 0x64])
        && decodes wordTxSubmission (TX.SingTxIds TX.SingNonBlocking) [0x82, 0x01, 0x9f, 0x82, 0x07, 0x18, 0x64, 0xff]
        && not (decodes wordTxSubmission (TX.SingTxIds TX.SingBlocking) [0x82, 0x01, 0x9f, 0xff])
    )
  , ( "divergence: the tx monitor has MsgGetMeasures"
    , decodes unitTxMonitor LTM.SingAcquired [0x81, 0x0b]
    )
  , ( "divergence: DebugLedgerConfig is a query"
    , case family False True (defMessage & #version .~ (0x8000 + 23)) of
        Right (N2C nv bv) -> either (const False) (const True) (decodeQueryBytes nv bv (BS.pack [0x81, 0x04]))
        _ -> False
    )
  , ( "divergence: a Byron transaction id is [0, [0, hash]]"
    , case family False True (defMessage & #version .~ (0x8000 + 23)) of
        Right (N2C nv bv) -> decodes (ltmCodec nv bv) LTM.SingAcquired ([0x82, 0x07, 0x82, 0x00, 0x82, 0x00] ++ hash32 9)
        _ -> False
    )
  , ( "divergence: block and header payloads are decoded"
    , case family True False (defMessage & #version .~ 14) of
        Right (N2N _ bv) ->
          not (decodes (bfCodec bv) BF.SingBFStreaming [0x82, 0x04, 0xd8, 0x18, 0x43, 0x82, 0x07, 0x01])
            && not (decodes (csN2NCodec bv) (CS.SingNext CS.SingCanAwait) ([0x83, 0x02, 0x82, 0x06, 0xd8, 0x18, 0x41, 0x01, 0x82, 0x82, 0x01] ++ hash32 2 ++ [0x01]))
        _ -> False
    )
  ]
  where
    isLeft' = either (const True) (const False)
