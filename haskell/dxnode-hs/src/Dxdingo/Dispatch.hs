{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Routes envelope calls to the area modules. Functions no area answers
-- reply UNIMPLEMENTED.
module Dxdingo.Dispatch
  ( handle
  , handleWith
  , handleBytes
  , implemented
  ) where

import Data.ByteString (ByteString)
import Data.Foldable (asum)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer)
import qualified Dxdingo.Codec as Codec
import qualified Dxdingo.Rule as Rule
import qualified Dxdingo.Fee as Fee
import qualified Dxdingo.Plutus as Plutus
import qualified Dxdingo.Rewards as Rewards
import qualified Dxdingo.Stake as Stake
import qualified Dxdingo.Epoch as Epoch
import qualified Dxdingo.Gov as Gov
import qualified Dxdingo.Praos as Praos
import qualified Dxdingo.Chainsel as Chainsel
import qualified Dxdingo.Time as Time
import qualified Dxdingo.Byron as Byron
import qualified Dxdingo.Crypto as Crypto
import qualified Dxdingo.Net as Net
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope
  ( Call
  , Call'Request (..)
  , HarnessErrorCode (..)
  , Reply
  )
import Proto.Dxdingo.V1.Meta (MetaInfoResponse)

areas :: [Call'Request -> Maybe Answer]
areas =
  [ Codec.dispatch
  , Rule.dispatch
  , Fee.dispatch
  , Plutus.dispatch
  , Rewards.dispatch
  , Stake.dispatch
  , Epoch.dispatch
  , Gov.dispatch
  , Praos.dispatch
  , Chainsel.dispatch
  , Time.dispatch
  , Byron.dispatch
  , Crypto.dispatch
  , Net.dispatch
  ]

-- | Envelope field names this node handles.
implemented :: [Text]
implemented =
  "meta_info"
    : concat
      [ Codec.implemented
      , Rule.implemented
      , Fee.implemented
      , Plutus.implemented
      , Rewards.implemented
      , Stake.implemented
      , Epoch.implemented
      , Gov.implemented
      , Praos.implemented
      , Chainsel.implemented
      , Time.implemented
      , Byron.implemented
      , Crypto.implemented
      , Net.implemented
      ]

handle :: Call -> Reply
handle = handleWith areas

-- | 'handle' over an explicit list of areas.
handleWith :: [Call'Request -> Maybe Answer] -> Call -> Reply
handleWith as call = case call ^. #maybe'request of
  Nothing -> harnessError HARNESS_ERROR_CODE_BAD_REQUEST "call has no request"
  Just (Call'MetaInfo _) -> base & #metaInfo .~ metaInfo
  Just req -> case asum (map ($ req) as) of
    Just answer -> answer base
    Nothing -> harnessError HARNESS_ERROR_CODE_UNIMPLEMENTED "not implemented by dxnode-hs"
  where
    base :: Reply
    base = defMessage & #id .~ (call ^. #id)
    harnessError code msg =
      base & #error .~ (defMessage & #code .~ code & #message .~ msg)

metaInfo :: MetaInfoResponse
metaInfo =
  defMessage
    & #implementation .~ "cardano-ledger"
    & #implemented .~ implemented

-- | Decode a call, evaluate it, encode the reply. An undecodable call gets a
-- BAD_REQUEST reply with id 0.
handleBytes :: ByteString -> ByteString
handleBytes bs = encodeMessage $ case decodeMessage bs of
  Left err ->
    (defMessage :: Reply)
      & #error
        .~ ( defMessage
              & #code .~ HARNESS_ERROR_CODE_BAD_REQUEST
              & #message .~ T.pack err
           )
  Right call -> handle call
