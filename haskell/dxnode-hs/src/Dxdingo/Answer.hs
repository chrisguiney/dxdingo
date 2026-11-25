{-# LANGUAGE OverloadedLabels #-}

-- | How an area module answers a call. Each area exports
-- @dispatch :: Call'Request -> Maybe Answer@ and @implemented :: [Text]@.
module Dxdingo.Answer
  ( Answer
  , respond
  ) where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Dxdingo.Domain (Result)
import Lens.Micro (ASetter', (&), (.~))
import Proto.Dxdingo.V1.Envelope (Reply)

-- | Completes a reply that already carries the call id.
type Answer = Reply -> Reply

-- | Set the response field, or the harness error when the handler failed.
respond :: ASetter' Reply a -> Result a -> Answer
respond field r base = case r of
  Left (code, msg) -> base & #error .~ (defMessage & #code .~ code & #message .~ msg)
  Right x -> base & field .~ x
