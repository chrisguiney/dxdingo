-- | Fee functions.
module Dxdingo.Fee
  ( dispatch
  , implemented
  ) where

import Data.Text (Text)
import Dxdingo.Answer (Answer)
import Proto.Dxdingo.V1.Envelope (Call'Request)

dispatch :: Call'Request -> Maybe Answer
dispatch _ = Nothing

implemented :: [Text]
implemented = []
