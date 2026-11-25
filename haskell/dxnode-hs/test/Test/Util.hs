{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared test helpers. Each Test.<Area> module exports @tests :: [Check]@.
module Test.Util
  ( Check
  , call
  , rat
  , ratIs
  ) where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Dxdingo.Dispatch (handle)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call, Reply)
import qualified Proto.Dxdingo.V1.Types as P

-- | A named assertion.
type Check = (String, Bool)

-- | Handle a call built from an empty Call with id 1.
call :: (Call -> Call) -> Reply
call f = handle ((defMessage :: Call) & #id .~ 1 & f)

rat :: (Text, Text) -> P.Rational
rat (a, b) = defMessage & #numerator .~ a & #denominator .~ b

ratIs :: (Text, Text) -> P.Rational -> Bool
ratIs (a, b) r = r ^. #numerator == a && r ^. #denominator == b
