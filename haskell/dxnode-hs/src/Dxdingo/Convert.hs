{-# LANGUAGE OverloadedLabels #-}

-- | Protobuf scalar types to Haskell values.
module Dxdingo.Convert
  ( rational
  ) where

import Data.ProtoLens.Labels ()
import Data.Ratio ((%))
import qualified Data.Text as T
import Lens.Micro ((^.))
import qualified Proto.Dxdingo.V1.Types as P
import Text.Read (readMaybe)

-- | Decode a 'P.Rational'. Both parts are base-10 integers; the denominator
-- must be positive.
rational :: P.Rational -> Maybe Rational
rational r = do
  n <- readMaybe (T.unpack (r ^. #numerator)) :: Maybe Integer
  d <- readMaybe (T.unpack (r ^. #denominator)) :: Maybe Integer
  if d <= 0 then Nothing else Just (n % d)
