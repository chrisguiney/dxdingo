{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Argument decoding and the domain checks shared with dxnode-go. An input
-- the reference types cannot represent (a UnitInterval above one, a zero
-- share denominator) is UNSUPPORTED on both nodes, so the pair is skipped.
module Dxdingo.Domain
  ( Result
  , badRequest
  , unsupported
  , internal
  , rationalArg
  , boundedArg
  , unitIntervalArg
  , positiveUnitIntervalArg
  , nonzero
  , rationalProto
  ) where

import Cardano.Ledger.BaseTypes
  ( BoundedRational (boundRational)
  , PositiveUnitInterval
  , UnitInterval
  )
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as T
import Dxdingo.Convert (rational)
import Lens.Micro ((&), (.~))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import qualified Proto.Dxdingo.V1.Types as P

-- | A handler outcome: a harness error (code, message) or the response.
type Result = Either (HarnessErrorCode, Text)

badRequest, unsupported, internal :: Text -> Result a
badRequest m = Left (HARNESS_ERROR_CODE_BAD_REQUEST, m)
unsupported m = Left (HARNESS_ERROR_CODE_UNSUPPORTED, m)
internal m = Left (HARNESS_ERROR_CODE_INTERNAL, m)

rationalArg :: Text -> Maybe P.Rational -> Result Rational
rationalArg name r = maybe (badRequest (name <> ": missing or malformed rational")) pure (rational =<< r)

-- | Decode into a bounded ratio type ('UnitInterval', 'NonNegativeInterval',
-- ...); out of range is UNSUPPORTED.
boundedArg :: BoundedRational b => Text -> Maybe P.Rational -> Result b
boundedArg name r = do
  x <- rationalArg name r
  maybe (unsupported (name <> " is not representable")) pure (boundRational x)

unitIntervalArg :: Text -> Maybe P.Rational -> Result UnitInterval
unitIntervalArg = boundedArg

positiveUnitIntervalArg :: Text -> Maybe P.Rational -> Result PositiveUnitInterval
positiveUnitIntervalArg = boundedArg

nonzero :: Text -> Word64 -> Result Integer
nonzero name 0 = unsupported (name <> " must be nonzero")
nonzero _ x = pure (toInteger x)

rationalProto :: Rational -> P.Rational
rationalProto x =
  defMessage
    & #numerator .~ T.pack (show (numerator x))
    & #denominator .~ T.pack (show (denominator x))
