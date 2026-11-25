{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reward functions, answered by cardano-ledger-shelley.
module Dxdingo.Rewards
  ( memberReward
  ) where

import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), UnitInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.Rewards (StakeShare (..), calcStakePoolMemberReward)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Convert (rational)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Rewards
  ( RewardsMemberRewardRequest
  , RewardsMemberRewardResponse
  )

-- | 'calcStakePoolMemberReward' with t = member / total and
-- sigma = pool / total. Inputs outside its domain (a zero denominator, or a
-- margin that is not a 'UnitInterval') are UNSUPPORTED, as in dxnode-go.
memberReward ::
  RewardsMemberRewardRequest ->
  Either (HarnessErrorCode, Text) RewardsMemberRewardResponse
memberReward req = do
  m <- maybe (Left (HARNESS_ERROR_CODE_BAD_REQUEST, "margin: malformed rational")) Right $
    rational =<< (req ^. #maybe'margin)
  margin <-
    maybe (Left (HARNESS_ERROR_CODE_UNSUPPORTED, "margin is not a UnitInterval")) Right $
      boundRational m :: Either (HarnessErrorCode, Text) UnitInterval
  let total = toInteger (req ^. #totalStake)
      pool = toInteger (req ^. #poolStake)
  if total == 0 || pool == 0
    then Left (HARNESS_ERROR_CODE_UNSUPPORTED, "pool_stake and total_stake must be nonzero")
    else do
      let Coin reward =
            calcStakePoolMemberReward
              (Coin (toInteger (req ^. #poolReward)))
              (Coin (toInteger (req ^. #cost)))
              margin
              (StakeShare (toInteger (req ^. #memberStake) % total))
              (StakeShare (pool % total))
      if reward < 0
        then Left (HARNESS_ERROR_CODE_INTERNAL, "negative reward: " <> T.pack (show reward))
        else Right (defMessage & #reward .~ fromInteger reward)
