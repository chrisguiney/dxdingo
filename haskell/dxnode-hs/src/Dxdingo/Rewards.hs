{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reward functions, answered by cardano-ledger-shelley and -core.
module Dxdingo.Rewards
  ( memberReward
  , leaderReward
  , maxPool
  , apparentPerformance
  ) where

import Cardano.Ledger.BaseTypes (NonNegativeInterval, UnitInterval, nonZero)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.Rewards
  ( StakeShare (..)
  , calcStakePoolMemberReward
  , calcStakePoolOperatorReward
  , mkApparentPerformance
  )
import Cardano.Ledger.State (maxPool')
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Word (Word16, Word64)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Rewards

-- | 'calcStakePoolMemberReward' with t = member / total and
-- sigma = pool / total.
memberReward :: RewardsMemberRewardRequest -> Result RewardsMemberRewardResponse
memberReward req = do
  margin <- unitIntervalArg "margin" (req ^. #maybe'margin)
  total <- nonzero "total_stake" (req ^. #totalStake)
  pool <- nonzero "pool_stake" (req ^. #poolStake)
  reward <-
    coinWord64 $
      calcStakePoolMemberReward
        (coin (req ^. #poolReward))
        (coin (req ^. #cost))
        margin
        (StakeShare (toInteger (req ^. #memberStake) % total))
        (StakeShare (pool % total))
  pure (defMessage & #reward .~ reward)

-- | 'calcStakePoolOperatorReward' with s = owner / total and
-- sigma = pool / total.
leaderReward :: RewardsLeaderRewardRequest -> Result RewardsLeaderRewardResponse
leaderReward req = do
  margin <- unitIntervalArg "margin" (req ^. #maybe'margin)
  total <- nonzero "total_stake" (req ^. #totalStake)
  pool <- nonzero "pool_stake" (req ^. #poolStake)
  reward <-
    coinWord64 $
      calcStakePoolOperatorReward
        (coin (req ^. #poolReward))
        (coin (req ^. #cost))
        margin
        (StakeShare (toInteger (req ^. #ownerStake) % total))
        (StakeShare (pool % total))
  pure (defMessage & #reward .~ reward)

-- | 'maxPool'' with sigma = pool / total and pR = pledge / total.
maxPool :: RewardsMaxPoolRequest -> Result RewardsMaxPoolResponse
maxPool req = do
  let pp = req ^. #pparams
  a0 <- boundedArg "a0" (pp ^. #maybe'a0) :: Result NonNegativeInterval
  nOpt <- case pp ^. #maybe'nOpt of
    Just n | n >= 1 && n <= 0xffff -> pure (fromIntegral n :: Word16)
    _ -> unsupported "n_opt must be in [1, 65535]"
  nOptNZ <- maybe (unsupported "n_opt is zero") pure (nonZero nOpt)
  total <- nonzero "total_stake" (req ^. #totalStake)
  m <-
    coinWord64 $
      maxPool'
        a0
        nOptNZ
        (coin (req ^. #poolPot))
        (toInteger (req ^. #poolStake) % total)
        (toInteger (req ^. #pledge) % total)
  pure (defMessage & #maxPool .~ m)

-- | 'mkApparentPerformance' with sigma = pool / total active stake.
apparentPerformance ::
  RewardsApparentPerformanceRequest -> Result RewardsApparentPerformanceResponse
apparentPerformance req = do
  d <- unitIntervalArg "d" (req ^. #maybe'd)
  total <- nonzero "total_active_stake" (req ^. #totalActiveStake)
  let p =
        mkApparentPerformance
          (d :: UnitInterval)
          (toInteger (req ^. #poolStake) % total)
          (fromIntegral (req ^. #blocks))
          (fromIntegral (req ^. #blocksTotal))
  pure (defMessage & #performance .~ rationalProto p)

coin :: Integral a => a -> Coin
coin = Coin . toInteger

coinWord64 :: Coin -> Result Word64
coinWord64 (Coin c)
  | c < 0 || c > toInteger (maxBound :: Word64) =
      internal ("coin out of Word64 range: " <> T.pack (show c))
  | otherwise = pure (fromInteger c)
