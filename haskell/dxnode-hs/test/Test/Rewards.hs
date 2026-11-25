{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Rewards (tests) where

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Test.Util

-- Same cases as dxnode-go.
tests :: [Check]
tests =
  [ ("member reward: half the pool, 1% margin", member 1000000000 340000000 ("1", "100") 25 50 1000 ^. #rewardsMemberReward . #reward == 326700000)
  , ("member reward: reward below cost is zero", member 300000000 340000000 ("0", "1") 25 50 1000 ^. #rewardsMemberReward . #reward == 0)
  , ("member reward: floors", member 10 0 ("0", "1") 1 3 3 ^. #rewardsMemberReward . #reward == 3)
  , ("member reward: zero pool stake is UNSUPPORTED", member 1000 1 ("1", "2") 1 0 4 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("member reward: zero total stake is UNSUPPORTED", member 1000 1 ("1", "2") 1 2 0 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("member reward: margin above one is UNSUPPORTED", member 1000 1 ("3", "2") 1 2 4 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("member reward: negative margin is UNSUPPORTED", member 1000 1 ("-1", "2") 1 2 4 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("member reward: margin over 64 bits is UNSUPPORTED", member 1000 1 ("1", "18446744073709551617") 1 2 4 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("member reward: malformed margin is BAD_REQUEST", member 1000 1 ("x", "2") 1 2 4 ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ( "leader reward: owner fifth of pool"
    , call (#rewardsLeaderReward .~ (defMessage & #poolReward .~ 1000000000 & #cost .~ 340000000
        & #margin .~ rat ("1", "100") & #ownerStake .~ 10 & #poolStake .~ 50 & #totalStake .~ 1000))
        ^. #rewardsLeaderReward . #reward == 477280000
    )
  , ( "leader reward: below cost goes to the operator"
    , call (#rewardsLeaderReward .~ (defMessage & #poolReward .~ 300000000 & #cost .~ 340000000
        & #margin .~ rat ("0", "1") & #ownerStake .~ 10 & #poolStake .~ 50 & #totalStake .~ 1000))
        ^. #rewardsLeaderReward . #reward == 300000000
    )
  , ( "max pool"
    , call (#rewardsMaxPool .~ (defMessage & #pparams .~ (defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 500)
        & #poolPot .~ 1000000000 & #totalStake .~ 1000000 & #poolStake .~ 1000 & #pledge .~ 500))
        ^. #rewardsMaxPool . #maxPool == 812500
    )
  , ( "max pool: missing n_opt is UNSUPPORTED"
    , call (#rewardsMaxPool .~ (defMessage & #pparams .~ (defMessage & #a0 .~ rat ("3", "10"))
        & #poolPot .~ 1 & #totalStake .~ 1 & #poolStake .~ 1))
        ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ("apparent performance: decentralized", ratIs ("6", "5") (perf ("0", "1") 1))
  , ("apparent performance: federated", ratIs ("1", "1") (perf ("9", "10") 1))
  , ("apparent performance: no stake", ratIs ("0", "1") (perf ("0", "1") 0))
  ]
  where
    member reward cost m mem pool total =
      call (#rewardsMemberReward .~ (defMessage & #poolReward .~ reward & #cost .~ cost & #margin .~ rat m
        & #memberStake .~ mem & #poolStake .~ pool & #totalStake .~ total))
    perf d ps =
      call (#rewardsApparentPerformance .~ (defMessage & #d .~ rat d & #poolStake .~ ps
        & #totalActiveStake .~ 4 & #blocks .~ 3 & #blocksTotal .~ 10))
        ^. #rewardsApparentPerformance . #performance
