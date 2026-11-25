{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Rewards (tests) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Word (Word32, Word64, Word8)
import Lens.Micro ((&), (.~), (^.))
import Data.Text (Text)
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..), Reply)
import qualified Proto.Dxdingo.V1.Rewards as R
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
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
    ++ updateTests
  where
    member reward cost m mem pool total =
      call (#rewardsMemberReward .~ (defMessage & #poolReward .~ reward & #cost .~ cost & #margin .~ rat m
        & #memberStake .~ mem & #poolStake .~ pool & #totalStake .~ total))
    perf d ps =
      call (#rewardsApparentPerformance .~ (defMessage & #d .~ rat d & #poolStake .~ ps
        & #totalActiveStake .~ 4 & #blocks .~ 3 & #blocksTotal .~ 10))
        ^. #rewardsApparentPerformance . #performance


-- Same cases as dxnode-go (rewards_update_test.go); the hand derivations are
-- there.

keyCred :: Word8 -> P.Credential
keyCred b = defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ BS.replicate 28 b

poolIdOf :: Word8 -> BS.ByteString
poolIdOf = BS.replicate 28

pv :: Word32 -> P.ProtocolVersion
pv m = defMessage & #major .~ m

errorCode :: Reply' -> HarnessErrorCode
errorCode r = r ^. #error . #code

type Reply' = Reply

-- | Rewards by credential first byte; leaders get a trailing marker.
rewardMap :: [S.Reward] -> Map.Map (Word8, Bool) Word64
rewardMap rs =
  Map.fromListWith (+)
    [ ((BS.head (r ^. #credential . #hash), r ^. #kind == S.REWARD_KIND_LEADER), r ^. #amount)
    | r <- rs
    ]

poolA :: S.Pool
poolA =
  defMessage
    & #poolId .~ poolIdOf 0xa1
    & #pledge .~ 10
    & #cost .~ 1000000
    & #margin .~ rat ("1", "10")
    & #rewardAccount .~ keyCred 0x01
    & #owners .~ [BS.replicate 28 0x02]

poolRewardReq :: Word32 -> R.RewardsPoolRewardRequest
poolRewardReq major =
  defMessage
    & #protocolVersion .~ pv major
    & #pparams .~ (defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 500 & #d .~ rat ("0", "1"))
    & #poolPot .~ 1000000000
    & #totalStake .~ 1000
    & #totalActiveStake .~ 500
    & #blocks .~ 5
    & #blocksTotal .~ 10
    & #pool .~ poolA
    & #members
      .~ [ defMessage & #credential .~ keyCred 0x02 & #amount .~ 20
         , defMessage & #credential .~ keyCred 0x03 & #amount .~ 50
         , defMessage & #credential .~ keyCred 0x04 & #amount .~ 30
         ]
    & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03, keyCred 0x04]

poolRewardOf :: R.RewardsPoolRewardRequest -> R.RewardsPoolRewardResponse
poolRewardOf q = call (#rewardsPoolReward .~ q) ^. #rewardsPoolReward

monetaryReq :: Word32 -> (Text, Text) -> Word64 -> R.RewardsMonetaryExpansionRequest
monetaryReq major d blocks =
  defMessage
    & #protocolVersion .~ pv major
    & #pparams
      .~ ( defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 500 & #rho .~ rat ("3", "1000")
             & #tau .~ rat ("1", "5") & #d .~ rat d
         )
    & #globals
      .~ ( defMessage & #epochLength .~ 432000 & #activeSlotCoeff .~ rat ("1", "20") & #securityParam .~ 2160
             & #maxLovelaceSupply .~ 45000000000000000 & #slotLength .~ rat ("1", "1")
         )
    & #reserves .~ 1000000000000
    & #fees .~ 2000000
    & #blocksTotal .~ blocks

monetaryOf :: R.RewardsMonetaryExpansionRequest -> R.RewardsMonetaryExpansionResponse
monetaryOf q = call (#rewardsMonetaryExpansion .~ q) ^. #rewardsMonetaryExpansion

monetaryIs :: R.RewardsMonetaryExpansionRequest -> (Word64, Word64, Word64, Word64) -> Bool
monetaryIs q (r1, pot, cut, pool) =
  let r = monetaryOf q
   in (r ^. #deltaR1, r ^. #rewardPot, r ^. #treasuryCut, r ^. #poolPot) == (r1, pot, cut, pool)

rewardUpdateReq :: Word32 -> R.RewardsRewardUpdateRequest
rewardUpdateReq major =
  defMessage
    & #protocolVersion .~ pv major
    & #pparams
      .~ ( defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 500 & #rho .~ rat ("3", "1000")
             & #tau .~ rat ("1", "5") & #d .~ rat ("0", "1")
         )
    & #globals
      .~ ( defMessage & #epochLength .~ 432000 & #activeSlotCoeff .~ rat ("1", "20") & #securityParam .~ 2160
             & #maxLovelaceSupply .~ 400000001000 & #slotLength .~ rat ("1", "1")
         )
    & #pots .~ (defMessage & #reserves .~ 400000000000 & #treasury .~ 7 & #fees .~ 50000000)
    & #snapshots
      .~ [ snap (keyCred 0x02) 20 (poolIdOf 0xa1)
         , snap (keyCred 0x03) 50 (poolIdOf 0xa1)
         , snap (keyCred 0x04) 30 (poolIdOf 0xa1)
         , snap (keyCred 0x06) 400 (poolIdOf 0xb2)
         ]
    & #pools
      .~ [ poolA
         , defMessage & #poolId .~ poolIdOf 0xb2 & #cost .~ 340000000 & #margin .~ rat ("1", "20")
             & #rewardAccount .~ keyCred 0x05 & #owners .~ [BS.replicate 28 0x06]
         ]
    & #blocks .~ [defMessage & #poolId .~ poolIdOf 0xa1 & #count .~ 21600]
    & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03, keyCred 0x04]
  where
    snap c a p = defMessage & #snapshot .~ S.SNAPSHOT_KIND_GO & #credential .~ c & #amount .~ a & #poolId .~ p

rewardUpdateOf :: R.RewardsRewardUpdateRequest -> R.RewardsRewardUpdateResponse
rewardUpdateOf q = call (#rewardsRewardUpdate .~ q) ^. #rewardsRewardUpdate

applyUpdateReq :: R.RewardsApplyUpdateRequest
applyUpdateReq =
  defMessage
    & #protocolVersion .~ pv 8
    & #pots .~ (defMessage & #reserves .~ 400000000000 & #treasury .~ 7 & #fees .~ 50000000)
    & #update
      .~ ( defMessage & #deltaR .~ (-210000000) & #deltaT .~ 250000000 & #deltaF .~ (-50000000)
             & #rewards
               .~ [ rew 0x01 S.REWARD_KIND_LEADER 3520000
                  , rew 0x03 S.REWARD_KIND_MEMBER 4050000
                  , rew 0x04 S.REWARD_KIND_MEMBER 2430000
                  ]
         )
    & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03]
    & #totalRewardPot .~ 1250000000
    & #treasuryExpansion .~ rat ("1", "5")
  where
    rew c k a = defMessage & #credential .~ keyCred c & #poolId .~ poolIdOf 0xa1 & #kind .~ k & #amount .~ a

applyUpdateOf :: R.RewardsApplyUpdateRequest -> R.RewardsApplyUpdateResponse
applyUpdateOf q = call (#rewardsApplyUpdate .~ q) ^. #rewardsApplyUpdate

unsupportedPoolReward :: (R.RewardsPoolRewardRequest -> R.RewardsPoolRewardRequest) -> Bool
unsupportedPoolReward f =
  errorCode (call (#rewardsPoolReward .~ f (poolRewardReq 8))) == HARNESS_ERROR_CODE_UNSUPPORTED

-- | Same case as TestNonMyopic in dxnode-go: pools A and B are the top pools
-- and C earns nothing (derivation in dingo's pure_logic_nonmyopic_test.go).
nonMyopicReq :: R.RewardsNonMyopicRequest
nonMyopicReq =
  defMessage
    & #pparams .~ (defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 2)
    & #totalStake .~ 1000
    & #rewardPot .~ 1000000000
    & #memberStake .~ 50
    & #pools
      .~ [ pool 0xa1 10 1000000 ("1", "10")
         , pool 0xb2 5 2000000 ("1", "5")
         , pool 0xc3 1 400000000 ("0", "1")
         ]
    & #poolDistribution
      .~ [ defMessage & #poolId .~ poolIdOf 0xa1 & #stake .~ 100
         , defMessage & #poolId .~ poolIdOf 0xb2 & #stake .~ 50
         , defMessage & #poolId .~ poolIdOf 0xc3 & #stake .~ 10
         ]
    & #performance
      .~ [ defMessage & #poolId .~ poolIdOf 0xa1 & #performance .~ rat ("1", "1")
         , defMessage & #poolId .~ poolIdOf 0xb2 & #performance .~ rat ("1", "2")
         , defMessage & #poolId .~ poolIdOf 0xc3 & #performance .~ rat ("1", "1")
         ]
    & #ownerStake
      .~ [ defMessage & #poolId .~ poolIdOf 0xa1 & #amount .~ 20
         , defMessage & #poolId .~ poolIdOf 0xb2 & #amount .~ 5
         , defMessage & #poolId .~ poolIdOf 0xc3 & #amount .~ 1
         ]
  where
    pool i pledge cost m =
      defMessage & #poolId .~ poolIdOf i & #pledge .~ pledge & #cost .~ cost & #margin .~ rat m
        & #rewardAccount .~ keyCred 0xee

nonMyopicOf :: R.RewardsNonMyopicRequest -> [(Word8, Word64)]
nonMyopicOf q =
  [ (BS.head (r ^. #poolId), r ^. #reward)
  | r <- (call (#rewardsNonMyopic .~ q) ^. #rewardsNonMyopic) ^. #rewards
  ]

updateTests :: [Check]
updateTests =
  [ ("non-myopic: top pools and a pool below its cost", nonMyopicOf nonMyopicReq == [(0xa1, 34733076), (0xb2, 15270769), (0xc3, 0)])
  , ( "non-myopic: a pool below its pledge pays nothing"
    , nonMyopicOf (nonMyopicReq & #ownerStake . ix0 . #amount .~ 9) == [(0xa1, 0), (0xb2, 15270769), (0xc3, 0)]
    )
  , ( "non-myopic: zero total stake is UNSUPPORTED"
    , errorCode (call (#rewardsNonMyopic .~ (nonMyopicReq & #totalStake .~ 0))) == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "non-myopic: zero n_opt is UNSUPPORTED"
    , errorCode (call (#rewardsNonMyopic .~ (nonMyopicReq & #pparams . #nOpt .~ 0))) == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "non-myopic: margin above one is UNSUPPORTED"
    , errorCode (call (#rewardsNonMyopic .~ (nonMyopicReq & #pools . ix0 . #margin .~ rat ("3", "2")))) == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "non-myopic: negative performance is UNSUPPORTED"
    , errorCode (call (#rewardsNonMyopic .~ (nonMyopicReq & #performance . ix0 . #performance .~ rat ("-1", "2")))) == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "pool reward: Babbage pool of 100 earns 5000000, leader 2120000"
    , let r = poolRewardOf (poolRewardReq 8)
       in r ^. #poolReward == 5000000 && r ^. #leaderReward == 2120000 && ratIs ("5", "2") (r ^. #performance)
            && rewardMap (r ^. #rewards)
              == Map.fromList [((0x01, True), 2120000), ((0x03, False), 1800000), ((0x04, False), 1080000)]
    )
  , ( "pool reward: Alonzo prefilter drops the unregistered member"
    , let r = poolRewardOf (poolRewardReq 6 & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03])
       in rewardMap (r ^. #rewards) == Map.fromList [((0x01, True), 2120000), ((0x03, False), 1800000)]
    )
  , ( "pool reward: Babbage keeps the unregistered member"
    , let r = poolRewardOf (poolRewardReq 7 & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03])
       in Map.lookup (0x04, False) (rewardMap (r ^. #rewards)) == Just 1080000
    )
  , ( "pool reward: pledge not met earns nothing"
    , let r = poolRewardOf (poolRewardReq 8 & #pool . #pledge .~ 21)
       in r ^. #poolReward == 0 && r ^. #leaderReward == 0 && null (r ^. #rewards)
    )
  , ("pool reward: zero total stake is UNSUPPORTED", unsupportedPoolReward (#totalStake .~ 0))
  , ("pool reward: zero total active stake is UNSUPPORTED", unsupportedPoolReward (#totalActiveStake .~ 0))
  , ("pool reward: margin above one is UNSUPPORTED", unsupportedPoolReward (#pool . #margin .~ rat ("3", "2")))
  , ("pool reward: zero member stake is UNSUPPORTED", unsupportedPoolReward (#members . ix0 . #amount .~ 0))
  , ("pool reward: major below Shelley is UNSUPPORTED", unsupportedPoolReward (#protocolVersion .~ pv 1))
  , ("monetary expansion: half efficiency", monetaryIs (monetaryReq 8 ("0", "1") 10800) (1500000000, 1502000000, 300400000, 1201600000))
  , ("monetary expansion: efficiency capped at one", monetaryIs (monetaryReq 8 ("0", "1") 30000) (3000000000, 3002000000, 600400000, 2401600000))
  , ("monetary expansion: d at least 4/5 is one", monetaryIs (monetaryReq 3 ("9", "10") 0) (3000000000, 3002000000, 600400000, 2401600000))
  , ( "monetary expansion: zero expected blocks is UNSUPPORTED"
    , errorCode (call (#rewardsMonetaryExpansion .~ (monetaryReq 8 ("0", "1") 1 & #globals . #epochLength .~ 10)))
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "reward update: Babbage keeps the unregistered member"
    , let u = rewardUpdateOf (rewardUpdateReq 8) ^. #update
       in (u ^. #deltaR, u ^. #deltaT, u ^. #deltaF) == (-210000000, 250000000, -50000000)
            && rewardMap (u ^. #rewards)
              == Map.fromList [((0x01, True), 3520000), ((0x03, False), 4050000), ((0x04, False), 2430000)]
    )
  , ( "reward update: Alonzo prefilter returns the dropped reward to reserves"
    , let u = rewardUpdateOf (rewardUpdateReq 6 & #registered .~ [keyCred 0x01, keyCred 0x02, keyCred 0x03]) ^. #update
       in u ^. #deltaR == (-207570000)
            && rewardMap (u ^. #rewards) == Map.fromList [((0x01, True), 3520000), ((0x03, False), 4050000)]
    )
  , ( "reward update: reserves above the supply are UNSUPPORTED"
    , errorCode (call (#rewardsRewardUpdate .~ (rewardUpdateReq 8 & #globals . #maxLovelaceSupply .~ 1)))
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "apply update: credits registered, unregistered reward to treasury"
    , let r = applyUpdateOf applyUpdateReq
          p = r ^. #pots
       in (p ^. #reserves, p ^. #treasury, p ^. #fees) == (399790000000, 252430007, 0)
            && r ^. #unregisteredToTreasury == 2430000
            && rewardMap' (r ^. #credited) == Map.fromList [(0x01, 3520000), (0x03, 4050000)]
    )
  , ("apply update: inconsistent delta_t is UNSUPPORTED", unsupportedApply (#update . #deltaT .~ 250000001))
  , ("apply update: fees increase is UNSUPPORTED", unsupportedApply (#update . #deltaF .~ 1))
  , ("apply update: before Allegra is UNSUPPORTED", unsupportedApply (#protocolVersion .~ pv 2))
  ]
  where
    ix0 f (x : xs) = (: xs) <$> f x
    ix0 _ [] = pure []
    rewardMap' cs = Map.fromList [(BS.head (c ^. #credential . #hash), c ^. #amount) | c <- cs]
    unsupportedApply f =
      errorCode (call (#rewardsApplyUpdate .~ f applyUpdateReq)) == HARNESS_ERROR_CODE_UNSUPPORTED
