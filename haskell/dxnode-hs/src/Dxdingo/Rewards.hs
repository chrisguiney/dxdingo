{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reward functions, answered by cardano-ledger-shelley and -core.
module Dxdingo.Rewards
  ( dispatch
  , implemented
  , memberReward
  , leaderReward
  , maxPool
  , apparentPerformance
  , rewardUpdateWith
  , sortRewards
  ) where

import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff
  , BlocksMade (..)
  , BoundedRational (boundRational, unboundRational)
  , Globals (..)
  , Network (..)
  , NonNegativeInterval
  , NonZero (..)
  , ProtVer (..)
  , StrictMaybe (..)
  , UnitInterval
  , mkActiveSlotCoeff
  , mkVersion
  , nonZero
  , unsafeNonZero
  )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core
  ( PParams
  , emptyPParams
  , ppA0L
  , ppDL
  , ppDG
  , ppNOptL
  , ppProtocolVersionL
  , ppRhoL
  , ppTauL
  )
import Cardano.Ledger.Credential (Credential (..), mkPtrNormalized)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.Shelley (ShelleyEra, hardforkBabbageForgoRewardPrefilter)

import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..)
  , FilteredRewards (..)
  , PulsingRewUpdate (..)
  , RewardSnapShot (..)
  , RewardUpdate (..)
  , applyRUpdFiltered
  , createRUpd
  , esLStateL
  , lsCertStateL
  , lsUTxOStateL
  , prevPParamsEpochStateL
  , startStep
  , utxosFeesL
  )
import qualified Cardano.Ledger.Shelley.LedgerState as LS
import Cardano.Ledger.Address (AccountId (..))
import Cardano.Ledger.Hashes (VRFVerKeyHash (..))
import Cardano.Ledger.Shelley.PoolRank
  ( PerformanceEstimate (..)
  , calcNonMyopicMemberReward
  , getTopRankedPools
  , percentile'
  )
import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Shelley.Rewards
  ( PoolRewardInfo (..)
  , LeaderOnlyReward (..)
  , StakeShare (..)
  , calcStakePoolMemberReward
  , calcStakePoolOperatorReward
  , filterRewards
  , mkApparentPerformance
  , mkPoolRewardInfo
  , rewardOnePoolMember
  )
import Cardano.Ledger.Shelley.State (ShelleyAccountState (..), ShelleyAccounts (..))
import Cardano.Ledger.State
  ( ChainAccountState (..)
  , SnapShot (..)
  , SnapShots (..)
  , StakePoolSnapShot (..)
  , certDStateL
  , emptySnapShot
  , accountsL
  , maxPool'
  )
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Cardano.Crypto.Hash (hashToBytes)
import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.VMap as VMap
import Data.Word (Word16, Word32, Word64)
import Dxdingo.Domain
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Stake (credentialOf, credentialProto, delegatedRows, poolHashOf, snapShotOf)
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Data.Text (Text)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Rewards
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

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

-- Reward update. The reference types are era-indexed; every era in the
-- supported range runs the same Shelley reward code, which reads only the
-- protocol version, so the state is a Shelley-era one with the request's
-- protocol version set.

minRewardMajor, maxRewardMajor, decentralizationMajorLimit :: Word32
minRewardMajor = 2
maxRewardMajor = 11
decentralizationMajorLimit = 6

-- | The pparams the reward functions read, after the domain checks shared
-- with dxnode-go. rho and tau are required only when @needMonetary@ is set.
-- From Babbage on the ledger reads d as zero, so the request's d is ignored.
rewardPParams ::
  Maybe P.ProtocolVersion ->
  Maybe S.ProtocolParameters ->
  Bool ->
  Result (Word32, PParams ShelleyEra)
rewardPParams mpv mpp needMonetary = do
  let major = maybe 0 (^. #major) mpv
      pp = maybe defMessage id mpp
  when (major < minRewardMajor || major > maxRewardMajor) $
    unsupported "protocol major outside [2, 11]"
  version <- maybe (internal "protocol version") pure (mkVersion major)
  a0 <- boundedArg "a0" (pp ^. #maybe'a0) :: Result NonNegativeInterval
  nOpt <- case pp ^. #maybe'nOpt of
    Just n | n >= 1 && n <= 0xffff -> pure (fromIntegral n :: Word16)
    _ -> unsupported "n_opt must be in [1, 65535]"
  let zero = maybe (internal "zero") pure (boundRational 0) :: Result UnitInterval
  (rho, tau) <-
    if needMonetary
      then (,) <$> unitIntervalArg "rho" (pp ^. #maybe'rho) <*> unitIntervalArg "tau" (pp ^. #maybe'tau)
      else (,) <$> zero <*> zero
  d <- if major <= decentralizationMajorLimit then unitIntervalArg "d" (pp ^. #maybe'd) else zero
  pure
    ( major
    , (emptyPParams :: PParams ShelleyEra)
        & ppA0L .~ a0
        & ppNOptL .~ nOpt
        & ppRhoL .~ rho
        & ppTauL .~ tau
        & ppDL .~ d
        & ppProtocolVersionL .~ ProtVer version 0
    )

-- | What the reward update reads of the request's globals.
data RewardGlobals = RewardGlobals
  { rgGlobals :: Globals
  , rgAsc :: ActiveSlotCoeff
  , rgAscRational :: Rational
  , rgEpochLength :: Word64
  , rgSecurity :: NonZero Word64
  , rgSupply :: Word64
  }

rewardGlobals :: Maybe S.Globals -> Result RewardGlobals
rewardGlobals mg = do
  let g = maybe defMessage id mg
  ascP <- boundedArg "active_slot_coeff" (g ^. #maybe'activeSlotCoeff)
  asc <- pure (mkActiveSlotCoeff ascP)
  when (g ^. #epochLength == 0) $ unsupported "epoch_length and security_param must be nonzero"
  k <- maybe (unsupported "epoch_length and security_param must be nonzero") pure (nonZero (g ^. #securityParam))
  pure
    RewardGlobals
      { rgGlobals =
          Globals
            { epochInfo = fixedEpochInfo (EpochSize (g ^. #epochLength)) (mkSlotLength 1)
            , slotsPerKESPeriod = 129600
            , stabilityWindow = 0
            , randomnessStabilisationWindow = 0
            , securityParameter = k
            , maxKESEvo = 62
            , quorum = 5
            , maxLovelaceSupply = g ^. #maxLovelaceSupply
            , activeSlotCoeff = asc
            , networkId = Testnet
            , systemStart = SystemStart (posixSecondsToUTCTime 0)
            }
      , rgAsc = asc
      , rgAscRational = unboundRational ascP
      , rgEpochLength = g ^. #epochLength
      , rgSecurity = k
      , rgSupply = g ^. #maxLovelaceSupply
      }

-- | With d < 4/5 the ledger divides the blocks made by
-- floor ((1 - d) * f * epoch length), a zero division when that is zero.
expectedBlocksIsZero :: PParams ShelleyEra -> RewardGlobals -> Bool
expectedBlocksIsZero pp g =
  d < 4 % 5 && (floor ((1 - d) * rgAscRational g * fromIntegral (rgEpochLength g)) :: Integer) == 0
  where
    d = unboundRational (pp ^. ppDG)

-- | A Shelley-era epoch state holding what the reward update reads: pots,
-- fees, the GO snapshot, the previous pparams and the registered accounts.
epochStateOf ::
  PParams ShelleyEra -> S.Pots -> Coin -> SnapShot -> [Credential Staking] -> EpochState ShelleyEra
epochStateOf pp pots fees go registered =
  (def :: EpochState ShelleyEra)
    { esChainAccountState =
        ChainAccountState
          { casTreasury = Coin (toInteger (pots ^. #treasury))
          , casReserves = Coin (toInteger (pots ^. #reserves))
          }
    , esSnapshots = (def :: SnapShots ShelleyEra) {ssStakeGo = go, ssFee = fees}
    }
    & prevPParamsEpochStateL .~ pp
    & esLStateL . lsUTxOStateL . utxosFeesL .~ fees
    & esLStateL . lsCertStateL . certDStateL . accountsL
      .~ ShelleyAccounts
        { saStates = Map.fromList [(c, ShelleyAccountState (mkPtrNormalized 0 0 0) (CompactCoin 0) (CompactCoin 0) SNothing) | c <- registered]
        , saPtrs = Map.empty
        }

registeredOf :: [P.Credential] -> Result [Credential Staking]
registeredOf = mapM (credentialOf "registered")

-- | The reward pot arithmetic of 'startStep'.
monetaryExpansion ::
  RewardsMonetaryExpansionRequest -> Result RewardsMonetaryExpansionResponse
monetaryExpansion req = do
  (_, pp) <- rewardPParams (req ^. #maybe'protocolVersion) (req ^. #maybe'pparams) True
  g <- rewardGlobals (req ^. #maybe'globals)
  when (req ^. #reserves > rgSupply g) $ unsupported "reserves exceed max_lovelace_supply"
  when (expectedBlocksIsZero pp g) $ unsupported "expected blocks per epoch is zero"
  dummy <- poolHashOf "pool" (BS.replicate 28 0)
  let pots = defMessage & #reserves .~ (req ^. #reserves)
      es = epochStateOf pp pots (Coin (toInteger (req ^. #fees))) emptySnapShot []
      blocks = BlocksMade (Map.singleton dummy (fromIntegral (req ^. #blocksTotal)))
  case startStep (EpochSize (rgEpochLength g)) blocks es (Coin (toInteger (rgSupply g))) (rgAsc g) (rgSecurity g) of
    Pulsing snap _ -> do
      let Coin r1 = rewDeltaR1 snap
          Coin cut = rewDeltaT1 snap
          Coin pool = rewR snap
      d1 <- coinWord64 (Coin r1)
      c <- coinWord64 (Coin cut)
      p <- coinWord64 (Coin pool)
      pot <- coinWord64 (Coin (r1 + toInteger (req ^. #fees)))
      pure (defMessage & #deltaR1 .~ d1 & #rewardPot .~ pot & #treasuryCut .~ c & #poolPot .~ p)
    Complete _ -> internal "startStep completed without pulsing"

-- | 'mkPoolRewardInfo' for the pool, then 'rewardOnePoolMember' for each
-- delegator. A zero reward is not listed.
poolReward :: RewardsPoolRewardRequest -> Result RewardsPoolRewardResponse
poolReward req = do
  (major, pp) <- rewardPParams (req ^. #maybe'protocolVersion) (req ^. #maybe'pparams) False
  when (major < 3) $ unsupported "pool_reward needs protocol major >= 3"
  totalStake <- nonzero "total_stake" (req ^. #totalStake)
  totalActive <- nonzero "total_active_stake" (req ^. #totalActiveStake)
  pool <- maybe (badRequest "pool: missing") pure (req ^. #maybe'pool)
  kh <- poolHashOf "pool_id" (pool ^. #poolId)
  rows <-
    delegatedRows "members"
      [(m ^. #credential, m ^. #amount, pool ^. #poolId) | m <- req ^. #members]
  _ <- unitIntervalArg "margin" (pool ^. #maybe'margin)
  registered <- Set.fromList <$> registeredOf (req ^. #registered)
  account <- maybe (badRequest "reward_account: missing") (credentialOf "reward_account") (pool ^. #maybe'rewardAccount)
  ss <- snapShotOf rows [pool]
  spss <- maybe (internal "pool snapshot missing") pure (VMap.lookup kh (ssStakePoolsSnapShot ss))
  let blocks = BlocksMade (if req ^. #blocks == 0 then Map.empty else Map.singleton kh (fromIntegral (req ^. #blocks)))
      pv = pp ^. ppProtocolVersionL
  case mkPoolRewardInfo
    pp
    (Coin (toInteger (req ^. #poolPot)))
    blocks
    (fromIntegral (req ^. #blocksTotal))
    (Coin totalStake)
    (unsafeNonZero (Coin totalActive))
    kh
    spss of
    Left _ -> pure defMessage
    Right info -> do
      let LeaderOnlyReward _ leader = poolLeaderReward info
          perf =
            mkApparentPerformance
              (pp ^. ppDG)
              (toInteger (sum [a | (_, a, _) <- rows]) % totalActive)
              (fromIntegral (req ^. #blocks))
              (fromIntegral (req ^. #blocksTotal))
          leaderRewards =
            [ reward account kh LeaderReward leader
            | leader > Coin 0
            , hardforkBabbageForgoRewardPrefilter pv || account `Set.member` registered
            ]
          memberRewards =
            [ reward c kh MemberReward r
            | (c, a, _) <- rows
            , Just r <- [rewardOnePoolMember pv (Coin totalStake) registered info c (Coin (toInteger a))]
            ]
      poolR <- coinWord64 (poolPot info)
      leaderR <- coinWord64 leader
      pure $
        defMessage
          & #poolReward .~ poolR
          & #leaderReward .~ leaderR
          & #performance .~ rationalProto perf
          & #rewards .~ sortRewards (leaderRewards ++ memberRewards)

reward :: Credential Staking -> KeyHash StakePool -> RewardType -> Coin -> (Credential Staking, Reward)
reward c kh ty amount = (c, Reward ty kh amount)

-- | Canonical order: credential kind then hash, pool, kind (leader first),
-- amount. Zero rewards are dropped.
sortRewards :: [(Credential Staking, Reward)] -> [S.Reward]
sortRewards rs =
  map snd . sortOn fst $
    [ ( (kindRank, BS.unpack (cp ^. #hash), BS.unpack (hashToBytes' h), if ty == LeaderReward then 1 else 2 :: Int, a)
      , defMessage
          & #credential .~ cp
          & #poolId .~ hashToBytes' h
          & #kind .~ (if ty == LeaderReward then S.REWARD_KIND_LEADER else S.REWARD_KIND_MEMBER)
          & #amount .~ fromInteger a
      )
    | (c, Reward ty (KeyHash h) (Coin a)) <- rs
    , a > 0
    , let cp = credentialProto c
    , let kindRank = fromEnum (cp ^. #kind)
    ]
  where
    hashToBytes' = hashToBytes

-- | The update 'createRUpd' builds, restricted to the rewards the ledger
-- delivers ('filterRewards').
rewardUpdate :: RewardsRewardUpdateRequest -> Result RewardsRewardUpdateResponse
rewardUpdate req = snd <$> rewardUpdateWith req

-- | 'rewardUpdate' with the unfiltered update the ledger built, as the
-- reward update a NEWEPOCH state holds.
rewardUpdateWith :: RewardsRewardUpdateRequest -> Result (LS.RewardUpdate, RewardsRewardUpdateResponse)
rewardUpdateWith req = do
  (_, pp) <- rewardPParams (req ^. #maybe'protocolVersion) (req ^. #maybe'pparams) True
  g <- rewardGlobals (req ^. #maybe'globals)
  let pots = req ^. #pots
  when (pots ^. #reserves > rgSupply g) $ unsupported "reserves exceed max_lovelace_supply"
  when (expectedBlocksIsZero pp g) $ unsupported "expected blocks per epoch is zero"
  registered <- registeredOf (req ^. #registered)
  blocks <- foldM' (req ^. #blocks)
  poolIds <- mapM (poolHashOf "pools" . (^. #poolId)) (filter (\p -> p ^. #stage /= S.POOL_STAGE_FUTURE) (req ^. #pools))
  unless (Set.size (Set.fromList poolIds) == length poolIds) $ badRequest "pools: repeated pool"
  unless (all (`Set.member` Set.fromList poolIds) (Map.keys blocks)) $
    unsupported "blocks: pool without a registration"
  rows <-
    delegatedRows "snapshots"
      [ (r ^. #credential, r ^. #amount, r ^. #poolId)
      | r <- req ^. #snapshots
      , r ^. #snapshot == S.SNAPSHOT_KIND_GO
      ]
  ss <- snapShotOf rows (req ^. #pools)
  let fees = Coin (toInteger (pots ^. #fees))
      es = epochStateOf pp pots fees ss registered
      ru =
        runIdentity . (`runReaderT` rgGlobals g) $
          createRUpd (EpochSize (rgEpochLength g)) (BlocksMade blocks) es (Coin (toInteger (rgSupply g))) (rgAsc g) (rgSecurity g)
      (delivered, _) = filterRewards (pp ^. ppProtocolVersionL) (rs ru)
      DeltaCoin dr = deltaR ru
      DeltaCoin dt = deltaT ru
      DeltaCoin df = deltaF ru
  when (any (\x -> abs x > toInteger (maxBound :: Int64)) [dr, dt, df]) $ unsupported "pot amounts exceed Int64"
  pure . (,) ru $
    defMessage
      & #update
        .~ ( defMessage
               & #deltaR .~ fromInteger dr
               & #deltaT .~ fromInteger dt
               & #deltaF .~ fromInteger df
               & #rewards .~ sortRewards [(c, r) | (c, set) <- Map.toList delivered, r <- Set.toList set]
           )
  where
    foldM' bs = do
      ids <- mapM (\b -> (,) <$> poolHashOf "blocks" (b ^. #poolId) <*> pure (fromIntegral (b ^. #count))) bs
      unless (Set.size (Set.fromList (map fst ids)) == length ids) $ badRequest "blocks: repeated pool"
      pure (Map.fromList ids)

-- | 'applyRUpdFiltered' on an epoch state holding the pots, the registered
-- accounts and the update.
applyUpdate :: RewardsApplyUpdateRequest -> Result RewardsApplyUpdateResponse
applyUpdate req = do
  let pvMsg = req ^. #maybe'protocolVersion
      major = maybe 0 (^. #major) pvMsg
  when (major < minRewardMajor || major > maxRewardMajor) $ unsupported "protocol major outside [2, 11]"
  when (major < 3) $ unsupported "apply_update needs protocol major >= 3"
  tau <- unitIntervalArg "treasury_expansion" (req ^. #maybe'treasuryExpansion)
  let up = req ^. #update
      pots = req ^. #pots
      pot = toInteger (req ^. #totalRewardPot)
      fees = negate (toInteger (up ^. #deltaF))
  when (floor (unboundRational tau * fromIntegral pot) /= toInteger (up ^. #deltaT)) $
    unsupported "delta_t differs from floor(treasury_expansion * total_reward_pot)"
  when (up ^. #deltaF > 0 || fees > pot || fees > toInteger (pots ^. #fees)) $
    unsupported "delta_f must spend fees the pots hold and the pot covers"
  registered <- registeredOf (req ^. #registered)
  rs' <- mapM rewardOf (up ^. #rewards)
  version <- maybe (internal "protocol version") pure (mkVersion major)
  let pp = (emptyPParams :: PParams ShelleyEra) & ppProtocolVersionL .~ ProtVer version 0
      es = epochStateOf pp pots (Coin (toInteger (pots ^. #fees))) emptySnapShot registered
      ru =
        RewardUpdate
          { deltaT = DeltaCoin (toInteger (up ^. #deltaT))
          , deltaR = DeltaCoin (toInteger (up ^. #deltaR))
          , rs = Map.fromListWith Set.union [(c, Set.singleton r) | (c, r) <- rs']
          , deltaF = DeltaCoin (toInteger (up ^. #deltaF))
          , nonMyopic = def
          }
      (es', filtered) = applyRUpdFiltered ru es
      ChainAccountState (Coin treasury) (Coin reserves) = esChainAccountState es'
      Coin fees' = es' ^. esLStateL . lsUTxOStateL . utxosFeesL
      balances = es' ^. esLStateL . lsCertStateL . certDStateL . accountsL
  reservesW <- coinWord64 (Coin reserves)
  treasuryW <- coinWord64 (Coin treasury)
  feesW <- coinWord64 (Coin fees')
  unregistered <- coinWord64 (frTotalUnregistered filtered)
  pure $
    defMessage
      & #pots
        .~ ( defMessage & #reserves .~ reservesW & #treasury .~ treasuryW & #fees .~ feesW
               & #donations .~ (pots ^. #donations) & #deposits .~ (pots ^. #deposits)
           )
      & #credited
        .~ map snd
          ( sortOn fst
              [ ((fromEnum (cp ^. #kind), BS.unpack (cp ^. #hash)), defMessage & #credential .~ cp & #amount .~ b)
              | (c, ShelleyAccountState _ (CompactCoin b) _ _) <- Map.toAscList (saStates balances)
              , b > 0
              , let cp = credentialProto c
              ]
          )
      & #unregisteredToTreasury .~ unregistered
  where
    rewardOf r = do
      c <- maybe (badRequest "update.rewards: credential missing") (credentialOf "update.rewards") (r ^. #maybe'credential)
      kh <- poolHashOf "update.rewards" (r ^. #poolId)
      let ty = if r ^. #kind == S.REWARD_KIND_LEADER then LeaderReward else MemberReward
      pure (c, Reward ty kh (Coin (toInteger (r ^. #amount))))

-- | 'getTopRankedPools' and 'calcNonMyopicMemberReward' over pool snapshots
-- built from the request, as 'getNonMyopicMemberRewards' does. A pool in
-- @performance@ has a likelihood history and ranks with that estimate; the
-- others use the estimate of an empty history. Pledge is checked against the
-- owners' delegated stake.
nonMyopicRewards :: RewardsNonMyopicRequest -> Result RewardsNonMyopicResponse
nonMyopicRewards req = do
  let pp0 = maybe defMessage id (req ^. #maybe'pparams)
      major = maybe 0 (^. #major) (pp0 ^. #maybe'protocolVersion)
  a0 <- boundedArg "a0" (pp0 ^. #maybe'a0) :: Result NonNegativeInterval
  nOpt <- case pp0 ^. #maybe'nOpt of
    Just n | n >= 1 && n <= 0xffff -> pure (fromIntegral n :: Word16)
    _ -> unsupported "n_opt must be in [1, 65535]"
  when (major >= 12) $ unsupported "protocol major reads a minimum pool cost"
  total <- nonzero "total_stake" (req ^. #totalStake)
  stakes <- poolValues "pool_distribution" [(x ^. #poolId, x ^. #stake) | x <- req ^. #poolDistribution]
  owners <- poolValues "owner_stake" [(x ^. #poolId, x ^. #amount) | x <- req ^. #ownerStake]
  perfs <- do
    xs <- mapM perfOf (req ^. #performance)
    unless (Set.size (Set.fromList (map fst xs)) == length xs) $ badRequest "performance: repeated pool"
    pure (Map.fromList xs)
  dummyHash <- maybe (internal "hash") (pure . KeyHash) (hashFromBytes (BS.replicate 28 0))
  dummyVrf <- maybe (internal "vrf") pure (hashFromBytes (BS.replicate 32 0))
  snaps <- mapM (snapOf dummyHash dummyVrf stakes owners) (filter (\p -> p ^. #stage /= S.POOL_STAGE_FUTURE) (req ^. #pools))
  let snapshots = Map.fromList snaps
  unless (Map.size snapshots == length snaps) $ badRequest "pools: repeated pool"
  let pp = (emptyPParams :: PParams ShelleyEra) & ppA0L .~ a0 & ppNOptL .~ nOpt
      pot = Coin (toInteger (req ^. #rewardPot))
      toShare (Coin x) = StakeShare (x % total)
      top = getTopRankedPools pot (Coin total) pp (Map.intersectionWith (,) perfs snapshots)
      member = toShare (Coin (toInteger (req ^. #memberStake)))
      rewardOf kh spss
        | spssPledge spss <= spssSelfDelegatedOwnersStake spss =
            calcNonMyopicMemberReward
              pp
              pot
              kh
              (spssCost spss)
              (spssMargin spss)
              (toShare (spssPledge spss))
              (toShare (fromCompact' (spssStake spss)))
              member
              top
              (Map.findWithDefault (percentile' mempty) kh perfs)
        | otherwise = Coin 0
  rewards' <- sequence [(,) h <$> coinWord64 (rewardOf kh spss) | (kh@(KeyHash h), spss) <- Map.toList snapshots]
  pure $
    defMessage
      & #rewards
        .~ [ defMessage & #poolId .~ hashToBytes h & #reward .~ r
           | (h, r) <- sortOn (hashToBytes . fst) rewards'
           ]
  where
    fromCompact' (CompactCoin w) = Coin (toInteger w)
    perfOf p = do
      kh <- poolHashOf "performance" (p ^. #poolId)
      r <- rationalArg "performance" (p ^. #maybe'performance)
      when (r < 0) $ unsupported "performance is negative"
      pure (kh, PerformanceEstimate (fromRational r))
    snapOf dummyHash dummyVrf stakes owners p = do
      kh <- poolHashOf "pools" (p ^. #poolId)
      margin <- unitIntervalArg "margin" (p ^. #maybe'margin)
      let stake = Map.findWithDefault 0 (p ^. #poolId) stakes
          ownerStake = Map.findWithDefault 0 (p ^. #poolId) owners
      pure
        ( kh
        , StakePoolSnapShot
            { spssStake = CompactCoin stake
            , spssStakeRatio = 0
            , spssSelfDelegatedOwners = Set.empty
            , spssSelfDelegatedOwnersStake = Coin (toInteger ownerStake)
            , spssVrf = VRFVerKeyHash dummyVrf
            , spssPledge = Coin (toInteger (p ^. #pledge))
            , spssCost = Coin (toInteger (p ^. #cost))
            , spssMargin = margin
            , spssNumDelegators = 1
            , spssAccountId = AccountId (KeyHashObj dummyHash)
            }
        )
    poolValues name xs = do
      ys <- mapM (\(i, a) -> (,) <$> (poolHashOf name i >> pure i) <*> pure a) xs
      unless (Set.size (Set.fromList (map fst ys)) == length ys) $ badRequest (name <> ": repeated pool")
      pure (Map.fromList ys)

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'RewardsNonMyopic q -> Just (respond #rewardsNonMyopic (nonMyopicRewards q))
  Call'RewardsMonetaryExpansion q -> Just (respond #rewardsMonetaryExpansion (monetaryExpansion q))
  Call'RewardsPoolReward q -> Just (respond #rewardsPoolReward (poolReward q))
  Call'RewardsRewardUpdate q -> Just (respond #rewardsRewardUpdate (rewardUpdate q))
  Call'RewardsApplyUpdate q -> Just (respond #rewardsApplyUpdate (applyUpdate q))
  Call'RewardsMemberReward q -> Just (respond #rewardsMemberReward (memberReward q))
  Call'RewardsLeaderReward q -> Just (respond #rewardsLeaderReward (leaderReward q))
  Call'RewardsMaxPool q -> Just (respond #rewardsMaxPool (maxPool q))
  Call'RewardsApparentPerformance q -> Just (respond #rewardsApparentPerformance (apparentPerformance q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "rewards_member_reward"
  , "rewards_leader_reward"
  , "rewards_max_pool"
  , "rewards_apparent_performance"
  , "rewards_monetary_expansion"
  , "rewards_pool_reward"
  , "rewards_reward_update"
  , "rewards_apply_update"
  , "rewards_non_myopic"
  ]
