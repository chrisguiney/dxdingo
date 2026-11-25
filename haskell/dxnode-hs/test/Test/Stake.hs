{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Stake (tests) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (computeDRepDistr)
import Cardano.Ledger.Conway.State (ConwayInstantStake (..), pattern ConwayAccountState)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRepState (..), pattern DRepCredential)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Word (Word64, Word8)
import Lens.Micro ((%~), (&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import qualified Proto.Dxdingo.V1.Stake as K
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Test.Util

-- Same cases as dxnode-go (stake_test.go).

keyCred :: Word8 -> P.Credential
keyCred b = defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ BS.replicate 28 b

poolId :: Word8 -> BS.ByteString
poolId = BS.replicate 28

snapRow :: P.Credential -> Word64 -> BS.ByteString -> S.SnapshotEntry
snapRow c a p = defMessage & #snapshot .~ S.SNAPSHOT_KIND_MARK & #credential .~ c & #amount .~ a & #poolId .~ p

stakePool :: Word8 -> S.PoolStage -> S.Pool
stakePool i stage =
  defMessage
    & #poolId .~ poolId i
    & #stage .~ stage
    & #vrfKeyHash .~ BS.replicate 32 i
    & #margin .~ rat ("1", "10")
    & #rewardAccount .~ keyCred 0xee
    & #cost .~ 1

-- | Pool A holds 30 + 20 = 50 of an active stake of 100, pool B 25, and 25 is
-- delegated to Z, which has no registration: it counts in the total but Z is
-- not in the distribution. Pool C has no delegators and E is a future
-- registration. The undelegated row is ignored.
distrReq :: K.StakePoolDistributionRequest
distrReq =
  defMessage
    & #snapshot
      .~ [ snapRow (keyCred 0x01) 30 (poolId 0xa1)
         , snapRow (keyCred 0x02) 20 (poolId 0xa1)
         , snapRow (keyCred 0x03) 25 (poolId 0xb2)
         , snapRow (keyCred 0x04) 25 (poolId 0xff)
         , snapRow (keyCred 0x05) 999 BS.empty
         ]
    & #pools
      .~ [ stakePool 0xa1 S.POOL_STAGE_CURRENT
         , stakePool 0xb2 S.POOL_STAGE_CURRENT
         , stakePool 0xc3 S.POOL_STAGE_CURRENT
         , stakePool 0xe5 S.POOL_STAGE_FUTURE
         ]

distrOf :: K.StakePoolDistributionRequest -> K.StakePoolDistributionResponse
distrOf q = call (#stakePoolDistribution .~ q) ^. #stakePoolDistribution

distrCode :: (K.StakePoolDistributionRequest -> K.StakePoolDistributionRequest) -> HarnessErrorCode
distrCode f = call (#stakePoolDistribution .~ f distrReq) ^. #error . #code

drepOf :: P.Credential -> P.DRep
drepOf c = defMessage & #credential .~ c

-- | Same case as TestDRepDistributionRunsDingoSQL. A DRep's stake is the
-- instant stake plus the reward balance of the accounts delegated to it,
-- plus the deposits of active proposals whose return account delegates to it.
drepReq :: K.StakeDRepDistributionRequest
drepReq =
  defMessage
    & #epoch .~ 10
    & #instantStake
      .~ [ defMessage & #credential .~ keyCred 0x01 & #amount .~ 10
         , defMessage & #credential .~ keyCred 0x02 & #amount .~ 7
         , defMessage & #credential .~ keyCred 0x03 & #amount .~ 1000
         ]
    & #accounts
      .~ [ defMessage & #credential .~ keyCred 0x01 & #balance .~ 5 & #drep .~ drepOf (keyCred 0xa0)
         , defMessage & #credential .~ keyCred 0x02 & #drep .~ (defMessage & #alwaysAbstain .~ True)
         , defMessage & #credential .~ keyCred 0x03 & #balance .~ 1
         ]
    & #dreps .~ [defMessage & #credential .~ keyCred 0xa0 & #expiry .~ 20]
    & #proposals
      .~ [ defMessage & #deposit .~ 100 & #returnAccount .~ keyCred 0x01 & #proposedEpoch .~ 9 & #expiresEpoch .~ 15
         ]

drepOfReq :: K.StakeDRepDistributionRequest -> [(Maybe P.Credential, Bool, Word64)]
drepOfReq q =
  [ (d ^. #drep . #maybe'credential, d ^. #drep . #alwaysAbstain, d ^. #stake)
  | d <- (call (#stakeDrepDistribution .~ q) ^. #stakeDrepDistribution) ^. #dreps
  ]

-- CBOR of a small unsigned integer.
cborUint :: Word64 -> BS.ByteString
cborUint n
  | n < 24 = BS.pack [fromIntegral n]
  | n < 256 = BS.pack [0x18, fromIntegral n]
  | n < 65536 = BS.pack [0x19, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | otherwise = error "cborUint"

-- | A Shelley output [address, coin]; header is the address type byte.
shelleyOutput :: Word8 -> BS.ByteString -> Word64 -> BS.ByteString
shelleyOutput header hashes amount =
  let addr = BS.cons header hashes
   in BS.concat [BS.pack [0x82, 0x58, fromIntegral (BS.length addr)], addr, cborUint amount]

baseOut :: Word8 -> Word8 -> Word64 -> S.UtxoEntry
baseOut header stake amount =
  defMessage
    & #txin .~ (defMessage & #txId .~ BS.replicate 32 stake & #index .~ fromIntegral amount)
    & #txout .~ shelleyOutput header (BS.replicate 28 0x70 <> BS.replicate 28 stake) amount

-- | Same case as TestInstantStake in dxnode-go.
instantReq :: K.StakeInstantStakeRequest
instantReq =
  defMessage
    & #utxo
      .~ [ baseOut 0x00 0x01 10
         , baseOut 0x00 0x01 5
         , baseOut 0x00 0x02 7
         , baseOut 0x20 0x53 9
         , defMessage
             & #txin .~ (defMessage & #txId .~ BS.replicate 32 0xee)
             & #txout .~ shelleyOutput 0x60 (BS.replicate 28 0x70) 100
         , baseOut 0x00 0x03 0
         ]

instantOf :: K.StakeInstantStakeRequest -> K.StakeInstantStakeResponse
instantOf q = call (#stakeInstantStake .~ q) ^. #stakeInstantStake

-- | Same case as TestStakeSnapshot in dxnode-go: K1 15 to A; K2 7 + 3 to B;
-- K3 2 to A (balance only); K4 has no pool, K5 is not registered and K6
-- holds nothing.
snapshotReq :: K.StakeSnapshotRequest
snapshotReq =
  defMessage
    & #instantStake
      .~ [ defMessage & #credential .~ keyCred 0x01 & #amount .~ 10
         , defMessage & #credential .~ keyCred 0x01 & #amount .~ 5
         , defMessage & #credential .~ keyCred 0x02 & #amount .~ 7
         , defMessage & #credential .~ keyCred 0x04 & #amount .~ 11
         , defMessage & #credential .~ keyCred 0x05 & #amount .~ 50
         ]
    & #accounts
      .~ [ defMessage & #credential .~ keyCred 0x01 & #poolId .~ poolId 0xa1
         , defMessage & #credential .~ keyCred 0x02 & #balance .~ 3 & #poolId .~ poolId 0xb2
         , defMessage & #credential .~ keyCred 0x03 & #balance .~ 2 & #poolId .~ poolId 0xa1
         , defMessage & #credential .~ keyCred 0x04
         , defMessage & #credential .~ keyCred 0x06 & #poolId .~ poolId 0xa1
         ]
    & #pools .~ [stakePool 0xa1 S.POOL_STAGE_CURRENT]

snapshotOf :: K.StakeSnapshotRequest -> K.StakeSnapshotResponse
snapshotOf q = call (#stakeSnapshot .~ q) ^. #stakeSnapshot

snapshotCode :: (K.StakeSnapshotRequest -> K.StakeSnapshotRequest) -> HarnessErrorCode
snapshotCode f = call (#stakeSnapshot .~ f snapshotReq) ^. #error . #code

instantCode :: (K.StakeInstantStakeRequest -> K.StakeInstantStakeRequest) -> HarnessErrorCode
instantCode f = call (#stakeInstantStake .~ f instantReq) ^. #error . #code

-- | Divergence, bypassing the adapter: computeDRepDistr keeps the entry of a
-- registered DRep whose only delegator holds nothing, with stake 0. dingo
-- omits it (cmd/dxnode-go/divergence_stake_test.go).
zeroStakeDRepEntryIsKept :: Bool
zeroStakeDRepEntryIsKept =
  case (hashFromBytes (BS.replicate 28 0xa2), hashFromBytes (BS.replicate 28 5)) of
    (Just d, Just c) ->
      let drepC = KeyHashObj (KeyHash d) :: Credential DRepRole
          account = KeyHashObj (KeyHash c) :: Credential Staking
          state = DRepState (EpochNo 20) SNothing (CompactCoin 0) Set.empty
          (distr, _) =
            computeDRepDistr
              (ConwayInstantStake Map.empty :: ConwayInstantStake ConwayEra)
              (Map.singleton drepC state)
              Map.empty
              def
              Map.empty
              (Map.singleton account (ConwayAccountState (CompactCoin 0) (CompactCoin 0) Nothing (Just (DRepCredential drepC))))
       in Map.lookup (DRepCredential drepC) distr == Just (CompactCoin 0)
    _ -> False

tests :: [Check]
tests =
  [ ( "instant stake: credentials by kind then hash, zero and enterprise outputs ignored"
    , [ (e ^. #credential . #kind, BS.head (e ^. #credential . #hash), e ^. #amount) | e <- instantOf instantReq ^. #stake ]
        == [(P.CREDENTIAL_KIND_KEY, 0x01, 15), (P.CREDENTIAL_KIND_KEY, 0x02, 7), (P.CREDENTIAL_KIND_SCRIPT, 0x53, 9)]
    )
  , ( "instant stake: an undecodable output is a DeserialiseFailure rejection"
    , let r = call (#stakeInstantStake .~ (instantReq & #utxo . ix0 . #txout .~ BS.pack [0xff])) ^. #stakeInstantStake
       in [(f ^. #rule, f ^. #name) | f <- r ^. #rejection . #failures] == [("instant-stake", "DeserialiseFailure")]
    )
  , ("instant stake: a repeated input is BAD_REQUEST", instantCode (#utxo . ix1 . #txin .~ (baseOut 0 1 10 ^. #txin)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ( "snapshot: active stake of registered delegated credentials"
    , let r = snapshotOf snapshotReq
       in r ^. #total == 27
            && [ (BS.head (e ^. #credential . #hash), e ^. #amount, e ^. #poolId) | e <- r ^. #snapshot ]
              == [(0x01, 15, poolId 0xa1), (0x02, 10, poolId 0xb2), (0x03, 2, poolId 0xa1)]
    )
  , ("snapshot: no active stake is UNSUPPORTED", snapshotCode (#accounts .~ [defMessage & #credential .~ keyCred 0x04]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("snapshot: a repeated account is BAD_REQUEST", snapshotCode (#accounts %~ (\as -> as ++ take 1 as)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot: a short pool id is BAD_REQUEST", snapshotCode (#accounts . ix0 . #poolId .~ BS.pack [1]) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("divergence: drep distribution lists a zero-stake DRep", zeroStakeDRepEntryIsKept)
  , ( "drep distribution: stake, balance and active proposal deposit"
    , drepOfReq drepReq == [(Just (keyCred 0xa0), False, 115), (Nothing, True, 7)]
    )
  , ( "drep distribution: an expired proposal deposit is not counted"
    , drepOfReq (drepReq & #proposals . ix0 . #expiresEpoch .~ 9) == [(Just (keyCred 0xa0), False, 15), (Nothing, True, 7)]
    )
  , ( "drep distribution: a deposit in the last active epoch is counted"
    , drepOfReq (drepReq & #proposals . ix0 . #expiresEpoch .~ 10) == [(Just (keyCred 0xa0), False, 115), (Nothing, True, 7)]
    )
  , ( "drep distribution: delegation to an unregistered DRep counts for nobody"
    , drepOfReq (drepReq & #dreps .~ []) == [(Nothing, True, 7)]
    )
  , ( "pool distribution: pools, shares and total active stake"
    , let r = distrOf distrReq
       in case r ^. #pools of
            [a, b] ->
              r ^. #totalActive == 100
                && a ^. #poolId == poolId 0xa1 && a ^. #stake == 50 && ratIs ("1", "2") (a ^. #share)
                && a ^. #vrfKeyHash == BS.replicate 32 0xa1
                && b ^. #poolId == poolId 0xb2 && b ^. #stake == 25 && ratIs ("1", "4") (b ^. #share)
            _ -> False
    )
  , ("pool distribution: no delegated stake is UNSUPPORTED", distrCode (#snapshot .~ [snapRow (keyCred 1) 5 BS.empty]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("pool distribution: zero stake is UNSUPPORTED", distrCode (#snapshot . ix0 . #amount .~ 0) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("pool distribution: repeated credential is BAD_REQUEST", distrCode (#snapshot . ix1 . #credential .~ keyCred 0x01) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("pool distribution: repeated pool is BAD_REQUEST", distrCode (#pools %~ (stakePool 0xa1 S.POOL_STAGE_CURRENT :)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("pool distribution: short pool id is BAD_REQUEST", distrCode (#snapshot . ix0 . #poolId .~ BS.pack [1]) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("pool distribution: margin above one is UNSUPPORTED", distrCode (#pools . ix0 . #margin .~ rat ("3", "2")) == HARNESS_ERROR_CODE_UNSUPPORTED)
  ]
  where
    ix0 f (x : xs) = (: xs) <$> f x
    ix0 _ [] = pure []
    ix1 f (x : y : xs) = (\y' -> x : y' : xs) <$> f y
    ix1 _ xs = pure xs
