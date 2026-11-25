{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stake functions, answered by the stake snapshot code in
-- cardano-ledger-core. The snapshot helpers are shared with "Dxdingo.Rewards".
module Dxdingo.Stake
  ( dispatch
  , implemented
  , credentialOf
  , credentialProto
  , poolHashOf
  , poolStateOf
  , delegatedRows
  , snapShotOf
  , currentPools
  ) where

import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.Address (AccountId (..))
import Cardano.Ledger.BaseTypes (NonZero (..), StrictMaybe (..), unsafeNonZero)
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Default (def)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (ScriptHash (..), VRFVerKeyHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.State
  ( ActiveStake (..)
  , IndividualPoolStake (..)
  , PoolDistr (..)
  , SnapShot (..)
  , StakePoolState (..)
  , StakeWithDelegation (..)
  , calculatePoolDistr
  , mkSnapShot
  , resetStakePoolsSnapShot
  )
import Cardano.Ledger.Binary (decodeFull')
import Cardano.Ledger.Conway.Governance (computeDRepDistr)
import Cardano.Ledger.Core (TxOut, eraProtVerLow)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.State (ShelleyAccountState (..), ShelleyAccounts (..), ShelleyInstantStake (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Credential (mkPtrNormalized)
import Cardano.Ledger.State (DState (..), PState (..), UTxO (..), addInstantStake, snapShotFromInstantStake, ssActiveStake, ssTotalActiveStake)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.BaseTypes (mkTxIxPartial)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (ConwayInstantStake (..), pattern ConwayAccountState)
import Cardano.Ledger.DRep (DRep (..), DRepState (..), pattern DRepCredential)
import Control.Monad (foldM, forM, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.VMap as VMap
import Data.Word (Word64)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.State as S
import Proto.Dxdingo.V1.Stake
import qualified Proto.Dxdingo.V1.Types as P


hashBytes :: Text -> Int -> ByteString -> Result ByteString
hashBytes name n b
  | BS.length b == n = pure b
  | otherwise = badRequest (name <> ": hash is not " <> T.pack (show n) <> " bytes")

-- | A staking credential; a hash that is not 28 bytes is BAD_REQUEST.
credentialOf :: Text -> P.Credential -> Result (Credential kr)
credentialOf name c = do
  h <- hashBytes name 28 (c ^. #hash)
  let bad = badRequest (name <> ": credential kind or hash")
  case c ^. #kind of
    P.CREDENTIAL_KIND_KEY -> maybe bad (pure . KeyHashObj . KeyHash) (hashFromBytes h)
    P.CREDENTIAL_KIND_SCRIPT -> maybe bad (pure . ScriptHashObj . ScriptHash) (hashFromBytes h)
    _ -> bad

credentialProto :: Credential kr -> P.Credential
credentialProto = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

poolHashOf :: Text -> ByteString -> Result (KeyHash StakePool)
poolHashOf name b = do
  h <- hashBytes name 28 b
  maybe (badRequest (name <> ": hash")) (pure . KeyHash) (hashFromBytes h)

-- | Pools in effect: a re-registration waiting for the next epoch is not
-- part of the snapshot.
currentPools :: [S.Pool] -> [S.Pool]
currentPools = filter (\p -> p ^. #stage /= S.POOL_STAGE_FUTURE)

-- | The state of one pool as the ledger holds it, with the credentials
-- delegated to it. An empty VRF hash reads as 32 zero bytes: only the pool
-- distribution reports it.
poolStateOf :: S.Pool -> Set.Set (Credential Staking) -> Result StakePoolState
poolStateOf p delegators = do
  vrf <- case BS.length (p ^. #vrfKeyHash) of
    0 -> pure (BS.replicate 32 0)
    32 -> pure (p ^. #vrfKeyHash)
    _ -> badRequest "vrf_key_hash: hash is not 32 bytes"
  vrfHash <- maybe (badRequest "vrf_key_hash") pure (hashFromBytes vrf)
  margin <- unitIntervalArg "margin" (p ^. #maybe'margin)
  account <- case p ^. #maybe'rewardAccount of
    Nothing -> badRequest "reward_account: missing"
    Just c -> credentialOf "reward_account" c
  owners <- forM (p ^. #owners) $ \o -> do
    h <- hashBytes "owners" 28 o
    maybe (badRequest "owners: hash") (pure . KeyHash) (hashFromBytes h)
  pure
    StakePoolState
      { spsVrf = VRFVerKeyHash vrfHash
      , spsPledge = Coin (toInteger (p ^. #pledge))
      , spsCost = Coin (toInteger (p ^. #cost))
      , spsMargin = margin
      , spsAccountId = AccountId account
      , spsOwners = Set.fromList owners
      , spsRelays = SSeq.empty
      , spsMetadata = SNothing
      , spsDeposit = CompactCoin (p ^. #deposit)
      , spsDelegators = delegators
      }

-- | Delegated stake rows (credential, amount, pool). A row without a pool is
-- not delegated and is dropped; a repeated credential is BAD_REQUEST; a zero
-- amount, or a total above Word64, is UNSUPPORTED (active stake is non-zero
-- compact coin).
delegatedRows ::
  Text ->
  [(P.Credential, Word64, ByteString)] ->
  Result [(Credential Staking, Word64, KeyHash StakePool)]
delegatedRows name rows = do
  (_, total, out) <- foldM step (Set.empty, 0 :: Integer, []) rows
  when (total > toInteger (maxBound :: Word64)) $
    unsupported (name <> ": stake total exceeds Word64")
  pure (reverse out)
  where
    step acc (_, _, pool) | BS.null pool = pure acc
    step (seen, total, out) (c, amount, pool) = do
      cred <- credentialOf name c
      kh <- poolHashOf name pool
      when (cred `Set.member` seen) $ badRequest (name <> ": repeated credential")
      when (amount == 0) $ unsupported (name <> ": zero stake")
      pure (Set.insert cred seen, total + toInteger amount, (cred, amount, kh) : out)

-- | The snapshot mkSnapShot and resetStakePoolsSnapShot build from the rows
-- and the pools in effect. A repeated pool is BAD_REQUEST.
snapShotOf ::
  [(Credential Staking, Word64, KeyHash StakePool)] -> [S.Pool] -> Result SnapShot
snapShotOf rows pools = do
  let delegators =
        Map.fromListWith Set.union [(kh, Set.singleton c) | (c, _, kh) <- rows]
  states <- forM (currentPools pools) $ \p -> do
    kh <- poolHashOf "pools" (p ^. #poolId)
    st <- poolStateOf p (Map.findWithDefault Set.empty kh delegators)
    pure (kh, st)
  let stateMap = Map.fromList states
  unless (Map.size stateMap == length states) $ badRequest "pools: repeated pool"
  let active =
        ActiveStake . VMap.fromMap . Map.fromList $
          [(c, StakeWithDelegation (unsafeNonZero (CompactCoin a)) kh) | (c, a, kh) <- rows]
  pure (resetStakePoolsSnapShot (VMap.fromMap stateMap) (mkSnapShot active VMap.empty))

-- | 'calculatePoolDistr' over the rows of one snapshot.
poolDistribution :: StakePoolDistributionRequest -> Result StakePoolDistributionResponse
poolDistribution req = do
  rows <-
    delegatedRows "snapshot"
      [(r ^. #credential, r ^. #amount, r ^. #poolId) | r <- req ^. #snapshot]
  when (null rows) $ unsupported "snapshot: no delegated stake"
  ss <- snapShotOf rows (req ^. #pools)
  let PoolDistr m total = calculatePoolDistr ss
  pure $
    defMessage
      & #pools
        .~ [ defMessage
               & #poolId .~ hashToBytes h
               & #stake .~ s
               & #share .~ rationalProto share
               & #vrfKeyHash .~ hashToBytes v
           | (KeyHash h, IndividualPoolStake share (CompactCoin s) (VRFVerKeyHash v)) <- sortOn (\(KeyHash h', _) -> hashToBytes h') (Map.toList m)
           ]
      & #totalActive .~ fromInteger (unCoin (unNonZero total))

-- | 'computeDRepDistr': the stake and reward balance of every account
-- delegated to a registered DRep or to a predefined one, plus the deposits
-- of the proposals still active at the request's epoch, which count for the
-- DRep their return account delegates to. Instant stake of a repeated
-- credential adds up, as UTxOs of one credential do.
drepDistribution :: StakeDRepDistributionRequest -> Result StakeDRepDistributionResponse
drepDistribution req = do
  instant <- foldM addStake Map.empty (req ^. #instantStake)
  accounts <- foldM addAccount Map.empty (req ^. #accounts)
  dreps <- foldM addDRep Map.empty (req ^. #dreps)
  deposits <- foldM addDeposit Map.empty (req ^. #proposals)
  let (distr, _) =
        computeDRepDistr
          (ConwayInstantStake instant :: ConwayInstantStake ConwayEra)
          dreps
          deposits
          (def :: PoolDistr)
          Map.empty
          accounts
  pure $ defMessage & #dreps .~ sortDReps [(d, s) | (d, CompactCoin s) <- Map.toList distr]
  where
    addStake acc e = do
      c <- credentialOf "instant_stake" (e ^. #credential)
      let total = toInteger (e ^. #amount) + maybe 0 (toInteger . unCompactCoin) (Map.lookup c acc)
      when (total > toInteger (maxBound :: Word64)) $ unsupported "instant_stake: total exceeds Word64"
      pure (Map.insert c (CompactCoin (fromInteger total)) acc)
    addAccount acc a = do
      c <- credentialOf "accounts" (a ^. #credential)
      when (c `Map.member` acc) $ badRequest "accounts: repeated credential"
      delegation <- case a ^. #maybe'drep of
        Nothing -> pure Nothing
        Just d -> Just <$> drepOf d
      pure (Map.insert c (ConwayAccountState (CompactCoin (a ^. #balance)) (CompactCoin (a ^. #deposit)) Nothing delegation) acc)
    drepOf d = case d ^. #maybe'target of
      Nothing -> pure DRepAlwaysAbstain
      Just (P.DRep'Credential c) -> DRepCredential <$> credentialOf "drep" c
      Just (P.DRep'AlwaysAbstain _) -> pure DRepAlwaysAbstain
      Just (P.DRep'AlwaysNoConfidence _) -> pure DRepAlwaysNoConfidence
    addDRep acc d = do
      c <- credentialOf "dreps" (d ^. #credential)
      when (c `Map.member` acc) $ badRequest "dreps: repeated credential"
      pure (Map.insert c (DRepState (EpochNo (d ^. #expiry)) SNothing (CompactCoin (d ^. #deposit)) Set.empty) acc)
    addDeposit acc p
      | p ^. #expiresEpoch < req ^. #epoch || p ^. #deposit == 0 = pure acc
      | otherwise = do
          c <- credentialOf "proposals" (p ^. #returnAccount)
          let total = toInteger (p ^. #deposit) + maybe 0 (toInteger . unCompactCoin) (Map.lookup c acc)
          when (total > toInteger (maxBound :: Word64)) $ unsupported "proposals: deposit total exceeds Word64"
          pure (Map.insert c (CompactCoin (fromInteger total)) acc)

-- | Credential DReps by kind then hash, then always-abstain, then
-- always-no-confidence.
sortDReps :: [(DRep, Word64)] -> [S.DRepStake]
sortDReps ds =
  map snd . sortOn fst $
    [ (rank d, defMessage & #drep .~ drepProto d & #stake .~ s) | (d, s) <- ds ]
  where
    rank d = case drepProto d ^. #maybe'target of
      Just (P.DRep'Credential c) -> (fromEnum (c ^. #kind), BS.unpack (c ^. #hash))
      Just (P.DRep'AlwaysAbstain _) -> (10, [])
      _ -> (11, [])

drepProto :: DRep -> P.DRep
drepProto = \case
  DRepKeyHash (KeyHash h) ->
    defMessage & #credential .~ (defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h)
  DRepScriptHash (ScriptHash h) ->
    defMessage & #credential .~ (defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h)
  DRepAlwaysAbstain -> defMessage & #alwaysAbstain .~ True
  DRepAlwaysNoConfidence -> defMessage & #alwaysNoConfidence .~ True

-- | 'addInstantStake' over the outputs, read as Conway outputs. Only the
-- credential part is reported: the stake of pointer addresses is separate in
-- earlier eras and absent in Conway. An output that does not decode rejects
-- the request as DeserialiseFailure.
instantStake :: StakeInstantStakeRequest -> Result StakeInstantStakeResponse
instantStake req = do
  entries <- foldM addOutput (Map.empty, []) (req ^. #utxo)
  case entries of
    (_, Left failure : _) -> pure (defMessage & #rejection .~ failure)
    (utxo, _) ->
      let ConwayInstantStake credentials =
            addInstantStake (UTxO utxo :: UTxO ConwayEra) (mempty :: ConwayInstantStake ConwayEra)
       in pure $
            defMessage
              & #stake
                .~ map snd
                  ( sortOn fst
                      [ ((fromEnum (cp ^. #kind), BS.unpack (cp ^. #hash)), defMessage & #credential .~ cp & #amount .~ a)
                      | (c, CompactCoin a) <- Map.toList credentials
                      , a > 0
                      , let cp = credentialProto c
                      ]
                  )
  where
    addOutput (acc, failures) u = do
      txId <- hashBytes "utxo" 32 (u ^. #txin . #txId)
      h <- maybe (badRequest "utxo: tx_id") pure (hashFromBytes txId)
      let ix = toInteger (u ^. #txin . #index)
      when (ix > 0xffff) $ unsupported "utxo: index above Word16"
      let txIn = TxIn (TxId (unsafeMakeSafeHash h)) (mkTxIxPartial ix)
      when (txIn `Map.member` acc) $ badRequest "utxo: repeated input"
      case decodeFull' (eraProtVerLow @ConwayEra) (u ^. #txout) :: Either DecoderError (TxOut ConwayEra) of
        Left _ ->
          pure (acc, Left (defMessage & #failures .~ [defMessage & #rule .~ "instant-stake" & #name .~ "DeserialiseFailure"]) : failures)
        Right out -> pure (Map.insert txIn out acc, failures)

-- | 'snapShotFromInstantStake': the instant stake of registered accounts that
-- delegate to a pool, plus their reward balances. Instant stake of a
-- repeated credential adds up.
stakeSnapshot :: StakeSnapshotRequest -> Result StakeSnapshotResponse
stakeSnapshot req = do
  instant <- foldM addStake Map.empty (req ^. #instantStake)
  accounts <- foldM addAccount Map.empty (req ^. #accounts)
  let delegators =
        Map.fromListWith Set.union [(kh, Set.singleton c) | (c, (_, Just kh)) <- Map.toList accounts]
  states <- forM (currentPools (req ^. #pools)) $ \p -> do
    kh <- poolHashOf "pools" (p ^. #poolId)
    st <- poolStateOf p (Map.findWithDefault Set.empty kh delegators)
    pure (kh, st)
  unless (Map.size (Map.fromList states) == length states) $ badRequest "pools: repeated pool"
  let dState = (def :: DState ShelleyEra) {dsAccounts = ShelleyAccounts {saStates = Map.map fst accounts, saPtrs = Map.empty}}
      pState = (def :: PState ShelleyEra) {psStakePools = Map.fromList states}
      ss = snapShotFromInstantStake (ShelleyInstantStake instant Map.empty :: ShelleyInstantStake ShelleyEra) dState pState
      rows =
        [ (c, a, kh)
        | (c, StakeWithDelegation nz kh) <- VMap.toList (unActiveStake (ssActiveStake ss))
        , let CompactCoin a = unNonZero nz
        ]
  when (null rows) $ unsupported "snapshot: no active stake"
  pure $
    defMessage
      & #snapshot
        .~ map snd
          ( sortOn fst
              [ ( (fromEnum (cp ^. #kind), BS.unpack (cp ^. #hash))
                , defMessage
                    & #snapshot .~ S.SNAPSHOT_KIND_MARK
                    & #credential .~ cp
                    & #amount .~ a
                    & #poolId .~ (let KeyHash h = kh in hashToBytes h)
                )
              | (c, a, kh) <- rows
              , let cp = credentialProto c
              ]
          )
      & #total .~ fromInteger (unCoin (unNonZero (ssTotalActiveStake ss)))
  where
    addStake acc e = do
      c <- credentialOf "instant_stake" (e ^. #credential)
      let total = toInteger (e ^. #amount) + maybe 0 (toInteger . unCompactCoin) (Map.lookup c acc)
      when (total > toInteger (maxBound :: Word64)) $ unsupported "instant_stake: total exceeds Word64"
      pure (if e ^. #amount == 0 then acc else Map.insert c (CompactCoin (fromInteger total)) acc)
    addAccount acc a = do
      c <- credentialOf "accounts" (a ^. #credential)
      when (c `Map.member` acc) $ badRequest "accounts: repeated credential"
      pool <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> poolHashOf "accounts" (a ^. #poolId)
      let st =
            ShelleyAccountState
              (mkPtrNormalized 0 0 0)
              (CompactCoin (a ^. #balance))
              (CompactCoin (a ^. #deposit))
              (maybe SNothing SJust pool)
      pure (Map.insert c (st, pool) acc)

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'StakeInstantStake q -> Just (respond #stakeInstantStake (instantStake q))
  Call'StakeSnapshot q -> Just (respond #stakeSnapshot (stakeSnapshot q))
  Call'StakePoolDistribution q -> Just (respond #stakePoolDistribution (poolDistribution q))
  Call'StakeDrepDistribution q -> Just (respond #stakeDrepDistribution (drepDistribution q))
  _ -> Nothing

implemented :: [Text]
implemented = ["stake_pool_distribution", "stake_drep_distribution", "stake_instant_stake", "stake_snapshot"]
