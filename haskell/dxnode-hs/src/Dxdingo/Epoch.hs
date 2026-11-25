{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

-- | Epoch functions, answered by running the real ledger STS rules (POOLREAP
-- and MIR through 'applySTSOpts') over states built from the request tables.
module Dxdingo.Epoch
  ( dispatch
  , implemented
  , donations
  , poolReap
  , mir
  , pparamUpdate
  , obligations
  , hardFork
  , snapshotRotate
  , newEpoch
  , unregisteredPoolStake
  , decodesInShelley
  , decodesInAllegra
  ) where

import Cardano.Ledger.Address (AccountId (..))
import Cardano.Ledger.BaseTypes
  ( EpochNo (..)
  , Globals (..)
  , ShelleyBase
  , Network (..)
  , StrictMaybe (..)
  , mkActiveSlotCoeff
  , nonZero
  , NonZero (..)
  , unsafeNonZero
  , boundRational
  , BoundedRational (unboundRational)
  )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (NEWEPOCH)
import Cardano.Ledger.Conway.State (ConwayEraAccounts (mkConwayAccountState))
import Cardano.Ledger.Credential (Credential (..), Ptr (..), SlotNo32 (..))
import Cardano.Ledger.Hashes (ScriptHash (..), VRFVerKeyHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Alonzo.PParams (UpgradeAlonzoPParams (..))
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams (..), PoolVotingThresholds (..), DRepVotingThresholds (..), ConwayEraPParams, ppPoolVotingThresholdsL, ppDRepVotingThresholdsL, ppCommitteeMinSizeL, ppCommitteeMaxTermLengthL, ppGovActionLifetimeL, ppGovActionDepositL, ppDRepDepositL, ppDRepActivityL, ppMinFeeRefScriptCostPerByteL)
import Data.Functor.Identity (Identity)
import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, CoinPerWord (..), ppCoinsPerUTxOWordL, ppCostModelsL, ppPricesL, ppMaxTxExUnitsL, ppMaxBlockExUnitsL, ppMaxValSizeL, ppCollateralPercentageL, ppMaxCollateralInputsL)
import Cardano.Ledger.Babbage.Core (ppCoinsPerUTxOByteL)
import Cardano.Ledger.Plutus (CostModels, Language (..), Prices (..), ExUnits (..), mkCostModel, mkCostModels, getCostModelParams, costModelsValid)
import Cardano.Ledger.Coin (CoinPerByte (..))
import Cardano.Ledger.Shelley.PParams (hasLegalProtVerUpdate)
import Data.Int (Int64)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState
  , LedgerState
  , NewEpochState (..)
  , StashedAVVMAddresses
  , esLStateL
  , utxosGovStateL
  , esSnapshotsL
  , curPParamsEpochStateL
  , prevPParamsEpochStateL
  , utxosDonationL
  , lsCertStateL
  , lsUTxOStateL
  , utxosFeesL
  , PulsingRewUpdate (..)
  , RewardUpdate (..)
  )
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.Shelley.Rules (votedFuturePParams, MIR, POOLREAP, SNAP, SnapEnv (..), ShelleyMirEvent (..), ShelleyPoolreapEvent (..), ShelleyPoolreapState (..))
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts (mkShelleyAccountState))
import Cardano.Ledger.State
import Cardano.Ledger.BaseTypes (CertIx (..), TxIx (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition
  ( ApplySTSOpts (..)
  , AssertionPolicy (..)
  , SingEP (..)
  , TRC (..)
  , ValidationPolicy (..)
  , applySTSOpts
  , STS (..)
  )
import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer (..), EpochInterval (..))
import Cardano.Ledger.Binary (decodeFull, getVersion, mkVersion)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Core (ppDL, ppExtraEntropyL, ppMinUTxOValueL)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import qualified Data.ByteString.Lazy as BSL
import Cardano.Ledger.Shelley.Governance (FuturePParams (PotentialPParamsUpdate), ShelleyGovState (..), solidifyFuturePParams)
import Cardano.Ledger.Conway.Governance
  ( GovAction (InfoAction, NoConfidence, UpdateCommittee), GovActionId (..), GovActionIx (..), GovActionState (..)
  , GovRelation (..), ProposalProcedure (..), Proposals, RatifyState (..), DRepPulsingState (DRComplete), ConwayGovState
  , cgsCommitteeL, cgsConstitutionL, cgsDRepPulsingStateL, cgsFuturePParamsL, cgsProposalsL, govStatePrevGovActionIds, predictFuturePParams, proposalsAddAction, proposalsIds, unGovPurposeId )
import Cardano.Ledger.Conway.State (vsDRepsL, vsCommitteeStateL, certVStateL, dRepDelegationAccountStateL)
import Cardano.Ledger.Address (AccountAddress (..))
import Cardano.Ledger.BaseTypes (Anchor (..), Url, textToUrl)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Ledger.Shelley.LedgerState (allObligations)
import Data.Default (Default, def)
import Data.Foldable (fold, toList)
import Data.Functor.Identity (runIdentity)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Ratio ((%))
import Dxdingo.Convert (rational)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Word (Word64)
import Cardano.Crypto.Hash.Class (hashFromBytes, hashToBytes)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Dxdingo.Gov (actionIdProto, committeeArg, committeeProto, constitutionArg, constitutionProto, ratifyTables, sortedIds)
import Dxdingo.Rewards (rewardUpdateWith, sortRewards)
import Dxdingo.Stake (credentialOf, poolHashOf, poolStateOf, currentPools, instantStake)
import qualified Proto.Dxdingo.V1.Rewards as RW
import qualified Data.VMap as VMap
import Cardano.Ledger.Conway.State (ConwayInstantStake (..))
import Data.Foldable (foldlM)
import Lens.Micro ((&), (.~), (^.), (%~))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import qualified Proto.Dxdingo.V1.Epoch as E
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- | The treasury after the epoch's donations move into it: the Conway EPOCH
-- step @casTreasury <>~ donations@. A Haskell 'Coin' is unbounded but the
-- response is a Word64, so an unrepresentable sum is UNSUPPORTED.
donations :: E.EpochDonationsRequest -> Result E.EpochDonationsResponse
donations req = do
  let cas = ChainAccountState (Coin (toInteger (req ^. #treasury))) (Coin 0)
      cas' = cas & casTreasuryL %~ (<> Coin (toInteger (req ^. #donations)))
  t <- coinWord64 (casTreasury cas')
  pure (defMessage & #treasury .~ t)

coinWord64 :: Coin -> Result Word64
coinWord64 (Coin c)
  | c < 0 || c > toInteger (maxBound :: Word64) = unsupported "coin does not fit a Word64"
  | otherwise = pure (fromInteger c)

-- | A 28-byte credential.
cred :: P.Credential -> Result (Credential Staking)
cred c = do
  let bytes = hashFromBytes (c ^. #hash)
      bad = badRequest "credential hash must be 28 bytes"
  case c ^. #kind of
    P.CREDENTIAL_KIND_KEY -> maybe bad (pure . KeyHashObj . KeyHash) bytes
    P.CREDENTIAL_KIND_SCRIPT -> maybe bad (pure . ScriptHashObj . ScriptHash) bytes
    _ -> badRequest "credential kind"

credProto :: Credential r -> P.Credential
credProto = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashBytes h
  where
    hashBytes = hashToBytes

credKey :: P.Credential -> (Int, BS.ByteString)
credKey c = (fromEnum (c ^. #kind), c ^. #hash)

poolKey :: BS.ByteString -> Result (KeyHash StakePool)
poolKey b = maybe (badRequest "pool id must be 28 bytes") (pure . KeyHash) (hashFromBytes b)

-- | Globals the rules here never read.
globals :: Globals
globals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , slotsPerKESPeriod = 1
    , stabilityWindow = 1
    , randomnessStabilisationWindow = 1
    , securityParameter = fromJust (nonZero 1)
    , maxKESEvo = 1
    , quorum = 1
    , maxLovelaceSupply = 1
    , activeSlotCoeff = mkActiveSlotCoeff (fromJust (boundRational 1))
    , networkId = Testnet
    , systemStart = SystemStart (read "1970-01-01 00:00:00 UTC")
    }

noDuplicates :: Ord a => Text -> [a] -> Result ()
noDuplicates what xs =
  when (length xs /= Set.size (Set.fromList xs)) $ unsupported ("duplicate " <> what)

-- | POOLREAP over Conway state. Accounts delegated to a pool become its
-- delegators. The result is read from the rule's event, the treasury and the
-- accounts after the transition.
poolReap :: E.EpochPoolReapRequest -> Result E.EpochPoolReapResponse
poolReap req = do
  noDuplicates "pool" [p ^. #poolId | p <- req ^. #pools]
  noDuplicates "account" (map (credKey . (^. #credential)) (req ^. #accounts))
  forM_ (req ^. #pools) $ \p ->
    unless (p ^. #stage == S.POOL_STAGE_CURRENT) (unsupported "pool stage is not current")
  let retiringDeposits = [p ^. #deposit | p <- req ^. #pools, p ^. #maybe'retiringEpoch == Just (req ^. #epoch)]
  when (sum (map toInteger retiringDeposits) > toInteger (maxBound :: Word64)) $
    unsupported "retiring deposits overflow Word64"
  accts <- traverse (\a -> (,) <$> cred (a ^. #credential) <*> traverse poolKey (nonEmpty' (a ^. #poolId))) (req ^. #accounts)
  let accountMap =
        foldr
          (\(c, mp) -> addAccountState c (mkConwayAccountState (CompactCoin 0) & stakePoolDelegationAccountStateL .~ mp))
          def
          accts
      delegatorsOf pid = Set.fromList [c | (c, Just p) <- accts, p == pid]
  pools <- traverse (mkPool delegatorsOf) (req ^. #pools)
  let stakePools = Map.fromList [(pid, sps) | (pid, sps, _) <- pools]
      retiring = Map.fromList [(pid, EpochNo e) | (pid, _, Just e) <- pools]
      cs0 =
        (def :: CertState ConwayEra)
          & certPStateL . psStakePoolsL .~ stakePools
          & certPStateL . psRetiringL .~ retiring
          & certDStateL . accountsL .~ accountMap
      st0 = PoolreapState def (ChainAccountState (Coin 0) (Coin 0)) cs0
      opts = ApplySTSOpts {asoAssertions = AssertionsOff, asoValidation = ValidateNone, asoEvents = EPReturn}
      ((st1, _failures), events) =
        runIdentity . (`runReaderT` globals) $
          applySTSOpts @(POOLREAP ConwayEra) opts (TRC ((), st0, EpochNo (req ^. #epoch)))
      retired = Map.keysSet stakePools `Set.difference` Map.keysSet (prCertState st1 ^. certPStateL . psStakePoolsL)
      refunds =
        Map.unionsWith (<>) [Map.map (fold . Map.elems) rp | RetiredPools {refundPools = rp} <- events]
      acctsOf s = s ^. certDStateL . accountsL . accountsMapL
      undelegated =
        [ c
        | (c, before) <- Map.toList (acctsOf cs0)
        , Just _ <- [before ^. stakePoolDelegationAccountStateL]
        , Just after <- [Map.lookup c (acctsOf (prCertState st1))]
        , Nothing <- [after ^. stakePoolDelegationAccountStateL]
        ]
  unclaimed <- coinWord64 (casTreasury (prChainAccountState st1))
  pure $
    defMessage
      & #retired .~ [h | KeyHash h' <- Set.toAscList retired, let h = hashToBytes h']
      & #refunds .~ L.sortOn (credKey . (^. #credential)) [defMessage & #credential .~ credProto c & #amount .~ v | (c, CompactCoin v) <- Map.toList refunds]
      & #unclaimedToTreasury .~ unclaimed
      & #undelegated .~ L.sortOn credKey (map credProto undelegated)
  where
    nonEmpty' b = if BS.null b then Nothing else Just b

mkPool ::
  (KeyHash StakePool -> Set.Set (Credential Staking)) ->
  S.Pool ->
  Result (KeyHash StakePool, StakePoolState, Maybe Word64)
mkPool delegatorsOf p = do
  pid <- poolKey (p ^. #poolId)
  acct <- cred (p ^. #rewardAccount)
  let vrfBytes = if BS.null (p ^. #vrfKeyHash) then BS.replicate 32 0 else p ^. #vrfKeyHash
  vrf <- maybe (badRequest "vrf_key_hash must be 32 bytes") pure (VRFVerKeyHash <$> hashFromBytes vrfBytes)
  pure
    ( pid
    , StakePoolState
        { spsVrf = vrf
        , spsPledge = Coin (toInteger (p ^. #pledge))
        , spsCost = Coin (toInteger (p ^. #cost))
        , spsMargin = fromJust (boundRational 0)
        , spsAccountId = AccountId acct
        , spsOwners = Set.empty
        , spsRelays = StrictSeq.empty
        , spsMetadata = SNothing
        , spsDeposit = CompactCoin (p ^. #deposit)
        , spsDelegators = delegatorsOf pid
        }
    , p ^. #maybe'retiringEpoch
    )

-- | MIR over Shelley state (the rule exists before Conway only). The domain
-- is shared with dxnode-go: no negative or repeated entries, since the
-- reference holds one non-negative Coin per (source, credential).
mir :: E.EpochMirRequest -> Result E.EpochMirResponse
mir req = do
  let major = req ^. #protocolVersion . #major
  when (major < 2 || major >= 9) $ unsupported "MIR exists in protocol versions 2 to 8"
  forM_ (req ^. #mir) $ \m -> do
    unless (m ^. #source `elem` [E.MIR_SOURCE_RESERVES, E.MIR_SOURCE_TREASURY]) $ unsupported "mir source unspecified"
    when (m ^. #amount < 0) $ unsupported "negative mir delta"
  noDuplicates "mir entry" [(fromEnum (m ^. #source), credKey (m ^. #credential)) | m <- req ^. #mir]
  let total =
        toInteger (req ^. #reserves) + toInteger (req ^. #treasury)
          + sum [toInteger (m ^. #amount) | m <- req ^. #mir]
          + toInteger (req ^. #reservesToTreasury) + toInteger (req ^. #treasuryToReserves)
  when (total > toInteger (maxBound :: Word64)) $ unsupported "pots and deltas overflow Word64"
  registered <- traverse cred (req ^. #registered)
  entries <- traverse (\m -> (,,) (m ^. #source) <$> cred (m ^. #credential) <*> pure (Coin (toInteger (m ^. #amount)))) (req ^. #mir)
  let ptr = Ptr (SlotNo32 0) (TxIx 0) (CertIx 0)
      accountMap = foldr (\c -> addAccountState c (mkShelleyAccountState ptr (CompactCoin 0))) def registered
      pot s = Map.fromListWith (<>) [(c, a) | (s', c, a) <- entries, s' == s]
      r2t = toInteger (req ^. #reservesToTreasury)
      t2r = toInteger (req ^. #treasuryToReserves)
      ir =
        InstantaneousRewards
          { iRReserves = pot E.MIR_SOURCE_RESERVES
          , iRTreasury = pot E.MIR_SOURCE_TREASURY
          , deltaReserves = DeltaCoin (t2r - r2t)
          , deltaTreasury = DeltaCoin (r2t - t2r)
          }
      es0 = def :: EpochState ShelleyEra
      es =
        es0
          & chainAccountStateL .~ ChainAccountState (Coin (toInteger (req ^. #treasury))) (Coin (toInteger (req ^. #reserves)))
          & esLStateL . lsCertStateL . certDStateL %~ ((dsIRewardsL .~ ir) . (accountsL .~ accountMap))
      opts = ApplySTSOpts {asoAssertions = AssertionsOff, asoValidation = ValidateNone, asoEvents = EPReturn}
      ((es', _failures), events) =
        runIdentity . (`runReaderT` globals) $
          applySTSOpts @(MIR ShelleyEra) opts (TRC ((), es, ()))
      applied = not (null [() | MirTransfer _ <- events])
      cas' = es' ^. chainAccountStateL
      balances =
        [ (c, unCompactCoin (a ^. balanceAccountStateL))
        | (c, a) <- Map.toList (es' ^. esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL)
        ]
  reserves <- coinWord64 (casReserves cas')
  treasury <- coinWord64 (casTreasury cas')
  pure $
    defMessage
      & #reserves .~ reserves
      & #treasury .~ treasury
      & #applied .~ applied
      & #credited .~ L.sortOn (credKey . (^. #credential)) [defMessage & #credential .~ credProto c & #amount .~ v | (c, v) <- balances, v > 0]

-- | Classic update-system enactment at the boundary into @context.epoch@,
-- Shelley through Babbage: the reference 'votedFuturePParams' over the
-- current proposals. Those are the proposals made during the previous epoch
-- (latest per genesis key) unioned over the carried-over ones, which were
-- submitted for that epoch during the one before it and survive only if every
-- one has a protocol version that can follow the parameters in force
-- ('hasLegalProtVerUpdate', as in updatePpup).
pparamUpdate :: E.EpochPParamUpdateRequest -> Result E.EpochPParamUpdateResponse
pparamUpdate req = case req ^. #context . #era of
  P.ERA_SHELLEY -> enact @ShelleyEra shelleyPParams shelleyPParamsProto req
  P.ERA_ALLEGRA -> enact @AllegraEra shelleyPParams shelleyPParamsProto req
  P.ERA_MARY -> enact @MaryEra shelleyPParams shelleyPParamsProto req
  P.ERA_ALONZO -> enact @AlonzoEra alonzoPParams alonzoPParamsProto req
  P.ERA_BABBAGE -> enact @BabbageEra babbagePParams babbagePParamsProto req
  _ -> unsupported "era is outside Shelley to Babbage"

enact ::
  forall era.
  (EraPParams era, AtMostEra "Babbage" era) =>
  (Maybe S.ProtocolParameters -> Result (PParams era)) ->
  (PParams era -> S.ProtocolParameters) ->
  E.EpochPParamUpdateRequest ->
  Result E.EpochPParamUpdateResponse
enact fromProto toProto req = do
  let epochLength = req ^. #globals . #epochLength
      epoch = req ^. #context . #epoch
  when (epochLength == 0) $ unsupported "epoch_length is zero"
  pp <- fromProto (req ^. #maybe'pparams)
  props <- traverse proposal (req ^. #proposals)
  let slots = map (^. #addedSlot) (req ^. #proposals)
  when (or (zipWith (>) slots (drop 1 slots))) $ unsupported "proposals are not in chain order"
  forM_ (req ^. #proposals) $ \p -> do
    let made = p ^. #addedSlot `div` epochLength
        t = p ^. #targetEpoch
    unless (made == t || made + 1 == t) $ unsupported "proposal targets neither its own epoch nor the next"
  pure $ case sequence props of
    -- The reference decoder rejects CBOR a chain would reject.
    Left msg -> defMessage & #rejection .~ rejection "PPUP" "DeserialiseFailure" msg
    Right decoded ->
      let start = (epoch - 1) * epochLength
          forEpoch = [(gk, slot, u) | (gk, t, slot, u) <- decoded, epoch > 0, t == epoch - 1]
          madeNow = Map.fromList [(gk, u) | (gk, slot, u) <- forEpoch, slot >= start]
          carried = Map.fromList [(gk, u) | (gk, slot, u) <- forEpoch, slot < start]
          kept = if all (hasLegalProtVerUpdate pp) carried then carried else Map.empty
          cur = ProposedPPUpdates (Map.union madeNow kept)
       in case votedFuturePParams cur pp (req ^. #globals . #quorum) of
            Nothing -> defMessage & #adopted .~ False & #pparams .~ (req ^. #pparams)
            Just pp' -> defMessage & #adopted .~ True & #pparams .~ toProto pp'
  where
    proposal p = do
      gk <- maybe (badRequest "genesis key hash must be 28 bytes") (pure . KeyHash) (hashFromBytes (p ^. #genesisKeyHash))
      pure $ case decodeFull (eraProtVerLow @era) (BSL.fromStrict (p ^. #update)) of
        Left e -> Left (T.pack (show e))
        Right u -> Right (gk, p ^. #targetEpoch, p ^. #addedSlot, u)

-- | A one-failure rejection.
rejection :: Text -> Text -> Text -> P.Rejection
rejection rule name detail =
  defMessage & #failures .~ [defMessage & #rule .~ rule & #name .~ name & #detail .~ detail]

-- | Whether a Shelley update decodes under the Shelley and Allegra eras'
-- protocol-version ranges (the cap is the era's highest major plus one).
decodesInShelley, decodesInAllegra :: BS.ByteString -> Bool
decodesInShelley b = either (const False) (const True) (decodeFull @(PParamsUpdate ShelleyEra) (eraProtVerLow @ShelleyEra) (BSL.fromStrict b))
decodesInAllegra b = either (const False) (const True) (decodeFull @(PParamsUpdate AllegraEra) (eraProtVerLow @AllegraEra) (BSL.fromStrict b))

-- | Shelley, Allegra and Mary share one parameter set.
shelleyPParams :: forall era. (EraPParams era, AtMostEra "Mary" era, AtMostEra "Alonzo" era) => Maybe S.ProtocolParameters -> Result (PParams era)
shelleyPParams Nothing = badRequest "missing pparams"
shelleyPParams (Just p) = do
  c <- commonSet p
  d <- unitIntervalArg "d" (p ^. #maybe'd)
  minUtxo <- maybe (badRequest "missing min_utxo_value") pure (p ^. #maybe'minUtxoValue)
  nonce <- nonceOf p
  pure $
    (emptyPParams :: PParams era)
      & c
      & ppDL .~ d
      & ppExtraEntropyL .~ nonce
      & ppMinUTxOValueL .~ Coin (toInteger minUtxo)

shelleyPParamsProto :: (EraPParams era, AtMostEra "Mary" era, AtMostEra "Alonzo" era) => PParams era -> S.ProtocolParameters
shelleyPParamsProto pp =
  commonProto pp
    & #d .~ rationalProto (unboundRational (pp ^. ppDL))
    & #extraEntropy .~ (defMessage & #hash .~ nonceBytes (pp ^. ppExtraEntropyL))
    & #minUtxoValue .~ coinW (pp ^. ppMinUTxOValueL)

-- | The Plutus-era parameters shared by Alonzo and Babbage.
plutusSet ::
  forall era.
  AlonzoEraPParams era =>
  S.ProtocolParameters ->
  Result (PParams era -> PParams era)
plutusSet p = do
  let need name = maybe (badRequest ("missing Plutus-era parameter " <> name)) pure
  txMem <- need "max_tx_ex_mem" (p ^. #maybe'maxTxExMem)
  txSteps <- need "max_tx_ex_steps" (p ^. #maybe'maxTxExSteps)
  blMem <- need "max_block_ex_mem" (p ^. #maybe'maxBlockExMem)
  blSteps <- need "max_block_ex_steps" (p ^. #maybe'maxBlockExSteps)
  valSize <- need "max_value_size" (p ^. #maybe'maxValueSize)
  collPct <- need "collateral_percentage" (p ^. #maybe'collateralPercentage)
  maxColl <- need "max_collateral_inputs" (p ^. #maybe'maxCollateralInputs)
  pm <- boundedArg "price_mem" (p ^. #maybe'priceMem)
  ps <- boundedArg "price_steps" (p ^. #maybe'priceSteps)
  when (collPct > 0xffff || maxColl > 0xffff) $ unsupported "collateral parameter out of range"
  when (any (> fromIntegral (maxBound :: Int64)) [txMem, txSteps, blMem, blSteps]) $
    unsupported "execution units exceed int64"
  models <- fmap (mkCostModels . Map.fromList . concat) . sequence $
    [ model PlutusV1 (p ^. #costModelPlutusV1)
    , model PlutusV2 (p ^. #costModelPlutusV2)
    ]
  pure $ \pp ->
    pp
      & ppCostModelsL .~ models
      & ppPricesL .~ Prices pm ps
      & ppMaxTxExUnitsL .~ ExUnits (fromIntegral txMem) (fromIntegral txSteps)
      & ppMaxBlockExUnitsL .~ ExUnits (fromIntegral blMem) (fromIntegral blSteps)
      & ppMaxValSizeL .~ valSize
      & ppCollateralPercentageL .~ fromIntegral collPct
      & ppMaxCollateralInputsL .~ fromIntegral maxColl
  where
    model _ [] = pure []
    model l ps = either (const (unsupported "invalid cost model")) (\m -> pure [(l, m)]) (mkCostModel l ps)

plutusProto :: AlonzoEraPParams era => PParams era -> S.ProtocolParameters -> S.ProtocolParameters
plutusProto pp out =
  out
    & #costModelPlutusV1 .~ params PlutusV1
    & #costModelPlutusV2 .~ params PlutusV2
    & #priceMem .~ rationalProto (unboundRational (prMem prices))
    & #priceSteps .~ rationalProto (unboundRational (prSteps prices))
    & #maxTxExMem .~ fromIntegral (exUnitsMem (pp ^. ppMaxTxExUnitsL))
    & #maxTxExSteps .~ fromIntegral (exUnitsSteps (pp ^. ppMaxTxExUnitsL))
    & #maxBlockExMem .~ fromIntegral (exUnitsMem (pp ^. ppMaxBlockExUnitsL))
    & #maxBlockExSteps .~ fromIntegral (exUnitsSteps (pp ^. ppMaxBlockExUnitsL))
    & #maxValueSize .~ (pp ^. ppMaxValSizeL)
    & #collateralPercentage .~ fromIntegral (pp ^. ppCollateralPercentageL)
    & #maxCollateralInputs .~ fromIntegral (pp ^. ppMaxCollateralInputsL)
  where
    prices = pp ^. ppPricesL
    params l = maybe [] getCostModelParams (Map.lookup l (costModelsValid (pp ^. ppCostModelsL)))

-- | The Shelley-set parameters every era keeps (no d, extra entropy or
-- min UTxO value: those are era-limited).
commonSet :: forall era. EraPParams era => S.ProtocolParameters -> Result (PParams era -> PParams era)
commonSet p = do
  let need name = maybe (badRequest ("missing parameter " <> name)) pure
  minFeeA <- need "min_fee_a" (p ^. #maybe'minFeeA)
  minFeeB <- need "min_fee_b" (p ^. #maybe'minFeeB)
  maxBB <- need "max_block_body_size" (p ^. #maybe'maxBlockBodySize)
  maxTx <- need "max_tx_size" (p ^. #maybe'maxTxSize)
  maxBH <- need "max_block_header_size" (p ^. #maybe'maxBlockHeaderSize)
  keyDep <- need "key_deposit" (p ^. #maybe'keyDeposit)
  poolDep <- need "pool_deposit" (p ^. #maybe'poolDeposit)
  eMax <- need "max_epoch" (p ^. #maybe'maxEpoch)
  nOpt <- need "n_opt" (p ^. #maybe'nOpt)
  minPool <- need "min_pool_cost" (p ^. #maybe'minPoolCost)
  pv <- need "protocol_version" (p ^. #maybe'protocolVersion)
  a0 <- boundedArg "a0" (p ^. #maybe'a0)
  rho <- unitIntervalArg "rho" (p ^. #maybe'rho)
  tau <- unitIntervalArg "tau" (p ^. #maybe'tau)
  when (nOpt > 0xffff || eMax > 0xffffffff || maxBH > 0xffff) $ unsupported "n_opt, max_epoch or max_block_header_size out of range"
  major <- maybe (unsupported "protocol major above the reference maximum") pure (mkVersion (pv ^. #major))
  pure $ \pp ->
    pp
      & ppMinFeeAL .~ Coin (toInteger minFeeA)
      & ppMinFeeBL .~ Coin (toInteger minFeeB)
      & ppMaxBBSizeL .~ maxBB
      & ppMaxTxSizeL .~ maxTx
      & ppMaxBHSizeL .~ fromIntegral maxBH
      & ppKeyDepositL .~ Coin (toInteger keyDep)
      & ppPoolDepositL .~ Coin (toInteger poolDep)
      & ppEMaxL .~ EpochInterval (fromIntegral eMax)
      & ppNOptL .~ fromIntegral nOpt
      & ppA0L .~ a0
      & ppRhoL .~ rho
      & ppTauL .~ tau
      & ppProtocolVersionL .~ ProtVer major (pv ^. #minor)
      & ppMinPoolCostL .~ Coin (toInteger minPool)

commonProto :: EraPParams era => PParams era -> S.ProtocolParameters
commonProto pp =
  defMessage
    & #minFeeA .~ coinW (pp ^. ppMinFeeAL)
    & #minFeeB .~ coinW (pp ^. ppMinFeeBL)
    & #maxBlockBodySize .~ (pp ^. ppMaxBBSizeL)
    & #maxTxSize .~ (pp ^. ppMaxTxSizeL)
    & #maxBlockHeaderSize .~ fromIntegral (pp ^. ppMaxBHSizeL)
    & #keyDeposit .~ coinW (pp ^. ppKeyDepositL)
    & #poolDeposit .~ coinW (pp ^. ppPoolDepositL)
    & #maxEpoch .~ fromIntegral (unEpochInterval (pp ^. ppEMaxL))
    & #nOpt .~ fromIntegral (pp ^. ppNOptL)
    & #a0 .~ rationalProto (unboundRational (pp ^. ppA0L))
    & #rho .~ rationalProto (unboundRational (pp ^. ppRhoL))
    & #tau .~ rationalProto (unboundRational (pp ^. ppTauL))
    & #protocolVersion .~ (defMessage & #major .~ getVersion (pvMajor pv) & #minor .~ pvMinor pv)
    & #minPoolCost .~ coinW (pp ^. ppMinPoolCostL)
  where
    pv = pp ^. ppProtocolVersionL

coinW :: Coin -> Word64
coinW (Coin c) = fromInteger c

nonceOf :: S.ProtocolParameters -> Result Nonce
nonceOf p = case BS.length (p ^. #extraEntropy . #hash) of
  0 -> pure NeutralNonce
  32 -> maybe (badRequest "extra entropy") (pure . Nonce) (hashFromBytes (p ^. #extraEntropy . #hash))
  _ -> badRequest "extra entropy must be 32 bytes"

nonceBytes :: Nonce -> BS.ByteString
nonceBytes NeutralNonce = BS.empty
nonceBytes (Nonce h) = hashToBytes h

-- | Alonzo adds the Plutus set to the Shelley set; there is no min_utxo_value.
alonzoPParams :: Maybe S.ProtocolParameters -> Result (PParams AlonzoEra)
alonzoPParams Nothing = badRequest "missing pparams"
alonzoPParams (Just p) = do
  c <- commonSet p
  pl <- plutusSet p
  d <- unitIntervalArg "d" (p ^. #maybe'd)
  nonce <- nonceOf p
  word <- maybe (badRequest "missing coins_per_utxo_word") pure (p ^. #maybe'coinsPerUtxoWord)
  pure $
    (emptyPParams :: PParams AlonzoEra)
      & c & pl
      & ppDL .~ d
      & ppExtraEntropyL .~ nonce
      & ppCoinsPerUTxOWordL .~ CoinPerWord (Coin (toInteger word))

alonzoPParamsProto :: PParams AlonzoEra -> S.ProtocolParameters
alonzoPParamsProto pp =
  plutusProto pp (commonProto pp)
    & #d .~ rationalProto (unboundRational (pp ^. ppDL))
    & #extraEntropy .~ (defMessage & #hash .~ nonceBytes (pp ^. ppExtraEntropyL))
    & #coinsPerUtxoWord .~ coinW (unCoinPerWord (pp ^. ppCoinsPerUTxOWordL))

babbagePParams :: Maybe S.ProtocolParameters -> Result (PParams BabbageEra)
babbagePParams Nothing = badRequest "missing pparams"
babbagePParams (Just p) = do
  c <- commonSet p
  pl <- plutusSet p
  byte <- maybe (badRequest "missing coins_per_utxo_byte") pure (p ^. #maybe'coinsPerUtxoByte)
  pure $
    (emptyPParams :: PParams BabbageEra)
      & c & pl
      & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin byte)

babbagePParamsProto :: PParams BabbageEra -> S.ProtocolParameters
babbagePParamsProto pp =
  plutusProto pp (commonProto pp)
    & #coinsPerUtxoByte .~ unCompactCoin (unCoinPerByte (pp ^. ppCoinsPerUTxOByteL))

-- | The deposit obligations of Conway state: 'allObligations' over the cert
-- state (account, pool and DRep deposits) and the governance state (proposal
-- deposits). Proposals are built as info actions: only the deposit counts.
obligations :: E.EpochObligationsRequest -> Result E.EpochObligationsResponse
obligations req = do
  noDuplicates "account" (map (credKey . (^. #credential)) (req ^. #accounts))
  noDuplicates "pool" [p ^. #poolId | p <- req ^. #pools]
  noDuplicates "drep" (map (credKey . (^. #credential)) (req ^. #dreps))
  noDuplicates "proposal" [(p ^. #id . #txId, p ^. #id . #index) | p <- req ^. #proposals]
  forM_ (req ^. #pools) $ \p ->
    unless (p ^. #stage == S.POOL_STAGE_CURRENT) (unsupported "pool stage is not current")
  accts <- traverse (\a -> (,) <$> cred (a ^. #credential) <*> pure (a ^. #deposit)) (req ^. #accounts)
  pools <- traverse (mkPool (const Set.empty)) (req ^. #pools)
  dreps <- traverse (\d -> (,) <$> drepCred (d ^. #credential) <*> pure (d ^. #deposit)) (req ^. #dreps)
  props <- traverse proposalState (req ^. #proposals)
  let accountMap = foldr (\(c, dep) -> addAccountState c (mkConwayAccountState (CompactCoin dep))) def accts
      drepMap = Map.fromList [(c, DRepState (EpochNo 0) SNothing (CompactCoin dep) Set.empty) | (c, dep) <- dreps]
      cs =
        (def :: CertState ConwayEra)
          & certPStateL . psStakePoolsL .~ Map.fromList [(pid, sps) | (pid, sps, _) <- pools]
          & certDStateL . accountsL .~ accountMap
          & certVStateL . vsDRepsL .~ drepMap
  proposals <- maybe (unsupported "proposals rejected by the reference") pure (foldr (\g acc -> acc >>= proposalsAddAction g) (Just def) props)
  let gs = (def :: ConwayGovState ConwayEra) & cgsProposalsL .~ proposals
      o = allObligations cs gs
  stake <- coinWord64 (oblStake o)
  pool <- coinWord64 (oblPool o)
  drep <- coinWord64 (oblDRep o)
  prop <- coinWord64 (oblProposal o)
  total <- coinWord64 (sumObligation o)
  pure $
    defMessage
      & #stakeDeposits .~ stake
      & #poolDeposits .~ pool
      & #drepDeposits .~ drep
      & #proposalDeposits .~ prop
      & #total .~ total
  where
    drepCred c = case c ^. #kind of
      P.CREDENTIAL_KIND_KEY -> maybe (badRequest "drep hash") (pure . KeyHashObj . KeyHash) (hashFromBytes (c ^. #hash))
      P.CREDENTIAL_KIND_SCRIPT -> maybe (badRequest "drep hash") (pure . ScriptHashObj . ScriptHash) (hashFromBytes (c ^. #hash))
      _ -> badRequest "credential kind"
    proposalState p = do
      ret <- cred (p ^. #returnAccount)
      txid <- maybe (badRequest "tx id must be 32 bytes") (pure . TxId . unsafeMakeSafeHash) (hashFromBytes (p ^. #id . #txId))
      let url = fromJust (textToUrl 128 "" :: Maybe Url)
          anchor = Anchor url (unsafeMakeSafeHash (fromJust (hashFromBytes (BS.replicate 32 0))))
      pure
        GovActionState
          { gasId = GovActionId txid (GovActionIx (fromIntegral (p ^. #id . #index)))
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes = Map.empty
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = ProposalProcedure (Coin (toInteger (p ^. #deposit))) (AccountAddress Testnet (AccountId ret)) InfoAction anchor
          , gasProposedIn = EpochNo (p ^. #proposedEpoch)
          , gasExpiresAfter = EpochNo (p ^. #expiresEpoch)
          }

-- | The era translation of the pre-fork parameters, for every fork: the
-- fork is named by the protocol majors on either side and enters the era
-- whose first major is @to@ (3 Allegra, 4 Mary, 5 Alonzo, 7 Babbage, 9
-- Conway). Allegra and Mary translate with 'translateEra''; Alonzo and Conway
-- apply their genesis values through 'upgradePParams'; Babbage translates.
hardFork :: E.EpochHardForkRequest -> Result E.EpochHardForkResponse
hardFork req = do
  let from = req ^. #from . #major
      to = req ^. #to . #major
      pp0 = req ^. #maybe'pparams
      delta p = defMessage & #delta .~ (defMessage & #pparamsAfter .~ p)
  when (from + 1 /= to) $ unsupported "a fork enters the era after `from`"
  case to of
    3 -> do
      pp <- shelleyPParams @ShelleyEra pp0
      pure (delta (shelleyPParamsProto (translateEra' @AllegraEra NoGenesis pp)))
    4 -> do
      pp <- shelleyPParams @AllegraEra pp0
      pure (delta (shelleyPParamsProto (translateEra' @MaryEra NoGenesis pp)))
    5 -> do
      pp <- shelleyPParams @MaryEra pp0
      g <- maybe (badRequest "alonzo_genesis is required for the Alonzo fork") pure (req ^. #maybe'alonzoGenesis)
      up <- alonzoUpgrade g
      pure (delta (alonzoPParamsProto (upgradePParams up pp)))
    7 -> do
      pp <- alonzoPParams pp0
      pure (delta (babbagePParamsProto (translateEra' @BabbageEra NoGenesis pp)))
    9 -> do
      pp <- babbagePParams pp0
      g <- maybe (badRequest "conway_genesis is required for the Conway fork") pure (req ^. #maybe'conwayGenesis)
      up <- conwayUpgrade g
      pure (delta (conwayPParamsProto (upgradePParams up pp)))
    _ -> unsupported "no fork enters this protocol major"

alonzoUpgrade :: E.AlonzoGenesisInput -> Result (UpgradeAlonzoPParams Identity)
alonzoUpgrade g = do
  pm <- boundedArg "price_mem" (g ^. #maybe'priceMem)
  ps <- boundedArg "price_steps" (g ^. #maybe'priceSteps)
  when (g ^. #collateralPercentage > 0xffff || g ^. #maxCollateralInputs > 0xffff) $ unsupported "collateral parameter out of range"
  model <- either (const (unsupported "invalid Plutus V1 cost model")) pure (mkCostModel PlutusV1 (g ^. #costModelPlutusV1))
  pure
    UpgradeAlonzoPParams
      { uappCoinsPerUTxOWord = CoinPerWord (Coin (toInteger (g ^. #coinsPerUtxoWord)))
      , uappPlutusV1CostModel = model
      , uappPrices = Prices pm ps
      , uappMaxTxExUnits = ExUnits (fromIntegral (g ^. #maxTxExMem)) (fromIntegral (g ^. #maxTxExSteps))
      , uappMaxBlockExUnits = ExUnits (fromIntegral (g ^. #maxBlockExMem)) (fromIntegral (g ^. #maxBlockExSteps))
      , uappMaxValSize = g ^. #maxValueSize
      , uappCollateralPercentage = fromIntegral (g ^. #collateralPercentage)
      , uappMaxCollateralInputs = fromIntegral (g ^. #maxCollateralInputs)
      }

conwayUpgrade :: E.ConwayGenesisInput -> Result (UpgradeConwayPParams Identity)
conwayUpgrade g = do
  let pool = g ^. #poolVotingThresholds
      drep = g ^. #drepVotingThresholds
      u n f = unitIntervalArg n (Just (f))
  pvt <- PoolVotingThresholds
    <$> u "pool motion_no_confidence" (pool ^. #motionNoConfidence)
    <*> u "pool committee_normal" (pool ^. #committeeNormal)
    <*> u "pool committee_no_confidence" (pool ^. #committeeNoConfidence)
    <*> u "pool hard_fork_initiation" (pool ^. #hardForkInitiation)
    <*> u "pool pp_security_group" (pool ^. #ppSecurityGroup)
  dvt <- DRepVotingThresholds
    <$> u "drep motion_no_confidence" (drep ^. #motionNoConfidence)
    <*> u "drep committee_normal" (drep ^. #committeeNormal)
    <*> u "drep committee_no_confidence" (drep ^. #committeeNoConfidence)
    <*> u "drep update_to_constitution" (drep ^. #updateToConstitution)
    <*> u "drep hard_fork_initiation" (drep ^. #hardForkInitiation)
    <*> u "drep pp_network_group" (drep ^. #ppNetworkGroup)
    <*> u "drep pp_economic_group" (drep ^. #ppEconomicGroup)
    <*> u "drep pp_technical_group" (drep ^. #ppTechnicalGroup)
    <*> u "drep pp_gov_group" (drep ^. #ppGovGroup)
    <*> u "drep treasury_withdrawal" (drep ^. #treasuryWithdrawal)
  refScript <- boundedArg "min_fee_ref_script_cost_per_byte" (g ^. #maybe'minFeeRefScriptCostPerByte)
  when (g ^. #committeeMinSize > 0xffff || g ^. #committeeMaxTermLength > 0xffffffff
        || g ^. #govActionLifetime > 0xffffffff || g ^. #drepActivity > 0xffffffff) $ unsupported "conway genesis parameter out of range"
  v3 <- either (const (unsupported "invalid Plutus V3 cost model")) pure (mkCostModel PlutusV3 (g ^. #costModelPlutusV3))
  pure
    UpgradeConwayPParams
      { ucppPoolVotingThresholds = pvt
      , ucppDRepVotingThresholds = dvt
      , ucppCommitteeMinSize = fromIntegral (g ^. #committeeMinSize)
      , ucppCommitteeMaxTermLength = EpochInterval (fromIntegral (g ^. #committeeMaxTermLength))
      , ucppGovActionLifetime = EpochInterval (fromIntegral (g ^. #govActionLifetime))
      , ucppGovActionDeposit = Coin (toInteger (g ^. #govActionDeposit))
      , ucppDRepDeposit = Coin (toInteger (g ^. #drepDeposit))
      , ucppDRepActivity = EpochInterval (fromIntegral (g ^. #drepActivity))
      , ucppMinFeeRefScriptCostPerByte = refScript
      , ucppPlutusV3CostModel = v3
      }

conwayPParamsProto :: PParams ConwayEra -> S.ProtocolParameters
conwayPParamsProto pp =
  plutusProto pp (commonProto pp)
    & #coinsPerUtxoByte .~ unCompactCoin (unCoinPerByte (pp ^. ppCoinsPerUTxOByteL))
    & #costModelPlutusV3 .~ maybe [] getCostModelParams (Map.lookup PlutusV3 (costModelsValid (pp ^. ppCostModelsL)))
    & #poolVotingThresholds .~ poolProto (pp ^. ppPoolVotingThresholdsL)
    & #drepVotingThresholds .~ drepProto (pp ^. ppDRepVotingThresholdsL)
    & #committeeMinSize .~ fromIntegral (pp ^. ppCommitteeMinSizeL)
    & #committeeMaxTermLength .~ fromIntegral (unEpochInterval (pp ^. ppCommitteeMaxTermLengthL))
    & #govActionLifetime .~ fromIntegral (unEpochInterval (pp ^. ppGovActionLifetimeL))
    & #govActionDeposit .~ coinW (pp ^. ppGovActionDepositL)
    & #drepDeposit .~ coinW (pp ^. ppDRepDepositL)
    & #drepActivity .~ fromIntegral (unEpochInterval (pp ^. ppDRepActivityL))
    & #minFeeRefScriptCostPerByte .~ rationalProto (unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL))
  where
    r = rationalProto . unboundRational
    poolProto t =
      defMessage
        & #motionNoConfidence .~ r (pvtMotionNoConfidence t)
        & #committeeNormal .~ r (pvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (pvtCommitteeNoConfidence t)
        & #hardForkInitiation .~ r (pvtHardForkInitiation t)
        & #ppSecurityGroup .~ r (pvtPPSecurityGroup t)
    drepProto t =
      defMessage
        & #motionNoConfidence .~ r (dvtMotionNoConfidence t)
        & #committeeNormal .~ r (dvtCommitteeNormal t)
        & #committeeNoConfidence .~ r (dvtCommitteeNoConfidence t)
        & #updateToConstitution .~ r (dvtUpdateToConstitution t)
        & #hardForkInitiation .~ r (dvtHardForkInitiation t)
        & #ppNetworkGroup .~ r (dvtPPNetworkGroup t)
        & #ppEconomicGroup .~ r (dvtPPEconomicGroup t)
        & #ppTechnicalGroup .~ r (dvtPPTechnicalGroup t)
        & #ppGovGroup .~ r (dvtPPGovGroup t)
        & #treasuryWithdrawal .~ r (dvtTreasuryWithdrawal t)

-- | SNAP over Conway state: the new mark is 'snapShotFromInstantStake' over
-- the instant stake, account balances and delegations, the previous mark
-- becomes set and the previous set becomes go, and the fee pot is kept.
-- The request's snapshots are the three snapshots before the boundary.
--
-- Domain, shared with dxnode-go: a snapshot row is delegated, non-zero stake
-- (the reference's active stake holds nothing else), no credential repeats
-- within a kind or among accounts, and no credential's stake exceeds Word64.
snapshotRotate :: E.EpochSnapshotRotateRequest -> Result E.EpochSnapshotRotateResponse
snapshotRotate req = do
  let pools = currentPools (req ^. #pools)
  poolStates <- traverse (\p -> (,) <$> poolHashOf "pools" (p ^. #poolId) <*> poolStateOf p Set.empty) pools
  when (Map.size (Map.fromList poolStates) /= length poolStates) $ badRequest "pools: repeated pool"
  priors <- priorSnapshots (req ^. #snapshots)
  instant <- foldlM addInstant Map.empty (req ^. #instantStake)
  accts <- foldlM addAccount Map.empty (req ^. #accounts)
  let totals = Map.unionWith (+) instant (Map.map fst accts)
  when (any (> toInteger (maxBound :: Word64)) (Map.elems totals)) $ unsupported "stake of one credential exceeds Word64"
  let accountMap =
        Map.foldrWithKey
          (\c (b, mp) -> addAccountState c (mkConwayAccountState (CompactCoin 0) & balanceAccountStateL .~ CompactCoin (fromInteger b) & stakePoolDelegationAccountStateL .~ mp))
          def
          accts
      cs =
        (def :: CertState ConwayEra)
          & certPStateL . psStakePoolsL .~ Map.fromList [(pid, sps) | (pid, sps) <- poolStates]
          & certDStateL . accountsL .~ accountMap
      ls =
        (def :: LedgerState ConwayEra)
          & lsCertStateL .~ cs
          & lsUTxOStateL . utxosFeesL .~ Coin (toInteger (req ^. #fees))
          & lsUTxOStateL . instantStakeL .~ ConwayInstantStake (Map.map (CompactCoin . fromInteger) (Map.filter (> 0) instant))
      snapOf k = snapshotOfRows (Map.findWithDefault [] k priors)
      mark0 = snapOf S.SNAPSHOT_KIND_MARK
      before = SnapShots mark0 (calculatePoolDistr mark0) (snapOf S.SNAPSHOT_KIND_SET) (snapOf S.SNAPSHOT_KIND_GO) (Coin 0)
      opts = ApplySTSOpts {asoAssertions = AssertionsOff, asoValidation = ValidateNone, asoEvents = EPReturn}
      ((after, _failures), _events) =
        runIdentity . (`runReaderT` globals) $
          applySTSOpts @(SNAP ConwayEra) opts (TRC (SnapEnv ls def, before, ()))
      rowsOf k ss =
        L.sortOn (\e -> (fromEnum (e ^. #credential . #kind), e ^. #credential . #hash))
          [ defMessage & #snapshot .~ k & #credential .~ credProto c & #amount .~ a & #poolId .~ hashToBytes ph
          | (c, StakeWithDelegation nz (KeyHash ph)) <- VMap.toList (unActiveStake (ssActiveStake ss))
          , let CompactCoin a = unNonZero nz
          ]
  fee <- coinWord64 (ssFee after)
  pure $
    defMessage
      & #snapshots
        .~ ( rowsOf S.SNAPSHOT_KIND_MARK (ssStakeMark after)
               ++ rowsOf S.SNAPSHOT_KIND_SET (ssStakeSet after)
               ++ rowsOf S.SNAPSHOT_KIND_GO (ssStakeGo after)
           )
      & #feeSnapshot .~ fee
  where
    addInstant acc e = do
      c <- credentialOf "instant_stake" (e ^. #credential)
      pure (Map.insertWith (+) c (toInteger (e ^. #amount)) acc)
    addAccount acc a = do
      c <- credentialOf "accounts" (a ^. #credential)
      when (c `Map.member` acc) $ badRequest "accounts: repeated credential"
      mp <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> poolHashOf "accounts" (a ^. #poolId)
      pure (Map.insert c (toInteger (a ^. #balance), mp) acc)

-- | The library alone, with no request translation: the active stake
-- 'snapShotFromInstantStake' reports for a credential that delegates to a pool
-- absent from the pool state. The reference resolves active stake from
-- registered, delegated credentials without consulting the pool set.
unregisteredPoolStake :: Maybe Word64
unregisteredPoolStake =
  let key n = KeyHash (fromJust (hashFromBytes (BS.replicate 28 n)))
      c = KeyHashObj (key 4) :: Credential Staking
      accts = addAccountState c (mkConwayAccountState (CompactCoin 0) & stakePoolDelegationAccountStateL .~ Just (key 0xa0)) def
      cs = (def :: CertState ConwayEra) & certDStateL . accountsL .~ accts
      is = ConwayInstantStake (Map.singleton c (CompactCoin 42)) :: ConwayInstantStake ConwayEra
      ss = snapShotFromInstantStake is (cs ^. certDStateL) (cs ^. certPStateL)
   in (\(StakeWithDelegation nz _) -> let CompactCoin a = unNonZero nz in a) <$> VMap.lookup c (unActiveStake (ssActiveStake ss))

-- | The snapshots in force before a boundary, by kind. Each row is
-- delegated, non-zero stake, and no credential repeats within a kind.
priorSnapshots :: [S.SnapshotEntry] -> Result (Map.Map S.SnapshotKind [(Credential Staking, Word64, KeyHash StakePool)])
priorSnapshots entries = snd <$> foldlM priorRow (Set.empty, Map.empty) entries
  where
    priorRow (seen, acc) e = do
      let k = e ^. #snapshot
      when (k `notElem` [S.SNAPSHOT_KIND_MARK, S.SNAPSHOT_KIND_SET, S.SNAPSHOT_KIND_GO]) $ badRequest "snapshots: kind is not mark, set or go"
      c <- credentialOf "snapshots" (e ^. #credential)
      unless (BS.null (e ^. #poolId)) $ void (poolHashOf "snapshots" (e ^. #poolId))
      when ((fromEnum k, c) `Set.member` seen) $ badRequest "snapshots: repeated credential"
      when (BS.null (e ^. #poolId)) $ unsupported "snapshots: a snapshot row must be delegated"
      when (e ^. #amount == 0) $ unsupported "snapshots: a snapshot row must hold stake"
      ph <- poolHashOf "snapshots" (e ^. #poolId)
      pure (Set.insert (fromEnum k, c) seen, Map.insertWith (flip (++)) k [(c, e ^. #amount, ph)] acc)

-- | A snapshot holding the given delegated stake rows.
snapshotOfRows :: [(Credential Staking, Word64, KeyHash StakePool)] -> SnapShot
snapshotOfRows rows =
  mkSnapShot
    (ActiveStake (VMap.fromMap (Map.fromList [(c, StakeWithDelegation (unsafeNonZero (CompactCoin a)) kh) | (c, a, kh) <- rows])))
    VMap.empty

-- | The Conway parameters of a request: the Babbage set plus the governance
-- parameters, applied through the same upgrade the Conway fork uses.
conwayPParams :: Maybe S.ProtocolParameters -> Result (PParams ConwayEra)
conwayPParams Nothing = badRequest "missing pparams"
conwayPParams (Just p) = do
  base <- babbagePParams (Just p)
  let need n = maybe (badRequest ("missing " <> n)) pure
  pool <- need "pool_voting_thresholds" (p ^. #maybe'poolVotingThresholds)
  drep <- need "drep_voting_thresholds" (p ^. #maybe'drepVotingThresholds)
  size <- need "committee_min_size" (p ^. #maybe'committeeMinSize)
  term <- need "committee_max_term_length" (p ^. #maybe'committeeMaxTermLength)
  life <- need "gov_action_lifetime" (p ^. #maybe'govActionLifetime)
  gdep <- need "gov_action_deposit" (p ^. #maybe'govActionDeposit)
  ddep <- need "drep_deposit" (p ^. #maybe'drepDeposit)
  act <- need "drep_activity" (p ^. #maybe'drepActivity)
  ref <- need "min_fee_ref_script_cost_per_byte" (p ^. #maybe'minFeeRefScriptCostPerByte)
  up <- conwayUpgrade
    ( defMessage
        & #poolVotingThresholds .~ pool
        & #drepVotingThresholds .~ drep
        & #committeeMinSize .~ size
        & #committeeMaxTermLength .~ term
        & #govActionLifetime .~ life
        & #govActionDeposit .~ gdep
        & #drepDeposit .~ ddep
        & #drepActivity .~ act
        & #minFeeRefScriptCostPerByte .~ ref
        & #costModelPlutusV3 .~ (p ^. #costModelPlutusV3)
    )
  pure (upgradePParams up base)

-- | The Globals of dingo's scratch ledger: the only ones a pending reward
-- update is checked under, because dingo recomputes the round from them.
newEpochGlobalsMatch :: Maybe S.Globals -> Bool
newEpochGlobalsMatch mg =
  let g = fromMaybe defMessage mg
   in (rational =<< (g ^. #maybe'activeSlotCoeff)) == Just (1 % 20)
        && g ^. #epochLength == 432000
        && g ^. #maxLovelaceSupply == 45000000000000000
        && g ^. #securityParam == 2160

-- | The reward_update call whose answer a pending update must equal. The
-- protocol version is the context's, else the parameters'. Registered
-- credentials are the accounts.
newEpochRewardCall :: E.EpochNewEpochRequest -> RW.RewardsRewardUpdateRequest
newEpochRewardCall req =
  let st = req ^. #state
      pp = req ^. #maybe'pparams
      pv = case req ^. #context . #maybe'protocolVersion of
        Just v -> Just v
        Nothing -> pp >>= (^. #maybe'protocolVersion)
      pots = st ^. #pots
   in defMessage
        & #maybe'protocolVersion .~ pv
        & #maybe'pparams .~ pp
        & #maybe'globals .~ (req ^. #maybe'globals)
        & #pots .~ (defMessage & #reserves .~ (pots ^. #reserves) & #treasury .~ (pots ^. #treasury) & #fees .~ (pots ^. #fees))
        & #snapshots .~ (req ^. #snapshots)
        & #pools .~ (st ^. #pools)
        & #blocks .~ (req ^. #blocksPrevious)
        & #registered .~ [a ^. #credential | a <- st ^. #accounts]

-- | The reward update NEWEPOCH applies: the one the real calculation builds
-- from the request's inputs, provided it equals the request's delivered
-- update. Dingo cannot take an update it did not compute, so any other is
-- UNSUPPORTED, as are zero, unconvertible or kind-less rewards, which dingo's
-- list never holds.
pendingRewardUpdate :: E.EpochNewEpochRequest -> Result (StrictMaybe PulsingRewUpdate)
pendingRewardUpdate req = case req ^. #maybe'rewardUpdate of
  Nothing -> pure SNothing
  Just up -> do
    unless (newEpochGlobalsMatch (req ^. #maybe'globals)) $ unsupported "reward_update needs the scratch ledger's globals"
    (ru, got) <- rewardUpdateWith (newEpochRewardCall req)
    let differs = unsupported "reward_update differs from the update the inputs produce"
    rs' <- either (const differs) pure (mapM rewardOf (up ^. #rewards))
    when (any (\r -> r ^. #amount == 0) (up ^. #rewards)) differs
    unless (got ^. #update == (up & #rewards .~ sortRewards rs')) differs
    pure (SJust (Complete ru))
  where
    rewardOf r = do
      c <- maybe (badRequest "credential missing") (credentialOf "reward_update") (r ^. #maybe'credential)
      kh <- poolHashOf "reward_update" (r ^. #poolId)
      ty <- case r ^. #kind of
        S.REWARD_KIND_LEADER -> pure LeaderReward
        S.REWARD_KIND_MEMBER -> pure MemberReward
        _ -> badRequest "reward kind"
      pure (c, Reward ty kh (Coin (toInteger (r ^. #amount))))

-- | NEWEPOCH over Conway state: the real rule applies the pending reward
-- update, then runs SNAP, POOLREAP and the Conway EPOCH step over a state
-- built from the request. The delta holds what changed in the accounts, the
-- removed pools and the reserves and treasury; the snapshots are the three
-- after rotation.
--
-- Governance. state.dreps, proposals, votes, committee and enact_state, with
-- pool_distribution and drep_distribution, are the DRep pulser's snapshot
-- taken at the boundary into the previous epoch: the proposals and votes it
-- ratified over. RATIFY runs over them ('ratifyTables') and its result is
-- the completed pulser the EPOCH rule consumes, so EPOCH enacts what was
-- accepted, drops what expired, pays withdrawals and refunds deposits.
-- enacted and expired are RATIFY's verdict; the delta adds the removed
-- proposals, DRep expiry changes, the enacted committee and the roots.
-- Proposals or votes that arrived after the snapshot are not representable.
--
-- Domain, shared with dxnode-go: Conway only. A reward_update is accepted when
-- the real reward calculation produces it from blocks_previous, the GO
-- snapshot, the pools and pots (pots.fees is the fee pot it pays out), under
-- the scratch ledger's Globals; blocks_previous is read only with a
-- reward_update. Governance is gov_ratify's domain at the previous epoch.
-- Enacted parameter changes and hard forks are reported as the delta's
-- pparams_after: the EPOCH rule adopts the parameters the pulser's RATIFY
-- reached, and a protocol version change runs HARDFORK, which at protocol 10
-- clears delegations to DReps that are not registered (an account row whose
-- DRep changed is reported). The instant stake is instant_stake or, when
-- state.utxo is given instead, what stake_instant_stake derives from the
-- UTxO table (a rejection of that derivation is the response's; both together
-- are UNSUPPORTED). The fee, deposit and donation pots are not reported
-- because dingo keeps none.
--
-- Before Conway ('newEpochPre'): pending reward update, MIR (mir and the two
-- pot transfers, as epoch_mir), the classic update system (pparam_proposals
-- under globals.quorum, as epoch_pparam_update, reported as pparams_after),
-- SNAP and POOLREAP. UNSUPPORTED, with the reason: genesis delegations
-- (neither node reads them at NEWEPOCH: they only validate a proposal when it
-- is made, Shelley/Rules/Ppup.hs, and UPEC adopts by quorum alone,
-- Shelley/Rules/Upec.hs); registration pointers, and with them a UTxO table,
-- because the reference counts pointer-addressed stake before Conway through
-- the accounts' pointers (ShelleyAccountState) while stake_instant_stake, the
-- Conway derivation shared with dxnode-go, drops pointer addresses and
-- dxoracle seeds registrations without a certificate position
-- (dxoracle/epoch_new_epoch.go, seedNewEpoch); governance state and DRep
-- delegations, which do not exist; an epoch below 5; proposals under another
-- epoch length than the scratch ledger's 432000; and the Conway-only fields.
newEpoch :: E.EpochNewEpochRequest -> Result E.EpochNewEpochResponse
newEpoch req = do
  unless (req ^. #context . #era `elem` [P.ERA_SHELLEY, P.ERA_ALLEGRA, P.ERA_MARY, P.ERA_ALONZO, P.ERA_BABBAGE, P.ERA_CONWAY]) $
    unsupported "NEWEPOCH is answered for Shelley through Conway"
  let st = req ^. #state
  when (not (null (st ^. #genesisDelegations))) $
    unsupported "genesis delegations are not read"
  when (not (null (st ^. #utxo)) && req ^. #context . #era /= P.ERA_CONWAY) $
    unsupported "a UTxO table is read for Conway only: before it pointer addresses carry stake"
  when (not (null (st ^. #utxo)) && not (null (req ^. #instantStake))) $
    unsupported "a UTxO table and instant_stake both give the instant stake"
  derived <- if null (st ^. #utxo) then pure Nothing else Just <$> instantStake (defMessage & #utxo .~ (st ^. #utxo))
  case derived >>= (^. #maybe'rejection) of
    Just rj -> pure (defMessage & #rejection .~ rj)
    Nothing -> newEpochOver req (maybe (req ^. #instantStake) (^. #stake) derived)

-- | The boundary over a request whose instant stake is the given entries.
newEpochOver :: E.EpochNewEpochRequest -> [S.StakeEntry] -> Result E.EpochNewEpochResponse
newEpochOver req instantEntries = case req ^. #context . #era of
  P.ERA_SHELLEY -> shelleyPParams @ShelleyEra (req ^. #maybe'pparams) >>= newEpochPre @ShelleyEra shelleyPParamsProto req instantEntries
  P.ERA_ALLEGRA -> shelleyPParams @AllegraEra (req ^. #maybe'pparams) >>= newEpochPre @AllegraEra shelleyPParamsProto req instantEntries
  P.ERA_MARY -> shelleyPParams @MaryEra (req ^. #maybe'pparams) >>= newEpochPre @MaryEra shelleyPParamsProto req instantEntries
  P.ERA_ALONZO -> alonzoPParams (req ^. #maybe'pparams) >>= newEpochPre @AlonzoEra alonzoPParamsProto req instantEntries
  P.ERA_BABBAGE -> babbagePParams (req ^. #maybe'pparams) >>= newEpochPre @BabbageEra babbagePParamsProto req instantEntries
  _ -> newEpochConway req instantEntries

-- | NEWEPOCH over Conway state.
newEpochConway :: E.EpochNewEpochRequest -> [S.StakeEntry] -> Result E.EpochNewEpochResponse
newEpochConway req instantEntries = do
  let st = req ^. #state
  unless (null (st ^. #pparamProposals)) $ unsupported "parameter proposals do not exist in Conway"
  unless (null (req ^. #mir) && req ^. #reservesToTreasury == 0 && req ^. #treasuryToReserves == 0) $ unsupported "MIR does not exist in Conway"
  let epoch = req ^. #context . #epoch
  when (epoch < 5) $ unsupported "epoch below 5 has no reward round to run"
  pp <- conwayPParams (req ^. #maybe'pparams)
  priors <- priorSnapshots (req ^. #snapshots)
  accts <- foldlM addAccount Map.empty (st ^. #accounts)
  let delegatorsOf pid = Set.fromList [c | (c, (_, _, Just p, _)) <- Map.toList accts, p == pid]
  forM_ (st ^. #pools) $ \p -> do
    unless (p ^. #stage == S.POOL_STAGE_CURRENT) $ unsupported "pools: stage is not current"
  pools <- traverse (mkPool delegatorsOf) (st ^. #pools)
  when (Map.size (Map.fromList [(pid, ()) | (pid, _, _) <- pools]) /= length pools) $ badRequest "pools: repeated pool"
  instant <- foldlM addInstant Map.empty instantEntries
  let totals = Map.unionWith (+) instant (Map.map (\(_, b, _, _) -> toInteger b) accts)
  when (any (> toInteger (maxBound :: Word64)) (Map.elems totals)) $ unsupported "stake of one credential exceeds Word64"
  pending <- pendingRewardUpdate req
  gov <- newEpochGov req pp
  let pots = st ^. #pots
      accountMap =
        Map.foldrWithKey
          ( \c (dep, bal, mp, dr) ->
              addAccountState c
                ( mkConwayAccountState (CompactCoin dep)
                    & balanceAccountStateL .~ CompactCoin bal
                    & stakePoolDelegationAccountStateL .~ mp
                    & dRepDelegationAccountStateL .~ dr
                )
          )
          def
          accts
      cs =
        (def :: CertState ConwayEra)
          & certPStateL . psStakePoolsL .~ Map.fromList [(pid, sps) | (pid, sps, _) <- pools]
          & certPStateL . psRetiringL .~ Map.fromList [(pid, EpochNo e) | (pid, _, Just e) <- pools]
          & certDStateL . accountsL .~ accountMap
          & certVStateL . vsDRepsL .~ maybe Map.empty ngDReps gov
          & certVStateL . vsCommitteeStateL .~ maybe (CommitteeState Map.empty) ngCommitteeState gov
      ls =
        (def :: LedgerState ConwayEra)
          & lsCertStateL .~ cs
          & lsUTxOStateL . utxosGovStateL .~ maybe def ngGovState gov
          & lsUTxOStateL . utxosDonationL .~ Coin (toInteger (pots ^. #donations))
          & lsUTxOStateL . utxosFeesL .~ maybe (Coin 0) (const (Coin (toInteger (pots ^. #fees)))) (req ^. #maybe'rewardUpdate)
          & lsUTxOStateL . instantStakeL .~ ConwayInstantStake (Map.map (CompactCoin . fromInteger) (Map.filter (> 0) instant))
      snapOf k = snapshotOfRows (Map.findWithDefault [] k priors)
      mark0 = snapOf S.SNAPSHOT_KIND_MARK
      before = SnapShots mark0 (calculatePoolDistr mark0) (snapOf S.SNAPSHOT_KIND_SET) (snapOf S.SNAPSHOT_KIND_GO) (Coin 0)
      es0 =
        (def :: EpochState ConwayEra)
          & esLStateL .~ ls
          & esSnapshotsL .~ before
          & chainAccountStateL .~ ChainAccountState (Coin (toInteger (pots ^. #treasury))) (Coin (toInteger (pots ^. #reserves)))
          & curPParamsEpochStateL .~ pp
          & prevPParamsEpochStateL .~ pp
      nes0 = (def :: NewEpochState ConwayEra) {nesEL = EpochNo (epoch - 1), nesEs = es0, nesPd = calculatePoolDistr mark0, nesRu = pending}
      opts = ApplySTSOpts {asoAssertions = AssertionsOff, asoValidation = ValidateNone, asoEvents = EPReturn}
      ((nes1, _failures), _events) =
        runIdentity . (`runReaderT` globals) $
          applySTSOpts @(NEWEPOCH ConwayEra) opts (TRC ((), nes0, EpochNo epoch))
      es1 = nesEs nes1
      cas1 = es1 ^. chainAccountStateL
      ppAfter = es1 ^. curPParamsEpochStateL
      acctsAfter = es1 ^. esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      poolsAfter = es1 ^. esLStateL . lsCertStateL . certPStateL . psStakePoolsL
      changed =
        [ defMessage & #credential .~ credProto c & #deposit .~ unCompactCoin (a ^. depositAccountStateL)
            & #balance .~ unCompactCoin (a ^. balanceAccountStateL)
            & #poolId .~ maybe BS.empty (\(KeyHash h) -> hashToBytes h) (a ^. stakePoolDelegationAccountStateL)
            & #maybe'drep .~ (drepToProto <$> (a ^. dRepDelegationAccountStateL))
        | (c, (_, bal, mp, dr)) <- Map.toList accts
        , Just a <- [Map.lookup c acctsAfter]
        , unCompactCoin (a ^. balanceAccountStateL) /= bal || a ^. stakePoolDelegationAccountStateL /= mp || a ^. dRepDelegationAccountStateL /= dr
        ]
      removed = [hashToBytes h | (pid@(KeyHash h), _, _) <- pools, not (pid `Map.member` poolsAfter)]
      rowsOf k ss =
        L.sortOn (\e -> (fromEnum (e ^. #credential . #kind), e ^. #credential . #hash))
          [ defMessage & #snapshot .~ k & #credential .~ credProto c & #amount .~ a & #poolId .~ hashToBytes ph
          | (c, StakeWithDelegation nz (KeyHash ph)) <- VMap.toList (unActiveStake (ssActiveStake ss))
          , let CompactCoin a = unNonZero nz
          ]
      snaps = es1 ^. esSnapshotsL
      deltaOf after before' = fromInteger (unCoin after - toInteger before') :: Int64
  pure $
    defMessage
      & #delta
        .~ ( defMessage
               & #accountsChanged .~ L.sortOn (credKey . (^. #credential)) changed
               & #poolsRemoved .~ L.sort removed
               & #reservesDelta .~ deltaOf (casReserves cas1) (pots ^. #reserves)
               & #treasuryDelta .~ deltaOf (casTreasury cas1) (pots ^. #treasury)
               & (if ppAfter == pp then id else #pparamsAfter .~ conwayPParamsProto ppAfter)
               & maybe id (govDelta st es1) gov
           )
      & maybe id govVerdict gov
      & #snapshots
        .~ ( rowsOf S.SNAPSHOT_KIND_MARK (ssStakeMark snaps)
               ++ rowsOf S.SNAPSHOT_KIND_SET (ssStakeSet snaps)
               ++ rowsOf S.SNAPSHOT_KIND_GO (ssStakeGo snaps)
           )
  where
    addAccount acc a = do
      c <- credentialOf "accounts" (a ^. #credential)
      when (c `Map.member` acc) $ badRequest "accounts: repeated credential"
      mp <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> poolHashOf "accounts" (a ^. #poolId)
      when (isJust (a ^. #maybe'pointer)) $ unsupported "accounts: pointers are not read"
      dr <- case a ^. #maybe'drep of
        Nothing -> pure Nothing
        Just d -> Just <$> drepFromProto d
      pure (Map.insert c (a ^. #deposit, a ^. #balance, mp, dr) acc)
    addInstant acc e = do
      c <- credentialOf "instant_stake" (e ^. #credential)
      pure (Map.insertWith (+) c (toInteger (e ^. #amount)) acc)

-- | NEWEPOCH over Shelley through Babbage state, with the rule of the era
-- ('EraRule' "NEWEPOCH"): the pending reward update is applied, then SNAP,
-- POOLREAP and the Shelley EPOCH step run over a state built from the
-- request, and the response reports what changed as for Conway.
--
-- Domain, shared with dxnode-go: the update system, MIR, pointers and
-- governance are not in it. A parameter proposal or a genesis delegation, a
-- DRep delegation, a pointer and every governance table are UNSUPPORTED.
newEpochPreRun ::
  forall era.
  ( EraGov era
  , EraStake era
  , EraCertState era
  , ShelleyEraAccounts era
  , STS (EraRule "NEWEPOCH" era)
  , BaseM (EraRule "NEWEPOCH" era) ~ ShelleyBase
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , Default (StashedAVVMAddresses era)
  , Default (EpochState era)
  , Default (LedgerState era)
  , EraPParams era
  , AtMostEra "Babbage" era
  , GovState era ~ ShelleyGovState era
  ) =>
  (PParams era -> S.ProtocolParameters) ->
  E.EpochNewEpochRequest ->
  [S.StakeEntry] ->
  PParams era ->
  ShelleyGovState era ->
  Result E.EpochNewEpochResponse
newEpochPreRun toProto req instantEntries pp govSt = do
  let st = req ^. #state
      epoch = req ^. #context . #epoch
  unless (null (st ^. #dreps) && null (st ^. #proposals) && null (st ^. #votes) && isNothing (st ^. #maybe'committee) && isNothing (st ^. #maybe'enactState)
          && null (req ^. #poolDistribution) && null (req ^. #drepDistribution)) $
    unsupported "governance state does not exist before Conway"
  when (epoch < 5) $ unsupported "epoch below 5 has no reward round to run"
  priors <- priorSnapshots (req ^. #snapshots)
  accts <- foldlM addAccount Map.empty (st ^. #accounts)
  let delegatorsOf pid = Set.fromList [c | (c, (_, _, Just p)) <- Map.toList accts, p == pid]
  forM_ (st ^. #pools) $ \p -> unless (p ^. #stage == S.POOL_STAGE_CURRENT) $ unsupported "pools: stage is not current"
  pools <- traverse (mkPool delegatorsOf) (st ^. #pools)
  when (Map.size (Map.fromList [(pid, ()) | (pid, _, _) <- pools]) /= length pools) $ badRequest "pools: repeated pool"
  instant <- foldlM addInstant Map.empty instantEntries
  let totals = Map.unionWith (+) instant (Map.map (\(_, b, _) -> toInteger b) accts)
  when (any (> toInteger (maxBound :: Word64)) (Map.elems totals)) $ unsupported "stake of one credential exceeds Word64"
  pending <- pendingRewardUpdate req
  ir <- instantaneousRewards req
  let pots = st ^. #pots
      ptr = Ptr (SlotNo32 0) (TxIx 0) (CertIx 0)
      accountMap =
        Map.foldrWithKey
          (\c (dep, bal, mp) -> addAccountState c (mkShelleyAccountState ptr (CompactCoin dep) & balanceAccountStateL .~ CompactCoin bal & stakePoolDelegationAccountStateL .~ mp))
          def
          accts
      cs =
        (def :: CertState era)
          & certPStateL . psStakePoolsL .~ Map.fromList [(pid, sps) | (pid, sps, _) <- pools]
          & certPStateL . psRetiringL .~ Map.fromList [(pid, EpochNo e) | (pid, _, Just e) <- pools]
          & certDStateL . accountsL .~ accountMap
          & certDStateL . dsIRewardsL .~ ir
      ls =
        (def :: LedgerState era)
          & lsCertStateL .~ cs
          & lsUTxOStateL . utxosGovStateL .~ govSt
          & lsUTxOStateL . utxosFeesL .~ maybe (Coin 0) (const (Coin (toInteger (pots ^. #fees)))) (req ^. #maybe'rewardUpdate)
          & lsUTxOStateL . instantStakeL . instantStakeCredentialsL .~ Map.map (CompactCoin . fromInteger) (Map.filter (> 0) instant)
      snapOf k = snapshotOfRows (Map.findWithDefault [] k priors)
      mark0 = snapOf S.SNAPSHOT_KIND_MARK
      before = SnapShots mark0 (calculatePoolDistr mark0) (snapOf S.SNAPSHOT_KIND_SET) (snapOf S.SNAPSHOT_KIND_GO) (Coin 0)
      es0 =
        (def :: EpochState era)
          & esLStateL .~ ls
          & esSnapshotsL .~ before
          & chainAccountStateL .~ ChainAccountState (Coin (toInteger (pots ^. #treasury))) (Coin (toInteger (pots ^. #reserves)))
          & curPParamsEpochStateL .~ pp
          & prevPParamsEpochStateL .~ pp
      nes0 = (def :: NewEpochState era) {nesEL = EpochNo (epoch - 1), nesEs = es0, nesPd = calculatePoolDistr mark0, nesRu = pending}
      opts = ApplySTSOpts {asoAssertions = AssertionsOff, asoValidation = ValidateNone, asoEvents = EPReturn}
      ((nes1, _failures), _events) =
        runIdentity . (`runReaderT` globals {quorum = req ^. #globals . #quorum}) $
          applySTSOpts @(EraRule "NEWEPOCH" era) opts (TRC ((), nes0, EpochNo epoch))
      es1 = nesEs nes1
      ppAfter = es1 ^. curPParamsEpochStateL
      cas1 = es1 ^. chainAccountStateL
      acctsAfter = es1 ^. esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      poolsAfter = es1 ^. esLStateL . lsCertStateL . certPStateL . psStakePoolsL
      changed =
        [ defMessage & #credential .~ credProto c & #deposit .~ unCompactCoin (a ^. depositAccountStateL)
            & #balance .~ unCompactCoin (a ^. balanceAccountStateL)
            & #poolId .~ maybe BS.empty (\(KeyHash h) -> hashToBytes h) (a ^. stakePoolDelegationAccountStateL)
        | (c, (_, bal, mp)) <- Map.toList accts
        , Just a <- [Map.lookup c acctsAfter]
        , unCompactCoin (a ^. balanceAccountStateL) /= bal || a ^. stakePoolDelegationAccountStateL /= mp
        ]
      removed = [hashToBytes h | (pid@(KeyHash h), _, _) <- pools, not (pid `Map.member` poolsAfter)]
      rowsOf k ss =
        L.sortOn (\e -> (fromEnum (e ^. #credential . #kind), e ^. #credential . #hash))
          [ defMessage & #snapshot .~ k & #credential .~ credProto c & #amount .~ a & #poolId .~ hashToBytes ph
          | (c, StakeWithDelegation nz (KeyHash ph)) <- VMap.toList (unActiveStake (ssActiveStake ss))
          , let CompactCoin a = unNonZero nz
          ]
      snaps = es1 ^. esSnapshotsL
      deltaOf after before' = fromInteger (unCoin after - toInteger before') :: Int64
  pure $
    defMessage
      & #delta
        .~ ( defMessage
               & #accountsChanged .~ L.sortOn (credKey . (^. #credential)) changed
               & #poolsRemoved .~ L.sort removed
               & #reservesDelta .~ deltaOf (casReserves cas1) (pots ^. #reserves)
               & #treasuryDelta .~ deltaOf (casTreasury cas1) (pots ^. #treasury)
               & (if ppAfter == pp then id else #pparamsAfter .~ toProto ppAfter)
           )
      & #snapshots
        .~ ( rowsOf S.SNAPSHOT_KIND_MARK (ssStakeMark snaps)
               ++ rowsOf S.SNAPSHOT_KIND_SET (ssStakeSet snaps)
               ++ rowsOf S.SNAPSHOT_KIND_GO (ssStakeGo snaps)
           )
  where
    addAccount acc a = do
      c <- credentialOf "accounts" (a ^. #credential)
      when (c `Map.member` acc) $ badRequest "accounts: repeated credential"
      mp <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> poolHashOf "accounts" (a ^. #poolId)
      when (isJust (a ^. #maybe'pointer)) $ unsupported "accounts: pointers are not read"
      when (isJust (a ^. #maybe'drep)) $ unsupported "accounts: a DRep delegation does not exist before Conway"
      pure (Map.insert c (a ^. #deposit, a ^. #balance, mp) acc)
    addInstant acc e = do
      c <- credentialOf "instant_stake" (e ^. #credential)
      pure (Map.insertWith (+) c (toInteger (e ^. #amount)) acc)

newEpochPre ::
  forall era.
  ( EraGov era
  , EraStake era
  , EraCertState era
  , ShelleyEraAccounts era
  , STS (EraRule "NEWEPOCH" era)
  , BaseM (EraRule "NEWEPOCH" era) ~ ShelleyBase
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , Default (StashedAVVMAddresses era)
  , Default (EpochState era)
  , Default (LedgerState era)
  , EraPParams era
  , AtMostEra "Babbage" era
  , GovState era ~ ShelleyGovState era
  ) =>
  (PParams era -> S.ProtocolParameters) ->
  E.EpochNewEpochRequest ->
  [S.StakeEntry] ->
  PParams era ->
  Result E.EpochNewEpochResponse
newEpochPre toProto req instantEntries pp = do
  let proposals = req ^. #state . #pparamProposals
      epoch = req ^. #context . #epoch
      epochLength = 432000
  unless (null proposals || req ^. #globals . #epochLength == epochLength) $ unsupported "parameter proposals need the scratch ledger's epoch length"
  decoded <- traverse proposal proposals
  let slots = map (^. #addedSlot) proposals
  when (or (zipWith (>) slots (drop 1 slots))) $ unsupported "proposals are not in chain order"
  forM_ proposals $ \p -> do
    let made = p ^. #addedSlot `div` epochLength
        t = p ^. #targetEpoch
    unless (made == t || made + 1 == t) $ unsupported "proposal targets neither its own epoch nor the next"
  case sequence decoded of
    Left msg -> pure (defMessage & #rejection .~ rejection "PPUP" "DeserialiseFailure" msg)
    Right ups ->
      let start = (epoch - 1) * epochLength
          forEpoch = [(gk, slot, u) | (gk, t, slot, u) <- ups, epoch > 0, t == epoch - 1]
          madeNow = Map.fromList [(gk, u) | (gk, slot, u) <- forEpoch, slot >= start]
          carried = Map.fromList [(gk, u) | (gk, slot, u) <- forEpoch, slot < start]
          kept = if all (hasLegalProtVerUpdate pp) carried then carried else Map.empty
          cur = ProposedPPUpdates (Map.union madeNow kept)
          -- The previous boundary left the future parameters potential and
          -- TICK solidifies them once past the point of no return; UPEC
          -- adopts only the solidified ones.
          govSt =
            ShelleyGovState
              { sgsCurProposals = cur
              , sgsFutureProposals = ProposedPPUpdates Map.empty
              , sgsCurPParams = pp
              , sgsPrevPParams = pp
              , sgsFuturePParams = solidifyFuturePParams (PotentialPParamsUpdate (votedFuturePParams cur pp (req ^. #globals . #quorum)))
              }
       in newEpochPreRun toProto req instantEntries pp govSt
  where
    proposal p = do
      when (BS.length (p ^. #genesisKeyHash) /= 28) $ badRequest "genesis key hash must be 28 bytes"
      gk <- maybe (badRequest "genesis key hash must be 28 bytes") (pure . KeyHash) (hashFromBytes (p ^. #genesisKeyHash))
      pure $ case decodeFull (eraProtVerLow @era) (BSL.fromStrict (p ^. #update)) of
        Left e -> Left (T.pack (show e))
        Right u -> Right (gk, p ^. #targetEpoch, p ^. #addedSlot, u)

-- | The InstantaneousRewards the ended epoch's MIR certificates accumulated,
-- with mir's domain: protocol versions 2 to 8, no negative or repeated entry,
-- and pots and deltas that fit Word64.
instantaneousRewards :: E.EpochNewEpochRequest -> Result InstantaneousRewards
instantaneousRewards req = do
  let ms = req ^. #mir
  unless (null ms && req ^. #reservesToTreasury == 0 && req ^. #treasuryToReserves == 0) $ do
    let major = req ^. #pparams . #protocolVersion . #major
    when (major < 2 || major >= 9) $ unsupported "MIR exists in protocol versions 2 to 8"
  forM_ ms $ \m -> do
    unless (m ^. #source `elem` [E.MIR_SOURCE_RESERVES, E.MIR_SOURCE_TREASURY]) $ unsupported "mir source unspecified"
    when (m ^. #amount < 0) $ unsupported "negative mir delta"
  noDuplicates "mir entry" [(fromEnum (m ^. #source), credKey (m ^. #credential)) | m <- ms]
  let total =
        toInteger (req ^. #state . #pots . #reserves) + toInteger (req ^. #state . #pots . #treasury)
          + sum [toInteger (m ^. #amount) | m <- ms]
          + toInteger (req ^. #reservesToTreasury) + toInteger (req ^. #treasuryToReserves)
  when (total > toInteger (maxBound :: Word64)) $ unsupported "pots and deltas overflow Word64"
  entries <- traverse (\m -> (,,) (m ^. #source) <$> cred (m ^. #credential) <*> pure (Coin (toInteger (m ^. #amount)))) ms
  let pot s = Map.fromListWith (<>) [(c, a) | (s', c, a) <- entries, s' == s]
      r2t = toInteger (req ^. #reservesToTreasury)
      t2r = toInteger (req ^. #treasuryToReserves)
  pure
    InstantaneousRewards
      { iRReserves = pot E.MIR_SOURCE_RESERVES
      , iRTreasury = pot E.MIR_SOURCE_TREASURY
      , deltaReserves = DeltaCoin (t2r - r2t)
      , deltaTreasury = DeltaCoin (r2t - t2r)
      }

-- | The governance part of a NEWEPOCH request: the state RATIFY decided over
-- when the DRep pulser was started at the boundary into the previous epoch.
data NewEpochGov = NewEpochGov
  { ngGovState :: ConwayGovState ConwayEra
  , ngDReps :: Map.Map (Credential DRepRole) DRepState
  , ngCommitteeState :: CommitteeState ConwayEra
  , ngProposals :: Proposals ConwayEra
  , ngRatified :: RatifyState ConwayEra
  }

-- | Governance state is present when any of its tables or distributions is.
-- It is decoded as for gov_ratify, taken at the previous epoch; RATIFY runs
-- over it ('ratifyTables') and its result stands as the completed pulser the
-- EPOCH rule consumes.
newEpochGov :: E.EpochNewEpochRequest -> PParams ConwayEra -> Result (Maybe NewEpochGov)
newEpochGov req pp
  | not present = pure Nothing
  | otherwise = do
      let epoch = req ^. #context . #epoch
      constitution <- constitutionArg (st ^. #maybe'enactState >>= (^. #maybe'constitution))
      (rs, props, _) <- ratifyTables (epoch - 1) pp constitution st (req ^. #poolDistribution) (req ^. #drepDistribution)
      (committee, cs) <- committeeArg (st ^. #maybe'committee)
      dreps <- forM (st ^. #dreps) $ \d -> do
        c <- credentialOf "dreps credential" (d ^. #credential)
        pure (c, DRepState (EpochNo (d ^. #expiry)) SNothing (CompactCoin (d ^. #deposit)) Set.empty)
      let gs =
            (def :: ConwayGovState ConwayEra)
              & cgsProposalsL .~ props
              & cgsCommitteeL .~ committee
              & cgsConstitutionL .~ constitution
              & cgsDRepPulsingStateL .~ DRComplete def rs
              -- The previous boundary left the future parameters potential,
              -- the NEWEPOCH ticks since predict the pulser's, and TICK
              -- solidifies them once past the point of no return; the EPOCH
              -- rule adopts only the solidified ones.
              & cgsFuturePParamsL .~ PotentialPParamsUpdate Nothing
              & predictFuturePParams
              & cgsFuturePParamsL %~ solidifyFuturePParams
      pure (Just (NewEpochGov gs (Map.fromList dreps) cs props rs))
  where
    st = req ^. #state
    present =
      not (null (st ^. #dreps)) || not (null (st ^. #proposals)) || not (null (st ^. #votes))
        || isJust (st ^. #maybe'committee) || isJust (st ^. #maybe'enactState)
        || not (null (req ^. #poolDistribution)) || not (null (req ^. #drepDistribution))

-- | The verdict RATIFY reached: the enacted actions in order and the expired
-- ones in canonical order.
govVerdict :: NewEpochGov -> E.EpochNewEpochResponse -> E.EpochNewEpochResponse
govVerdict g resp =
  resp
    & #enacted .~ map (actionIdProto . gasId) (toList (rsEnacted rs))
    & #expired .~ sortedIds (Set.toList (rsExpired rs))
  where
    rs = ngRatified g

-- | What the boundary did to the governance state: the proposals that left
-- (enacted, expired, and their subtrees), the DReps whose expiry changed, the
-- enacted committee when a committee action was enacted, and the roots and
-- constitution after.
govDelta :: S.LedgerState -> EpochState ConwayEra -> NewEpochGov -> S.LedgerDelta -> S.LedgerDelta
govDelta st es1 g delta =
  delta
    & #proposalsRemoved .~ sortedIds (Set.toList removed)
    & #drepsChanged .~ changed
    & maybe id (#committeeAfter .~) committeeAfter
    & #enactStateAfter .~ enactState
  where
    gs1 = es1 ^. esLStateL . lsUTxOStateL . utxosGovStateL :: ConwayGovState ConwayEra
    before = Set.fromList (toList (proposalsIds (ngProposals g)))
    removed = before `Set.difference` Set.fromList (toList (proposalsIds (gs1 ^. cgsProposalsL)))
    dreps1 = es1 ^. esLStateL . lsCertStateL . certVStateL . vsDRepsL
    changed =
      L.sortOn
        (\r -> (fromEnum (r ^. #credential . #kind), r ^. #credential . #hash))
        [ r & #expiry .~ e1
        | r <- st ^. #dreps
        , Right c <- [credentialOf "dreps credential" (r ^. #credential)]
        , Just d1 <- [Map.lookup c dreps1]
        , let EpochNo e1 = drepExpiry d1
        , e1 /= r ^. #expiry
        ]
    committeeAfter
      | any committeeAction (toList (rsEnacted (ngRatified g))) = Just (committeeProto (gs1 ^. cgsCommitteeL))
      | otherwise = Nothing
    committeeAction gas = case pProcGovAction (gasProposalProcedure gas) of
      NoConfidence {} -> True
      UpdateCommittee {} -> True
      _ -> False
    GovRelation rPP rHF rCC rCons = govStatePrevGovActionIds @ConwayEra gs1
    rootProto = \case
      SJust i -> Just (actionIdProto (unGovPurposeId i))
      SNothing -> Nothing
    enactState =
      (defMessage :: S.EnactState)
        & maybe id (#prevPparamUpdate .~) (rootProto rPP)
        & maybe id (#prevHardFork .~) (rootProto rHF)
        & maybe id (#prevCommittee .~) (rootProto rCC)
        & maybe id (#prevConstitution .~) (rootProto rCons)
        & maybe id (#constitution .~) (constitutionProto (gs1 ^. cgsConstitutionL) (st ^. #maybe'enactState >>= (^. #maybe'constitution)))

-- | The DRep a request account delegates to. An unset target is malformed.
drepFromProto :: P.DRep -> Result DRep
drepFromProto d = case d ^. #maybe'target of
  Nothing -> badRequest "accounts: drep target unset"
  Just (P.DRep'AlwaysAbstain _) -> pure DRepAlwaysAbstain
  Just (P.DRep'AlwaysNoConfidence _) -> pure DRepAlwaysNoConfidence
  Just (P.DRep'Credential c) -> DRepCredential <$> cred' c
  where
    cred' c = case c ^. #kind of
      P.CREDENTIAL_KIND_KEY -> maybe bad (pure . KeyHashObj . KeyHash) (hashFromBytes (c ^. #hash))
      P.CREDENTIAL_KIND_SCRIPT -> maybe bad (pure . ScriptHashObj . ScriptHash) (hashFromBytes (c ^. #hash))
      _ -> badRequest "credential kind"
    bad = badRequest "credential hash must be 28 bytes"

drepToProto :: DRep -> P.DRep
drepToProto = \case
  DRepAlwaysAbstain -> defMessage & #alwaysAbstain .~ True
  DRepAlwaysNoConfidence -> defMessage & #alwaysNoConfidence .~ True
  DRepCredential c -> defMessage & #credential .~ credProto c

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'EpochSnapshotRotate q -> Just (respond #epochSnapshotRotate (snapshotRotate q))
  Call'EpochNewEpoch q -> Just (respond #epochNewEpoch (newEpoch q))
  Call'EpochDonations q -> Just (respond #epochDonations (donations q))
  Call'EpochPoolReap q -> Just (respond #epochPoolReap (poolReap q))
  Call'EpochMir q -> Just (respond #epochMir (mir q))
  Call'EpochHardFork q -> Just (respond #epochHardFork (hardFork q))
  Call'EpochObligations q -> Just (respond #epochObligations (obligations q))
  Call'EpochPparamUpdate q -> Just (respond #epochPparamUpdate (pparamUpdate q))
  _ -> Nothing

implemented :: [Text]
implemented = ["epoch_donations", "epoch_pool_reap", "epoch_mir", "epoch_pparam_update", "epoch_obligations", "epoch_hard_fork", "epoch_snapshot_rotate", "epoch_new_epoch"]
