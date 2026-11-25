{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Conway governance, answered by cardano-ledger-conway: the voting
-- thresholds (@Governance/Internal.hs@) and the accepted ratios of the
-- RATIFY rule (@Rules/Ratify.hs@).
module Dxdingo.Gov
  ( dispatch
  , implemented
  ) where

import Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes, hashToBytes)
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import Cardano.Ledger.Babbage.Core (CoinPerByte (..), ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes
  ( BoundedRational (boundRational, unboundRational)
  , EpochInterval (..)
  , EpochSize (..)
  , Globals (..)
  , Network (Testnet)
  , ProtVer (..)
  , StrictMaybe (..)
  , UnitInterval
  , mkActiveSlotCoeff
  , mkVersion
  , textToUrl
  , unsafeNonZero
  , urlToText
  )
import Cardano.Ledger.Binary (decodeFull, getVersion)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.State (ConwayEraAccounts (..), VState (..))
import Cardano.Ledger.Conway.Rules
  ( ENACT
  , EPOCH
  , EnactSignal (..)
  , RATIFY
  , committeeAcceptedRatio
  , dRepAcceptedRatio
  , spoAcceptedRatio
  , updateDormantDRepExpiry
  )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Shelley.LedgerState (EpochState (..), esLStateL, lsCertStateL, lsUTxOStateL, utxosGovStateL)
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition.Extended (STS (..), TRC (..), applySTS)
import Data.Functor.Identity (runIdentity)
import qualified Data.Sequence.Strict as SSeq
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Foldable (foldl', toList)
import qualified Data.OMap.Strict as OMap
import Data.List (sortOn)
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Gov
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

type Cw = ConwayEra

-- Argument decoding ------------------------------------------------------

decodeAction :: ByteString -> Result (GovAction Cw)
decodeAction b
  | BS.null b = badRequest "action: empty"
  | otherwise = case decodeFull (eraProtVerLow @Cw) (BL.fromStrict b) of
      Left e -> badRequest ("action: " <> T.pack (show e))
      Right a -> pure a

hashArg :: HashAlgorithm h => Text -> Int -> ByteString -> Result (Hash h a)
hashArg name size b = case hashFromBytes b of
  Just h | BS.length b == size -> pure h
  _ -> badRequest (name <> ": want " <> T.pack (show size) <> " bytes")

keyHashArg :: Text -> ByteString -> Result (KeyHash r)
keyHashArg name = fmap KeyHash . hashArg name 28

credentialArg :: Text -> Maybe P.Credential -> Result (Credential r)
credentialArg name = \case
  Nothing -> badRequest (name <> ": missing credential")
  Just c -> case c ^. #kind of
    P.CREDENTIAL_KIND_KEY -> KeyHashObj <$> keyHashArg name (c ^. #hash)
    P.CREDENTIAL_KIND_SCRIPT -> ScriptHashObj . ScriptHash <$> hashArg name 28 (c ^. #hash)
    _ -> badRequest (name <> ": credential kind")

voteArg :: P.Vote -> Result Vote
voteArg = \case
  P.VOTE_NO -> pure VoteNo
  P.VOTE_YES -> pure VoteYes
  P.VOTE_ABSTAIN -> pure Abstain
  _ -> badRequest "vote: unspecified"

protVerArg :: Maybe P.ProtocolVersion -> Result ProtVer
protVerArg mpv = do
  let pv = fromMaybe defMessage mpv
  v <- maybe (unsupported "protocol major version") pure (mkVersion (toInteger (pv ^. #major)))
  pure (ProtVer v (fromIntegral (pv ^. #minor)))

-- | The parameters these functions read. An unset threshold group keeps the
-- era's empty value (zero), like dxnode-go.
paramsArg :: Maybe S.ProtocolParameters -> Maybe P.ProtocolVersion -> Result (PParams Cw)
paramsArg mpp mpv = do
  pv <- protVerArg mpv
  let pp = fromMaybe defMessage mpp
  pool <- case pp ^. #maybe'poolVotingThresholds of
    Nothing -> pure def
    Just t ->
      PoolVotingThresholds
        <$> unit "pool motion_no_confidence" (t ^. #maybe'motionNoConfidence)
        <*> unit "pool committee_normal" (t ^. #maybe'committeeNormal)
        <*> unit "pool committee_no_confidence" (t ^. #maybe'committeeNoConfidence)
        <*> unit "pool hard_fork_initiation" (t ^. #maybe'hardForkInitiation)
        <*> unit "pool pp_security_group" (t ^. #maybe'ppSecurityGroup)
  drep <- case pp ^. #maybe'drepVotingThresholds of
    Nothing -> pure def
    Just t ->
      DRepVotingThresholds
        <$> unit "drep motion_no_confidence" (t ^. #maybe'motionNoConfidence)
        <*> unit "drep committee_normal" (t ^. #maybe'committeeNormal)
        <*> unit "drep committee_no_confidence" (t ^. #maybe'committeeNoConfidence)
        <*> unit "drep update_to_constitution" (t ^. #maybe'updateToConstitution)
        <*> unit "drep hard_fork_initiation" (t ^. #maybe'hardForkInitiation)
        <*> unit "drep pp_network_group" (t ^. #maybe'ppNetworkGroup)
        <*> unit "drep pp_economic_group" (t ^. #maybe'ppEconomicGroup)
        <*> unit "drep pp_technical_group" (t ^. #maybe'ppTechnicalGroup)
        <*> unit "drep pp_gov_group" (t ^. #maybe'ppGovGroup)
        <*> unit "drep treasury_withdrawal" (t ^. #maybe'treasuryWithdrawal)
  pure
    ( emptyPParams
        & ppProtocolVersionL .~ pv
        & ppPoolVotingThresholdsL .~ pool
        & ppDRepVotingThresholdsL .~ drep
        & ppCommitteeMinSizeL .~ fromIntegral (fromMaybe 0 (pp ^. #maybe'committeeMinSize))
        & ppCommitteeMaxTermLengthL .~ EpochInterval (fromIntegral (fromMaybe 0 (pp ^. #maybe'committeeMaxTermLength)))
    )
  where
    unit :: Text -> Maybe P.Rational -> Result UnitInterval
    unit = unitIntervalArg

-- | The enacted committee and the committee state (hot key authorizations
-- and resignations) of a request committee.
committeeArg :: Maybe S.Committee -> Result (StrictMaybe (Committee Cw), CommitteeState Cw)
committeeArg Nothing = pure (SNothing, CommitteeState Map.empty)
committeeArg (Just c) = do
  q <- unitIntervalArg "committee.threshold" (c ^. #maybe'threshold)
  ms <- traverse member (c ^. #members)
  pure
    ( SJust (Committee (Map.fromList [(cold, EpochNo (m ^. #expiry)) | (cold, m, _) <- ms]) q)
    , CommitteeState (Map.fromList [(cold, a) | (cold, _, Just a) <- ms])
    )
  where
    member m = do
      cold <- credentialArg "committee cold" (m ^. #maybe'cold)
      auth <-
        if m ^. #resigned
          then pure (Just (CommitteeMemberResigned SNothing))
          else case m ^. #maybe'hot of
            Nothing -> pure Nothing
            Just h -> Just . CommitteeHotCredential <$> credentialArg "committee hot" (Just h)
      pure (cold, m, auth)

ratifyState :: PParams Cw -> StrictMaybe (Committee Cw) -> RatifyState Cw
ratifyState pp committee =
  RatifyState
    { rsEnactState = def & ensCurPParamsL .~ pp & ensCommitteeL .~ committee
    , rsEnacted = mempty
    , rsExpired = mempty
    , rsDelayed = False
    }

zeroHash :: HashAlgorithm h => Int -> Hash h a
zeroHash n = fromMaybe (error "zeroHash") (hashFromBytes (BS.replicate n 0))

-- | A proposal holding only what the ratios read: the action and the votes.
govActionState :: GovAction Cw -> Map.Map (KeyHash StakePool) Vote -> GovActionState Cw
govActionState action spoVotes =
  GovActionState
    { gasId = GovActionId (TxId (unsafeMakeSafeHash (zeroHash 32))) (GovActionIx 0)
    , gasCommitteeVotes = Map.empty
    , gasDRepVotes = Map.empty
    , gasStakePoolVotes = spoVotes
    , gasProposalProcedure =
        ProposalProcedure
          { pProcDeposit = Coin 0
          , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (KeyHash (zeroHash 28))))
          , pProcGovAction = action
          , pProcAnchor = Anchor (fromMaybe (error "url") (textToUrl 64 "x")) (unsafeMakeSafeHash (zeroHash 32))
          }
    , gasProposedIn = EpochNo 0
    , gasExpiresAfter = EpochNo 0
    }

sameId :: Maybe P.GovActionId -> Maybe P.GovActionId -> Bool
sameId a b = idKey a == idKey b
  where
    idKey m = let x = fromMaybe defMessage m in (x ^. #txId, x ^. #index)

-- | The votes of one role on one action, keyed by voter.
votesOf ::
  P.VoterRole ->
  Maybe P.GovActionId ->
  [S.VoteRecord] ->
  (P.Voter -> Result k) ->
  Result [(k, Vote)]
votesOf role aid votes key =
  traverse
    (\v -> (,) <$> key (v ^. #voter) <*> voteArg (v ^. #vote))
    [v | v <- votes, v ^. #voter . #role == role, sameId aid (v ^. #maybe'actionId)]

-- gov_thresholds ----------------------------------------------------------

-- | Each role's threshold from the reference's @votingXThreshold@, set only
-- when the role votes and the threshold exists.
govThresholds :: GovThresholdsRequest -> Result GovThresholdsResponse
govThresholds req = do
  action <- decodeAction (req ^. #action)
  pp <- paramsArg (req ^. #maybe'pparams) (req ^. #maybe'protocolVersion)
  (committee, cs) <- committeeArg (req ^. #maybe'committee)
  let rs = ratifyState pp committee
      epoch = EpochNo (req ^. #epoch)
      spo
        | isStakePoolVotingAllowed action = votingStakePoolThreshold rs action
        | otherwise = SNothing
      drep
        | isDRepVotingAllowed action = votingDRepThreshold rs action
        | otherwise = SNothing
      cc
        | isCommitteeVotingAllowed epoch cs action = votingCommitteeThreshold epoch rs cs action
        | otherwise = SNothing
      set f = \case
        SJust t -> f .~ rationalProto (unboundRational t)
        SNothing -> id
  pure (defMessage & set #spo spo & set #drep drep & set #committee cc)

-- gov_spo_ratio -----------------------------------------------------------

govSpoRatio :: GovSpoRatioRequest -> Result GovSpoRatioResponse
govSpoRatio req = do
  action <- decodeAction (req ^. #action)
  pv <- protVerArg (req ^. #maybe'protocolVersion)
  dist <- traverse poolEntry (req ^. #poolDistribution)
  let total = sum (map snd dist)
  totalNZ <-
    if total == 0
      then unsupported "pool distribution has no stake"
      else pure (unsafeNonZero (Coin (toInteger total)))
  pools <- traverse poolState (req ^. #pools)
  accounts <- accountsArg (req ^. #accounts)
  votes <- votesOf P.VOTER_ROLE_STAKE_POOL (req ^. #maybe'actionId) (req ^. #votes) (keyHashArg "vote pool_id" . (^. #poolId))
  let env =
        (def :: RatifyEnv Cw)
          { reStakePoolDistr =
              PoolDistr
                (Map.fromList [(h, IndividualPoolStake 0 (CompactCoin s) (VRFVerKeyHash (zeroHash 32))) | (h, s) <- dist])
                totalNZ
          , reAccounts = accounts
          , reStakePools = Map.fromList pools
          }
  pure (defMessage & #ratio .~ rationalProto (spoAcceptedRatio env (govActionState action (Map.fromList votes)) pv))
  where
    poolEntry p = do
      h <- keyHashArg "pool_distribution pool_id" (p ^. #poolId)
      pure (h, p ^. #stake)

poolState :: S.Pool -> Result (KeyHash StakePool, StakePoolState)
poolState p = do
  h <- keyHashArg "pools pool_id" (p ^. #poolId)
  ra <- credentialArg "pools reward_account" (p ^. #maybe'rewardAccount)
  pure
    ( h
    , StakePoolState
        { spsVrf = VRFVerKeyHash (zeroHash 32)
        , spsPledge = Coin 0
        , spsCost = Coin 0
        , spsMargin = minBound
        , spsAccountId = AccountId ra
        , spsOwners = Set.empty
        , spsRelays = mempty
        , spsMetadata = SNothing
        , spsDeposit = CompactCoin 0
        , spsDelegators = Set.empty
        }
    )

accountsArg :: [S.Account] -> Result (Accounts Cw)
accountsArg as = foldl' (flip ($)) def <$> traverse one as
  where
    one a = do
      cred <- credentialArg "accounts credential" (a ^. #maybe'credential)
      delegation <- traverse drepArg (a ^. #maybe'drep)
      pure (addAccountState cred (mkConwayAccountState (CompactCoin 0) & dRepDelegationAccountStateL .~ delegation))

drepArg :: P.DRep -> Result DRep
drepArg d = case d ^. #maybe'target of
  Nothing -> badRequest "drep target unset"
  Just (P.DRep'AlwaysAbstain _) -> pure DRepAlwaysAbstain
  Just (P.DRep'AlwaysNoConfidence _) -> pure DRepAlwaysNoConfidence
  Just (P.DRep'Credential c) -> DRepCredential <$> credentialArg "drep" (Just c)

-- gov_drep_ratio ----------------------------------------------------------

govDRepRatio :: GovDRepRatioRequest -> Result GovDRepRatioResponse
govDRepRatio req = do
  action <- decodeAction (req ^. #action)
  states <- traverse drepState (req ^. #dreps)
  dist <- traverse distEntry (req ^. #drepDistribution)
  votes <- votesOf P.VOTER_ROLE_DREP (req ^. #maybe'actionId) (req ^. #votes) (credentialArg "vote credential" . (^. #maybe'credential))
  let env =
        (def :: RatifyEnv Cw)
          { reDRepDistr = Map.fromListWith (<>) dist
          , reDRepState = Map.fromList states
          , reCurrentEpoch = EpochNo (req ^. #epoch)
          }
  pure (defMessage & #ratio .~ rationalProto (dRepAcceptedRatio env (Map.fromList votes) action))
  where
    drepState d = do
      c <- credentialArg "dreps credential" (d ^. #maybe'credential)
      pure (c, DRepState (EpochNo (d ^. #expiry)) SNothing (CompactCoin 0) Set.empty)
    distEntry e = do
      d <- maybe (badRequest "drep_distribution: missing drep") drepArg (e ^. #maybe'drep)
      pure (d, CompactCoin (e ^. #stake))

-- gov_committee_ratio -----------------------------------------------------

govCommitteeRatio :: GovCommitteeRatioRequest -> Result GovCommitteeRatioResponse
govCommitteeRatio req = do
  (committee, cs) <- committeeArg (req ^. #maybe'committee)
  votes <- votesOf P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE (req ^. #maybe'actionId) (req ^. #votes) (credentialArg "vote credential" . (^. #maybe'credential))
  let members = case committee of
        SJust c -> committeeMembers c
        SNothing -> Map.empty
      ratio = committeeAcceptedRatio members (Map.fromList votes) cs (EpochNo (req ^. #epoch))
  pure (defMessage & #ratio .~ rationalProto ratio)

-- gov_committee_quorum ----------------------------------------------------

-- | The threshold of an action that needs committee approval (a treasury
-- withdrawal); unset when the committee cannot approve.
govCommitteeQuorum :: GovCommitteeQuorumRequest -> Result GovCommitteeQuorumResponse
govCommitteeQuorum req = do
  pp <- paramsArg (req ^. #maybe'pparams) (req ^. #maybe'protocolVersion)
  (committee, cs) <- committeeArg (req ^. #maybe'committee)
  let rs = ratifyState pp committee
      action = TreasuryWithdrawals Map.empty SNothing :: GovAction Cw
  pure $ case votingCommitteeThreshold (EpochNo (req ^. #epoch)) rs cs action of
    SJust t -> defMessage & #quorum .~ rationalProto (unboundRational t)
    SNothing -> defMessage

-- gov_proposal_tree -------------------------------------------------------

actionIdArg :: Maybe P.GovActionId -> Result GovActionId
actionIdArg Nothing = badRequest "action id: missing"
actionIdArg (Just i) = do
  h <- hashArg "action id tx_id" 32 (i ^. #txId)
  ix <- if i ^. #index > 0xffff then unsupported "action id index above 65535" else pure (fromIntegral (i ^. #index))
  pure (GovActionId (TxId (unsafeMakeSafeHash h)) (GovActionIx ix))

actionIdProto :: GovActionId -> P.GovActionId
actionIdProto (GovActionId (TxId h) (GovActionIx ix)) =
  defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral ix

-- | Canonical order: tx id, then index.
sortedIds :: [GovActionId] -> [P.GovActionId]
sortedIds = sortOn (\i -> (i ^. #txId, i ^. #index)) . map actionIdProto

-- | A proposal as the reference stores it: the action and its deposit data;
-- votes are not read by the proposal forest.
proposalState :: S.Proposal -> Result (GovActionState Cw)
proposalState p = do
  gid <- actionIdArg (p ^. #maybe'id)
  action <- decodeAction (p ^. #action)
  ret <- credentialArg "return_account" (p ^. #maybe'returnAccount)
  pure
    GovActionState
      { gasId = gid
      , gasCommitteeVotes = Map.empty
      , gasDRepVotes = Map.empty
      , gasStakePoolVotes = Map.empty
      , gasProposalProcedure =
          ProposalProcedure
            { pProcDeposit = Coin (toInteger (p ^. #deposit))
            , pProcReturnAddr = AccountAddress Testnet (AccountId ret)
            , pProcGovAction = action
            , pProcAnchor = Anchor (fromMaybe (error "url") (textToUrl 64 "x")) (unsafeMakeSafeHash (zeroHash 32))
            }
      , gasProposedIn = EpochNo (p ^. #proposedEpoch)
      , gasExpiresAfter = EpochNo (p ^. #expiresEpoch)
      }

-- | 'proposalsApplyEnactment' on the forest the proposals form under the
-- enacted roots. A forest the reference cannot build ('mkProposals' fails) is
-- UNSUPPORTED. Removed is everything that leaves without being enacted:
-- the expired proposals, the competitors of enacted ones, and their
-- descendants.
govProposalTree :: GovProposalTreeRequest -> Result GovProposalTreeResponse
govProposalTree req = do
  states <- traverse proposalState (req ^. #proposals)
  let byId = Map.fromList [(gasId g, g) | g <- states]
  if Map.size byId /= length states then badRequest "duplicate proposal" else pure ()
  roots <- rootsArg (req ^. #maybe'enactState)
  forest <- maybe (unsupported "proposals do not form a forest under the enacted roots") pure (mkProposals roots (OMap.fromFoldable states))
  enactedIds <- traverse (actionIdArg . Just) (req ^. #enacted)
  expiredIds <- traverse (actionIdArg . Just) (req ^. #expired)
  enacted <- traverse (\i -> maybe (badRequest "enacted: unknown proposal") pure (Map.lookup i byId)) enactedIds
  mapM_ (\i -> maybe (badRequest "expired: unknown proposal") (const (pure ())) (Map.lookup i byId)) expiredIds
  let (forest', _, removedEnact, removedExpired) = proposalsApplyEnactment (Seq.fromList enacted) (Set.fromList expiredIds) forest
  pure
    ( defMessage
        & #remaining .~ sortedIds (toList (proposalsIds forest'))
        & #removed .~ sortedIds (Map.keys removedEnact ++ Map.keys removedExpired)
    )

rootsArg :: Maybe S.EnactState -> Result (GovRelation StrictMaybe)
rootsArg Nothing = pure (GovRelation SNothing SNothing SNothing SNothing)
rootsArg (Just e) =
  GovRelation
    <$> root (e ^. #maybe'prevPparamUpdate)
    <*> root (e ^. #maybe'prevHardFork)
    <*> root (e ^. #maybe'prevCommittee)
    <*> root (e ^. #maybe'prevConstitution)
  where
    root Nothing = pure SNothing
    root m = SJust . GovPurposeId <$> actionIdArg m

-- gov_drep_expiry ---------------------------------------------------------

-- | 'updateDormantDRepExpiry': the pending dormant-epoch count is added to
-- every DRep expiry unless the sum is below the current epoch. Lists the
-- DReps whose expiry changed.
govDRepExpiry :: GovDRepExpiryRequest -> Result GovDRepExpiryResponse
govDRepExpiry req = do
  entries <- traverse entry (req ^. #dreps)
  let vs =
        VState
          { vsDReps = Map.fromList [(c, DRepState (EpochNo (r ^. #expiry)) SNothing (CompactCoin 0) Set.empty) | (c, r) <- entries]
          , vsCommitteeState = def
          , vsNumDormantEpochs = EpochNo (req ^. #dormantEpochs)
          }
      after = vsDReps (updateDormantDRepExpiry (EpochNo (req ^. #epoch)) vs)
      changed =
        [ r & #expiry .~ e
        | (c, r) <- entries
        , Just st <- [Map.lookup c after]
        , let EpochNo e = drepExpiry st
        , e /= r ^. #expiry
        ]
  pure (defMessage & #drepsChanged .~ sortOn (\r -> (fromEnum (r ^. #credential . #kind), r ^. #credential . #hash)) changed)
  where
    entry r = do
      c <- credentialArg "dreps credential" (r ^. #maybe'credential)
      pure (c, r)

-- Boundary rules ------------------------------------------------------------

-- | The constants the rules read. RATIFY, ENACT and the pieces of EPOCH these
-- functions reach read none of them; the record only has to be total.
globals :: Globals
globals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , slotsPerKESPeriod = 100
    , stabilityWindow = 10
    , randomnessStabilisationWindow = 10
    , securityParameter = unsafeNonZero 5
    , maxKESEvo = 10
    , quorum = 5
    , maxLovelaceSupply = 45000000000000000
    , activeSlotCoeff = mkActiveSlotCoeff (fromMaybe maxBound (boundRational (1 % 20)))
    , networkId = Testnet
    , systemStart = SystemStart (posixSecondsToUTCTime 0)
    }

runRatify :: RatifyEnv Cw -> RatifyState Cw -> RatifySignal Cw -> Result (RatifyState Cw)
runRatify env st sig =
  case runIdentity (runReaderT (applySTS @(RATIFY Cw) (TRC (env, st, sig))) globals) of
    Right s -> pure s
    Left _ -> internal "RATIFY failed"

runEnact :: EnactState Cw -> EnactSignal Cw -> Result (EnactState Cw)
runEnact st sig =
  case runIdentity (runReaderT (applySTS @(ENACT Cw) (TRC ((), st, sig))) globals) of
    Right s -> pure s
    Left _ -> internal "ENACT failed"

runEpoch :: EpochNo -> EpochState Cw -> Result (EpochState Cw)
runEpoch eNo st =
  case runIdentity (runReaderT (applySTS @(EPOCH Cw) (TRC ((), st, eNo))) globals) of
    Right s -> pure s
    Left _ -> internal "EPOCH failed"

-- | The epoch state the EPOCH rule runs on: the proposals, the accounts, the
-- treasury, and the result of a RATIFY pass waiting to be applied. Everything
-- else is the era's empty value.
epochStateOf :: PParams Cw -> Proposals Cw -> Accounts Cw -> Coin -> RatifyState Cw -> EpochState Cw
epochStateOf pp props accounts treasury rs =
  (def :: EpochState Cw)
    { esChainAccountState = (esChainAccountState (def :: EpochState Cw)) & casTreasuryL .~ treasury
    }
    & esLStateL . lsUTxOStateL . utxosGovStateL .~ gov
    & esLStateL . lsCertStateL . certDStateL . accountsL .~ accounts
  where
    gov =
      (def :: ConwayGovState Cw)
        & cgsProposalsL .~ props
        & cgsCurPParamsL .~ pp
        & cgsPrevPParamsL .~ pp
        & cgsDRepPulsingStateL .~ DRComplete def rs

forestOf :: GovRelation StrictMaybe -> [GovActionState Cw] -> Result (Proposals Cw)
forestOf roots gass = do
  when (Map.size (Map.fromList [(gasId g, ()) | g <- gass]) /= length gass) (badRequest "duplicate proposal")
  maybe (unsupported "proposals do not form a forest under the enacted roots") pure (mkProposals roots (OMap.fromFoldable gass))

-- | The proposal with the votes on it.
proposalWith :: [S.VoteRecord] -> S.Proposal -> Result (GovActionState Cw)
proposalWith votes p = do
  gas <- proposalState p
  let aid = p ^. #maybe'id
  cc <- votesOf P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE aid votes (credentialArg "vote credential" . (^. #maybe'credential))
  dr <- votesOf P.VOTER_ROLE_DREP aid votes (credentialArg "vote credential" . (^. #maybe'credential))
  sp <- votesOf P.VOTER_ROLE_STAKE_POOL aid votes (keyHashArg "vote pool_id" . (^. #poolId))
  pure gas {gasCommitteeVotes = Map.fromList cc, gasDRepVotes = Map.fromList dr, gasStakePoolVotes = Map.fromList sp}

credProto :: Credential r -> P.Credential
credProto = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

-- | Canonical order of credentials: kind, then hash.
credOrder :: P.Credential -> (Int, ByteString)
credOrder c = (fromEnum (c ^. #kind), c ^. #hash)

-- gov_ratify ----------------------------------------------------------------

-- | The RATIFY rule over the request's tables. The proposals are walked in
-- the pulser's order ('reorderActions' of the submission order).
--
-- A request whose proposals are not in submission order (by proposed epoch),
-- or hold one that expired before the previous epoch, is UNSUPPORTED: dingo
-- orders its proposals by proposed epoch and a proposal that old has already
-- left the set on both nodes.
govRatify :: GovRatifyRequest -> Result GovRatifyResponse
govRatify req = do
  let st = fromMaybe defMessage (req ^. #maybe'state)
      mpp = req ^. #maybe'pparams
      epochW = fromMaybe defMessage (req ^. #maybe'context) ^. #epoch
  when (epochW == 0) (unsupported "no ratification before epoch 1")
  pp <- paramsArg mpp (mpp >>= (^. #maybe'protocolVersion))
  checkRatifyProposals epochW (st ^. #proposals)
  gass <- traverse (proposalWith (st ^. #votes)) (st ^. #proposals)
  roots <- rootsArg (st ^. #maybe'enactState)
  _ <- forestOf roots gass
  (committee, cs) <- committeeArg (st ^. #maybe'committee)
  drepStates <- traverse drepStateOf (st ^. #dreps)
  accounts <- accountsArg (st ^. #accounts)
  pools <- traverse poolState (st ^. #pools)
  poolDist <- traverse poolEntry (req ^. #poolDistribution)
  let total = sum (map snd poolDist)
  totalNZ <-
    if total == 0
      then unsupported "pool distribution has no stake"
      else pure (unsafeNonZero (Coin (toInteger total)))
  drepDist <- traverse distEntry (req ^. #drepDistribution)
  let env =
        (def :: RatifyEnv Cw)
          { reStakePoolDistr =
              PoolDistr
                (Map.fromList [(h, IndividualPoolStake 0 (CompactCoin s) (VRFVerKeyHash (zeroHash 32))) | (h, s) <- poolDist])
                totalNZ
          , reDRepDistr = Map.fromListWith (<>) drepDist
          , reDRepState = Map.fromList drepStates
          , reCurrentEpoch = EpochNo epochW
          , reCommitteeState = cs
          , reAccounts = accounts
          , reStakePools = Map.fromList pools
          }
      enact =
        def
          & ensCurPParamsL .~ pp
          & ensCommitteeL .~ committee
          & ensTreasuryL .~ Coin (toInteger (fromMaybe defMessage (st ^. #maybe'pots) ^. #treasury))
          & ensPrevGovActionIdsL .~ roots
  rs <- runRatify env (RatifyState enact mempty mempty False) (RatifySignal (reorderActions (SSeq.fromList gass)))
  pure
    ( defMessage
        & #enacted .~ map (actionIdProto . gasId) (toList (rsEnacted rs))
        & #expired .~ sortedIds (Set.toList (rsExpired rs))
        & #delayed .~ rsDelayed rs
    )
  where
    poolEntry p = do
      h <- keyHashArg "pool_distribution pool_id" (p ^. #poolId)
      pure (h, p ^. #stake)
    distEntry e = do
      d <- maybe (badRequest "drep_distribution: missing drep") drepArg (e ^. #maybe'drep)
      pure (d, CompactCoin (e ^. #stake))
    drepStateOf d = do
      c <- credentialArg "dreps credential" (d ^. #maybe'credential)
      pure (c, DRepState (EpochNo (d ^. #expiry)) SNothing (CompactCoin 0) Set.empty)

checkRatifyProposals :: Word64 -> [S.Proposal] -> Result ()
checkRatifyProposals epoch ps = do
  when (or (zipWith (\a b -> b ^. #proposedEpoch < a ^. #proposedEpoch) ps (drop 1 ps))) (unsupported "proposals are not in submission order")
  unless (all (\p -> p ^. #expiresEpoch + 1 >= epoch) ps) (unsupported "a proposal expired before the previous epoch")

-- gov_proposal_expiry ---------------------------------------------------------

-- | Every voting threshold 1, protocol version 10: with no stake behind any
-- action, RATIFY accepts nothing.
expiryParams :: PParams Cw
expiryParams =
  emptyPParams
    & ppProtocolVersionL .~ ProtVer (fromMaybe (error "version") (mkVersion (10 :: Integer))) 0
    & ppPoolVotingThresholdsL .~ PoolVotingThresholds one one one one one
    & ppDRepVotingThresholdsL .~ DRepVotingThresholds one one one one one one one one one one
  where
    one = maxBound :: UnitInterval

-- | The proposals that leave at the boundary that expires them and what they
-- pay back. RATIFY, with nothing accepted, decides which proposals expired;
-- the EPOCH rule that applies the verdict removes them with their subtrees
-- and returns each deposit to its registered account or, unclaimed, to the
-- treasury. The removed proposals, the refunds and the treasury are read off
-- the state the rule returns.
govProposalExpiry :: GovProposalExpiryRequest -> Result GovProposalExpiryResponse
govProposalExpiry req = do
  let epochW = req ^. #epoch
  when (epochW == 0) (unsupported "no expiry before epoch 1")
  gass <- traverse proposalState (req ^. #proposals)
  props <- forestOf (GovRelation SNothing SNothing SNothing SNothing) gass
  accounts <- balanceAccountsArg (req ^. #accounts)
  let env = (def :: RatifyEnv Cw) {reCurrentEpoch = EpochNo epochW}
      rs0 = RatifyState (def & ensCurPParamsL .~ expiryParams) mempty mempty False
  rs <- runRatify env rs0 (RatifySignal (reorderActions (SSeq.fromList gass)))
  es <- runEpoch (EpochNo (epochW + 1)) (epochStateOf expiryParams props accounts (Coin 0) rs)
  let after = es ^. esLStateL . lsUTxOStateL . utxosGovStateL . cgsProposalsL
      removed = Set.fromList (toList (proposalsIds props)) `Set.difference` Set.fromList (toList (proposalsIds after))
  pure
    ( defMessage
        & #expired .~ sortedIds (Set.toList removed)
        & #refunds .~ balanceCredits accounts (accountBalances es)
        & #unclaimedToTreasury .~ treasuryOf es
    )

treasuryOf :: EpochState Cw -> Word64
treasuryOf es = fromInteger (unCoin (casTreasury (esChainAccountState es)))

accountBalances :: EpochState Cw -> Accounts Cw
accountBalances es = es ^. esLStateL . lsCertStateL . certDStateL . accountsL

-- | The accounts with their balances and no delegation.
balanceAccountsArg :: [S.Account] -> Result (Accounts Cw)
balanceAccountsArg as = foldl' (flip ($)) def <$> traverse one as
  where
    one a = do
      cred <- credentialArg "accounts credential" (a ^. #maybe'credential)
      pure (addAccountState cred (mkConwayAccountState (CompactCoin 0) & balanceAccountStateL .~ CompactCoin (a ^. #balance)))

balanceMap :: Accounts Cw -> Map.Map (Credential Staking) Word64
balanceMap accts = Map.map (\a -> let CompactCoin b = a ^. balanceAccountStateL in b) (accts ^. accountsMapL)

-- | The accounts whose balance rose, with the increase, in canonical order.
balanceCredits :: Accounts Cw -> Accounts Cw -> [S.Credit]
balanceCredits before after =
  sortOn
    (credOrder . (^. #credential))
    [ defMessage & #credential .~ credProto c & #amount .~ (b' - b)
    | (c, b) <- Map.toList (balanceMap before)
    , Just b' <- [Map.lookup c (balanceMap after)]
    , b' /= b
    ]

-- gov_enact -----------------------------------------------------------------

-- | The parameters of a Conway request in full, but for the cost models.
fullParamsArg :: Maybe S.ProtocolParameters -> Result (PParams Cw)
fullParamsArg mpp = do
  base <- paramsArg mpp (mpp >>= (^. #maybe'protocolVersion))
  let p = fromMaybe defMessage mpp
  a0 <- boundedArg "a0" (p ^. #maybe'a0)
  rho <- boundedArg "rho" (p ^. #maybe'rho)
  tau <- boundedArg "tau" (p ^. #maybe'tau)
  mem <- optional (p ^. #maybe'priceMem) "price_mem"
  steps <- optional (p ^. #maybe'priceSteps) "price_steps"
  refScript <- optional (p ^. #maybe'minFeeRefScriptCostPerByte) "min_fee_ref_script_cost_per_byte"
  pure $
    base
      & ppMinFeeAL .~ Coin (toInteger (p ^. #minFeeA))
      & ppMinFeeBL .~ Coin (toInteger (p ^. #minFeeB))
      & ppMaxBBSizeL .~ (p ^. #maxBlockBodySize)
      & ppMaxTxSizeL .~ (p ^. #maxTxSize)
      & ppMaxBHSizeL .~ fromIntegral (p ^. #maxBlockHeaderSize)
      & ppKeyDepositL .~ Coin (toInteger (p ^. #keyDeposit))
      & ppPoolDepositL .~ Coin (toInteger (p ^. #poolDeposit))
      & ppEMaxL .~ EpochInterval (fromIntegral (p ^. #maxEpoch))
      & ppNOptL .~ fromIntegral (p ^. #nOpt)
      & ppA0L .~ a0
      & ppRhoL .~ rho
      & ppTauL .~ tau
      & ppMinPoolCostL .~ Coin (toInteger (p ^. #minPoolCost))
      & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin (p ^. #coinsPerUtxoByte))
      & ppPricesL .~ Prices mem steps
      & ppMaxTxExUnitsL .~ ExUnits (fromIntegral (p ^. #maxTxExMem)) (fromIntegral (p ^. #maxTxExSteps))
      & ppMaxBlockExUnitsL .~ ExUnits (fromIntegral (p ^. #maxBlockExMem)) (fromIntegral (p ^. #maxBlockExSteps))
      & ppMaxValSizeL .~ (p ^. #maxValueSize)
      & ppCollateralPercentageL .~ fromIntegral (p ^. #collateralPercentage)
      & ppMaxCollateralInputsL .~ fromIntegral (p ^. #maxCollateralInputs)
      & ppGovActionLifetimeL .~ EpochInterval (fromIntegral (p ^. #govActionLifetime))
      & ppGovActionDepositL .~ Coin (toInteger (p ^. #govActionDeposit))
      & ppDRepDepositL .~ Coin (toInteger (p ^. #drepDeposit))
      & ppDRepActivityL .~ EpochInterval (fromIntegral (p ^. #drepActivity))
      & ppMinFeeRefScriptCostPerByteL .~ refScript
  where
    optional :: BoundedRational b => Maybe P.Rational -> Text -> Result b
    optional Nothing _ = maybe (badRequest "zero rational") pure (boundRational 0)
    optional m name = boundedArg name m

-- | The same parameters back as a request carries them.
pparamsProto :: PParams Cw -> S.ProtocolParameters
pparamsProto pp =
  defMessage
    & #minFeeA .~ coin (pp ^. ppMinFeeAL)
    & #minFeeB .~ coin (pp ^. ppMinFeeBL)
    & #maxBlockBodySize .~ (pp ^. ppMaxBBSizeL)
    & #maxTxSize .~ (pp ^. ppMaxTxSizeL)
    & #maxBlockHeaderSize .~ fromIntegral (pp ^. ppMaxBHSizeL)
    & #keyDeposit .~ coin (pp ^. ppKeyDepositL)
    & #poolDeposit .~ coin (pp ^. ppPoolDepositL)
    & #maxEpoch .~ interval (pp ^. ppEMaxL)
    & #nOpt .~ fromIntegral (pp ^. ppNOptL)
    & #a0 .~ rat (pp ^. ppA0L)
    & #rho .~ rat (pp ^. ppRhoL)
    & #tau .~ rat (pp ^. ppTauL)
    & #protocolVersion .~ (defMessage & #major .~ fromIntegral (getVersion (pvMajor pv)) & #minor .~ fromIntegral (pvMinor pv))
    & #minPoolCost .~ coin (pp ^. ppMinPoolCostL)
    & #coinsPerUtxoByte .~ (let CoinPerByte (CompactCoin b) = pp ^. ppCoinsPerUTxOByteL in b)
    & #priceMem .~ rat (prMem prices)
    & #priceSteps .~ rat (prSteps prices)
    & #maxTxExMem .~ fromIntegral (exUnitsMem txEx)
    & #maxTxExSteps .~ fromIntegral (exUnitsSteps txEx)
    & #maxBlockExMem .~ fromIntegral (exUnitsMem blockEx)
    & #maxBlockExSteps .~ fromIntegral (exUnitsSteps blockEx)
    & #maxValueSize .~ (pp ^. ppMaxValSizeL)
    & #collateralPercentage .~ fromIntegral (pp ^. ppCollateralPercentageL)
    & #maxCollateralInputs .~ fromIntegral (pp ^. ppMaxCollateralInputsL)
    & #committeeMinSize .~ fromIntegral (pp ^. ppCommitteeMinSizeL)
    & #committeeMaxTermLength .~ interval (pp ^. ppCommitteeMaxTermLengthL)
    & #govActionLifetime .~ interval (pp ^. ppGovActionLifetimeL)
    & #govActionDeposit .~ coin (pp ^. ppGovActionDepositL)
    & #drepDeposit .~ coin (pp ^. ppDRepDepositL)
    & #drepActivity .~ interval (pp ^. ppDRepActivityL)
    & #minFeeRefScriptCostPerByte .~ rat (pp ^. ppMinFeeRefScriptCostPerByteL)
    & #poolVotingThresholds
      .~ ( defMessage
             & #motionNoConfidence .~ rat (pvtMotionNoConfidence pool)
             & #committeeNormal .~ rat (pvtCommitteeNormal pool)
             & #committeeNoConfidence .~ rat (pvtCommitteeNoConfidence pool)
             & #hardForkInitiation .~ rat (pvtHardForkInitiation pool)
             & #ppSecurityGroup .~ rat (pvtPPSecurityGroup pool)
         )
    & #drepVotingThresholds
      .~ ( defMessage
             & #motionNoConfidence .~ rat (dvtMotionNoConfidence drep)
             & #committeeNormal .~ rat (dvtCommitteeNormal drep)
             & #committeeNoConfidence .~ rat (dvtCommitteeNoConfidence drep)
             & #updateToConstitution .~ rat (dvtUpdateToConstitution drep)
             & #hardForkInitiation .~ rat (dvtHardForkInitiation drep)
             & #ppNetworkGroup .~ rat (dvtPPNetworkGroup drep)
             & #ppEconomicGroup .~ rat (dvtPPEconomicGroup drep)
             & #ppTechnicalGroup .~ rat (dvtPPTechnicalGroup drep)
             & #ppGovGroup .~ rat (dvtPPGovGroup drep)
             & #treasuryWithdrawal .~ rat (dvtTreasuryWithdrawal drep)
         )
  where
    pv = pp ^. ppProtocolVersionL
    prices = pp ^. ppPricesL
    txEx = pp ^. ppMaxTxExUnitsL
    blockEx = pp ^. ppMaxBlockExUnitsL
    pool = pp ^. ppPoolVotingThresholdsL
    drep = pp ^. ppDRepVotingThresholdsL
    coin (Coin c) = fromInteger c
    interval (EpochInterval e) = fromIntegral e
    rat :: BoundedRational b => b -> P.Rational
    rat = rationalProto . unboundRational

-- | The constitution of a request, the era's empty one when unset.
constitutionArg :: Maybe S.Constitution -> Result (Constitution Cw)
constitutionArg Nothing = pure def
constitutionArg (Just c) = do
  h <- hashArg "constitution anchor_hash" 32 (c ^. #anchorHash)
  url <- maybe (badRequest "constitution anchor_url") pure (textToUrl 128 (c ^. #anchorUrl))
  guard' <- case BS.length (c ^. #guardrailScriptHash) of
    0 -> pure SNothing
    _ -> SJust . ScriptHash <$> hashArg "constitution guardrail_script_hash" 28 (c ^. #guardrailScriptHash)
  pure (Constitution (Anchor url (unsafeMakeSafeHash h)) guard')

-- | Enacting one ratified action: the ENACT rule, then the EPOCH rule that
-- applies the enacted state, which pays the withdrawals to registered
-- accounts and returns the deposit.
--
-- The action is the only proposal; it must follow the enact state's root of
-- its purpose (RATIFY established that before the action got here) and a
-- treasury withdrawal must fit the treasury, or the request is UNSUPPORTED.
-- Committee, constitution, roots and parameters are read off the ENACT
-- result; balances and the treasury off the EPOCH result.
govEnact :: GovEnactRequest -> Result GovEnactResponse
govEnact req = do
  action <- decodeAction (req ^. #action)
  gid <- actionIdArg (req ^. #maybe'actionId)
  pp <- fullParamsArg (req ^. #maybe'pparams)
  let st = fromMaybe defMessage (req ^. #maybe'state)
      treasury = Coin (toInteger (fromMaybe defMessage (st ^. #maybe'pots) ^. #treasury))
  case action of
    TreasuryWithdrawals w _ ->
      when (sum (map unCoin (Map.elems w)) > unCoin treasury) (unsupported "withdrawal exceeds the treasury")
    _ -> pure ()
  (committee, _) <- committeeArg (st ^. #maybe'committee)
  roots <- rootsArg (st ^. #maybe'enactState)
  constitution <- constitutionArg (st ^. #maybe'enactState >>= (^. #maybe'constitution))
  accounts <- balanceAccountsArg (st ^. #accounts)
  gas <- enactTarget st (req ^. #maybe'actionId) action gid
  props <- forestOf roots [gas]
  let enact0 =
        def
          & ensCurPParamsL .~ pp
          & ensCommitteeL .~ committee
          & ensConstitutionL .~ constitution
          & ensTreasuryL .~ treasury
          & ensPrevGovActionIdsL .~ roots
  enact1 <- runEnact enact0 (EnactSignal gid action)
  es <- runEpoch (EpochNo 6) (epochStateOf pp props accounts treasury (RatifyState enact1 (Seq.singleton gas) Set.empty False))
  let credits = balanceCredits accounts (accountBalances es)
      changed =
        sortOn
          (credOrder . (^. #credential))
          [ defMessage & #credential .~ credProto c & #deposit .~ 0 & #balance .~ b'
          | (c, b) <- Map.toList (balanceMap accounts)
          , Just b' <- [Map.lookup c (balanceMap (accountBalances es))]
          , b' /= b
          ]
      treasuryAfter = fromInteger (unCoin (casTreasury (esChainAccountState es))) :: Integer
      committeeAfter = case action of
        NoConfidence _ -> Just defMessage
        UpdateCommittee {} -> Just (committeeProto (enact1 ^. ensCommitteeL))
        _ -> Nothing
      GovRelation rPP rHF rCC rCons = enact1 ^. ensPrevGovActionIdsL
      enactState =
        defMessage
          & maybe id (#prevPparamUpdate .~) (rootProto (unGovPurposeId <$> rPP))
          & maybe id (#prevHardFork .~) (rootProto (unGovPurposeId <$> rHF))
          & maybe id (#prevCommittee .~) (rootProto (unGovPurposeId <$> rCC))
          & maybe id (#prevConstitution .~) (rootProto (unGovPurposeId <$> rCons))
          & maybe id (#constitution .~) (constitutionProto (enact1 ^. ensConstitutionL) (st ^. #maybe'enactState >>= (^. #maybe'constitution)))
      delta =
        defMessage
          & #accountsChanged .~ changed
          & #treasuryDelta .~ fromInteger (treasuryAfter - unCoin treasury)
          & maybe id (#committeeAfter .~) committeeAfter
          & #enactStateAfter .~ enactState
          & ( case action of
                ParameterChange {} -> #pparamsAfter .~ pparamsProto (enact1 ^. ensCurPParamsL)
                HardForkInitiation {} -> #pparamsAfter .~ pparamsProto (enact1 ^. ensCurPParamsL)
                _ -> id
            )
  pure (defMessage & #withdrawals .~ credits & #delta .~ delta)
  where
    rootProto :: StrictMaybe GovActionId -> Maybe P.GovActionId
    rootProto = \case
      SJust i -> Just (actionIdProto i)
      SNothing -> Nothing

-- | The proposal being enacted: the state's own when it holds one, else one
-- with no deposit.
enactTarget :: S.LedgerState -> Maybe P.GovActionId -> GovAction Cw -> GovActionId -> Result (GovActionState Cw)
enactTarget st mid action gid = do
  let own = [p | p <- st ^. #proposals, sameId (p ^. #maybe'id) mid]
      (deposit, ret, proposedIn) = case own of
        p : _ -> (p ^. #deposit, p ^. #maybe'returnAccount, p ^. #proposedEpoch)
        [] -> (0, Nothing, 0)
  returnCred <- case ret of
    Just c -> credentialArg "return_account" (Just c)
    Nothing -> pure (KeyHashObj (KeyHash (zeroHash 28)))
  pure
    GovActionState
      { gasId = gid
      , gasCommitteeVotes = Map.empty
      , gasDRepVotes = Map.empty
      , gasStakePoolVotes = Map.empty
      , gasProposalProcedure =
          ProposalProcedure
            { pProcDeposit = Coin (toInteger deposit)
            , pProcReturnAddr = AccountAddress Testnet (AccountId returnCred)
            , pProcGovAction = action
            , pProcAnchor = Anchor (fromMaybe (error "url") (textToUrl 64 "x")) (unsafeMakeSafeHash (zeroHash 32))
            }
      , gasProposedIn = EpochNo proposedIn
      , gasExpiresAfter = EpochNo 100
      }

committeeProto :: StrictMaybe (Committee Cw) -> S.Committee
committeeProto = \case
  SNothing -> defMessage
  SJust (Committee ms t) ->
    defMessage
      & #members
        .~ sortOn
          (credOrder . (^. #cold))
          [defMessage & #cold .~ credProto c & #expiry .~ e | (c, EpochNo e) <- Map.toList ms]
      & #threshold .~ rationalProto (unboundRational t)

-- | The enact state's constitution, unset when the request set none and the
-- enacted state still holds the era's empty one.
constitutionProto :: Constitution Cw -> Maybe S.Constitution -> Maybe S.Constitution
constitutionProto c requested
  | Nothing <- requested, c == def = Nothing
  | otherwise =
      Just
        ( defMessage
            & #anchorUrl .~ urlToText (anchorUrl (constitutionAnchor c))
            & #anchorHash .~ hashToBytes (extractHash (anchorDataHash (constitutionAnchor c)))
            & #guardrailScriptHash .~ (case constitutionGuardrailsScriptHash c of SJust (ScriptHash h) -> hashToBytes h; SNothing -> BS.empty)
        )

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'GovThresholds q -> Just (respond #govThresholds (govThresholds q))
  Call'GovSpoRatio q -> Just (respond #govSpoRatio (govSpoRatio q))
  Call'GovDrepRatio q -> Just (respond #govDrepRatio (govDRepRatio q))
  Call'GovCommitteeRatio q -> Just (respond #govCommitteeRatio (govCommitteeRatio q))
  Call'GovCommitteeQuorum q -> Just (respond #govCommitteeQuorum (govCommitteeQuorum q))
  Call'GovProposalTree q -> Just (respond #govProposalTree (govProposalTree q))
  Call'GovDrepExpiry q -> Just (respond #govDrepExpiry (govDRepExpiry q))
  Call'GovRatify q -> Just (respond #govRatify (govRatify q))
  Call'GovEnact q -> Just (respond #govEnact (govEnact q))
  Call'GovProposalExpiry q -> Just (respond #govProposalExpiry (govProposalExpiry q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "gov_thresholds"
  , "gov_spo_ratio"
  , "gov_drep_ratio"
  , "gov_committee_ratio"
  , "gov_committee_quorum"
  , "gov_proposal_tree"
  , "gov_drep_expiry"
  , "gov_ratify"
  , "gov_enact"
  , "gov_proposal_expiry"
  ]
