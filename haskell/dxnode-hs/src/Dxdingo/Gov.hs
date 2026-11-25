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
import Cardano.Ledger.BaseTypes
  ( BoundedRational (unboundRational)
  , EpochInterval (..)
  , Network (Testnet)
  , ProtVer (..)
  , StrictMaybe (..)
  , UnitInterval
  , mkVersion
  , textToUrl
  , unsafeNonZero
  )
import Cardano.Ledger.Binary (decodeFull)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State (ConwayEraAccounts (..), VState (..))
import Cardano.Ledger.Conway.Rules
  ( committeeAcceptedRatio
  , dRepAcceptedRatio
  , spoAcceptedRatio
  , updateDormantDRepExpiry
  )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Slotting.Slot (EpochNo (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Foldable (foldl', toList)
import qualified Data.OMap.Strict as OMap
import Data.List (sortOn)
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

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'GovThresholds q -> Just (respond #govThresholds (govThresholds q))
  Call'GovSpoRatio q -> Just (respond #govSpoRatio (govSpoRatio q))
  Call'GovDrepRatio q -> Just (respond #govDrepRatio (govDRepRatio q))
  Call'GovCommitteeRatio q -> Just (respond #govCommitteeRatio (govCommitteeRatio q))
  Call'GovCommitteeQuorum q -> Just (respond #govCommitteeQuorum (govCommitteeQuorum q))
  Call'GovProposalTree q -> Just (respond #govProposalTree (govProposalTree q))
  Call'GovDrepExpiry q -> Just (respond #govDrepExpiry (govDRepExpiry q))
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
  ]
