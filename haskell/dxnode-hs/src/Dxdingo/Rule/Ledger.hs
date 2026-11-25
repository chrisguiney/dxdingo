{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The composed ledger rules PPUP, GOV, LEDGER and BBODY, answered by running
-- the real STS rule of cardano-ledger in 'ShelleyBase'. The state is built from
-- the request tables and the output state is diffed against the input into a
-- 'LedgerDelta'.
module Dxdingo.Rule.Ledger
  ( dispatch
  , implemented
  -- * For the divergence checks in the tests
  , ppupDirectFailures
  , withdrawalsDelegatedDirect
  , ledgersDirectFailures
  , babbageLedgerDirectFailures
  , legacyBbodyDirectFailures
  , reencodeBabbageOutput
  ) where

import Cardano.Crypto.Hash (Hash, HashAlgorithm)
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes
  ( Anchor (..)
  , BoundedRational (boundRational)
  , EpochInterval (..)
  , EpochSize (..)
  , Globals (..)
  , Network (..)
  , ShelleyBase
  , StrictMaybe (..)
  , TxIx (..)
  , mkActiveSlotCoeff
  , nonZero
  , textToUrl
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (isValidTxL), pattern IsValid, totExUnits)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Block (BbodySignal (..), Block (..), EraBlockHeader, slotNoBlockHeaderL)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import Cardano.Protocol.TPraos.BlockHeader (BHeader)
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure)
import Cardano.Ledger.Shelley.Rules (ShelleyBbodyPredFailure, ShelleyLedgersPredFailure)
import qualified Data.ByteString.Lazy as BL
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Conway.Rules (GovEnv (..), GovSignal (..), totalRefScriptSizeInBlock, validateWithdrawalsDelegated)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (mkStAnnTx))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.Rules (BbodyEnv (..), LedgerEnv (..), ShelleyBbodyState (..), ShelleyLedgerPredFailure (..), ShelleyLedgersEnv (..))
import qualified Data.Sequence.Strict as StrictSeq
import Cardano.Ledger.State
import Data.Int (Int64)
import Data.Word (Word64)
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad (filterM)
import Data.Default (def)
import Data.Foldable (foldl')
import qualified Data.OMap.Strict as OMap
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Dxdingo.Rule.Certs as Certs
import Cardano.Ledger.Binary (Annotator, DecCBOR, DecoderError, decCBOR, decodeFull', decodeFullAnnotator, serialize')
import Cardano.Ledger.Core
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..), KeyHash (..), VRFVerKeyHash (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.API (ApplyTx)
import Cardano.Ledger.Shelley.Governance (FuturePParams (..), ShelleyGovState (..))
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.Rules (PpupEnv (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..))
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Crypto.Hash (hashFromBytes, hashToBytes)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition.Extended (STS (..), TRC (..), applySTS)
import qualified Data.ByteString as BS
import Data.Functor.Identity (runIdentity)
import Data.List (nub, sort, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Dxdingo.Rule.Ledger.Base
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Rule
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

-- ---------------------------------------------------------------------
-- PPUP

ppupGenDelegs :: S.LedgerState -> Result GenDelegs
ppupGenDelegs st = GenDelegs . Map.fromList <$> traverse row (st ^. #genesisDelegations)
  where
    row d = do
      g <- hash28 "genesis_delegations genesis_key_hash" (d ^. #genesisKeyHash)
      k <- hash28 "genesis_delegations delegate_key_hash" (d ^. #delegateKeyHash)
      v <- case hashFromBytes (d ^. #vrfKeyHash) of
        Just h | BS.length (d ^. #vrfKeyHash) == 32 -> pure (VRFVerKeyHash h)
        _ -> badRequest "genesis_delegations vrf_key_hash: want 32 bytes"
      pure (KeyHash g, GenDelegPair (KeyHash k) v)
    hash28 name b = case hashFromBytes b of
      Just h | BS.length b == 28 -> pure h
      _ -> badRequest (name <> ": want 28 bytes")

-- | The pending proposals of the table: a row targeting the current epoch is
-- a current proposal, one targeting the next epoch a future proposal.
ppupProposals ::
  forall era. EraPParams era => EpochNo -> S.LedgerState -> Result (ProposedPPUpdates era, ProposedPPUpdates era, [(KeyHash GenesisRole, EpochNo, PParamsUpdate era)])
ppupProposals cur st = do
  rows <- traverse row (st ^. #pparamProposals)
  let pick e = ProposedPPUpdates (Map.fromList [(k, u) | (k, t, u) <- rows, t == e])
  pure (pick cur, pick (succ cur), rows)
  where
    row p = do
      k <- case hashFromBytes (p ^. #genesisKeyHash) of
        Just h | BS.length (p ^. #genesisKeyHash) == 28 -> pure (KeyHash h)
        _ -> badRequest "pparam_proposals genesis_key_hash: want 28 bytes"
      let t = EpochNo (p ^. #targetEpoch)
      if t == cur || t == succ cur then pure () else unsupported "pparam_proposals target is not the current or next epoch"
      u <- either (\e -> badRequest ("pparam_proposals update: " <> T.pack (show e))) pure (decodeFull' (eraProtVerLow @era) (p ^. #update))
      pure (k, t, u)

runPpup ::
  forall era.
  ( ShelleyEraTxBody era
  , EraTx era
  , EraPParams era
  , STS (EraRule "PPUP" era)
  , BaseM (EraRule "PPUP" era) ~ ShelleyBase
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Show (PredicateFailure (EraRule "PPUP" era))
  , Embed (PredicateFailure (EraRule "PPUP" era))
  ) =>
  (S.ProtocolParameters -> Result (PParams era)) ->
  RulePpupRequest ->
  Result RulePpupResponse
runPpup mkPP req = do
  let ctx = fromMaybe defMessage (req ^. #maybe'context)
      g = fromMaybe defMessage (req ^. #maybe'globals)
      st = fromMaybe defMessage (req ^. #maybe'state)
  globals <- globalsArg g (fromIntegral (ctx ^. #networkId))
  pp <- mkPP (fromMaybe defMessage (req ^. #maybe'pparams))
  tx <- txArg @era (req ^. #tx)
  gds <- ppupGenDelegs st
  let slot = ctx ^. #slot
      cur = EpochNo (slot `div` (g ^. #epochLength))
  (curP, futP, rows) <- ppupProposals @era cur st
  let gov = ShelleyGovState curP futP pp pp NoPParamsUpdate
      update = tx ^. bodyTxL . updateTxBodyL
      trc = TRC (PPUPEnv (SlotNo slot) pp gds, gov, update)
  case runIdentity (runReaderT (applySTS @(EraRule "PPUP" era) trc) globals) of
    Left fs ->
      pure (defMessage & #maybe'rejection .~ rejection (sort (nub (concatMap (nameFailure "PPUP") (NE.toList fs)))))
    Right gov' ->
      pure (defMessage & #delta .~ ppupDelta @era cur rows gov')

ppupDelta ::
  forall era.
  EraPParams era =>
  EpochNo -> [(KeyHash GenesisRole, EpochNo, PParamsUpdate era)] -> ShelleyGovState era -> S.LedgerDelta
ppupDelta cur before gov' =
  defMessage & #pparamProposalsChanged .~ sortOn (\p -> (p ^. #genesisKeyHash, p ^. #targetEpoch)) changed
  where
    ProposedPPUpdates c = sgsCurProposals gov'
    ProposedPPUpdates f = sgsFutureProposals gov'
    old = Map.fromList [((k, t), u) | (k, t, u) <- before]
    after = [((k, cur), u) | (k, u) <- Map.toList c] ++ [((k, succ cur), u) | (k, u) <- Map.toList f]
    changed =
      [ defMessage
          & #genesisKeyHash .~ keyBytes k
          & #targetEpoch .~ unEpochNo t
          & #update .~ serialize' (eraProtVerLow @era) u
      | ((k, t), u) <- after
      , Map.lookup (k, t) old /= Just u
      ]
    keyBytes (KeyHash h) = hashToBytes h

onPpupEra :: RulePpupRequest -> Result RulePpupResponse
onPpupEra req = do
  era <- eraArg (req ^. #context . #era)
  case era of
    RShelley -> runPpup @ShelleyEra pparamsShelley req
    RAllegra -> runPpup @AllegraEra pparamsAllegra req
    RMary -> runPpup @MaryEra pparamsMary req
    RAlonzo -> runPpup @AlonzoEra pparamsAlonzo req
    RBabbage -> runPpup @BabbageEra pparamsBabbage req
    RConway -> unsupported "the PPUP rule exists from Shelley to Babbage, not in Conway"

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'RulePpup q -> Just (respond #rulePpup (onPpupEra q))
  Call'RuleGov q -> Just (respond #ruleGov (ruleGov q))
  Call'RuleLedger q -> Just (respond #ruleLedger (ruleLedger q))
  Call'RuleBlockBody q -> Just (respond #ruleBlockBody (ruleBlockBody q))
  _ -> Nothing

implemented :: [Text]
implemented = ["rule_ppup", "rule_gov", "rule_ledger", "rule_block_body"]

-- ---------------------------------------------------------------------
-- GOV

type Cw = ConwayEra

-- | A hash of the given size from a request field.
sizedHash :: HashAlgorithm h => Text -> Int -> BS.ByteString -> Result (Hash h a)
sizedHash = Certs.rawHash

txIdArg :: Text -> BS.ByteString -> Result TxId
txIdArg name b = TxId . unsafeMakeSafeHash <$> sizedHash name 32 b

govIdArg :: Maybe P.GovActionId -> Result GovActionId
govIdArg Nothing = badRequest "gov action id missing"
govIdArg (Just g) = do
  t <- txIdArg "gov action id tx_id" (g ^. #txId)
  pure (GovActionId t (GovActionIx (fromIntegral (g ^. #index))))

govIdProto :: GovActionId -> P.GovActionId
govIdProto (GovActionId (TxId h) (GovActionIx ix)) =
  defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral ix

networkOf :: S.LedgerContext -> Network
networkOf ctx = if ctx ^. #networkId == 0 then Testnet else Mainnet

voteArg :: P.Vote -> Result Vote
voteArg = \case
  P.VOTE_NO -> pure VoteNo
  P.VOTE_YES -> pure VoteYes
  P.VOTE_ABSTAIN -> pure Abstain
  _ -> badRequest "vote"

-- | The proposals of the table as a 'Proposals' forest, with the votes of the
-- votes table filed under their actions.
proposalsArg :: S.LedgerContext -> S.LedgerState -> Result (Proposals Cw)
proposalsArg ctx st = do
  states <- traverse govState (st ^. #proposals)
  let omap = foldl' (OMap.||>) OMap.empty states
      enact = fromMaybe defMessage (st ^. #maybe'enactState)
  roots <- do
    a <- root (enact ^. #maybe'prevPparamUpdate)
    b <- root (enact ^. #maybe'prevHardFork)
    c <- root (enact ^. #maybe'prevCommittee)
    d <- root (enact ^. #maybe'prevConstitution)
    pure (GovRelation a b c d)
  maybe (unsupported "proposals do not form a valid forest") pure (mkProposals roots omap)
  where
    root :: Maybe P.GovActionId -> Result (StrictMaybe (GovPurposeId p))
    root Nothing = pure SNothing
    root g = SJust . GovPurposeId <$> govIdArg g
    govState p = do
      gid <- govIdArg (p ^. #maybe'id)
      action <- either (\e -> badRequest ("proposals action: " <> T.pack (show e))) pure (decodeFull' (eraProtVerLow @Cw) (p ^. #action))
      ret <- Certs.credentialArg "proposals return_account" (p ^. #maybe'returnAccount)
      let own v = govIdArg (v ^. #maybe'actionId) >>= \g -> pure (g == gid)
      mine <- filterM own (st ^. #votes)
      votes <- traverse (\v -> (,) <$> pure v <*> voteArg (v ^. #vote)) mine
      cc <- sequence [(\c -> (c, x)) <$> Certs.credentialArg "vote voter" (v ^. #voter . #maybe'credential) | (v, x) <- votes, v ^. #voter . #role == P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE]
      dr <- sequence [(\c -> (c, x)) <$> Certs.credentialArg "vote voter" (v ^. #voter . #maybe'credential) | (v, x) <- votes, v ^. #voter . #role == P.VOTER_ROLE_DREP]
      sp <- sequence [(\h -> (h, x)) <$> (KeyHash <$> sizedHash "vote pool_id" 28 (v ^. #voter . #poolId)) | (v, x) <- votes, v ^. #voter . #role == P.VOTER_ROLE_STAKE_POOL]
      pure
        GovActionState
          { gasId = gid
          , gasCommitteeVotes = Map.fromList cc
          , gasDRepVotes = Map.fromList dr
          , gasStakePoolVotes = Map.fromList sp
          , gasProposalProcedure = ProposalProcedure (Coin (toInteger (p ^. #deposit))) (AccountAddress (networkOf ctx) (AccountId ret)) action dummyAnchor
          , gasProposedIn = EpochNo (p ^. #proposedEpoch)
          , gasExpiresAfter = EpochNo (p ^. #expiresEpoch)
          }

-- | The anchor the table does not carry.
dummyAnchor :: Anchor
dummyAnchor = case (textToUrl 128 "https://example.com/a", hashFromBytes (BS.replicate 32 0x33)) of
  (Just u, Just h) -> Anchor u (unsafeMakeSafeHash h)
  _ -> error "dummyAnchor"

-- | Conway parameters the GOV rule reads, over the shared builder.
govPParams :: S.ProtocolParameters -> Result (PParams Cw)
govPParams p = do
  pp <- pparamsConway p
  pure $
    pp
      & ppGovActionDepositL .~ Coin (toInteger (p ^. #govActionDeposit))
      & ppGovActionLifetimeL .~ EpochInterval (fromIntegral (p ^. #govActionLifetime))
      & ppDRepDepositL .~ Coin (toInteger (p ^. #drepDeposit))
      & ppDRepActivityL .~ EpochInterval (fromIntegral (p ^. #drepActivity))
      & ppCommitteeMaxTermLengthL .~ EpochInterval (fromIntegral (p ^. #committeeMaxTermLength))
      & ppCommitteeMinSizeL .~ fromIntegral (p ^. #committeeMinSize)

constitutionHash :: S.LedgerState -> Result (StrictMaybe ScriptHash)
constitutionHash st = case st ^. #maybe'enactState >>= (^. #maybe'constitution) of
  Just c | not (BS.null (c ^. #guardrailScriptHash)) -> SJust . ScriptHash <$> sizedHash "guardrail_script_hash" 28 (c ^. #guardrailScriptHash)
  _ -> pure SNothing

-- | A transaction's id from the body bytes.
txIdOf :: Tx TopTx Cw -> TxId
txIdOf t = txIdTx t

data GovRun = GovRun
  { grGlobals :: Globals
  , grEnv :: GovEnv Cw
  , grTable :: Certs.Tables
  , grProposals :: Proposals Cw
  , grTx :: Tx TopTx Cw
  , grPP :: PParams Cw
  }

govRun :: S.LedgerContext -> S.ProtocolParameters -> S.LedgerState -> BS.ByteString -> Result GovRun
govRun ctx pparams st txBytes = do
  tx <- txArg @Cw txBytes
  pp <- govPParams pparams
  tables <- Certs.tablesArg st
  props <- proposalsArg ctx st
  guard' <- constitutionHash st
  globals <- fixedGlobals (fromIntegral (ctx ^. #networkId))
  let env = GovEnv (txIdOf tx) (EpochNo (ctx ^. #epoch)) pp guard' (Certs.tCertState tables) (Certs.tCommittee tables)
  pure (GovRun globals env tables props tx pp)

-- | Globals the GOV rule never reads beyond the network.
fixedGlobals :: Word -> Result Globals
fixedGlobals netId = do
  k <- maybe (unsupported "security parameter") pure (nonZero 2160)
  f <- maybe (unsupported "active slot coeff") pure (boundRational (1 / 20))
  pure
    Globals
      { epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)
      , slotsPerKESPeriod = 129600
      , stabilityWindow = 129600
      , randomnessStabilisationWindow = 172800
      , securityParameter = k
      , maxKESEvo = 62
      , quorum = 5
      , maxLovelaceSupply = 45000000000000000
      , activeSlotCoeff = mkActiveSlotCoeff f
      , networkId = if netId == 0 then Testnet else Mainnet
      , systemStart = SystemStart (posixSecondsToUTCTime 0)
      }

govSignal :: Tx TopTx Cw -> GovSignal Cw
govSignal t =
  GovSignal
    (t ^. bodyTxL . votingProceduresTxBodyL)
    (t ^. bodyTxL . proposalProceduresTxBodyL)
    (t ^. bodyTxL . certsTxBodyL)

-- | The failures of GOV on the transaction, or the proposals it leaves.
runGov :: GovRun -> Either [(Text, Text)] (Proposals Cw)
runGov r =
  case runIdentity (runReaderT (applySTS @(EraRule "GOV" Cw) (TRC (grEnv r, grProposals r, govSignal (grTx r)))) (grGlobals r)) of
    Right ps -> Right ps
    Left fs -> Left (sort (nub (concatMap (nameFailure "GOV") (NE.toList fs))))

-- | Proposals and votes the run added, as delta rows.
govDelta :: Proposals Cw -> PParams Cw -> Proposals Cw -> S.LedgerDelta
govDelta before pp after =
  defMessage
    & #proposalsAdded .~ sortOn (\p -> (p ^. #id . #txId, p ^. #id . #index)) [proposalRow pp gas | gas <- newOnes]
    & #votesAdded .~ sortOn voteSortKey (concatMap newVotes (Map.elems afterMap))
  where
    beforeMap = proposalsActionsMap before
    afterMap = proposalsActionsMap after
    newOnes = [g | (k, g) <- Map.toList afterMap, Map.notMember k beforeMap]
    newVotes g =
      let old = Map.lookup (gasId g) beforeMap
          oldCC = maybe Map.empty gasCommitteeVotes old
          oldDR = maybe Map.empty gasDRepVotes old
          oldSP = maybe Map.empty gasStakePoolVotes old
          fresh m o = Map.toList (Map.differenceWith (\a b -> if a == b then Nothing else Just a) m o `Map.union` Map.difference m o)
       in [voteRow (gasId g) (committeeVoter c) v | (c, v) <- fresh (gasCommitteeVotes g) oldCC]
            ++ [voteRow (gasId g) (drepVoter c) v | (c, v) <- fresh (gasDRepVotes g) oldDR]
            ++ [voteRow (gasId g) (poolVoter h) v | (h, v) <- fresh (gasStakePoolVotes g) oldSP]
    committeeVoter c = defMessage & #role .~ P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE & #credential .~ Certs.credProto c
    drepVoter c = defMessage & #role .~ P.VOTER_ROLE_DREP & #credential .~ Certs.credProto c
    poolVoter (KeyHash h) = defMessage & #role .~ P.VOTER_ROLE_STAKE_POOL & #poolId .~ hashToBytes h
    voteSortKey v = (v ^. #actionId . #txId, v ^. #actionId . #index, v ^. #voter . #role, v ^. #voter . #credential . #hash, v ^. #voter . #poolId)

voteRow :: GovActionId -> P.Voter -> Vote -> S.VoteRecord
voteRow gid voter v =
  defMessage
    & #actionId .~ govIdProto gid
    & #voter .~ voter
    & #vote .~ (case v of VoteNo -> P.VOTE_NO; VoteYes -> P.VOTE_YES; Abstain -> P.VOTE_ABSTAIN)

proposalRow :: PParams Cw -> GovActionState Cw -> S.Proposal
proposalRow pp g =
  defMessage
    & #id .~ govIdProto (gasId g)
    & #actionType .~ actionType (pProcGovAction proc)
    & #action .~ serialize' (eraProtVerLow @Cw) (pProcGovAction proc)
    & #maybe'parent .~ parent (pProcGovAction proc)
    & #proposedEpoch .~ unEpochNo (gasProposedIn g)
    & #expiresEpoch .~ unEpochNo (gasExpiresAfter g)
    & #deposit .~ (case pProcDeposit proc of Coin c -> fromInteger c)
    & #returnAccount .~ (case pProcReturnAddr proc of AccountAddress _ (AccountId c) -> Certs.credProto c)
  where
    _ = pp
    proc = gasProposalProcedure g
    pid (GovPurposeId x) = govIdProto x
    parent = \case
      ParameterChange (SJust x) _ _ -> Just (pid x)
      HardForkInitiation (SJust x) _ -> Just (pid x)
      NoConfidence (SJust x) -> Just (pid x)
      UpdateCommittee (SJust x) _ _ _ -> Just (pid x)
      NewConstitution (SJust x) _ -> Just (pid x)
      _ -> Nothing
    actionType = \case
      ParameterChange {} -> P.GOV_ACTION_TYPE_PARAMETER_CHANGE
      HardForkInitiation {} -> P.GOV_ACTION_TYPE_HARD_FORK_INITIATION
      TreasuryWithdrawals {} -> P.GOV_ACTION_TYPE_TREASURY_WITHDRAWALS
      NoConfidence {} -> P.GOV_ACTION_TYPE_NO_CONFIDENCE
      UpdateCommittee {} -> P.GOV_ACTION_TYPE_UPDATE_COMMITTEE
      NewConstitution {} -> P.GOV_ACTION_TYPE_NEW_CONSTITUTION
      InfoAction -> P.GOV_ACTION_TYPE_INFO

ruleGov :: RuleGovRequest -> Result RuleGovResponse
ruleGov q = do
  r <-
    govRun
      (fromMaybe defMessage (q ^. #maybe'context))
      (fromMaybe defMessage (q ^. #maybe'pparams))
      (fromMaybe defMessage (q ^. #maybe'state))
      (q ^. #tx)
  pure $ case runGov r of
    Left fs -> defMessage & #maybe'rejection .~ rejection fs
    Right after -> defMessage & #delta .~ govDelta (grProposals r) (grPP r) after

-- ---------------------------------------------------------------------
-- LEDGER

txInProto :: TxIn -> P.TxIn
txInProto (TxIn (TxId h) (TxIx ix)) =
  defMessage & #txId .~ hashToBytes (extractHash h) & #index .~ fromIntegral ix

utxoRow :: TxIn -> TxOut Cw -> S.UtxoEntry
utxoRow i o = defMessage & #txin .~ txInProto i & #txout .~ serialize' (eraProtVerLow @Cw) o

constitutionArg :: S.LedgerState -> Result (Constitution Cw)
constitutionArg st = case st ^. #maybe'enactState >>= (^. #maybe'constitution) of
  Nothing -> pure (Constitution dummyAnchor SNothing)
  Just c -> do
    a <- Certs.anchorArgOf (c ^. #anchorUrl) (c ^. #anchorHash)
    h <- constitutionHash st
    pure (Constitution (case a of SJust x -> x; SNothing -> dummyAnchor) h)

coinWord :: Coin -> Word64
coinWord (Coin c) = fromInteger c

signedDelta :: Coin -> Coin -> Int64
signedDelta (Coin a) (Coin b) = fromInteger (a - b)

-- | The ledger state a whole-transaction or whole-block run starts from, built
-- from the request tables.
data Boot = Boot
  { bGlobals :: Globals
  , bPP :: PParams Cw
  , bUtxo :: UTxO Cw
  , bProps :: Proposals Cw
  , bLS :: LedgerState Cw
  , bAccount :: ChainAccountState
  }

conwayBoot :: S.LedgerContext -> S.LedgerState -> S.Globals -> S.EraHistory -> S.ProtocolParameters -> Result Boot
conwayBoot ctx st g hist pparams = do
  globals0 <- globalsArg g (fromIntegral (ctx ^. #networkId))
  globals <- if null (hist ^. #eras) then pure globals0 else historyGlobals hist globals0
  pp <- govPParams pparams
  utxo <- utxoArg @Cw st
  tables <- Certs.tablesArg st
  props <- proposalsArg ctx st
  constitution <- constitutionArg st
  let gov =
        (emptyGovState :: GovState Cw)
          & proposalsGovStateL .~ props
          & committeeGovStateL .~ Certs.tCommittee tables
          & constitutionGovStateL .~ constitution
          & curPParamsGovStateL .~ pp
          & prevPParamsGovStateL .~ pp
      us = smartUTxOState pp utxo (Coin 0) (Coin 0) gov (Coin 0)
      pots = fromMaybe defMessage (st ^. #maybe'pots)
      acct = ChainAccountState (Coin (toInteger (pots ^. #treasury))) (Coin (toInteger (pots ^. #reserves)))
  pure (Boot globals pp utxo props (LedgerState us (Certs.tCertState tables)) acct)

ledgerConway :: RuleLedgerRequest -> Result RuleLedgerResponse
ledgerConway q = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  Boot globals pp utxo props ls acct <-
    conwayBoot ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  tx <- txArg @Cw (q ^. #tx)
  let env = LedgerEnv (SlotNo (ctx ^. #slot)) (Just (EpochNo (ctx ^. #epoch))) (TxIx 0) pp acct
      annotated = mkStAnnTx (epochInfo globals) (systemStart globals) pp utxo mempty tx
      flagged = case tx ^. isValidTxL of IsValid b -> b
  case runIdentity (runReaderT (applySTS @(EraRule "LEDGER" Cw) (TRC (env, ls, annotated))) globals) of
    Left fs ->
      let failures = sort (nub (concatMap (nameFailure "LEDGER") (NE.toList fs)))
       in pure
            ( defMessage
                & #maybe'rejection .~ rejection failures
                & #scriptsValid .~ (("UTXOS", "ValidationTagMismatch") `elem` failures && not flagged)
            )
    Right ls' ->
      pure
        ( defMessage
            & #scriptsValid .~ flagged
            & #delta .~ ledgerDelta (st ^. #maybe'committee) props pp ls ls'
        )

ledgerDelta :: Maybe S.Committee -> Proposals Cw -> PParams Cw -> LedgerState Cw -> LedgerState Cw -> S.LedgerDelta
ledgerDelta reqCommittee props pp before after =
  (Certs.delta reqCommittee (lsCertState before) (lsCertState after))
    & #utxoAdded .~ [utxoRow i o | (i, o) <- sortOn (txInProto . fst) (Map.toList addedU)]
    & #utxoRemoved .~ sortOn (\t -> (t ^. #txId, t ^. #index)) (map txInProto (Map.keys removedU))
    & #proposalsAdded .~ (proposalsPart ^. #proposalsAdded)
    & #votesAdded .~ (proposalsPart ^. #votesAdded)
    & #feesDelta .~ signedDelta (utxosFees usA) (utxosFees usB)
    & #depositsDelta .~ signedDelta (utxosDeposited usA) (utxosDeposited usB)
    & #donationsDelta .~ signedDelta (utxosDonation usA) (utxosDonation usB)
  where
    usB = lsUTxOState before
    usA = lsUTxOState after
    ub = unUTxO (utxosUtxo usB)
    ua = unUTxO (utxosUtxo usA)
    addedU = Map.difference ua ub
    removedU = Map.difference ub ua
    proposalsPart = govDelta props pp (utxosGovState usA ^. proposalsGovStateL)

-- | What the LEDGER rule of a Shelley-family era needs.
type LegacyLedger era =
  ( Certs.ShelleyCerts era
  , EraStake era
  , EraGov era
  , ApplyTx era
  , STS (EraRule "LEDGER" era)
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Embed (PredicateFailure (EraRule "LEDGER" era))
  )

-- | The LEDGER failures of a Shelley-family era: the withdrawal checks are
-- LEDGER's own constructors; UTXOW is named through its embedding and the
-- certificate rules through the shown wrappers down to DELEG or POOL.
legacyLedgerEmbed ::
  ( Show (PredicateFailure (EraRule "UTXOW" era))
  , Embed (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  ShelleyLedgerPredFailure era ->
  Maybe [(Text, Text)]
legacyLedgerEmbed = Just . \case
  UtxowFailure a -> nameFailure "UTXOW" a
  DelegsFailure a -> Certs.flattenShelley (show a)
  ShelleyWithdrawalsMissingAccounts _ -> [("LEDGER", "ShelleyWithdrawalsMissingAccounts")]
  ShelleyIncompleteWithdrawals _ -> [("LEDGER", "ShelleyIncompleteWithdrawals")]

instance Embed (ShelleyLedgerPredFailure ShelleyEra) where embed = legacyLedgerEmbed
instance Embed (ShelleyLedgerPredFailure AllegraEra) where embed = legacyLedgerEmbed
instance Embed (ShelleyLedgerPredFailure MaryEra) where embed = legacyLedgerEmbed
instance Embed (ShelleyLedgerPredFailure AlonzoEra) where embed = legacyLedgerEmbed
instance Embed (ShelleyLedgerPredFailure BabbageEra) where embed = legacyLedgerEmbed

-- | The ledger state a Shelley-family run starts from, built from the request
-- tables.
legacyBoot ::
  forall era.
  LegacyLedger era =>
  (S.ProtocolParameters -> Result (PParams era)) ->
  S.LedgerContext -> S.LedgerState -> S.Globals -> S.EraHistory -> S.ProtocolParameters ->
  Result (Globals, PParams era, UTxO era, LedgerState era, ChainAccountState)
legacyBoot mkPP ctx st g hist pparams = do
  globals0 <- globalsArg g (fromIntegral (ctx ^. #networkId))
  globals <- if null (hist ^. #eras) then pure globals0 else historyGlobals hist globals0
  pp <- mkPP pparams
  utxo <- utxoArg @era st
  certState <- Certs.shelleyState @era st
  let gov = (emptyGovState :: GovState era) & curPParamsGovStateL .~ pp & prevPParamsGovStateL .~ pp
      us = smartUTxOState pp utxo (Coin 0) (Coin 0) gov (Coin 0)
      pots = fromMaybe defMessage (st ^. #maybe'pots)
      acct = ChainAccountState (Coin (toInteger (pots ^. #treasury))) (Coin (toInteger (pots ^. #reserves)))
  pure (globals, pp, utxo, LedgerState us certState, acct)

-- | LEDGER of a Shelley-family era over one transaction. @flagged@ reads the
-- transaction's is_valid flag, which only Alonzo and Babbage have.
ledgerLegacy ::
  forall era.
  LegacyLedger era =>
  (S.ProtocolParameters -> Result (PParams era)) ->
  (Tx TopTx era -> Bool) ->
  RuleLedgerRequest ->
  Result RuleLedgerResponse
ledgerLegacy mkPP isValid q = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  (globals, pp, utxo, ls, acct) <-
    legacyBoot @era mkPP ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  tx <- txArg @era (q ^. #tx)
  let env = LedgerEnv (SlotNo (ctx ^. #slot)) (Just (EpochNo (ctx ^. #epoch))) (TxIx 0) pp acct
      annotated = mkStAnnTx (epochInfo globals) (systemStart globals) pp utxo mempty tx
      flagged = isValid tx
  case runIdentity (runReaderT (applySTS @(EraRule "LEDGER" era) (TRC (env, ls, annotated))) globals) of
    Left fs ->
      let failures = sort (nub (concatMap (nameFailure "LEDGER") (NE.toList fs)))
       in pure
            ( defMessage
                & #maybe'rejection .~ rejection failures
                & #scriptsValid .~ (("UTXOS", "ValidationTagMismatch") `elem` failures && not flagged)
            )
    Right ls' ->
      pure
        ( defMessage
            & #scriptsValid .~ flagged
            & #delta .~ legacyLedgerDelta ls ls'
        )

legacyLedgerDelta :: forall era. LegacyLedger era => LedgerState era -> LedgerState era -> S.LedgerDelta
legacyLedgerDelta before after =
  (Certs.shelleyDelta (lsCertState before) (lsCertState after))
    & #utxoAdded .~ [defMessage & #txin .~ txInProto i & #txout .~ serialize' (eraProtVerLow @era) o | (i, o) <- sortOn (txInProto . fst) (Map.toList addedU)]
    & #utxoRemoved .~ sortOn (\t -> (t ^. #txId, t ^. #index)) (map txInProto (Map.keys removedU))
    & #feesDelta .~ signedDelta (utxosFees usA) (utxosFees usB)
    & #depositsDelta .~ signedDelta (utxosDeposited usA) (utxosDeposited usB)
    & #donationsDelta .~ signedDelta (utxosDonation usA) (utxosDonation usB)
  where
    usB = lsUTxOState before
    usA = lsUTxOState after
    ub = unUTxO (utxosUtxo usB)
    ua = unUTxO (utxosUtxo usA)
    addedU = Map.difference ua ub
    removedU = Map.difference ub ua

alonzoFlag :: AlonzoEraTx era => Tx TopTx era -> Bool
alonzoFlag t = case t ^. isValidTxL of IsValid b -> b

ruleLedger :: RuleLedgerRequest -> Result RuleLedgerResponse
ruleLedger q = do
  era <- eraArg (q ^. #context . #era)
  case era of
    RConway -> ledgerConway q
    RShelley -> ledgerLegacy @ShelleyEra pparamsShelley (const True) q
    RAllegra -> ledgerLegacy @AllegraEra pparamsAllegra (const True) q
    RMary -> ledgerLegacy @MaryEra pparamsMary (const True) q
    RAlonzo -> ledgerLegacy @AlonzoEra pparamsAlonzo alonzoFlag q
    RBabbage -> ledgerLegacy @BabbageEra pparamsBabbage alonzoFlag q

-- ---------------------------------------------------------------------
-- BBODY

-- | BBODY on a Conway block: the block is decoded from its wire bytes and the
-- rule runs over a LEDGERS state built from the request tables. The totals are
-- those the rule's own checks measure, read from the same blocks and tables.
bbodyConway :: RuleBlockBodyRequest -> Result RuleBlockBodyResponse
bbodyConway q = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  Boot globals pp utxo props ls acct <-
    conwayBoot ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  blk <-
    either (\e -> badRequest ("block: " <> T.pack (show e))) pure $
      (decodeFullAnnotator (eraProtVerLow @Cw) "Block" decCBOR (BL.fromStrict (q ^. #block)) :: Either DecoderError (Block (Praos.Header StandardCrypto) Cw))
  let txs = blockBody blk ^. txSeqBlockBodyL
      ExUnits mem steps = foldMap totExUnits txs
      totals =
        (defMessage :: RuleBlockBodyResponse)
          & #exMemTotal .~ fromIntegral mem
          & #exStepsTotal .~ fromIntegral steps
          & #refScriptBytes .~ fromIntegral (totalRefScriptSizeInBlock (pp ^. ppProtocolVersionL) txs utxo)
      trc = TRC (BbodyEnv pp acct, BbodyState ls (BlocksMade mempty), BbodySignal blk)
  pure $ case runIdentity (runReaderT (applySTS @(EraRule "BBODY" Cw) trc) globals) of
    Left fs -> totals & #maybe'rejection .~ rejection (sort (nub (concatMap (nameFailure "BBODY") (NE.toList fs))))
    Right (BbodyState ls' _) -> totals & #delta .~ ledgerDelta (st ^. #maybe'committee) props pp ls ls'

-- | What the BBODY rule of a Shelley-family era needs, over a block with the
-- header type @h@: a TPraos header up to Alonzo, a Praos header in Babbage.
type LegacyBbody era h =
  ( LegacyLedger era
  , EraBlockBody era
  , EraBlockHeader h era
  , DecCBOR (Annotator (Block h era))
  , STS (EraRule "BBODY" era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ BbodySignal era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Show (PredicateFailure (EraRule "BBODY" era))
  , Embed (PredicateFailure (EraRule "BBODY" era))
  )

instance Embed (ShelleyBbodyPredFailure ShelleyEra) where embed = owned "BBODY"
instance Embed (ShelleyBbodyPredFailure AllegraEra) where embed = owned "BBODY"
instance Embed (ShelleyBbodyPredFailure MaryEra) where embed = owned "BBODY"
instance Embed (ShelleyBbodyPredFailure AlonzoEra) where embed = owned "BBODY"
instance Embed (ShelleyBbodyPredFailure BabbageEra) where embed = owned "BBODY"
instance Embed (AlonzoBbodyPredFailure AlonzoEra) where embed = owned "BBODY"
instance Embed (AlonzoBbodyPredFailure BabbageEra) where embed = owned "BBODY"
instance Embed (ShelleyLedgersPredFailure ShelleyEra) where embed = owned "LEDGERS"
instance Embed (ShelleyLedgersPredFailure AllegraEra) where embed = owned "LEDGERS"
instance Embed (ShelleyLedgersPredFailure MaryEra) where embed = owned "LEDGERS"
instance Embed (ShelleyLedgersPredFailure AlonzoEra) where embed = owned "LEDGERS"
instance Embed (ShelleyLedgersPredFailure BabbageEra) where embed = owned "LEDGERS"

-- | BBODY on a Shelley-family block, decoded from its wire bytes: the
-- response's totals, the ledger state the run started from, and the rule's
-- outcome. @totals@ reads the block's declared execution units, which only
-- Alonzo and Babbage have; the reference-script budget is Conway's alone.
bbodyRun ::
  forall era h r.
  LegacyBbody era h =>
  (S.ProtocolParameters -> Result (PParams era)) ->
  (Block h era -> (Word64, Word64)) ->
  RuleBlockBodyRequest ->
  (RuleBlockBodyResponse -> LedgerState era -> Either (NE.NonEmpty (PredicateFailure (EraRule "BBODY" era))) (LedgerState era) -> r) ->
  Result r
bbodyRun mkPP totals q k = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  (globals, pp, _utxo, ls, acct) <-
    legacyBoot @era mkPP ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  blk <-
    either (\e -> badRequest ("block: " <> T.pack (show e))) pure $
      (decodeFullAnnotator (eraProtVerLow @era) "Block" decCBOR (BL.fromStrict (q ^. #block)) :: Either DecoderError (Block h era))
  let (mem, steps) = totals blk
      summary =
        (defMessage :: RuleBlockBodyResponse)
          & #exMemTotal .~ mem
          & #exStepsTotal .~ steps
      trc = TRC (BbodyEnv pp acct, BbodyState ls (BlocksMade mempty), BbodySignal blk)
  pure (k summary ls (fmap (\(BbodyState ls' _) -> ls') (runIdentity (runReaderT (applySTS @(EraRule "BBODY" era) trc) globals))))

-- | The response of 'bbodyRun': the rejection, or the delta.
bbodyResponse ::
  forall era h.
  LegacyBbody era h =>
  RuleBlockBodyResponse -> LedgerState era -> Either (NE.NonEmpty (PredicateFailure (EraRule "BBODY" era))) (LedgerState era) -> RuleBlockBodyResponse
bbodyResponse summary ls = \case
  Left fs -> summary & #maybe'rejection .~ rejection (sort (nub (concatMap (nameFailure "BBODY") (NE.toList fs))))
  Right ls' -> summary & #delta .~ legacyLedgerDelta ls ls'

-- | The constructors of the innermost failures, empty when the rule accepts.
bbodyConstructors ::
  forall era h.
  LegacyBbody era h =>
  RuleBlockBodyResponse -> LedgerState era -> Either (NE.NonEmpty (PredicateFailure (EraRule "BBODY" era))) (LedgerState era) -> [Text]
bbodyConstructors _ _ = \case
  Left fs -> sort (nub [c | f <- NE.toList fs, (_, c) <- nameFailure "BBODY" f])
  Right _ -> []

noExUnits :: Block h era -> (Word64, Word64)
noExUnits _ = (0, 0)

blockExUnits :: (AlonzoEraTx era, EraBlockBody era) => Block h era -> (Word64, Word64)
blockExUnits blk =
  let ExUnits mem steps = foldMap totExUnits (blockBody blk ^. txSeqBlockBodyL)
   in (fromIntegral mem, fromIntegral steps)

ruleBlockBody :: RuleBlockBodyRequest -> Result RuleBlockBodyResponse
ruleBlockBody q = do
  era <- eraArg ((fromMaybe defMessage (q ^. #maybe'context)) ^. #era)
  case era of
    RConway -> bbodyConway q
    RShelley -> bbodyRun @ShelleyEra @(BHeader StandardCrypto) pparamsShelley noExUnits q (bbodyResponse @ShelleyEra @(BHeader StandardCrypto))
    RAllegra -> bbodyRun @AllegraEra @(BHeader StandardCrypto) pparamsAllegra noExUnits q (bbodyResponse @AllegraEra @(BHeader StandardCrypto))
    RMary -> bbodyRun @MaryEra @(BHeader StandardCrypto) pparamsMary noExUnits q (bbodyResponse @MaryEra @(BHeader StandardCrypto))
    RAlonzo -> bbodyRun @AlonzoEra @(BHeader StandardCrypto) pparamsAlonzo blockExUnits q (bbodyResponse @AlonzoEra @(BHeader StandardCrypto))
    RBabbage -> bbodyRun @BabbageEra @(Praos.Header StandardCrypto) pparamsBabbage blockExUnits q (bbodyResponse @BabbageEra @(Praos.Header StandardCrypto))

-- | The failures of BBODY alone on a Shelley-to-Babbage request, with no
-- adapter naming beyond the constructors of the innermost failures.
legacyBbodyDirectFailures :: RuleBlockBodyRequest -> Result [Text]
legacyBbodyDirectFailures q = do
  era <- eraArg ((fromMaybe defMessage (q ^. #maybe'context)) ^. #era)
  case era of
    RConway -> unsupported "Conway has its own direct check"
    RShelley -> bbodyRun @ShelleyEra @(BHeader StandardCrypto) pparamsShelley noExUnits q (bbodyConstructors @ShelleyEra @(BHeader StandardCrypto))
    RAllegra -> bbodyRun @AllegraEra @(BHeader StandardCrypto) pparamsAllegra noExUnits q (bbodyConstructors @AllegraEra @(BHeader StandardCrypto))
    RMary -> bbodyRun @MaryEra @(BHeader StandardCrypto) pparamsMary noExUnits q (bbodyConstructors @MaryEra @(BHeader StandardCrypto))
    RAlonzo -> bbodyRun @AlonzoEra @(BHeader StandardCrypto) pparamsAlonzo blockExUnits q (bbodyConstructors @AlonzoEra @(BHeader StandardCrypto))
    RBabbage -> bbodyRun @BabbageEra @(Praos.Header StandardCrypto) pparamsBabbage blockExUnits q (bbodyConstructors @BabbageEra @(Praos.Header StandardCrypto))

-- | The failures of the LEDGERS rule alone over the transactions of the
-- request's block, with no BBODY and no adapter naming: the constructors of the
-- innermost failure of each transaction. LEDGERS runs every transaction, so a
-- block with two failing transactions reports the failures of both.
ledgersDirectFailures :: RuleBlockBodyRequest -> Result [Text]
ledgersDirectFailures q = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  Boot globals pp _ _ ls acct <-
    conwayBoot ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  blk <-
    either (\e -> badRequest ("block: " <> T.pack (show e))) pure $
      (decodeFullAnnotator (eraProtVerLow @Cw) "Block" decCBOR (BL.fromStrict (q ^. #block)) :: Either DecoderError (Block (Praos.Header StandardCrypto) Cw))
  let env = LedgersEnv (blk ^. slotNoBlockHeaderL) (EpochNo (ctx ^. #epoch)) pp acct
      txs = StrictSeq.fromStrict (blockBody blk ^. txSeqBlockBodyL)
  pure $ case runIdentity (runReaderT (applySTS @(EraRule "LEDGERS" Cw) (TRC (env, ls, txs))) globals) of
    Right _ -> []
    Left fs -> sort (nub [c | f <- NE.toList fs, (_, c) <- nameFailure "LEDGERS" f])

-- | The failures the reference's PPUP reports for an update by a genesis key
-- for epoch 233 at a slot of epoch 231, with no witness anywhere in sight: the
-- delegate's signature is a UTXOW predicate, so PPUP sees the target epoch
-- alone.
ppupDirectFailures :: Result [Text]
ppupDirectFailures = do
  globals <- fixedGlobals 1
  gk <- maybe (badRequest "hash") pure (KeyHash <$> hashFromBytes (BS.replicate 28 0x91))
  dk <- maybe (badRequest "hash") pure (KeyHash <$> hashFromBytes (BS.replicate 28 0xa1))
  vrf <- maybe (badRequest "hash") pure (VRFVerKeyHash <$> hashFromBytes (BS.replicate 32 0xb1))
  let pp = emptyPParams :: PParams ShelleyEra
      gds = GenDelegs (Map.singleton gk (GenDelegPair dk vrf))
      upd = emptyPParamsUpdate & ppuMinFeeAL .~ SJust (Coin 45)
      gov = ShelleyGovState mempty mempty pp pp NoPParamsUpdate
      signal = SJust (Update (ProposedPPUpdates (Map.singleton gk upd)) (EpochNo 233))
      trc = TRC (PPUPEnv (SlotNo 99_793_000) pp gds, gov, signal)
  pure $ case runIdentity (runReaderT (applySTS @(EraRule "PPUP" ShelleyEra) trc) globals) of
    Right _ -> []
    Left fs -> [c | f <- NE.toList fs, (_, c) <- nameFailure "PPUP" f]

-- | The text of LEDGER's check that withdrawing accounts are delegated to a
-- DRep, over no accounts at all: every withdrawal of the transaction fails it,
-- whether or not its account is registered.
withdrawalsDelegatedDirect :: BS.ByteString -> Result String
withdrawalsDelegatedDirect txBytes = do
  tx <- txArg @Cw txBytes
  pure (show (validateWithdrawalsDelegated (def :: Accounts Cw) tx))

-- | The failures of LEDGER alone on a Babbage request, with no adapter
-- naming beyond the constructors of the innermost failures.
babbageLedgerDirectFailures :: RuleLedgerRequest -> Result [Text]
babbageLedgerDirectFailures q = do
  let ctx = fromMaybe defMessage (q ^. #maybe'context)
      st = fromMaybe defMessage (q ^. #maybe'state)
  (globals, pp, utxo, ls, acct) <-
    legacyBoot @BabbageEra pparamsBabbage ctx st (fromMaybe defMessage (q ^. #maybe'globals)) (fromMaybe defMessage (q ^. #maybe'eraHistory)) (fromMaybe defMessage (q ^. #maybe'pparams))
  tx <- txArg @BabbageEra (q ^. #tx)
  let env = LedgerEnv (SlotNo (ctx ^. #slot)) (Just (EpochNo (ctx ^. #epoch))) (TxIx 0) pp acct
      annotated = mkStAnnTx (epochInfo globals) (systemStart globals) pp utxo mempty tx
  pure $ case runIdentity (runReaderT (applySTS @(EraRule "LEDGER" BabbageEra) (TRC (env, ls, annotated))) globals) of
    Right _ -> []
    Left fs -> sort (nub [c | f <- NE.toList fs, (_, c) <- nameFailure "LEDGER" f])

-- | A Babbage transaction output decoded from its bytes and encoded again by
-- the reference.
reencodeBabbageOutput :: BS.ByteString -> Result BS.ByteString
reencodeBabbageOutput bytes =
  either (\e -> badRequest ("output: " <> T.pack (show e))) (pure . serialize' (eraProtVerLow @BabbageEra))
    (decodeFull' (eraProtVerLow @BabbageEra) bytes :: Either DecoderError (TxOut BabbageEra))
