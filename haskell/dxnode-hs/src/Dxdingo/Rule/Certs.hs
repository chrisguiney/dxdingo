{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Certificate rules (DELEG, POOL, GOVCERT, CERTS) of Conway, answered by
-- running the real STS rules of cardano-ledger in 'ShelleyBase'. The state
-- is built from the request tables and the output state is diffed against
-- the input into a 'LedgerDelta'.
module Dxdingo.Rule.Certs
  ( dispatch
  , implemented
  -- * For the composed ledger rules
  , Tables (..)
  , tablesArg
  , credentialArg
  , credProto
  , rawHash
  , anchorArgOf
  , delta
  , ShelleyCerts
  , shelleyState
  , shelleyDelta
  , flattenShelley
  ) where

import Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes, hashToBytes)
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes
  ( Anchor (..)
  , BoundedRational (boundRational, unboundRational)
  , EpochInterval (..)
  , Globals (..)
  , Network (..)
  , EpochSize (..)
  , ProtVer (..)
  , ShelleyBase
  , StrictMaybe (..)
  , UnitInterval
  , mkActiveSlotCoeff
  , mkVersion
  , nonZero
  , textToUrl
  , unsafeNonZero
  , urlToText
  )
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody, ShelleyEraTxCert)
import Cardano.Ledger.Shelley.Rules (DelegsEnv (..), DelplEnv (..))
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts (mkShelleyAccountState))
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (zipWithM)
import Dxdingo.Rule.Ledger.Base (RuleEra (..), basePParams, eraArg)
import qualified Dxdingo.Rule.Ledger.Base as Base
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (Committee (..))
import Cardano.Ledger.Conway.Rules (CertEnv (..), CertsEnv (..))
import Cardano.Ledger.Conway.State (ConwayEraAccounts (..), VState (..), certVStateL)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Cardano.Ledger.Credential (Credential (..), mkPtrNormalized)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (ScriptHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.State
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad.Reader (runReaderT)
import Control.State.Transition.Extended (STS (..), TRC (..), applySTS)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Data.List (nub, sort, sortOn)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dxdingo.Answer (Answer, respond)
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Proto.Dxdingo.V1.Rule
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P

type Cw = ConwayEra

-- ---------------------------------------------------------------------
-- Argument decoding

rawHash :: HashAlgorithm h => Text -> Int -> ByteString -> Result (Hash h a)
rawHash name size b = case hashFromBytes b of
  Just h | BS.length b == size -> pure h
  _ -> badRequest (name <> ": want " <> T.pack (show size) <> " bytes")

hashArg :: Text -> Int -> ByteString -> Result (KeyHash r)
hashArg name size b = KeyHash <$> rawHash name size b

credentialArg :: Text -> Maybe P.Credential -> Result (Credential r)
credentialArg name = \case
  Nothing -> badRequest (name <> ": missing credential")
  Just c -> case c ^. #kind of
    P.CREDENTIAL_KIND_KEY -> KeyHashObj <$> hashArg name 28 (c ^. #hash)
    P.CREDENTIAL_KIND_SCRIPT -> ScriptHashObj . ScriptHash <$> rawHash name 28 (c ^. #hash)
    _ -> badRequest (name <> ": credential kind")

credProto :: Credential r -> P.Credential
credProto = \case
  KeyHashObj (KeyHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
  ScriptHashObj (ScriptHash h) -> defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

drepArg :: P.DRep -> Result DRep
drepArg d = case d ^. #maybe'target of
  Nothing -> badRequest "drep target unset"
  Just (P.DRep'AlwaysAbstain _) -> pure DRepAlwaysAbstain
  Just (P.DRep'AlwaysNoConfidence _) -> pure DRepAlwaysNoConfidence
  Just (P.DRep'Credential c) -> DRepCredential <$> credentialArg "drep" (Just c)

drepProto :: DRep -> P.DRep
drepProto = \case
  DRepAlwaysAbstain -> defMessage & #alwaysAbstain .~ True
  DRepAlwaysNoConfidence -> defMessage & #alwaysNoConfidence .~ True
  DRepCredential c -> defMessage & #credential .~ credProto c

anchorArg :: Text -> BS.ByteString -> Result (StrictMaybe Anchor)
anchorArg url h
  | T.null url && BS.null h = pure SNothing
  | otherwise = do
      u <- maybe (badRequest "anchor_url") pure (textToUrl 128 url)
      hh <- maybe (badRequest "anchor_hash: want 32 bytes") pure (hashFromBytes h)
      pure (SJust (Anchor u (unsafeMakeSafeHash hh)))

-- | Conway parameters the certificate rules read.
pparamsArg :: S.ProtocolParameters -> Result (PParams Cw)
pparamsArg p = do
  let pv = p ^. #protocolVersion
  v <- maybe (unsupported "protocol major version") pure (mkVersion (pv ^. #major))
  pure $
    emptyPParams
      & ppKeyDepositL .~ Coin (toInteger (p ^. #keyDeposit))
      & ppPoolDepositL .~ Coin (toInteger (p ^. #poolDeposit))
      & ppEMaxL .~ EpochInterval (fromIntegral (p ^. #maxEpoch))
      & ppMinPoolCostL .~ Coin (toInteger (p ^. #minPoolCost))
      & ppProtocolVersionL .~ ProtVer v (fromIntegral (pv ^. #minor))
      & ppDRepDepositL .~ Coin (toInteger (p ^. #drepDeposit))
      & ppDRepActivityL .~ EpochInterval (fromIntegral (p ^. #drepActivity))
      & ppCommitteeMaxTermLengthL .~ EpochInterval (fromIntegral (p ^. #committeeMaxTermLength))

globalsArg :: Word -> Result Globals
globalsArg netId = do
  k <- maybe (unsupported "security parameter") pure (nonZero 2160)
  f <- maybe (unsupported "active slot coeff") pure (boundRationalP (1 / 20))
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
  where
    boundRationalP = boundRational

txArg :: ByteString -> Result (Tx TopTx Cw)
txArg bs =
  either (\err -> badRequest ("tx: " <> T.pack (show err))) pure $
    decodeFullAnnotator (eraProtVerLow @Cw) "Tx" decCBOR (BL.fromStrict bs)

-- ---------------------------------------------------------------------
-- State tables to CertState

data Tables = Tables
  { tAccounts :: [(Credential Staking, S.Account)]
  , tCertState :: CertState Cw
  , tCommittee :: StrictMaybe (Committee Cw)
  }

tablesArg :: S.LedgerState -> Result Tables
tablesArg st = do
  accts <- traverse (\a -> (,) <$> credentialArg "accounts credential" (a ^. #maybe'credential) <*> pure a) (st ^. #accounts)
  accountMap <- Map.fromList <$> traverse accountState accts
  pools <- traverse poolRows (st ^. #pools)
  let current = Map.fromList [(h, s) | (h, S.POOL_STAGE_CURRENT, Left s, _) <- pools]
      future = Map.fromList [(h, p) | (h, S.POOL_STAGE_FUTURE, Right p, _) <- pools]
      retiring = Map.fromList [(h, e) | (h, _, _, Just e) <- pools]
      withDelegs =
        Map.mapWithKey
          ( \h s ->
              s & spsDelegatorsL .~ Set.fromList [c | (c, a) <- accts, a ^. #poolId == keyHashBytes h]
          )
          current
      vrfs = Map.fromList [(spsVrf s, unsafeNonZero 1) | s <- Map.elems current]
  dreps <- traverse drepRow (st ^. #dreps)
  let delegsOf cred = Set.fromList [c | (c, a) <- accts, drepTargetIs cred a]
      drepMap = Map.fromList [(c, d {drepDelegs = delegsOf c}) | (c, d) <- dreps]
  (committee, cstate) <- committeeArg (st ^. #maybe'committee)
  let vstate =
        VState
          { vsDReps = drepMap
          , vsCommitteeState = cstate
          , vsNumDormantEpochs = EpochNo 0
          }
      pstate =
        PState
          { psVRFKeyHashes = vrfs
          , psStakePools = withDelegs
          , psFutureStakePoolParams = future
          , psRetiring = retiring
          }
      cs =
        (def :: CertState Cw)
          & certVStateL .~ vstate
          & certPStateL .~ pstate
          & certDStateL . accountsL .~ foldr (\(c, s) acc -> addAccountState c s acc) def (Map.toList accountMap)
  pure (Tables accts cs committee)
  where
    keyHashBytes (KeyHash h) = hashToBytes h
    drepTargetIs cred a = case a ^. #maybe'drep >>= (^. #maybe'target) of
      Just (P.DRep'Credential c) -> case credentialArg "" (Just c) of
        Right x -> x == cred
        Left _ -> False
      _ -> False

accountState :: (Credential Staking, S.Account) -> Result (Credential Staking, AccountState Cw)
accountState (cred, a) = do
  pool <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> hashArg "accounts pool_id" 28 (a ^. #poolId)
  drep <- traverse drepArg (a ^. #maybe'drep)
  pure
    ( cred
    , mkConwayAccountState (CompactCoin (a ^. #deposit))
        & balanceAccountStateL .~ CompactCoin (a ^. #balance)
        & stakePoolDelegationAccountStateL .~ pool
        & dRepDelegationAccountStateL .~ drep
    )

-- | A pool table row: the current state or the future parameters, plus the
-- retirement epoch.
poolRows ::
  S.Pool ->
  Result (KeyHash StakePool, S.PoolStage, Either StakePoolState StakePoolParams, Maybe EpochNo)
poolRows p = do
  h <- hashArg "pools pool_id" 28 (p ^. #poolId)
  ra <- credentialArg "pools reward_account" (p ^. #maybe'rewardAccount)
  vrf <- case hashFromBytes (p ^. #vrfKeyHash) of
    Just x | BS.length (p ^. #vrfKeyHash) == 32 -> pure (VRFVerKeyHash x)
    _ -> badRequest "pools vrf_key_hash: want 32 bytes"
  margin <- unitIntervalArg "pools margin" (p ^. #maybe'margin)
  owners <- traverse (hashArg "pools owners" 28) (p ^. #owners)
  let retiring = EpochNo <$> p ^. #maybe'retiringEpoch
      params =
        StakePoolParams
          { sppId = h
          , sppVrf = vrf
          , sppPledge = Coin (toInteger (p ^. #pledge))
          , sppCost = Coin (toInteger (p ^. #cost))
          , sppMargin = margin
          , sppAccountAddress = AccountAddress Mainnet (AccountId ra)
          , sppOwners = Set.fromList owners
          , sppRelays = mempty
          , sppMetadata = SNothing
          }
  pure $ case p ^. #stage of
    S.POOL_STAGE_FUTURE -> (h, S.POOL_STAGE_FUTURE, Right params, retiring)
    _ -> (h, S.POOL_STAGE_CURRENT, Left (mkStakePoolState (CompactCoin (p ^. #deposit)) mempty params), retiring)

drepRow :: S.DRepRecord -> Result (Credential DRepRole, DRepState)
drepRow d = do
  c <- credentialArg "dreps credential" (d ^. #maybe'credential)
  a <- anchorArg (d ^. #anchorUrl) (d ^. #anchorHash)
  pure (c, DRepState (EpochNo (d ^. #expiry)) a (CompactCoin (d ^. #deposit)) Set.empty)

committeeArg ::
  Maybe S.Committee -> Result (StrictMaybe (Committee Cw), CommitteeState Cw)
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

-- ---------------------------------------------------------------------
-- Delta

accountRow :: Credential Staking -> AccountState Cw -> S.Account
accountRow c a =
  defMessage
    & #credential .~ credProto c
    & #deposit .~ unCompactCoin (a ^. depositAccountStateL)
    & #balance .~ unCompactCoin (a ^. balanceAccountStateL)
    & #poolId .~ maybe BS.empty (\(KeyHash h) -> hashToBytes h) (a ^. stakePoolDelegationAccountStateL)
    & #maybe'drep .~ (drepProto <$> (a ^. dRepDelegationAccountStateL))

coinWord :: Coin -> Word64
coinWord (Coin c) = fromInteger c

unitProto :: UnitInterval -> P.Rational
unitProto = rationalProto . unboundRational

poolRowCurrent :: KeyHash StakePool -> StakePoolState -> Maybe EpochNo -> S.Pool
poolRowCurrent (KeyHash h) s retiring =
  defMessage
    & #poolId .~ hashToBytes h
    & #stage .~ S.POOL_STAGE_CURRENT
    & #vrfKeyHash .~ (case spsVrf s of VRFVerKeyHash v -> hashToBytes v)
    & #pledge .~ coinWord (spsPledge s)
    & #cost .~ coinWord (spsCost s)
    & #margin .~ unitProto (spsMargin s)
    & #rewardAccount .~ (case spsAccountId s of AccountId c -> credProto c)
    & #owners .~ sort [hashToBytes o | KeyHash o <- Set.toList (spsOwners s)]
    & #deposit .~ unCompactCoin (spsDeposit s)
    & #maybe'retiringEpoch .~ (unEpochNo <$> retiring)

poolRowFuture :: StakePoolParams -> Word64 -> S.Pool
poolRowFuture p deposit =
  defMessage
    & #poolId .~ (case sppId p of KeyHash h -> hashToBytes h)
    & #stage .~ S.POOL_STAGE_FUTURE
    & #vrfKeyHash .~ (case sppVrf p of VRFVerKeyHash v -> hashToBytes v)
    & #pledge .~ coinWord (sppPledge p)
    & #cost .~ coinWord (sppCost p)
    & #margin .~ unitProto (sppMargin p)
    & #rewardAccount .~ (case sppAccountAddress p of AccountAddress _ (AccountId c) -> credProto c)
    & #owners .~ sort [hashToBytes o | KeyHash o <- Set.toList (sppOwners p)]
    & #deposit .~ deposit

drepRecord :: Credential DRepRole -> DRepState -> S.DRepRecord
drepRecord c d =
  defMessage
    & #credential .~ credProto c
    & #deposit .~ unCompactCoin (drepDeposit d)
    & #expiry .~ unEpochNo (drepExpiry d)
    & #anchorUrl .~ (case drepAnchor d of SJust a -> urlToText (anchorUrl a); SNothing -> "")
    & #anchorHash .~ (case drepAnchor d of SJust a -> hashToBytes (extractHash (anchorDataHash a)); SNothing -> BS.empty)

credKey :: P.Credential -> (Int, BS.ByteString)
credKey c = (fromEnum (c ^. #kind), c ^. #hash)

poolRowsOf :: PState era -> [S.Pool]
poolRowsOf ps =
  [poolRowCurrent h s (Map.lookup h (psRetiring ps)) | (h, s) <- Map.toList (psStakePools ps)]
    ++ [ poolRowFuture p (maybe 0 (unCompactCoin . spsDeposit) (Map.lookup h (psStakePools ps)))
       | (h, p) <- Map.toList (psFutureStakePoolParams ps)
       ]

poolKey :: S.Pool -> (BS.ByteString, Int)
poolKey p = (p ^. #poolId, fromEnum (p ^. #stage))

-- | Rows of @after@ that are new or differ from @before@, and keys of
-- @before@ that are gone.
changes :: (Eq a, Ord k) => (a -> k) -> [a] -> [a] -> ([a], [a], [a])
changes key before after =
  ( [a | a <- after, Map.notMember (key a) bm]
  , [b | b <- before, Map.notMember (key b) am]
  , [a | a <- after, Just b <- [Map.lookup (key a) bm], a /= b]
  )
  where
    bm = Map.fromList [(key b, b) | b <- before]
    am = Map.fromList [(key a, a) | a <- after]

accountRowsOf :: CertState Cw -> [S.Account]
accountRowsOf cs = [accountRow c a | (c, a) <- Map.toList (cs ^. certDStateL . accountsL . accountsMapL)]

drepRowsOf :: CertState Cw -> [S.DRepRecord]
drepRowsOf cs = [drepRecord c d | (c, d) <- Map.toList (vsDReps (cs ^. certVStateL))]

delta :: Maybe S.Committee -> CertState Cw -> CertState Cw -> S.LedgerDelta
delta reqCommittee before after =
  defMessage
    & #accountsAdded .~ sortOn (credKey . (^. #credential)) addedA
    & #accountsRemoved .~ sortOn credKey [a ^. #credential | a <- removedA]
    & #accountsChanged .~ sortOn (credKey . (^. #credential)) changedA
    & #poolsChanged .~ sortOn poolKey (addedP ++ changedP)
    & #poolsRemoved .~ sort [p ^. #poolId | p <- removedP, p ^. #stage == S.POOL_STAGE_CURRENT]
    & #drepsChanged .~ sortOn (credKey . (^. #credential)) (addedD ++ changedD)
    & #drepsRemoved .~ sortOn credKey [d ^. #credential | d <- removedD]
    & #maybe'committeeAfter .~ committeeAfter
  where
    (addedA, removedA, changedA) = changes (credKey . (^. #credential)) (accountRowsOf before) (accountRowsOf after)
    (addedP, removedP, changedP) = changes poolKey (poolRowsOf (before ^. certPStateL)) (poolRowsOf (after ^. certPStateL))
    (addedD, removedD, changedD) = changes (credKey . (^. #credential)) (drepRowsOf before) (drepRowsOf after)
    credsOf cs = csCommitteeCreds (vsCommitteeState (cs ^. certVStateL))
    committeeAfter
      | credsOf before == credsOf after = Nothing
      | otherwise = Just (committeeRows reqCommittee (credsOf after))

-- | The full committee after a change in authorizations: the request's
-- members, then credentials only the committee state knows, by credential.
committeeRows :: Maybe S.Committee -> Map.Map (Credential ColdCommitteeRole) CommitteeAuthorization -> S.Committee
committeeRows req creds =
  defMessage
    & #members .~ sortOn (credKey . (^. #cold)) (map row keys)
    & #maybe'threshold .~ (req >>= (^. #maybe'threshold))
  where
    reqMembers =
      Map.fromList
        [ (credKey (m ^. #cold), m ^. #expiry)
        | m <- maybe [] (^. #members) req
        ]
    credKeyOf :: Credential r -> (Int, BS.ByteString)
    credKeyOf = credKey . credProto
    stateKeys = Map.fromList [(credKeyOf c, c) | c <- Map.keys creds]
    keys = nub (map fst (Map.toList reqMembers) ++ map fst (Map.toList stateKeys))
    row k =
      let auth = Map.lookup k stateAuth
          cold = case [m ^. #cold | m <- maybe [] (^. #members) req, credKey (m ^. #cold) == k] of
            (c : _) -> c
            [] -> maybe defMessage credProto (Map.lookup k stateKeys)
       in defMessage
            & #cold .~ cold
            & #expiry .~ fromMaybe 0 (Map.lookup k reqMembers)
            & #maybe'hot .~ (case auth of Just (CommitteeHotCredential h) -> Just (credProto h); _ -> Nothing)
            & #resigned .~ (case auth of Just (CommitteeMemberResigned _) -> True; _ -> False)
    stateAuth = Map.fromList [(credKeyOf c, a) | (c, a) <- Map.toList creds]

-- ---------------------------------------------------------------------
-- Running the rules

data Run = Run
  { runGlobals :: Globals
  , runPP :: PParams Cw
  , runEpoch :: EpochNo
  , runCommittee :: StrictMaybe (Committee Cw)
  , runState :: CertState Cw
  , runTx :: Tx TopTx Cw
  }

data Cat = CDeleg | CPool | CGov deriving (Eq)

inCat :: Cat -> ConwayTxCert Cw -> Bool
inCat CDeleg = \case ConwayTxCertDeleg _ -> True; _ -> False
inCat CPool = \case ConwayTxCertPool _ -> True; _ -> False
inCat CGov = \case ConwayTxCertGov _ -> True; _ -> False

prepare ::
  S.LedgerContext -> S.ProtocolParameters -> S.LedgerState -> ByteString -> Result (Run, Tables)
prepare ctx p st tx = do
  if ctx ^. #era == P.ERA_CONWAY
    then pure ()
    else unsupported ("certificate rules are modelled for Conway, not " <> T.pack (show (ctx ^. #era)))
  if null (st ^. #proposals) then pure () else unsupported "committee proposals are not modelled"
  globals <- globalsArg (fromIntegral (ctx ^. #networkId))
  pp <- pparamsArg p
  tables <- tablesArg st
  t <- txArg tx
  pure (Run globals pp (EpochNo (ctx ^. #epoch)) (tCommittee tables) (tCertState tables) t, tables)

-- | The failures of one rule run as (rule, constructor) pairs.
type Failures = [(Text, Text)]

stepCert :: Run -> CertState Cw -> ConwayTxCert Cw -> Either Failures (CertState Cw)
stepCert r s c =
  case runIdentity
    ( runReaderT
        ( applySTS @(EraRule "CERT" Cw)
            (TRC (CertEnv (runPP r) (runEpoch r) (runCommittee r) Map.empty, s, c))
        )
        (runGlobals r)
    ) of
    Right s' -> Right s'
    Left fs -> Left (sort (nub (concatMap (flatten . show) (NE.toList fs))))

certsOf :: Run -> [ConwayTxCert Cw]
certsOf r = toList (runTx r ^. bodyTxL . certsTxBodyL)

-- | Run the category's certificates, or the one at the index. A sequence runs
-- through CERTS with the withdrawals removed, so failures accumulate across
-- certificates the way the reference reports them; one certificate runs
-- through CERT.
runCategory :: Cat -> Maybe Word -> Run -> Result (Either Failures (CertState Cw))
runCategory cat idx r = case idx of
  Nothing ->
    pure (runCertSeq (r {runTx = runTx r & bodyTxL . withdrawalsTxBodyL .~ Withdrawals Map.empty}) (filter (inCat cat) (certsOf r)))
  Just i -> case drop (fromIntegral i) (certsOf r) of
    [] -> badRequest "cert_index out of range"
    (c : _)
      | inCat cat c -> pure (stepCert r (runState r) c)
      | otherwise -> unsupported "the certificate at cert_index is not handled by this rule"

runCerts :: Run -> Either Failures (CertState Cw)
runCerts r = runCertSeq r (certsOf r)

runCertSeq :: Run -> [ConwayTxCert Cw] -> Either Failures (CertState Cw)
runCertSeq r certs =
  case runIdentity
    ( runReaderT
        ( applySTS @(EraRule "CERTS" Cw)
            ( TRC
                ( CertsEnv (runTx r) (runPP r) (runEpoch r) (runCommittee r) Map.empty
                , runState r
                , Seq.fromList certs
                )
            )
        )
        (runGlobals r)
    ) of
    Right s' -> Right s'
    Left fs -> Left (sort (nub (concatMap (flatten . show) (NE.toList fs))))

-- | Walk a shown failure down the wrapper constructors to the innermost one.
flatten :: String -> Failures
flatten s = case words s of
  [] -> []
  (c : _)
    | c `elem` ["CertFailure", "DelegFailure", "PoolFailure", "GovCertFailure"]
    , Just inner <- argument (drop (length c) s) ->
        flatten inner
    | otherwise -> [(ruleOf c, T.pack c)]

ruleOf :: String -> Text
ruleOf c
  | "DELEG" `T.isSuffixOf` t = "DELEG"
  | "POOL" `T.isSuffixOf` t || c `elem` ["PoolMedataHashTooBig", "VRFKeyHashAlreadyRegistered"] = "POOL"
  | "CERTS" `T.isSuffixOf` t = "CERTS"
  | "Conway" `T.isPrefixOf` t = "GOVCERT"
  | otherwise = "CERT"
  where
    t = T.pack c

argument :: String -> Maybe String
argument rest = case dropWhile (== ' ') rest of
  ('(' : more) -> Just (init more)
  _ -> Nothing

rejection :: Failures -> Maybe P.Rejection
rejection [] = Nothing
rejection xs =
  Just (defMessage & #failures .~ [defMessage & #rule .~ r & #name .~ n | (r, n) <- xs])

-- ---------------------------------------------------------------------
-- Functions

outcome ::
  Maybe S.Committee ->
  Run ->
  Either Failures (CertState Cw) ->
  (Maybe P.Rejection, Maybe S.LedgerDelta)
outcome req r = \case
  Left fs -> (rejection fs, Nothing)
  Right s' -> (Nothing, Just (delta req (runState r) s'))

ruleCategory ::
  Cat ->
  S.LedgerContext ->
  S.ProtocolParameters ->
  S.LedgerState ->
  ByteString ->
  Maybe Word ->
  Result (Maybe P.Rejection, Maybe S.LedgerDelta)
ruleCategory cat ctx p st tx idx = case ctx ^. #era of
  P.ERA_CONWAY -> do
    (r, _) <- prepare ctx p st tx
    res <- runCategory cat idx r
    pure (outcome (st ^. #maybe'committee) r res)
  e -> do
    era <- eraArg e
    if cat == CGov then unsupported "governance certificates exist from Conway" else pure ()
    onShelleyEra era (shelleyCategory cat idx ctx p st tx)

fromMaybeState :: Maybe a -> a -> a
fromMaybeState = flip fromMaybe

ruleDeleg :: RuleDelegRequest -> Result RuleDelegResponse
ruleDeleg q = do
  (rej, d) <- ruleCategory CDeleg (ctxOf q) (ppOf q) (stOf q) (q ^. #tx) (fromIntegral <$> q ^. #maybe'certIndex)
  pure (defMessage & #maybe'rejection .~ rej & #maybe'delta .~ d)
  where
    ctxOf x = fromMaybe defMessage (x ^. #maybe'context)
    ppOf x = fromMaybe defMessage (x ^. #maybe'pparams)
    stOf x = fromMaybe defMessage (x ^. #maybe'state)

rulePool :: RulePoolRequest -> Result RulePoolResponse
rulePool q = do
  (rej, d) <- ruleCategory CPool (ctxOf q) (ppOf q) (stOf q) (q ^. #tx) (fromIntegral <$> q ^. #maybe'certIndex)
  pure (defMessage & #maybe'rejection .~ rej & #maybe'delta .~ d)
  where
    ctxOf x = fromMaybe defMessage (x ^. #maybe'context)
    ppOf x = fromMaybe defMessage (x ^. #maybe'pparams)
    stOf x = fromMaybe defMessage (x ^. #maybe'state)

ruleGovCert :: RuleGovCertRequest -> Result RuleGovCertResponse
ruleGovCert q = do
  (rej, d) <- ruleCategory CGov (ctxOf q) (ppOf q) (stOf q) (q ^. #tx) (fromIntegral <$> q ^. #maybe'certIndex)
  pure (defMessage & #maybe'rejection .~ rej & #maybe'delta .~ d)
  where
    ctxOf x = fromMaybe defMessage (x ^. #maybe'context)
    ppOf x = fromMaybe defMessage (x ^. #maybe'pparams)
    stOf x = fromMaybe defMessage (x ^. #maybe'state)

ruleCerts :: RuleCertsRequest -> Result RuleCertsResponse
ruleCerts q
  | ctx ^. #era == P.ERA_CONWAY = do
      (r, _) <- prepare ctx (fromMaybe defMessage (q ^. #maybe'pparams)) st (q ^. #tx)
      let (rej, d) = outcome (st ^. #maybe'committee) r (runCerts r)
      pure (defMessage & #maybe'rejection .~ rej & #maybe'delta .~ d)
  | otherwise = do
      era <- eraArg (ctx ^. #era)
      (rej, d) <- onShelleyEra era (shelleyAll ctx (fromMaybe defMessage (q ^. #maybe'pparams)) st (q ^. #tx))
      pure (defMessage & #maybe'rejection .~ rej & #maybe'delta .~ d)
  where
    st = fromMaybe defMessage (q ^. #maybe'state)
    ctx = fromMaybe defMessage (q ^. #maybe'context)

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'RuleDeleg q -> Just (respond #ruleDeleg (ruleDeleg q))
  Call'RulePool q -> Just (respond #rulePool (rulePool q))
  Call'RuleGovCert q -> Just (respond #ruleGovCert (ruleGovCert q))
  Call'RuleCerts q -> Just (respond #ruleCerts (ruleCerts q))
  _ -> Nothing

implemented :: [Text]
implemented = ["rule_deleg", "rule_pool", "rule_gov_cert", "rule_certs"]

-- | 'anchorArg', for the composed ledger rules.
anchorArgOf :: Text -> BS.ByteString -> Result (StrictMaybe Anchor)
anchorArgOf = anchorArg

-- ---------------------------------------------------------------------
-- Shelley to Babbage: DELEGS, DELPL, DELEG and POOL

-- | What the Shelley-family certificate rules need of an era.
type ShelleyCerts era =
  ( EraTx era
  , EraCertState era
  , ShelleyEraTxBody era
  , ShelleyEraTxCert era
  , ShelleyEraAccounts era
  , STS (EraRule "DELEGS" era)
  , BaseM (EraRule "DELEGS" era) ~ ShelleyBase
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq.Seq (TxCert era)
  , Show (PredicateFailure (EraRule "DELEGS" era))
  , STS (EraRule "DELPL" era)
  , BaseM (EraRule "DELPL" era) ~ ShelleyBase
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , Show (PredicateFailure (EraRule "DELPL" era))
  )

-- | Run a function of the era named by the request.
onShelleyEra ::
  RuleEra ->
  (forall era. (ShelleyCerts era) => Proxy era -> Result a) ->
  Result a
onShelleyEra era f = case era of
  RShelley -> f (Proxy @ShelleyEra)
  RAllegra -> f (Proxy @AllegraEra)
  RMary -> f (Proxy @MaryEra)
  RAlonzo -> f (Proxy @AlonzoEra)
  RBabbage -> f (Proxy @BabbageEra)
  RConway -> unsupported "Conway certificates run through CERTS"

data SRun era = SRun
  { srGlobals :: Globals
  , srPP :: PParams era
  , srSlot :: SlotNo
  , srEpoch :: EpochNo
  , srAccount :: ChainAccountState
  , srState :: CertState era
  , srTx :: Tx TopTx era
  }

shelleyPrepare ::
  forall era.
  ShelleyCerts era =>
  S.LedgerContext -> S.ProtocolParameters -> S.LedgerState -> ByteString -> Result (SRun era)
shelleyPrepare ctx p st tx = do
  globals <- globalsArg (fromIntegral (ctx ^. #networkId))
  pp <- basePParams @era p
  state <- shelleyState @era st
  t <- Base.txArg @era tx
  let pots = fromMaybe defMessage (st ^. #maybe'pots)
      account = ChainAccountState (Coin (toInteger (pots ^. #treasury))) (Coin (toInteger (pots ^. #reserves)))
  pure (SRun globals pp (SlotNo (ctx ^. #slot)) (EpochNo (ctx ^. #epoch)) account state t)

-- | The certificate state of the request tables: accounts with their
-- deposits, balances and pool delegations, and the pools. Accounts carry
-- distinct pointers, which only the Shelley delegation rules read.
shelleyState :: forall era. ShelleyCerts era => S.LedgerState -> Result (CertState era)
shelleyState st = do
  accts <- traverse (\a -> (,) <$> credentialArg "accounts credential" (a ^. #maybe'credential) <*> pure a) (st ^. #accounts)
  accountRows <- zipWithM row [0 ..] accts
  pools <- traverse poolRows (st ^. #pools)
  let current = Map.fromList [(h, s) | (h, S.POOL_STAGE_CURRENT, Left s, _) <- pools]
      future = Map.fromList [(h, p) | (h, S.POOL_STAGE_FUTURE, Right p, _) <- pools]
      retiring = Map.fromList [(h, e) | (h, _, _, Just e) <- pools]
      withDelegs =
        Map.mapWithKey
          (\h s -> s & spsDelegatorsL .~ Set.fromList [c | (c, a) <- accts, a ^. #poolId == keyHashBytes h])
          current
      vrfs = Map.fromList [(spsVrf s, unsafeNonZero 1) | s <- Map.elems current]
      pstate =
        PState
          { psVRFKeyHashes = vrfs
          , psStakePools = withDelegs
          , psFutureStakePoolParams = future
          , psRetiring = retiring
          }
  pure $
    (def :: CertState era)
      & certPStateL .~ pstate
      & certDStateL . accountsL .~ foldr (\(c, s) acc -> addAccountState c s acc) def accountRows
  where
    keyHashBytes (KeyHash h) = hashToBytes h
    row :: Word64 -> (Credential Staking, S.Account) -> Result (Credential Staking, AccountState era)
    row i (cred, a) = do
      pool <- if BS.null (a ^. #poolId) then pure Nothing else Just <$> hashArg "accounts pool_id" 28 (a ^. #poolId)
      pure
        ( cred
        , mkShelleyAccountState (mkPtrNormalized 0 i 0) (CompactCoin (a ^. #deposit))
            & balanceAccountStateL .~ CompactCoin (a ^. #balance)
            & stakePoolDelegationAccountStateL .~ pool
        )

isPoolCert :: ShelleyCerts era => TxCert era -> Bool
isPoolCert = isJust . getPoolCertTxCert

certsOfTx :: ShelleyCerts era => Tx TopTx era -> [TxCert era]
certsOfTx t = toList (t ^. bodyTxL . certsTxBodyL)

-- | The failures of a rule as (rule, constructor) pairs: the shown failure
-- walked down the wrapper constructors to the innermost one.
flattenShelley :: String -> Failures
flattenShelley s = case words s of
  [] -> []
  (c : _)
    | c `elem` ["DelplFailure", "PoolFailure", "DelegFailure"]
    , Just inner <- argument (drop (length c) s) ->
        flattenShelley inner
    | otherwise -> [(shelleyRuleOf c, T.pack c)]

shelleyRuleOf :: String -> Text
shelleyRuleOf c
  | "DELEGS" `T.isSuffixOf` t = "DELEGS"
  | "DELEG" `T.isSuffixOf` t || c `elem` ["MIRTransferNotCurrentlyAllowed", "MIRNegativesNotCurrentlyAllowed", "InsufficientForInstantaneousRewardsDELEG", "MIRProducesNegativeUpdate"] = "DELEG"
  | "POOL" `T.isSuffixOf` t || c `elem` ["PoolMedataHashTooBig", "VRFKeyHashAlreadyRegistered"] = "POOL"
  | otherwise = "DELEG"
  where
    t = T.pack c

runDelegs :: forall era. ShelleyCerts era => SRun era -> Tx TopTx era -> [TxCert era] -> Either Failures (CertState era)
runDelegs r tx certs =
  case runIdentity
    ( runReaderT
        ( applySTS @(EraRule "DELEGS" era)
            (TRC (DelegsEnv (srSlot r) (srEpoch r) (TxIx 0) (srPP r) tx (srAccount r), srState r, Seq.fromList certs))
        )
        (srGlobals r)
    ) of
    Right s' -> Right s'
    Left fs -> Left (sort (nub (concatMap (flattenShelley . show) (NE.toList fs))))

runDelpl :: forall era. ShelleyCerts era => SRun era -> TxCert era -> Either Failures (CertState era)
runDelpl r cert =
  case runIdentity
    ( runReaderT
        ( applySTS @(EraRule "DELPL" era)
            (TRC (DelplEnv (srSlot r) (srEpoch r) (mkPtrNormalized (unSlotNo (srSlot r)) 0 0) (srPP r) (srAccount r), srState r, cert))
        )
        (srGlobals r)
    ) of
    Right s' -> Right s'
    Left fs -> Left (sort (nub (concatMap (flattenShelley . show) (NE.toList fs))))

-- | The delta of a Shelley-family run: accounts and pools, which are all the
-- certificates of these eras touch.
shelleyDelta :: forall era. ShelleyCerts era => CertState era -> CertState era -> S.LedgerDelta
shelleyDelta before after =
  defMessage
    & #accountsAdded .~ sortOn (credKey . (^. #credential)) addedA
    & #accountsRemoved .~ sortOn credKey [a ^. #credential | a <- removedA]
    & #accountsChanged .~ sortOn (credKey . (^. #credential)) changedA
    & #poolsChanged .~ sortOn poolKey (addedP ++ changedP)
    & #poolsRemoved .~ sort [p ^. #poolId | p <- removedP, p ^. #stage == S.POOL_STAGE_CURRENT]
  where
    rows cs = [shelleyAccountRow c a | (c, a) <- Map.toList (cs ^. certDStateL . accountsL . accountsMapL)]
    (addedA, removedA, changedA) = changes (credKey . (^. #credential)) (rows before) (rows after)
    (addedP, removedP, changedP) = changes poolKey (poolRowsOf (before ^. certPStateL)) (poolRowsOf (after ^. certPStateL))

shelleyAccountRow :: EraAccounts era => Credential Staking -> AccountState era -> S.Account
shelleyAccountRow c a =
  defMessage
    & #credential .~ credProto c
    & #deposit .~ unCompactCoin (a ^. depositAccountStateL)
    & #balance .~ unCompactCoin (a ^. balanceAccountStateL)
    & #poolId .~ maybe BS.empty (\(KeyHash h) -> hashToBytes h) (a ^. stakePoolDelegationAccountStateL)

shelleyOutcome ::
  ShelleyCerts era =>
  SRun era -> Either Failures (CertState era) -> (Maybe P.Rejection, Maybe S.LedgerDelta)
shelleyOutcome r = \case
  Left fs -> (rejection fs, Nothing)
  Right s' -> (Nothing, Just (shelleyDelta (srState r) s'))

-- | A category's certificates through DELEGS with the withdrawals removed,
-- or the one at the index through DELPL.
shelleyCategory ::
  forall era.
  ShelleyCerts era =>
  Cat ->
  Maybe Word ->
  S.LedgerContext ->
  S.ProtocolParameters ->
  S.LedgerState ->
  ByteString ->
  Proxy era ->
  Result (Maybe P.Rejection, Maybe S.LedgerDelta)
shelleyCategory cat idx ctx p st tx _ = do
  r <- shelleyPrepare @era ctx p st tx
  let tx0 = srTx r
      certs = certsOfTx tx0
      inCategory c = (cat == CPool) == isPoolCert c
  res <- case idx of
    Nothing -> pure (runDelegs r (tx0 & bodyTxL . withdrawalsTxBodyL .~ Withdrawals Map.empty) (filter inCategory certs))
    Just i -> case drop (fromIntegral i) certs of
      [] -> badRequest "cert_index out of range"
      (c : _)
        | inCategory c -> pure (runDelpl r c)
        | otherwise -> unsupported "the certificate at cert_index is not handled by this rule"
  pure (shelleyOutcome r res)

shelleyAll ::
  forall era.
  ShelleyCerts era =>
  S.LedgerContext ->
  S.ProtocolParameters ->
  S.LedgerState ->
  ByteString ->
  Proxy era ->
  Result (Maybe P.Rejection, Maybe S.LedgerDelta)
shelleyAll ctx p st tx _ = do
  r <- shelleyPrepare @era ctx p st tx
  pure (shelleyOutcome r (runDelegs r (srTx r) (certsOfTx (srTx r))))
