{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Epoch (tests) where

import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.), (%~))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import qualified Proto.Dxdingo.V1.Epoch as E
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Dxdingo.Epoch (decodesInAllegra, decodesInShelley, unregisteredPoolStake)
import Test.Util

-- Same cases as dxnode-go cmd/dxnode-go/epoch_test.go.
tests :: [Check]
tests =
  [ ("donations: sum", (donate 1000 234 ^. #epochDonations . #treasury) == 1234)
  , ("donations: overflow is UNSUPPORTED", donate maxBound 1 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("pool reap: retired set", reapResp ^. #epochPoolReap . #retired == [h 1, h 2, h 3])
  , ("pool reap: refunds merged per account", map (^. #amount) (reapResp ^. #epochPoolReap . #refunds) == [800])
  , ("pool reap: unclaimed", reapResp ^. #epochPoolReap . #unclaimedToTreasury == 200)
  , ("pool reap: undelegated", map (^. #hash) (reapResp ^. #epochPoolReap . #undelegated) == [h 0xc])
  , ("pool reap: future stage is UNSUPPORTED", reapWith [pool 1 0xa 5 & #stage .~ S.POOL_STAGE_FUTURE] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("mir: applied", mirApplied ^. #epochMir . #applied)
  , ("mir: pots", (mirApplied ^. #epochMir . #reserves, mirApplied ^. #epochMir . #treasury) == (600, 550))
  , ("mir: credited", map (^. #amount) (mirApplied ^. #epochMir . #credited) == [150, 200])
  , ("mir: over budget moves nothing", not (mirOver ^. #epochMir . #applied) && mirOver ^. #epochMir . #reserves == 100 && null (mirOver ^. #epochMir . #credited))
  , ("mir: negative delta is UNSUPPORTED", mirWith [entry E.MIR_SOURCE_RESERVES 1 (-5)] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("mir: repeated entry is UNSUPPORTED", mirWith [entry E.MIR_SOURCE_RESERVES 1 5, entry E.MIR_SOURCE_RESERVES 1 6] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("pparam update: quorum reached", pp [prop 1 10 minFeeA100, prop 2 10 minFeeA100] 2 `is` (True, 100, 2))
  , ("pparam update: below quorum", pp [prop 1 10 minFeeA100, prop 2 10 minFeeA100] 3 `is` (False, 44, 2))
  , ("pparam update: two values both reach quorum", pp [prop 1 10 minFeeA100, prop 2 10 minFeeA100, prop 3 10 minFeeA200, prop 4 10 minFeeA200] 2 `is` (False, 44, 2))
  , ("pparam update: latest proposal per key", pp [prop 1 10 minFeeA100, prop 1 10 minFeeA200, prop 2 10 minFeeA200] 2 `is` (True, 200, 2))
  , ("pparam update: other target epoch ignored", pp [prop 1 10 minFeeA100, prop 2 11 minFeeA100] 2 `is` (False, 44, 2))
  , ("pparam update: protocol version follows", pp [prop 1 10 pv30] 1 `is` (True, 44, 3))
  , ("divergence: pparam update: a Shelley update with protocol version 4 is a DeserialiseFailure", rejName (ppReply [prop 1 10 pv40] 1) == ["DeserialiseFailure"])
  , ("pparam update: the version cap is the era range: 4 decodes in Allegra, not in Shelley", decodesInAllegra pv40 && not (decodesInShelley pv40) && decodesInShelley pv30)
  , ("pparam update: latest proposal replaces the earlier vote", pp [prop 1 10 minFeeA100, prop 1 10 pv30] 1 `is` (True, 44, 3))
  , ("pparam update: carried proposal counts", pp [propAt 1 10 950 minFeeA100] 1 `is` (True, 100, 2))
  , ("pparam update: carried proposals with an illegal version are all dropped", pp [propAt 1 10 950 minFeeA100, propAt 2 10 960 pv25] 1 `is` (False, 44, 2))
  , ("pparam update: a newer proposal overrides the carried one", pp [propAt 1 10 950 minFeeA100, propAt 1 10 1010 minFeeA200] 1 `is` (True, 200, 2))
  , ("pparam update: a proposal made this epoch survives dropped carry-over", pp [propAt 2 10 950 pv25, propAt 1 10 1010 minFeeA100] 1 `is` (True, 100, 2))
  , ("pparam update: Allegra", eraIs P.ERA_ALLEGRA shelleyPP)
  , ("pparam update: Mary", eraIs P.ERA_MARY shelleyPP)
  , ("pparam update: Alonzo", eraIs P.ERA_ALONZO alonzoPP)
  , ("pparam update: Babbage", eraIs P.ERA_BABBAGE babbagePP)
  , ("pparam update: block sizes guard", pp [prop 1 10 maxTx70000] 1 `is` (False, 44, 2))
  , ("pparam update: undecodable update is a DeserialiseFailure", rejName (ppReply [prop 1 10 (BS.pack [0xff])] 1) == ["DeserialiseFailure"])
  , ("pparam update: Conway era is UNSUPPORTED", call (#epochPparamUpdate .~ (ppReq [] 1 & #context . #era .~ P.ERA_CONWAY)) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("obligations: stake", obl ^. #epochObligations . #stakeDeposits == 4000000)
  , ("obligations: pool", obl ^. #epochObligations . #poolDeposits == 500000000)
  , ("obligations: drep", obl ^. #epochObligations . #drepDeposits == 500000000)
  , ("obligations: proposal", obl ^. #epochObligations . #proposalDeposits == 100000000000)
  , ("obligations: total", obl ^. #epochObligations . #total == 4000000 + 500000000 + 500000000 + 100000000000)
  , ("obligations: empty", (call (#epochObligations .~ defMessage) ^. #epochObligations . #total) == 0)
  , ("obligations: future pool is UNSUPPORTED", call (#epochObligations .~ (defMessage & #pools .~ [pool 1 0xa 5 & #stage .~ S.POOL_STAGE_FUTURE])) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("hard fork: Shelley to Allegra keeps every parameter", fork 2 3 3 ^. #epochHardFork . #delta . #pparamsAfter == (shelleyPP & #protocolVersion .~ (defMessage & #major .~ 3)))
  , ("hard fork: Allegra to Mary keeps every parameter", fork 3 4 3 ^. #epochHardFork . #delta . #pparamsAfter == (shelleyPP & #protocolVersion .~ (defMessage & #major .~ 3)))
  , ("hard fork: Shelley to Allegra leaves the protocol major unchanged", fork 2 3 2 ^. #epochHardFork . #delta . #pparamsAfter . #protocolVersion . #major == 2)
  , ("hard fork: Alonzo to Babbage divides coins per word by 8", fork67 ^. #epochHardFork . #delta . #pparamsAfter . #coinsPerUtxoByte == 4310)
  , ("divergence: hard fork: Alonzo to Babbage adds no Plutus V2 cost model", null (fork67 ^. #epochHardFork . #delta . #pparamsAfter . #costModelPlutusV2))
  , ("hard fork: Alonzo to Babbage drops d, extra entropy and min UTxO value", let r = fork67 ^. #epochHardFork . #delta . #pparamsAfter in r ^. #maybe'd == Nothing && r ^. #maybe'minUtxoValue == Nothing)
  , ("hard fork: Alonzo takes its genesis values", let r = forkAlonzo ^. #epochHardFork . #delta . #pparamsAfter in
      r ^. #coinsPerUtxoWord == 34482 && r ^. #maxTxExMem == 10000000 && r ^. #maxValueSize == 5000 && length (r ^. #costModelPlutusV1) == 166
        && r ^. #minFeeA == 44 && r ^. #protocolVersion . #major == 4)
  , ("hard fork: Conway takes its genesis values", let r = forkConway ^. #epochHardFork . #delta . #pparamsAfter in
      r ^. #govActionDeposit == 100000000000 && r ^. #drepDeposit == 500000000 && r ^. #drepActivity == 20 && length (r ^. #costModelPlutusV3) == 251
        && ratIs ("3", "4") (r ^. #drepVotingThresholds . #ppGovGroup) && r ^. #coinsPerUtxoByte == 4310 && r ^. #protocolVersion . #major == 8)
  , ("hard fork: not consecutive is UNSUPPORTED", fork 4 6 4 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("hard fork: no era starts at 6 is UNSUPPORTED", fork 5 6 5 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("hard fork: Alonzo needs its genesis", fork 4 5 4 ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot rotate: mark is the delegated, registered stake plus rewards", rotRows S.SNAPSHOT_KIND_MARK == [(4, 47, 0xa0)])
  , ("snapshot rotate: set is the previous mark", rotRows S.SNAPSHOT_KIND_SET == [(1, 100, 0xa0)])
  , ("snapshot rotate: go is the previous set", rotRows S.SNAPSHOT_KIND_GO == [(2, 200, 0xa0)])
  , ("snapshot rotate: fee snapshot", rotate rotateReq ^. #epochSnapshotRotate . #feeSnapshot == 77)
  , ("snapshot rotate: repeated account is BAD_REQUEST", rotateErr (rotateReq & #accounts %~ (\as -> as ++ take 1 as)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot rotate: short pool id is BAD_REQUEST", rotateErr (rotateReq & #accounts %~ (\(a : as) -> (a & #poolId .~ BS.pack [1]) : as)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot rotate: repeated snapshot row is BAD_REQUEST", rotateErr (rotateReq & #snapshots %~ (\ss -> ss ++ take 1 ss)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot rotate: unspecified kind is BAD_REQUEST", rotateErr (rotateReq & #snapshots %~ (\(x : xs) -> (x & #snapshot .~ S.SNAPSHOT_KIND_UNSPECIFIED) : xs)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("snapshot rotate: zero snapshot stake is UNSUPPORTED", rotateErr (rotateReq & #snapshots %~ (\(x : xs) -> (x & #amount .~ 0) : xs)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("snapshot rotate: undelegated snapshot row is UNSUPPORTED", rotateErr (rotateReq & #snapshots %~ (\(x : xs) -> (x & #poolId .~ BS.empty) : xs)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: retired pool", newRemoved == [h 0xa0])
  , ("new epoch: refund and undelegation", newAccounts == [(1, 500, BS.empty), (2, 5, BS.empty)])
  , ("new epoch: pots unchanged", (newDelta ^. #reservesDelta, newDelta ^. #treasuryDelta) == (0, 0))
  , ("new epoch: mark is read before POOLREAP", newRows S.SNAPSHOT_KIND_MARK == [(2, 45, 0xa0)])
  , ("new epoch: set is the previous mark", newRows S.SNAPSHOT_KIND_SET == [(3, 100, 0xa0)])
  , ("new epoch: go is empty", null (newRows S.SNAPSHOT_KIND_GO))
  , ("new epoch: not Conway is UNSUPPORTED", newErr (newReq & #context . #era .~ P.ERA_BABBAGE) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: reward update is UNSUPPORTED", newErr (newReq & #rewardUpdate .~ defMessage) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: proposals are UNSUPPORTED", newErr (newReq & #state . #proposals .~ [defMessage]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: UTxO is UNSUPPORTED", newErr (newReq & #state . #utxo .~ [defMessage]) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: repeated account is BAD_REQUEST", newErr (newReq & #state . #accounts %~ (\as -> as ++ take 1 as)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("new epoch: short pool id is BAD_REQUEST", newErr (newReq & #state . #pools %~ (\(x : xs) -> (x & #poolId .~ BS.pack [1]) : xs)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("new epoch: pointer is UNSUPPORTED", newErr (newReq & #state . #accounts %~ (\(x : xs) -> (x & #pointer .~ defMessage) : xs)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch: drep target unset is BAD_REQUEST", newErr (newReq & #state . #accounts %~ (\(x : xs) -> (x & #drep .~ defMessage) : xs)) == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ("new epoch: a DRep delegation is kept and reported on the changed row", all (\d -> drepRow d == Just d) drepTargets)
  , ("new epoch: an account without a delegation reports none", isNothing (drepRowOf (newReq & #state . #accounts %~ (\(x : y : zs) -> x : y : zs)) 0 ^. #maybe'drep))
  , ("new epoch reward: leader and member are credited", rwAccounts == [(1, 18461, BS.empty), (2, 1827691, h 0xa0)])
  , ("new epoch reward: pots follow the update", (rwDelta ^. #reservesDelta, rwDelta ^. #treasuryDelta) == (-601846152, 600000000))
  , ("new epoch reward: the credited reward is in the new mark", rwRows == [(2, 1827691, 0xa0)])
  , ("new epoch reward: an update the inputs do not produce is UNSUPPORTED", newErr (rewardReq & #rewardUpdate . #rewards %~ (\(x : xs) -> (x & #amount %~ (+ 1)) : xs)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: a different delta_t is UNSUPPORTED", newErr (rewardReq & #rewardUpdate . #deltaT %~ (+ 1)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: other globals are UNSUPPORTED", newErr (rewardReq & #globals . #epochLength .~ 100) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: no globals is UNSUPPORTED", newErr (rewardReq & #maybe'globals .~ Nothing) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: no blocks is UNSUPPORTED", newErr (rewardReq & #blocksPrevious .~ []) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: blocks of a pool not in state are UNSUPPORTED", newErr (rewardReq & #blocksPrevious . traverse . #poolId .~ h 0xb0) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: a zero reward is UNSUPPORTED", newErr (rewardReq & #rewardUpdate . #rewards %~ (++ [defMessage & #credential .~ cred 1 & #poolId .~ h 0xa0 & #kind .~ S.REWARD_KIND_LEADER])) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: a reward without a kind is UNSUPPORTED", newErr (rewardReq & #rewardUpdate . #rewards %~ (\(x : xs) -> (x & #kind .~ S.REWARD_KIND_UNSPECIFIED) : xs)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("new epoch reward: blocks without an update leave the boundary alone", null (blocksOnlyDelta ^. #accountsChanged) && (blocksOnlyDelta ^. #reservesDelta, blocksOnlyDelta ^. #treasuryDelta) == (0, 0))
  , ("divergence: snapshot rotate: stake delegated to an unregistered pool is active stake", unregisteredPoolStake == Just 42)
  , ("snapshot rotate: stake overflow is UNSUPPORTED", rotateErr (rotateReq & #instantStake %~ (++ [defMessage & #credential .~ cred 4 & #amount .~ maxBound])) == HARNESS_ERROR_CODE_UNSUPPORTED)
  ]
  where
    -- Same case as dxnode-go cmd/dxnode-go/epoch_new_epoch_test.go: pool a0
    -- retires at epoch 11 with deposit 500 and reward account 1, account 2
    -- delegates to it with balance 5 and UTxO stake 40, and the old mark holds
    -- credential 3 with 100.
    newReq :: E.EpochNewEpochRequest
    newReq = defMessage
      & #context .~ (defMessage & #era .~ P.ERA_CONWAY & #epoch .~ 11)
      & #pparams .~ (forkConway ^. #epochHardFork . #delta . #pparamsAfter & #protocolVersion . #major .~ 10)
      & #state .~ (defMessage
          & #accounts .~ [defMessage & #credential .~ cred 1, defMessage & #credential .~ cred 2 & #balance .~ 5 & #poolId .~ h 0xa0]
          & #pools .~ [pool 0xa0 1 11 & #deposit .~ 500]
          & #pots .~ (defMessage & #reserves .~ 10000000000 & #treasury .~ 1000000))
      & #snapshots .~ [rotEntry S.SNAPSHOT_KIND_MARK 3 100 0xa0]
      & #instantStake .~ [stakeOf 2 40]
    newReply = call (#epochNewEpoch .~ newReq)
    newDelta = newReply ^. #epochNewEpoch . #delta
    newRemoved = newDelta ^. #poolsRemoved
    newAccounts = [(BS.head (a ^. #credential . #hash), a ^. #balance, a ^. #poolId) | a <- newDelta ^. #accountsChanged]
    newRows k = [(BS.head (e ^. #credential . #hash), e ^. #amount, BS.head (e ^. #poolId)) | e <- newReply ^. #epochNewEpoch . #snapshots, e ^. #snapshot == k]
    newErr r = call (#epochNewEpoch .~ r) ^. #error . #code
    drepTargets :: [P.DRep]
    drepTargets =
      [ defMessage & #alwaysAbstain .~ True
      , defMessage & #alwaysNoConfidence .~ True
      , defMessage & #credential .~ cred 0xd1
      , defMessage & #credential .~ (cred 0xd2 & #kind .~ P.CREDENTIAL_KIND_SCRIPT)
      ]
    drepRowOf r i = ((call (#epochNewEpoch .~ r)) ^. #epochNewEpoch . #delta . #accountsChanged) !! i
    drepRow d = drepRowOf (newReq & #state . #accounts %~ (\(x : y : zs) -> x : (y & #drep .~ d) : zs)) 1 ^. #maybe'drep
    -- A pending reward round, the case of dxnode-go
    -- cmd/dxnode-go/epoch_new_epoch_test.go. Pool a0 holds all 4.4999e13
    -- active stake (sigma = 1/1000 of the 4.4999e16 circulation) and made all
    -- 21600 blocks, the expected count for f = 1/20 over 432000 slots, so
    -- eta = 1. With rho = 3/1000 and tau = 1/5 on reserves of 10^12: deltaR1 =
    -- 3_000_000_000, the treasury cut is 600_000_000 and the pool pot
    -- 2_400_000_000. maxPool = floor(2.4e9 / 1.3 * 0.001) = 1_846_153 at
    -- performance 1; with margin 1/100, cost 0 and no owners the leader gets
    -- floor(1_846_153 / 100) = 18_461 and the member floor(1_846_153 * 99 /
    -- 100) = 1_827_691. The 1 left over stays in reserves: deltaR = -3e9 +
    -- (2.4e9 - 1_846_152).
    rewardReq :: E.EpochNewEpochRequest
    rewardReq = newReq
      & #globals .~ (defMessage & #epochLength .~ 432000 & #securityParam .~ 2160 & #maxLovelaceSupply .~ 45000000000000000 & #activeSlotCoeff .~ rat ("1", "20"))
      & #state . #accounts .~ [defMessage & #credential .~ cred 1, defMessage & #credential .~ cred 2 & #poolId .~ h 0xa0]
      & #state . #pools .~ [pool 0xa0 1 11 & #maybe'retiringEpoch .~ Nothing & #margin .~ rat ("1", "100")]
      & #state . #pots .~ (defMessage & #reserves .~ 1000000000000 & #treasury .~ 1000000)
      & #snapshots .~ [rotEntry S.SNAPSHOT_KIND_GO 2 44999000000000 0xa0]
      & #instantStake .~ []
      & #blocksPrevious .~ [defMessage & #poolId .~ h 0xa0 & #count .~ 21600]
      & #rewardUpdate .~ (defMessage & #deltaR .~ (-601846152) & #deltaT .~ 600000000
          & #rewards .~ [ defMessage & #credential .~ cred 1 & #poolId .~ h 0xa0 & #kind .~ S.REWARD_KIND_LEADER & #amount .~ 18461
                        , defMessage & #credential .~ cred 2 & #poolId .~ h 0xa0 & #kind .~ S.REWARD_KIND_MEMBER & #amount .~ 1827691 ])
    rwReply = call (#epochNewEpoch .~ rewardReq)
    rwDelta = rwReply ^. #epochNewEpoch . #delta
    rwAccounts = [(BS.head (a ^. #credential . #hash), a ^. #balance, a ^. #poolId) | a <- rwDelta ^. #accountsChanged]
    rwRows = [(BS.head (e ^. #credential . #hash), e ^. #amount, BS.head (e ^. #poolId)) | e <- rwReply ^. #epochNewEpoch . #snapshots, e ^. #snapshot == S.SNAPSHOT_KIND_MARK]
    blocksOnlyDelta = call (#epochNewEpoch .~ (rewardReq & #maybe'rewardUpdate .~ Nothing)) ^. #epochNewEpoch . #delta
    -- Same case as dxnode-go cmd/dxnode-go/epoch_snapshot_test.go.
    rotEntry k c a p = defMessage & #snapshot .~ k & #credential .~ cred c & #amount .~ a & #poolId .~ h p :: S.SnapshotEntry
    rotateReq :: E.EpochSnapshotRotateRequest
    rotateReq = defMessage
      & #snapshots .~ [rotEntry S.SNAPSHOT_KIND_MARK 1 100 0xa0, rotEntry S.SNAPSHOT_KIND_SET 2 200 0xa0, rotEntry S.SNAPSHOT_KIND_GO 3 300 0xa0]
      & #instantStake .~ [stakeOf 4 40, stakeOf 4 2, stakeOf 5 9, stakeOf 6 7]
      & #accounts .~ [defMessage & #credential .~ cred 4 & #balance .~ 5 & #poolId .~ h 0xa0, defMessage & #credential .~ cred 5]
      & #pools .~ [pool 0xa0 0xee 0 & #maybe'retiringEpoch .~ Nothing & #margin .~ rat ("1", "100")]
      & #fees .~ 77
    stakeOf c a = defMessage & #credential .~ cred c & #amount .~ a :: S.StakeEntry
    rotate r = call (#epochSnapshotRotate .~ r)
    rotateErr r = rotate r ^. #error . #code
    rotRows k = [(BS.head (e ^. #credential . #hash), e ^. #amount, BS.head (e ^. #poolId)) | e <- rotate rotateReq ^. #epochSnapshotRotate . #snapshots, e ^. #snapshot == k]
    alonzoGenesis = defMessage & #coinsPerUtxoWord .~ 34482 & #priceMem .~ rat ("577", "10000") & #priceSteps .~ rat ("721", "10000000")
      & #maxTxExMem .~ 10000000 & #maxTxExSteps .~ 10000000000 & #maxBlockExMem .~ 50000000 & #maxBlockExSteps .~ 40000000000
      & #maxValueSize .~ 5000 & #collateralPercentage .~ 150 & #maxCollateralInputs .~ 3 & #costModelPlutusV1 .~ replicate 166 1 :: E.AlonzoGenesisInput
    forkAlonzo = call (#epochHardFork .~ (defMessage & #from .~ (defMessage & #major .~ 4) & #to .~ (defMessage & #major .~ 5)
      & #pparams .~ (shelleyPP & #protocolVersion .~ (defMessage & #major .~ 4)) & #alonzoGenesis .~ alonzoGenesis))
    conwayGenesis = defMessage
      & #poolVotingThresholds .~ (defMessage & #motionNoConfidence .~ rat ("51", "100") & #committeeNormal .~ rat ("51", "100") & #committeeNoConfidence .~ rat ("51", "100") & #hardForkInitiation .~ rat ("51", "100") & #ppSecurityGroup .~ rat ("51", "100"))
      & #drepVotingThresholds .~ (defMessage & #motionNoConfidence .~ rat ("67", "100") & #committeeNormal .~ rat ("67", "100") & #committeeNoConfidence .~ rat ("3", "5") & #updateToConstitution .~ rat ("3", "4") & #hardForkInitiation .~ rat ("3", "5") & #ppNetworkGroup .~ rat ("67", "100") & #ppEconomicGroup .~ rat ("67", "100") & #ppTechnicalGroup .~ rat ("67", "100") & #ppGovGroup .~ rat ("3", "4") & #treasuryWithdrawal .~ rat ("67", "100"))
      & #committeeMinSize .~ 0 & #committeeMaxTermLength .~ 365 & #govActionLifetime .~ 30 & #govActionDeposit .~ 100000000000
      & #drepDeposit .~ 500000000 & #drepActivity .~ 20 & #minFeeRefScriptCostPerByte .~ rat ("15", "1") & #costModelPlutusV3 .~ replicate 251 1 :: E.ConwayGenesisInput
    forkConway = call (#epochHardFork .~ (defMessage & #from .~ (defMessage & #major .~ 8) & #to .~ (defMessage & #major .~ 9)
      & #pparams .~ (babbagePP & #protocolVersion .~ (defMessage & #major .~ 8)) & #conwayGenesis .~ conwayGenesis))
    fork67 = call (#epochHardFork .~ (defMessage & #from .~ (defMessage & #major .~ 6) & #to .~ (defMessage & #major .~ 7) & #pparams .~ (alonzoPP & #protocolVersion .~ (defMessage & #major .~ 6))))
    fork from to major = call (#epochHardFork .~ (defMessage & #from .~ (defMessage & #major .~ from) & #to .~ (defMessage & #major .~ to)
      & #pparams .~ (shelleyPP & #protocolVersion .~ (defMessage & #major .~ major))))
    obl = call (#epochObligations .~ (defMessage
      & #accounts .~ [defMessage & #credential .~ cred 1 & #deposit .~ 2000000, defMessage & #credential .~ cred 2 & #deposit .~ 2000000]
      & #pools .~ [dep 500000000 (pool 1 0xa 5)]
      & #dreps .~ [defMessage & #credential .~ cred 3 & #deposit .~ 500000000]
      & #proposals .~ [defMessage & #id .~ (defMessage & #txId .~ BS.replicate 32 7 & #index .~ 0) & #deposit .~ 100000000000 & #returnAccount .~ cred 1]))
    rejName r = [f ^. #name | f <- r ^. #epochPparamUpdate . #rejection . #failures]
    minFeeA100 = BS.pack [0xa1, 0x00, 0x18, 0x64]
    minFeeA200 = BS.pack [0xa1, 0x00, 0x18, 0xc8]
    pv30 = BS.pack [0xa1, 0x0e, 0x82, 0x03, 0x00]
    pv40 = BS.pack [0xa1, 0x0e, 0x82, 0x04, 0x00]
    maxTx70000 = BS.pack [0xa1, 0x03, 0x1a, 0x00, 0x01, 0x11, 0x70]
    prop k t u = propAt k t (t * 100 + 10) u
    propAt k t slot u = defMessage & #genesisKeyHash .~ h k & #targetEpoch .~ t & #addedSlot .~ slot & #update .~ u :: S.PParamProposal
    pv25 = BS.pack [0xa1, 0x0e, 0x82, 0x02, 0x05]
    plutus p = p & #priceMem .~ rat ("577", "10000") & #priceSteps .~ rat ("721", "10000000")
      & #maxTxExMem .~ 14000000 & #maxTxExSteps .~ 10000000000 & #maxBlockExMem .~ 62000000 & #maxBlockExSteps .~ 40000000000
      & #maxValueSize .~ 5000 & #collateralPercentage .~ 150 & #maxCollateralInputs .~ 3
    alonzoPP :: S.ProtocolParameters
    alonzoPP = plutus shelleyPP & #maybe'minUtxoValue .~ Nothing & #coinsPerUtxoWord .~ 34482 & #protocolVersion .~ (defMessage & #major .~ 5)
    babbagePP :: S.ProtocolParameters
    babbagePP = plutus shelleyPP & #maybe'minUtxoValue .~ Nothing & #maybe'd .~ Nothing & #maybe'extraEntropy .~ Nothing
      & #coinsPerUtxoByte .~ 4310 & #protocolVersion .~ (defMessage & #major .~ 7)
    eraIs era base =
      let r = call (#epochPparamUpdate .~ (ppReq [prop 1 10 minFeeA100] 1 & #context . #era .~ era & #pparams .~ base)) ^. #epochPparamUpdate
       in r ^. #adopted && r ^. #pparams == (base & #minFeeA .~ 100)
    shelleyPP :: S.ProtocolParameters
    shelleyPP = defMessage
      & #minFeeA .~ 44 & #minFeeB .~ 155381 & #maxBlockBodySize .~ 65536 & #maxTxSize .~ 16384
      & #maxBlockHeaderSize .~ 1100 & #keyDeposit .~ 2000000 & #poolDeposit .~ 500000000
      & #maxEpoch .~ 18 & #nOpt .~ 150 & #a0 .~ rat ("3", "10") & #rho .~ rat ("3", "1000")
      & #tau .~ rat ("1", "5") & #d .~ rat ("1", "2") & #extraEntropy .~ defMessage
      & #protocolVersion .~ (defMessage & #major .~ 2) & #minUtxoValue .~ 1000000 & #minPoolCost .~ 340000000
    ppReq ps q = (defMessage :: E.EpochPParamUpdateRequest)
      & #context .~ (defMessage & #era .~ P.ERA_SHELLEY & #epoch .~ 11)
      & #globals .~ (defMessage & #quorum .~ q & #epochLength .~ 100) & #pparams .~ shelleyPP & #proposals .~ ps
    ppReply ps q = call (#epochPparamUpdate .~ ppReq ps q)
    pp ps q = ppReply ps q ^. #epochPparamUpdate
    is r (adopted, fee, major) =
      r ^. #adopted == adopted && r ^. #pparams . #minFeeA == fee && r ^. #pparams . #protocolVersion . #major == major
    donate t d = call (#epochDonations .~ (defMessage & #treasury .~ t & #donations .~ d))
    h b = BS.replicate 28 b
    cred b = defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ h b :: P.Credential
    pool :: Int -> Int -> Int -> S.Pool
    pool i acct e = defMessage & #poolId .~ h (fromIntegral i) & #stage .~ S.POOL_STAGE_CURRENT
      & #rewardAccount .~ cred (fromIntegral acct) & #retiringEpoch .~ fromIntegral e
    dep d p = p & #deposit .~ d
    reapWith ps = call (#epochPoolReap .~ (defMessage & #epoch .~ 5 & #pools .~ ps & #accounts .~ reapAccounts))
    reapAccounts =
      [ defMessage & #credential .~ cred 0xa
      , defMessage & #credential .~ cred 0xc & #poolId .~ h 1
      , defMessage & #credential .~ cred 0xd & #poolId .~ h 4
      ]
    reapResp = reapWith
      [ dep 500 (pool 1 0xa 5), dep 300 (pool 2 0xa 5), dep 200 (pool 3 0xb 5), dep 900 (pool 4 0xa 6) ]
    entry s b a = defMessage & #source .~ s & #credential .~ cred b & #amount .~ a :: E.MirEntry
    mirReq = defMessage & #protocolVersion .~ (defMessage & #major .~ 6) :: E.EpochMirRequest
    mirWith es = call (#epochMir .~ (mirReq & #mir .~ es & #registered .~ [cred 1]))
    mirApplied = call (#epochMir .~ (mirReq & #reserves .~ 1000 & #treasury .~ 500
      & #mir .~ [entry E.MIR_SOURCE_RESERVES 1 100, entry E.MIR_SOURCE_TREASURY 1 50, entry E.MIR_SOURCE_RESERVES 2 200, entry E.MIR_SOURCE_RESERVES 3 999]
      & #registered .~ [cred 1, cred 2] & #reservesToTreasury .~ 100))
    mirOver = call (#epochMir .~ (mirReq & #reserves .~ 100 & #treasury .~ 0
      & #mir .~ [entry E.MIR_SOURCE_RESERVES 1 150] & #registered .~ [cred 1] & #reservesToTreasury .~ 10))
