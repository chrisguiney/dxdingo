{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gov
  ( tests
  , actInfo
  , actNoConfidence
  , actionId
  , allVote
  , cred
  , hfAction
  , keyDRep
  , pcAction
  , poolStake
  , proposal
  , spoVote
  , testCommittee
  , voteOn
  , withdrawalAction
  ) where

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), EpochNo (..), ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.Coin (CompactForm (..))
import Cardano.Ledger.Conway.Rules (dRepAcceptedRatio, updateDormantDRepExpiry)
import Cardano.Ledger.Conway.State (VState (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyHash (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Cardano.Ledger.Binary (natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (PParams, PParamsUpdate, emptyPParams, emptyPParamsUpdate, ppDRepVotingThresholdsL)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), DRepVotingThresholds (..))
import Data.Ratio ((%))
import Cardano.Ledger.Conway.Governance
import Data.Default (def)
import qualified Data.ByteString as BS
import Data.Bits (shiftR)
import Data.Foldable (toList)
import Data.Char (digitToInt)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..), Reply)
import Proto.Dxdingo.V1.Gov (GovEnactResponse, GovRatifyRequest)
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Test.Util

-- Same cases as dxnode-go (cmd/dxnode-go/gov_test.go).
tests :: [Check]
tests = thresholdTests ++ spoTests ++ drepTests ++ committeeTests ++ treeTests ++ expiryTests ++ ratifyTests ++ enactTests ++ proposalExpiryTests ++ divergenceTests

hex :: String -> BS.ByteString
hex = BS.pack . go
  where
    go (a : b : r) = fromIntegral (digitToInt a * 16 + digitToInt b) : go r
    go _ = []

-- GovAction CBOR: tags 0 parameter change, 1 hard fork, 2 treasury
-- withdrawals, 3 no confidence, 4 update committee, 5 new constitution, 6 info.
actNoConfidence, actInfo, actHardFork, actTreasury, actUpdateCmte, actConstitution :: BS.ByteString
actNoConfidence = hex "8203f6"
actInfo = hex "8106"
actHardFork = hex "8301f6820a00"
actTreasury = hex "8302a0f6"
actUpdateCmte = hex "8504f6d9010280a0d81e820102"
actConstitution = hex ("8305f68282617858" ++ "20" ++ replicate 64 '0' ++ "f6")

actPCNetworkSecurity, actPCEconomic, actPCTechnical, actPCGov :: BS.ByteString
actPCNetworkSecurity = hex "8400f6a1021a00016000f6"
actPCEconomic = hex "8400f6a1051a001e8480f6"
actPCTechnical = hex "8400f6a1081901f4f6"
actPCGov = hex "8400f6a1181c0af6"

ids :: Int -> BS.ByteString
ids b = BS.replicate 28 (fromIntegral b)

cred :: Int -> P.Credential
cred b = defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ ids b

pv :: Int -> P.ProtocolVersion
pv major = defMessage & #major .~ fromIntegral major

govParams :: Int -> S.ProtocolParameters
govParams major =
  defMessage
    & #protocolVersion .~ pv major
    & #poolVotingThresholds
      .~ ( defMessage
             & #motionNoConfidence .~ rat ("51", "100")
             & #committeeNormal .~ rat ("52", "100")
             & #committeeNoConfidence .~ rat ("53", "100")
             & #hardForkInitiation .~ rat ("54", "100")
             & #ppSecurityGroup .~ rat ("55", "100")
         )
    & #drepVotingThresholds
      .~ ( defMessage
             & #motionNoConfidence .~ rat ("61", "100")
             & #committeeNormal .~ rat ("62", "100")
             & #committeeNoConfidence .~ rat ("63", "100")
             & #updateToConstitution .~ rat ("64", "100")
             & #hardForkInitiation .~ rat ("65", "100")
             & #ppNetworkGroup .~ rat ("66", "100")
             & #ppEconomicGroup .~ rat ("67", "100")
             & #ppTechnicalGroup .~ rat ("68", "100")
             & #ppGovGroup .~ rat ("69", "100")
             & #treasuryWithdrawal .~ rat ("70", "100")
         )
    & #committeeMinSize .~ 3

-- Three active members, quorum 2/3; with extra, a resigned, an expired and
-- an unauthorized one that never count.
testCommittee :: Bool -> S.Committee
testCommittee extra =
  defMessage
    & #threshold .~ rat ("2", "3")
    & #members
      .~ ( [defMessage & #cold .~ cred (0x10 + i) & #hot .~ cred (0x20 + i) & #expiry .~ 100 | i <- [1 .. 3]]
             ++ if extra
               then
                 [ defMessage & #cold .~ cred 0x14 & #hot .~ cred 0x24 & #expiry .~ 100 & #resigned .~ True
                 , defMessage & #cold .~ cred 0x15 & #hot .~ cred 0x25 & #expiry .~ 10
                 , defMessage & #cold .~ cred 0x16 & #expiry .~ 100
                 ]
               else []
         )

withoutMember :: S.Committee -> S.Committee
withoutMember c = c & #members .~ take 2 (c ^. #members)

-- Thresholds ---------------------------------------------------------------

type Want = (Maybe (Text, Text), Maybe (Text, Text), Maybe (Text, Text))

thresholds :: BS.ByteString -> Int -> Maybe S.Committee -> Reply
thresholds action major committee =
  call
    ( #govThresholds
        .~ ( defMessage
               & #protocolVersion .~ pv major
               & #pparams .~ govParams major
               & maybe id (#committee .~) committee
               & #action .~ action
               & #epoch .~ 50
           )
    )

opt :: Maybe (Text, Text) -> Maybe P.Rational -> Bool
opt Nothing r = null r
opt (Just w) (Just r) = ratIs w r
opt (Just _) Nothing = False

thresholdIs :: String -> BS.ByteString -> Int -> Maybe S.Committee -> Want -> Check
thresholdIs name action major committee (spo, drep, cc) =
  ( "thresholds: " ++ name
  , opt spo (r ^. #maybe'spo) && opt drep (r ^. #maybe'drep) && opt cc (r ^. #maybe'committee)
  )
  where
    r = thresholds action major committee ^. #govThresholds

thresholdTests :: [Check]
thresholdTests =
  [ thresholdIs "no confidence" actNoConfidence 10 c (Just ("51", "100"), Just ("61", "100"), Nothing)
  , thresholdIs "info" actInfo 10 c (Nothing, Nothing, Nothing)
  , thresholdIs "hard fork" actHardFork 10 c (Just ("27", "50"), Just ("13", "20"), Just ("2", "3"))
  , thresholdIs "treasury withdrawals" actTreasury 10 c (Nothing, Just ("7", "10"), Just ("2", "3"))
  , thresholdIs "new constitution" actConstitution 10 c (Nothing, Just ("16", "25"), Just ("2", "3"))
  , thresholdIs "update committee, committee" actUpdateCmte 10 c (Just ("13", "25"), Just ("31", "50"), Nothing)
  , thresholdIs "update committee, none" actUpdateCmte 10 Nothing (Just ("53", "100"), Just ("63", "100"), Nothing)
  , thresholdIs "parameter change network+sec" actPCNetworkSecurity 10 c (Just ("11", "20"), Just ("33", "50"), Just ("2", "3"))
  , thresholdIs "parameter change economic" actPCEconomic 10 c (Nothing, Just ("67", "100"), Just ("2", "3"))
  , thresholdIs "parameter change technical" actPCTechnical 10 c (Nothing, Just ("17", "25"), Just ("2", "3"))
  , thresholdIs "parameter change governance" actPCGov 10 c (Nothing, Just ("69", "100"), Just ("2", "3"))
  , thresholdIs "no committee" actHardFork 10 Nothing (Just ("27", "50"), Just ("13", "20"), Nothing)
  , thresholdIs "below minimum committee size" actHardFork 10 (Just (withoutMember (testCommittee False))) (Just ("27", "50"), Just ("13", "20"), Nothing)
  , thresholdIs "inactive members do not count" actHardFork 10 (Just (testCommittee True)) (Just ("27", "50"), Just ("13", "20"), Just ("2", "3"))
  , thresholdIs "bootstrap zeroes drep" actHardFork 9 c (Just ("27", "50"), Just ("0", "1"), Just ("2", "3"))
  , thresholdIs "bootstrap waives minimum size" actHardFork 9 (Just (withoutMember (testCommittee False))) (Just ("27", "50"), Just ("0", "1"), Just ("2", "3"))
  , thresholdIs "bootstrap info" actInfo 9 c (Nothing, Nothing, Nothing)
  , ( "thresholds: undecodable action is BAD_REQUEST"
    , thresholds (BS.pack [0xff]) 10 Nothing ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
    )
  , ( "thresholds: out-of-range threshold is UNSUPPORTED"
    , call
        ( #govThresholds
            .~ ( defMessage
                   & #protocolVersion .~ pv 10
                   & #pparams .~ (govParams 10 & #poolVotingThresholds . #hardForkInitiation .~ rat ("3", "2"))
                   & #action .~ actHardFork
               )
        )
        ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  ]
  where
    c = Just (testCommittee False)

-- SPO ratio ----------------------------------------------------------------

actionId :: Int -> P.GovActionId
actionId b = defMessage & #txId .~ BS.replicate 32 (fromIntegral b)

alwaysAbstain, alwaysNoConfidence :: P.DRep
alwaysAbstain = defMessage & #alwaysAbstain .~ True
alwaysNoConfidence = defMessage & #alwaysNoConfidence .~ True

keyDRep :: Int -> P.DRep
keyDRep b = defMessage & #credential .~ cred b

poolStake :: Int -> Integer -> S.PoolStake
poolStake b s = defMessage & #poolId .~ ids b & #stake .~ fromInteger s

spoVote :: Int -> P.Vote -> P.GovActionId -> S.VoteRecord
spoVote b v aid =
  defMessage & #actionId .~ aid & #voter .~ (defMessage & #role .~ P.VOTER_ROLE_STAKE_POOL & #poolId .~ ids b) & #vote .~ v

-- Four pools holding 10, 20, 30, 40. Pools 1, 2 and 3 vote yes, no and
-- abstain; pool 4 is silent.
spoRatio :: Int -> BS.ByteString -> Maybe P.DRep -> Reply
spoRatio major action p4 =
  call
    ( #govSpoRatio
        .~ ( defMessage
               & #protocolVersion .~ pv major
               & #actionId .~ aid
               & #action .~ action
               & #poolDistribution .~ [poolStake 1 10, poolStake 2 20, poolStake 3 30, poolStake 4 40]
               & #votes
                 .~ [ spoVote 1 P.VOTE_YES aid
                    , spoVote 2 P.VOTE_NO aid
                    , spoVote 3 P.VOTE_ABSTAIN aid
                    , spoVote 4 P.VOTE_YES (actionId 8)
                    ]
               & maybe
                 id
                 ( \d ->
                     (#pools .~ [defMessage & #poolId .~ ids 4 & #rewardAccount .~ cred 0x44])
                       . (#accounts .~ [defMessage & #credential .~ cred 0x44 & #drep .~ d])
                 )
                 p4
           )
    )
  where
    aid = actionId 7

spoTests :: [Check]
spoTests =
  [ spoIs "silent pool is no" (spoRatio 10 actPCNetworkSecurity Nothing) ("1", "7")
  , spoIs "silent pool abstains in bootstrap" (spoRatio 9 actPCNetworkSecurity Nothing) ("1", "3")
  , spoIs "hard fork: silent pool is no" (spoRatio 10 actHardFork (Just alwaysAbstain)) ("1", "7")
  , spoIs "hard fork bootstrap: silent pool is no" (spoRatio 9 actHardFork Nothing) ("1", "7")
  , spoIs "always-no-confidence yes on motion" (spoRatio 10 actNoConfidence (Just alwaysNoConfidence)) ("5", "7")
  , spoIs "always-no-confidence is no elsewhere" (spoRatio 10 actPCNetworkSecurity (Just alwaysNoConfidence)) ("1", "7")
  , spoIs "always-abstain abstains" (spoRatio 10 actPCNetworkSecurity (Just alwaysAbstain)) ("1", "3")
  , spoIs "always-abstain ignored in bootstrap yes" (spoRatio 9 actNoConfidence (Just alwaysNoConfidence)) ("1", "3")
  , ( "spo ratio: all abstain is zero"
    , ratIs
        ("0", "1")
        ( call
            ( #govSpoRatio
                .~ ( defMessage
                       & #protocolVersion .~ pv 10
                       & #actionId .~ actionId 7
                       & #action .~ actPCNetworkSecurity
                       & #poolDistribution .~ [poolStake 1 10]
                       & #votes .~ [spoVote 1 P.VOTE_ABSTAIN (actionId 7)]
                   )
            )
            ^. #govSpoRatio . #ratio
        )
    )
  , ( "spo ratio: empty distribution is UNSUPPORTED"
    , call (#govSpoRatio .~ (defMessage & #protocolVersion .~ pv 10 & #actionId .~ actionId 7 & #action .~ actPCNetworkSecurity))
        ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  ]
  where
    spoIs name r want = ("spo ratio: " ++ name, ratIs want (r ^. #govSpoRatio . #ratio))

-- DRep ratio ---------------------------------------------------------------

dstake :: P.DRep -> Integer -> S.DRepStake
dstake d s = defMessage & #drep .~ d & #stake .~ fromInteger s

drepVote :: Int -> P.Vote -> P.GovActionId -> S.VoteRecord
drepVote b v aid =
  defMessage & #actionId .~ aid & #voter .~ (defMessage & #role .~ P.VOTER_ROLE_DREP & #credential .~ cred b) & #vote .~ v

-- DReps 1..5 hold 10..50 with expiry 100 (5 expires at 40; 6 is
-- unregistered); 1 votes yes, 2 no, 3 abstains, 4 is silent. Always-abstain
-- holds 7 and always-no-confidence 9.
drepRatio :: Int -> BS.ByteString -> Reply
drepRatio epoch action =
  call
    ( #govDrepRatio
        .~ ( defMessage
               & #epoch .~ fromIntegral epoch
               & #actionId .~ aid
               & #action .~ action
               & #drepDistribution
                 .~ [ dstake (keyDRep 1) 10
                    , dstake (keyDRep 2) 20
                    , dstake (keyDRep 3) 30
                    , dstake (keyDRep 4) 40
                    , dstake (keyDRep 5) 50
                    , dstake (keyDRep 6) 60
                    , dstake alwaysAbstain 7
                    , dstake alwaysNoConfidence 9
                    ]
               & #dreps
                 .~ [ defMessage & #credential .~ cred n & #expiry .~ e
                    | (n, e) <- [(1, 100), (2, 100), (3, 100), (4, 100), (5, 40)]
                    ]
               & #votes
                 .~ [ drepVote 1 P.VOTE_YES aid
                    , drepVote 2 P.VOTE_NO aid
                    , drepVote 3 P.VOTE_ABSTAIN aid
                    , drepVote 4 P.VOTE_YES (actionId 8)
                    ]
           )
    )
  where
    aid = actionId 7

drepTests :: [Check]
drepTests =
  [ drepIs "parameter change" (drepRatio 50 actPCNetworkSecurity) ("10", "79")
  , drepIs "no confidence" (drepRatio 50 actNoConfidence) ("19", "79")
  , drepIs "expiry is inclusive" (drepRatio 40 actPCNetworkSecurity) ("10", "129")
  ]
  where
    drepIs name r want = ("drep ratio: " ++ name, ratIs want (r ^. #govDrepRatio . #ratio))

-- Committee ratio and quorum -----------------------------------------------

ccVote :: Int -> P.Vote -> P.GovActionId -> S.VoteRecord
ccVote b v aid =
  defMessage
    & #actionId .~ aid
    & #voter .~ (defMessage & #role .~ P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE & #credential .~ cred b)
    & #vote .~ v

ccRatio :: [S.VoteRecord] -> Reply
ccRatio votes =
  call
    ( #govCommitteeRatio
        .~ ( defMessage
               & #epoch .~ 50
               & #protocolVersion .~ pv 10
               & #pparams .~ govParams 10
               & #committee .~ testCommittee True
               & #actionId .~ actionId 7
               & #votes .~ votes
           )
    )

quorum :: Int -> Maybe S.Committee -> Reply
quorum major committee =
  call
    ( #govCommitteeQuorum
        .~ ( defMessage
               & #epoch .~ 50
               & #protocolVersion .~ pv major
               & #pparams .~ govParams major
               & maybe id (#committee .~) committee
           )
    )

committeeTests :: [Check]
committeeTests =
  [ ccIs "yes, no, silent" [ccVote 0x21 P.VOTE_YES a, ccVote 0x22 P.VOTE_NO a] ("1", "3")
  , ccIs "abstain leaves out" [ccVote 0x21 P.VOTE_YES a, ccVote 0x22 P.VOTE_ABSTAIN a] ("1", "2")
  , ccIs "all yes" [ccVote 0x21 P.VOTE_YES a, ccVote 0x22 P.VOTE_YES a, ccVote 0x23 P.VOTE_YES a] ("1", "1")
  , ccIs "inactive hot keys do not vote" [ccVote 0x24 P.VOTE_YES a, ccVote 0x25 P.VOTE_YES a, ccVote 0x21 P.VOTE_YES a] ("1", "3")
  , ccIs "other action" [ccVote 0x21 P.VOTE_YES (actionId 8)] ("0", "1")
  , ( "committee ratio: no committee is zero"
    , ratIs ("0", "1") (call (#govCommitteeRatio .~ (defMessage & #epoch .~ 50 & #actionId .~ actionId 7)) ^. #govCommitteeRatio . #ratio)
    )
  , quorumIs "in force" 10 (Just (testCommittee True)) (Just ("2", "3"))
  , quorumIs "no committee" 10 Nothing Nothing
  , quorumIs "below minimum size" 10 (Just (withoutMember (testCommittee False))) Nothing
  , quorumIs "bootstrap waives minimum" 9 (Just (withoutMember (testCommittee False))) (Just ("2", "3"))
  , quorumIs "bootstrap empty members" 9 (Just (defMessage & #threshold .~ rat ("0", "1"))) (Just ("0", "1"))
  ]
  where
    a = actionId 7
    ccIs name votes want = ("committee ratio: " ++ name, ratIs want (ccRatio votes ^. #govCommitteeRatio . #ratio))
    quorumIs name major committee want =
      ("committee quorum: " ++ name, opt want (quorum major committee ^. #govCommitteeQuorum . #maybe'quorum))

-- Proposal tree ------------------------------------------------------------

idBytes :: Int -> BS.ByteString
idBytes b = BS.replicate 32 (fromIntegral b)

pcAction, hfAction :: Int -> BS.ByteString
pcAction parent = hex ((if parent == 0 then "8400f6" else "840082" ++ "5820" ++ concat (replicate 32 (byteHex parent)) ++ "00") ++ "a1021a00016000f6")
hfAction parent = hex ((if parent == 0 then "8301f6" else "830182" ++ "5820" ++ concat (replicate 32 (byteHex parent)) ++ "00") ++ "820a00")

byteHex :: Int -> String
byteHex n = [digits !! (n `div` 16), digits !! (n `mod` 16)]
  where
    digits = "0123456789abcdef"

proposal :: Int -> BS.ByteString -> S.Proposal
proposal i action =
  defMessage
    & #id .~ (defMessage & #txId .~ idBytes i)
    & #action .~ action
    & #proposedEpoch .~ 1
    & #expiresEpoch .~ 10
    & #deposit .~ 1000
    & #returnAccount .~ cred 0x50

idList :: [Int] -> [P.GovActionId]
idList = map (\i -> defMessage & #txId .~ idBytes i)

firstBytes :: [P.GovActionId] -> [Int]
firstBytes ids' = [fromIntegral (BS.head (i ^. #txId)) | i <- ids']

-- 1 and 2 compete for the parameter root; 3 follows 1 and 4 follows 2; 5 is a
-- hard fork; 6 is a treasury withdrawal; 7 is an info action.
treeProposals :: [S.Proposal]
treeProposals =
  [ proposal 1 (pcAction 0)
  , proposal 2 (pcAction 0)
  , proposal 3 (pcAction 1)
  , proposal 4 (pcAction 2)
  , proposal 5 (hfAction 0)
  , proposal 6 (hex "8302a0f6")
  , proposal 7 (hex "8106")
  ]

tree :: [S.Proposal] -> Maybe S.EnactState -> [Int] -> [Int] -> Reply
tree props enact enacted expired =
  call
    ( #govProposalTree
        .~ ( defMessage
               & #proposals .~ props
               & maybe id (#enactState .~) enact
               & #enacted .~ idList enacted
               & #expired .~ idList expired
           )
    )

treeTests :: [Check]
treeTests =
  [ treeIs "enacted action removes competitors and their subtrees" [1] [] [3, 5, 6, 7] [2, 4]
  , treeIs "expired action removes its subtree" [] [1] [2, 4, 5, 6, 7] [1, 3]
  , treeIs "chain enacted in one round" [1, 3] [] [5, 6, 7] [2, 4]
  , treeIs "enactment and expiry together" [1] [5] [3, 6, 7] [2, 4, 5]
  , treeIs "treasury withdrawal has no competitors" [6] [] [1, 2, 3, 4, 5, 7] []
  , treeIs "nothing happens" [] [] [1, 2, 3, 4, 5, 6, 7] []
  , ( "proposal tree: competitors of an action following the enacted root"
    , let r =
            tree
              [proposal 1 (pcAction 9), proposal 2 (pcAction 9), proposal 3 (pcAction 1)]
              (Just (defMessage & #prevPparamUpdate .~ (defMessage & #txId .~ idBytes 9)))
              [2]
              []
          in firstBytes (r ^. #govProposalTree . #remaining) == [] && firstBytes (r ^. #govProposalTree . #removed) == [1, 3]
    )
  , ( "proposal tree: a parent that is not the root or a proposal is UNSUPPORTED"
    , tree [proposal 1 (pcAction 9)] Nothing [] [] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "proposal tree: unknown enacted id is BAD_REQUEST"
    , tree treeProposals Nothing [42] [] ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
    )
  ]
  where
    treeIs name enacted expired remaining removed =
      let r = tree treeProposals Nothing enacted expired ^. #govProposalTree
       in ( "proposal tree: " ++ name
          , firstBytes (r ^. #remaining) == remaining && firstBytes (r ^. #removed) == removed
          )

-- DRep expiry --------------------------------------------------------------

drepRecord :: Int -> Integer -> S.DRepRecord
drepRecord b e = defMessage & #credential .~ cred b & #deposit .~ 500 & #expiry .~ fromInteger e & #anchorUrl .~ "https://x"

expiryCall :: Integer -> Integer -> [S.DRepRecord] -> Reply
expiryCall epoch dormant dreps =
  call (#govDrepExpiry .~ (defMessage & #epoch .~ fromInteger epoch & #dormantEpochs .~ fromInteger dormant & #dreps .~ dreps))

-- The dormant count is added to each expiry unless the sum is below the
-- current epoch.
expiryTests :: [Check]
expiryTests =
  [ ( "drep expiry: sums, the current-epoch boundary and zero expiry"
    , let r = expiryCall 20 3 [drepRecord 1 30, drepRecord 3 17, drepRecord 4 0] ^. #govDrepExpiry . #drepsChanged
       in [(BS.head (d ^. #credential . #hash), d ^. #expiry, d ^. #deposit, d ^. #anchorUrl) | d <- r]
            == [(1, 33, 500, "https://x"), (3, 20, 500, "https://x")]
    )
  , ( "drep expiry: no dormant epochs changes nothing"
    , null (expiryCall 20 0 [drepRecord 1 30] ^. #govDrepExpiry . #drepsChanged)
    )
  ]

-- Ratify, enact and proposal expiry --------------------------------------------

-- Same cases as dxnode-go (cmd/dxnode-go/gov_ratify_test.go and
-- gov_enact_test.go).

-- | A TreasuryWithdrawals action paying each (key, amount) to the key-hash
-- reward account made of key repeated.
withdrawalAction :: [(Int, Integer)] -> BS.ByteString
withdrawalAction ps =
  BS.concat
    ( [BS.pack [0x83, 0x02, 0xa0 + fromIntegral (length ps)]]
        ++ [BS.concat [BS.pack [0x58, 0x1d, 0xe0], BS.replicate 28 (fromIntegral k), BS.pack (0x1b : be64 a)] | (k, a) <- ps]
        ++ [BS.pack [0xf6]]
    )
  where
    be64 a = [fromIntegral (a `shiftR` (8 * i)) | i <- [7, 6 .. 0]]

voteOn :: Int -> P.VoterRole -> P.Credential -> P.Vote -> S.VoteRecord
voteOn i role c v =
  defMessage & #actionId .~ actionId i & #voter .~ (defMessage & #role .~ role & #credential .~ c) & #vote .~ v

allVote :: Int -> P.Vote -> [S.VoteRecord]
allVote i v =
  [ voteOn i P.VOTER_ROLE_DREP (cred 0xd1) v
  , spoVote 0xb1 v (actionId i)
  ]
    ++ [voteOn i P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE (cred (0x20 + k)) v | k <- [1 .. 3]]

ratifyProposal :: Int -> BS.ByteString -> Integer -> S.Proposal
ratifyProposal i action expires = proposal i action & #expiresEpoch .~ fromInteger expires

ratifyReq :: Integer -> Integer -> [S.Proposal] -> [S.VoteRecord] -> GovRatifyRequest
ratifyReq epoch treasury props votes =
  defMessage
    & #context .~ (defMessage & #epoch .~ fromInteger epoch)
    & #pparams .~ govParams 10
    & #state
      .~ ( defMessage
             & #dreps .~ [defMessage & #credential .~ cred 0xd1 & #expiry .~ 100]
             & #committee .~ testCommittee False
             & #proposals .~ props
             & #votes .~ votes
             & #pots .~ (defMessage & #treasury .~ fromInteger treasury)
         )
    & #poolDistribution .~ [poolStake 0xb1 100]
    & #drepDistribution .~ [defMessage & #drep .~ keyDRep 0xd1 & #stake .~ 100]

ratifyCall :: GovRatifyRequest -> Reply
ratifyCall r = call (#govRatify .~ r)

ratifyTests :: [Check]
ratifyTests =
  [ ratifyIs
      "no confidence accepted"
      (ratifyReq 5 0 [ratifyProposal 1 actNoConfidence 9] [voteOn 1 P.VOTER_ROLE_DREP (cred 0xd1) P.VOTE_YES, spoVote 0xb1 P.VOTE_YES (actionId 1)])
      [1]
      []
      True
  , ratifyIs
      "silent pool leaves the pool ratio at zero"
      (ratifyReq 5 0 [ratifyProposal 1 actNoConfidence 9] [voteOn 1 P.VOTER_ROLE_DREP (cred 0xd1) P.VOTE_YES])
      []
      []
      False
  , ratifyIs
      "a delaying action blocks the actions after it"
      (ratifyReq 5 0 [ratifyProposal 1 (hfAction 0) 9, ratifyProposal 2 (pcAction 0) 9] (allVote 1 P.VOTE_YES ++ allVote 2 P.VOTE_YES))
      [1]
      []
      True
  , ratifyIs
      "committee below quorum"
      ( ratifyReq
          5
          0
          [ratifyProposal 1 (hfAction 0) 9]
          [ voteOn 1 P.VOTER_ROLE_DREP (cred 0xd1) P.VOTE_YES
          , spoVote 0xb1 P.VOTE_YES (actionId 1)
          , voteOn 1 P.VOTER_ROLE_CONSTITUTIONAL_COMMITTEE (cred 0x21) P.VOTE_YES
          ]
      )
      []
      []
      False
  , ratifyIs
      "unaccepted action past its expiry expires"
      (ratifyReq 5 0 [ratifyProposal 1 actInfo 4, ratifyProposal 2 actInfo 5] [])
      []
      [1]
      False
  , ratifyIs
      "accepted action at its last chance is not expired"
      (ratifyReq 5 0 [ratifyProposal 1 (hfAction 0) 4] (allVote 1 P.VOTE_YES))
      [1]
      []
      True
  , ratifyIs
      "a parameter change chain is accepted in one pass"
      (ratifyReq 5 0 [ratifyProposal 1 (pcAction 0) 9, ratifyProposal 2 (pcAction 1) 9] (allVote 1 P.VOTE_YES ++ allVote 2 P.VOTE_YES))
      [1, 2]
      []
      False
  , ratifyIs
      "treasury withdrawals share one budget"
      ( ratifyReq
          5
          1500
          [ratifyProposal 1 (withdrawalAction [(0x61, 1000)]) 9, ratifyProposal 2 (withdrawalAction [(0x62, 1000)]) 9]
          (allVote 1 P.VOTE_YES ++ allVote 2 P.VOTE_YES)
      )
      [1]
      []
      False
  , ratifyError "epoch zero" (ratifyReq 0 0 [ratifyProposal 1 actInfo 9] []) HARNESS_ERROR_CODE_UNSUPPORTED
  , ratifyError "no pool stake" (ratifyReq 5 0 [ratifyProposal 1 actInfo 9] [] & #poolDistribution .~ []) HARNESS_ERROR_CODE_UNSUPPORTED
  , ratifyError "expired before the previous epoch" (ratifyReq 5 0 [ratifyProposal 1 actInfo 2] []) HARNESS_ERROR_CODE_UNSUPPORTED
  , ratifyError
      "proposals out of submission order"
      (ratifyReq 5 0 [ratifyProposal 1 actInfo 9 & #proposedEpoch .~ 3, ratifyProposal 2 actInfo 9 & #proposedEpoch .~ 2] [])
      HARNESS_ERROR_CODE_UNSUPPORTED
  , ratifyError "parent that is not the root" (ratifyReq 5 0 [ratifyProposal 1 (pcAction 9) 9] []) HARNESS_ERROR_CODE_UNSUPPORTED
  , ratifyError "undecodable action" (ratifyReq 5 0 [ratifyProposal 1 (BS.pack [0xff]) 9] []) HARNESS_ERROR_CODE_BAD_REQUEST
  ]
  where
    ratifyIs name req enacted expired delayed =
      let r = ratifyCall req ^. #govRatify
       in ( "ratify: " ++ name
          , null (r ^. #maybe'rejection) && firstBytes (r ^. #enacted) == enacted && firstBytes (r ^. #expired) == expired && r ^. #delayed == delayed
          )
    ratifyError name req code = ("ratify: " ++ name ++ " is a harness error", ratifyCall req ^. #error . #code == code)

-- With every threshold 1 and nothing delegated, only expiry removes a
-- proposal.
proposalExpiryTests :: [Check]
proposalExpiryTests =
  [ ( "proposal expiry: expired subtrees leave, deposits are refunded or left to the treasury"
    , let e =
            expiryReq
              5
              [ mk 1 (pcAction 0) 3 1000 0xa1
              , mk 2 (pcAction 1) 20 500 0xa2
              , mk 3 actInfo 4 300 0xa3
              , mk 4 actInfo 5 200 0xa1
              ]
              [defMessage & #credential .~ cred 0xa1 & #balance .~ 10]
              ^. #govProposalExpiry
         in firstBytes (e ^. #expired) == [1, 2, 3]
              && [(BS.head (c ^. #credential . #hash), c ^. #amount) | c <- e ^. #refunds] == [(0xa1, 1000)]
              && e ^. #unclaimedToTreasury == 800
    )
  , ( "proposal expiry: epoch zero is UNSUPPORTED"
    , expiryReq 0 [] [] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "proposal expiry: a parent that is not a root is UNSUPPORTED"
    , expiryReq 5 [proposal 1 (pcAction 9)] [] ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  ]
  where
    mk i action expires deposit ret = proposal i action & #expiresEpoch .~ expires & #deposit .~ deposit & #returnAccount .~ cred ret
    expiryReq epoch props accts =
      call (#govProposalExpiry .~ (defMessage & #epoch .~ epoch & #proposals .~ props & #accounts .~ accts))

-- Enact --------------------------------------------------------------------

enactParams :: S.ProtocolParameters
enactParams =
  govParams 10
    & #minFeeA .~ 44
    & #minFeeB .~ 155381
    & #maxBlockBodySize .~ 65536
    & #maxTxSize .~ 16384
    & #maxBlockHeaderSize .~ 1100
    & #keyDeposit .~ 2000000
    & #poolDeposit .~ 500000000
    & #maxEpoch .~ 18
    & #nOpt .~ 500
    & #a0 .~ rat ("3", "10")
    & #rho .~ rat ("3", "1000")
    & #tau .~ rat ("1", "5")
    & #minPoolCost .~ 170000000
    & #coinsPerUtxoByte .~ 4310
    & #priceMem .~ rat ("577", "10000")
    & #priceSteps .~ rat ("721", "10000000")
    & #maxTxExMem .~ 14000000
    & #maxTxExSteps .~ 10000000000
    & #maxBlockExMem .~ 62000000
    & #maxBlockExSteps .~ 20000000000
    & #maxValueSize .~ 5000
    & #collateralPercentage .~ 150
    & #maxCollateralInputs .~ 3
    & #committeeMaxTermLength .~ 146
    & #govActionLifetime .~ 6
    & #govActionDeposit .~ 100000000000
    & #drepDeposit .~ 500000000
    & #drepActivity .~ 20
    & #minFeeRefScriptCostPerByte .~ rat ("15", "1")

enactCall :: Int -> BS.ByteString -> S.LedgerState -> Reply
enactCall i action st =
  call (#govEnact .~ (defMessage & #pparams .~ enactParams & #state .~ st & #actionId .~ actionId i & #action .~ action))

-- 'Nothing' when the response carries a rejection or a harness error.
enacted :: Reply -> Maybe GovEnactResponse
enacted r
  | null (r ^. #govEnact . #maybe'rejection) && r ^. #error . #code == HARNESS_ERROR_CODE_UNSPECIFIED = Just (r ^. #govEnact)
  | otherwise = Nothing

updateCommitteeAction :: BS.ByteString
updateCommitteeAction =
  BS.concat
    [ hex "8504f6d90102818200581c"
    , ids 0x11
    , hex "a18200581c"
    , ids 0x77
    , hex "1864d81e820304"
    ]

enactTests :: [Check]
enactTests =
  [ ( "enact: parameter change"
    , case enacted (enactCall 1 (pcAction 0) defMessage) of
        Just r ->
          let d = r ^. #delta
           in d ^. #pparamsAfter . #maxBlockBodySize == 0x16000
                && d ^. #pparamsAfter . #maxTxSize == 16384
                && firstBytes (toList (d ^. #enactStateAfter . #maybe'prevPparamUpdate)) == [1]
                && null (d ^. #enactStateAfter . #maybe'prevHardFork)
                && null (r ^. #withdrawals)
                && d ^. #treasuryDelta == 0
        Nothing -> False
    )
  , ( "enact: hard fork"
    , case enacted (enactCall 1 (hfAction 0) defMessage) of
        Just r ->
          let d = r ^. #delta
           in d ^. #pparamsAfter . #protocolVersion . #major == 10
                && d ^. #pparamsAfter . #protocolVersion . #minor == 0
                && firstBytes (toList (d ^. #enactStateAfter . #maybe'prevHardFork)) == [1]
        Nothing -> False
    )
  , ( "enact: treasury withdrawal pays registered accounts and leaves the rest"
    , case enacted (enactCall 1 (withdrawalAction [(0x61, 500), (0x62, 300)]) withdrawalState) of
        Just r ->
          let d = r ^. #delta
           in [(BS.head (c ^. #credential . #hash), c ^. #amount) | c <- r ^. #withdrawals] == [(0x61, 500)]
                && [(BS.head (a ^. #credential . #hash), a ^. #balance) | a <- d ^. #accountsChanged] == [(0x61, 507)]
                && d ^. #treasuryDelta == -500
        Nothing -> False
    )
  , depositCase "registered" True 100 0
  , depositCase "unregistered" False 0 100
  , ( "enact: no confidence leaves no committee"
    , case enacted (enactCall 1 actNoConfidence (defMessage & #committee .~ testCommittee False)) of
        Just r ->
          let d = r ^. #delta
           in null (d ^. #committeeAfter . #members)
                && null (d ^. #committeeAfter . #maybe'threshold)
                && firstBytes (toList (d ^. #enactStateAfter . #maybe'prevCommittee)) == [1]
                && null (d ^. #maybe'pparamsAfter)
        Nothing -> False
    )
  , ( "enact: update committee"
    , case enacted (enactCall 1 updateCommitteeAction (defMessage & #committee .~ testCommittee False)) of
        Just r ->
          let c = r ^. #delta . #committeeAfter
           in [BS.head (m ^. #cold . #hash) | m <- c ^. #members] == [0x12, 0x13, 0x77]
                && all (\m -> m ^. #expiry /= 0) (c ^. #members)
                && ratIs ("3", "4") (c ^. #threshold)
                && firstBytes (toList (r ^. #delta . #enactStateAfter . #maybe'prevCommittee)) == [1]
        Nothing -> False
    )
  , ( "enact: new constitution"
    , case enacted (enactCall 1 actConstitution defMessage) of
        Just r ->
          let es = r ^. #delta . #enactStateAfter
           in es ^. #constitution . #anchorUrl == "x"
                && es ^. #constitution . #anchorHash == BS.replicate 32 0
                && firstBytes (toList (es ^. #maybe'prevConstitution)) == [1]
        Nothing -> False
    )
  , ( "enact: the other purposes keep their roots"
    , let st =
            ( defMessage
                & #committee .~ testCommittee False
                & #enactState
                  .~ ( defMessage
                         & #prevHardFork .~ actionId 9
                         & #prevCommittee .~ actionId 8
                         & #constitution .~ (defMessage & #anchorUrl .~ "https://c" & #anchorHash .~ BS.replicate 32 3)
                     )
            ) ::
            S.LedgerState
       in case enacted (enactCall 1 (pcAction 0) st) of
            Just r ->
              let es = r ^. #delta . #enactStateAfter
               in firstBytes (toList (es ^. #maybe'prevHardFork)) == [9]
                    && firstBytes (toList (es ^. #maybe'prevCommittee)) == [8]
                    && es ^. #constitution . #anchorUrl == "https://c"
            Nothing -> False
    )
  , ( "enact: withdrawal above the treasury is UNSUPPORTED"
    , enactCall 1 (withdrawalAction [(0x61, 500)]) (defMessage & #pots . #treasury .~ 499) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "enact: a parent that is not the root is UNSUPPORTED"
    , enactCall 1 (pcAction 9) defMessage ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "enact: an undecodable action is BAD_REQUEST"
    , enactCall 1 (BS.pack [0xff]) defMessage ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
    )
  ]
  where
    withdrawalState =
      defMessage
        & #pots . #treasury .~ 2000
        & #accounts .~ [defMessage & #credential .~ cred 0x61 & #balance .~ 7]
    depositCase name registered credit treasury =
      ( "enact: the deposit goes to the " ++ name ++ " return account"
      , let p = proposal 1 actInfo & #deposit .~ 100 & #returnAccount .~ cred 0x61
            st =
              defMessage
                & #proposals .~ [p]
                & (if registered then #accounts .~ [defMessage & #credential .~ cred 0x61 & #balance .~ 7] else id)
         in case enacted (enactCall 1 actInfo st) of
              Just r ->
                sum [c ^. #amount | c <- r ^. #withdrawals] == credit
                  && r ^. #delta . #treasuryDelta == treasury
              Nothing -> False
      )

-- Divergences ----------------------------------------------------------------

-- The reference library called directly, bypassing the adapter. The
-- reference takes the maximum DRep threshold over the groups an update
-- touches, which is zero for none; dingo falls back to the governance group.
-- The ledger rejects an empty update as malformed.
divergenceTests :: [Check]
divergenceTests =
  [ ( "divergence: an empty parameter change update has DRep threshold zero"
    , votingDRepThreshold rs (ParameterChange SNothing emptyPParamsUpdate SNothing) == SJust minBound
    )
  , ( "divergence: the ledger rejects an empty parameter update as malformed"
    , not (ppuWellFormed (ProtVer (natVersion @10) 0) (emptyPParamsUpdate :: PParamsUpdate ConwayEra))
    )
  , ( "divergence: an expired DRep keeps its expiry when the dormant count does not reach the epoch"
    , let vs =
            VState
              (Map.singleton (KeyHashObj (KeyHash (maybe (error "hash") id (hashFromBytes (BS.replicate 28 0xd2))))) (DRepState (EpochNo 10) SNothing (CompactCoin 0) Set.empty))
              def
              (EpochNo 3) ::
            VState ConwayEra
       in map drepExpiry (Map.elems (vsDReps (updateDormantDRepExpiry (EpochNo 20) vs))) == [EpochNo 10]
    )
  , ( "divergence: a DRep with expiry 0 is expired from epoch 1"
    , drepRatio 0 0 == 1 && drepRatio 0 1 == 0
    )
  , ( "divergence: a DRep that expired one epoch earlier counts nothing at RATIFY"
    , drepRatio 4 5 == 0 && drepRatio 5 5 == 1
    )
  ]
  where
    -- The accepted ratio of the only DRep (all the stake, voted yes) with
    -- the given expiry at the given epoch.
    drepRatio expiry epoch =
      let c = KeyHashObj (KeyHash (maybe (error "hash") id (hashFromBytes (BS.replicate 28 0xd1))))
          env =
            (def :: RatifyEnv ConwayEra)
              { reDRepDistr = Map.singleton (DRepCredential c) (CompactCoin 100)
              , reDRepState = Map.singleton c (DRepState (EpochNo expiry) SNothing (CompactCoin 0) Set.empty)
              , reCurrentEpoch = EpochNo epoch
              }
       in dRepAcceptedRatio env (Map.singleton c VoteYes) (InfoAction :: GovAction ConwayEra)
    pp = emptyPParams & ppDRepVotingThresholdsL .~ (def {dvtPPGovGroup = maybe minBound id (boundRational (69 % 100))})
    rs = RatifyState (def & ensCurPParamsL .~ (pp :: PParams ConwayEra)) mempty mempty False
