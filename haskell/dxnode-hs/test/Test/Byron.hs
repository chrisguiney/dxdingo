{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Byron (tests) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Interface as DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Common (BlockCount (..), KeyHash (..), hashKey)
import Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import Cardano.Chain.UTxO (defaultUTxOConfiguration)
import qualified Cardano.Chain.UTxO as UTxO
import Cardano.Chain.ValidationMode (ValidationMode (..))
import Cardano.Crypto (RequiresNetworkMagic (..), abstractHashFromBytes, abstractHashToBytes, hashRaw)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dxdingo.Byron (delegationPairs, fullParamsArg, upiStateArg)
import Proto.Dxdingo.V1.Byron (ByronDelegation, ByronSigner)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Crypto (ProtocolMagicId (..))
import Cardano.Crypto.DSIGN (SignedDSIGN (..), verifySignedDSIGN)
import Cardano.Ledger.Binary (byronProtVer, decodeFullAnnotatedBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ouroboros.Consensus.Byron.Crypto.DSIGN (SigDSIGN (..), VerKeyDSIGN (..))
import Data.Char (digitToInt)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import qualified Proto.Dxdingo.V1.State as S
import Test.Util

-- The same cases as dxnode-go's byron_test.go, with the transactions from
-- internal/byronfixture written out in hex: a 10 ADA output at key 1's
-- address spent by one witnessed transaction.
tests :: [Check]
tests =
  [ ("min fee: mainnet 200 bytes is ceil(8.7892)", fee ("155381", "1") mainnetB 200 == Just 155390)
  , ("min fee: zero size is the summand", fee ("155381", "1") mainnetB 0 == Just 155381)
  , ("min fee: exact product is not rounded up", fee ("0", "1") ("1", "2") 10 == Just 5)
  , ("min fee: half a lovelace rounds up", fee ("0", "1") ("1", "2") 11 == Just 6)
  , ("min fee: zero multiplier", fee ("7", "1") ("0", "1") 1000000 == Just 7)
  , ("min fee: summand at the Lovelace bound", fee ("45000000000000000", "1") ("0", "1") 5 == Just 45000000000000000)
  , ("min fee: overflow is rejected", failures (minFee ("45000000000000000", "1") ("1", "1") 1) == ["LovelaceOverflow"])
  , ("tx: valid", txValid == (Nothing, 1000000, 1, 1))
  , ("tx: fee below the minimum", txName "fee" == ["TxValidationFeeTooSmall"])
  , ("tx: outputs exceed inputs", txName "exceed" == ["TxValidationLovelaceError"])
  , ("tx: witness signs another transaction", txName "badsig" == ["TxValidationWitnessWrongSignature"])
  , ("tx: witness key does not own the input", txName "wrongkey" == ["TxValidationWitnessWrongKey"])
  , ("tx: output on another network", txName "network" == ["TxValidationNetworkMagicMismatch"])
  , ("tx: input not in the UTxO", txName "missing" == ["UTxOMissingInput"])
  , ("tx: over the size limit", failures (txResp "valid" (#maxTxSize .~ 100)) == ["TxValidationTxTooLarge"])
  , ("header: real preprod header is accepted", hdrFailures (hdrReq headerHex dlgs) == [])
  , ("header: signed bytes changed", hdrFailures (hdrReq badHeaderHex dlgs) == ["PBftInvalidSignature"])
  , ("header: removing the signer's delegation is the only removal that rejects", removals == (1, length dlgs - 1))
  , ("header: not a header", hdrFailures (hdrReq "ff" dlgs) == ["DeserialiseFailure"])
  , ("header: recent_signers is unsupported", hdrUnsupported)
  , ("header window: 474 signatures plus this one", winNames (signersFor 474 (const True)) == [])
  , ("header window: 475 signatures plus this one exceeds floor(0.22 k)", winNames (signersFor 475 (const True)) == ["PBftExceededSignThreshold"])
  , ("header window: the oldest signature leaves a full window", winNames (signersFor 2160 (< 475)) == [])
  , ("header window: another's oldest signature leaves 476 in the window", winNames (signersFor 2160 (\i -> i >= 1 && i < 476)) == ["PBftExceededSignThreshold"])
  , ("header window: slot before the last signer", winNames [signer (BS.replicate 28 1) 6481] == ["PBftInvalidSlot"])
  , ("header window: slot equal to the last signer", winNames [signer (BS.replicate 28 1) 6480] == [])
  , ("divergence: the header verifies under the magic it was signed for", signatureUnder 1 == Just True)
  , ("divergence: another magic fails the signature, there is no separate magic check", signatureUnder 764824073 == Just False)
  , ("chain: preprod main block 3 is accepted", chainName (chainReq id) == [] && chainPrevSlot (chainReq id) == 4325)
  , ("chain: another network's magic", chainName (chainReq (#protocolMagic .~ 2)) == ["ChainValidationProtocolMagicMismatch"])
  , ("chain: not a block", chainName (chainReq (#block .~ BS.pack [0xff])) == ["DeserialiseFailure"])
  , ("chain: a PBFT window is unsupported", chainUnsupported)
  , ("update: preprod block 2 registers its proposal and confirms it", updateOutcome == (Nothing, 1, 7, True))
  , ("update: not a payload", updateName (#updatePayload .~ BS.pack [0xff]) == ["DeserialiseFailure"])
  , ("update: votes without their proposal", updateName (#updatePayload .~ unhex votesOnlyHex) == ["Voting", "VotingProposalNotRegistered"])
  , ("divergence: a main block with the wrong previous hash is accepted by updateChainBlockOrBoundary", isRight mainWrongPrev)
  , ("tx: undecodable", failures (runTx (BS.pack [0xff]) id) == ["DeserialiseFailure"])
  ]
  where
    -- Direct library call: the delegate's signature checked with
    -- verifySignedDSIGN under a given protocol magic, as PBFT does.
    signatureUnder magic =
      case decodeFullAnnotatedBytes byronProtVer "Header" (CC.decCBORAHeader (EpochSlots 21600)) (BSL.fromStrict (unhex headerHex)) of
        Left _ -> Nothing
        Right hdr ->
          let genKey = VerKeyByronDSIGN (CC.headerGenesisKey hdr)
              issuer = VerKeyByronDSIGN (Delegation.delegateVK (CC.delegationCertificate (CC.headerSignature hdr)))
              sig = SignedDSIGN (SigByronDSIGN (CC.signature (CC.headerSignature hdr)))
           in Just (either (const False) (const True) (verifySignedDSIGN (ProtocolMagicId magic, genKey) issuer (CC.recoverSignedBytes (EpochSlots 21600) hdr) sig))
    hdrFailures r = failures' (r ^. #byronHeaderValidate)
    failures' r = [f ^. #name | f <- r ^. #rejection . #failures]
    removals =
      let outs = [hdrFailures (hdrReq headerHex (take i dlgs ++ drop (i + 1) dlgs)) | i <- [0 .. length dlgs - 1]]
       in (length (filter (== ["PBftNotGenesisDelegate"]) outs), length (filter null outs))
    hdrReq hex ds =
      call
        ( #byronHeaderValidate
            .~ ( defMessage
                  & #protocolMagic .~ 1
                  & #securityParam .~ 2160
                  & #pparams .~ pparams
                  & #chainState .~ (defMessage & #delegations .~ [defMessage & #issuerVkey .~ unhex i & #delegateVkey .~ unhex dl | (i, dl) <- ds])
                  & #header .~ unhex hex
               )
        )
    hdrUnsupported =
      let r =
            call
              ( #byronHeaderValidate
                  .~ ( defMessage
                        & #securityParam .~ 2160
                        & #chainState .~ (defMessage & #recentSigners .~ [BS.pack [1]] & #delegations .~ [defMessage & #issuerVkey .~ unhex (fst (head dlgs)) & #delegateVkey .~ unhex (snd (head dlgs))])
                        & #header .~ unhex headerHex
                     )
              )
       in r ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    dlgProtos = [defMessage & #issuerVkey .~ unhex i & #delegateVkey .~ unhex dl | (i, dl) <- dlgs] :: [ByronDelegation]
    chainReq adj =
      call
        ( #byronChainUpdate
            .~ ( defMessage
                  & #protocolMagic .~ 1
                  & #securityParam .~ 2160
                  & #pparams .~ chainParams
                  & #chainState
                    .~ ( defMessage
                          & #prevHash .~ unhex prev3Hex
                          & #prevSlot .~ 2163
                          & #delegations .~ dlgProtos
                       )
                  & #block .~ unhex block3Hex
                  & adj
               )
        )
        ^. #byronChainUpdate
    chainName r = failures' r
    chainPrevSlot r = r ^. #chainState . #prevSlot
    chainUnsupported =
      let r =
            call
              ( #byronChainUpdate
                  .~ ( defMessage
                        & #securityParam .~ 2160
                        & #pparams .~ chainParams
                        & #chainState .~ (defMessage & #signers .~ [defMessage & #key .~ BS.replicate 28 1] & #delegations .~ dlgProtos)
                        & #block .~ unhex block3Hex
                     )
              )
       in r ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
    chainParams :: S.ByronProtocolParameters
    chainParams =
      pparams
        & #slotDuration .~ 20000
        & #maxProposalSize .~ 700
        & #updateProposalTtl .~ 10000
        & #updateAdoptionThreshold .~ rat ("600000000000000", "1000000000000000")
        & #softforkInitThreshold .~ rat ("900000000000000", "1000000000000000")
        & #softforkDecrement .~ rat ("50000000000000", "1000000000000000")
    updateReq adj =
      call
        ( #byronUpdateProposal
            .~ ( defMessage
                  & #slot .~ 2163
                  & #state .~ (defMessage & #pparams .~ chainParams)
                  & #updatePayload .~ unhex payload2Hex
                  & #issuerVkey .~ unhex issuer2Hex
                  & #protocolMagic .~ 1
                  & #securityParam .~ 2160
                  & #delegations .~ dlgProtos
                  & #numGenesisKeys .~ 7
                  & adj
               )
        )
        ^. #byronUpdateProposal
    updateName adj = [f ^. #name | f <- updateReq adj ^. #rejection . #failures]
    updateOutcome =
      let r = updateReq id
          st = r ^. #state
       in ( if null (updateName id) then Nothing else Just (updateName id)
          , length (st ^. #proposals)
          , length (st ^. #votes)
          , all (\p -> p ^. #maybe'confirmedSlot /= Nothing) (st ^. #proposals)
          )
    -- Direct library call: updateChainBlockOrBoundary on main block 3 with a
    -- previous hash that is not its parent's.
    mainWrongPrev =
      let wrong = BS.pack (0 : replicate 31 0)
          eitherState = do
            pairs <- delegationPairs dlgProtos
            pp <- fullParamsArg (Just chainParams)
            upi <- upiStateArg Nothing (Just chainParams) (EpochNumber 0)
            pure (pairs, pp, upi)
       in case (eitherState, decodeFullAnnotatedBytes byronProtVer "ABlockOrBoundary" (CC.decCBORABlockOrBoundary (EpochSlots 21600)) (BSL.fromStrict (unhex block3Hex)), abstractHashFromBytes wrong) of
            (Right (pairs, pp, upi), Right blk, Just hh) ->
              let pm = ProtocolMagicId 1
                  cfg =
                    Genesis.Config
                      { Genesis.configGenesisData =
                          Genesis.GenesisData
                            { Genesis.gdGenesisKeyHashes = Genesis.GenesisKeyHashes (Set.fromList (map fst pairs))
                            , Genesis.gdHeavyDelegation = Genesis.UnsafeGenesisDelegation Map.empty
                            , Genesis.gdStartTime = posixSecondsToUTCTime 0
                            , Genesis.gdNonAvvmBalances = Genesis.GenesisNonAvvmBalances Map.empty
                            , Genesis.gdProtocolParameters = pp
                            , Genesis.gdK = BlockCount 2160
                            , Genesis.gdProtocolMagicId = pm
                            , Genesis.gdAvvmDistr = Genesis.GenesisAvvmBalances Map.empty
                            }
                      , Genesis.configGenesisHash = Genesis.GenesisHash (hashRaw BSL.empty)
                      , Genesis.configReqNetMagic = RequiresMagic
                      , Genesis.configUTxOConfiguration = defaultUTxOConfiguration
                      }
                  cvs =
                    CC.ChainValidationState
                      { CC.cvsLastSlot = SlotNumber 2163
                      , CC.cvsPreviousHash = Right hh
                      , CC.cvsUtxo = UTxO.fromList []
                      , CC.cvsUpdateState = upi
                      , CC.cvsDelegationState =
                          DI.State
                            (Scheduling.State Seq.empty Set.empty)
                            (Activation.State (Delegation.fromList pairs) (Map.fromList [(i, SlotNumber 0) | (i, _) <- pairs]))
                      }
               in either (Left . show) (const (Right ())) (runExcept (runReaderT (CC.updateChainBlockOrBoundary cfg cvs blk) (ValidationMode CC.BlockValidation UTxO.TxValidation)))
            _ -> Left "setup"
    -- Signers G (the header's genesis key) where gAt says, a filler otherwise,
    -- in slots 1..n.
    genesisKeyBytes =
      case decodeFullAnnotatedBytes byronProtVer "Header" (CC.decCBORAHeader (EpochSlots 21600)) (BSL.fromStrict (unhex headerHex)) of
        Right hdr -> abstractHashToBytes (unKeyHash (hashKey (CC.headerGenesisKey hdr)))
        Left _ -> BS.empty
    signer key slot = defMessage & #key .~ key & #slot .~ slot :: ByronSigner
    signersFor n gAt = [signer (if gAt i then genesisKeyBytes else BS.pack [fromIntegral (1 + i `mod` 7), 0xee] <> BS.replicate 26 0) (fromIntegral (i + 1)) | i <- [0 .. n - 1]]
    winNames signers =
      hdrFailures
        ( call
            ( #byronHeaderValidate
                .~ ( defMessage
                      & #protocolMagic .~ 1
                      & #securityParam .~ 2160
                      & #pparams .~ pparams
                      & #chainState .~ (defMessage & #delegations .~ [defMessage & #issuerVkey .~ unhex i & #delegateVkey .~ unhex dl | (i, dl) <- dlgs] & #signers .~ signers)
                      & #header .~ unhex headerHex
                   )
            )
        )
    mainnetB = ("43946", "1000000")
    minFee a b size =
      call
        ( #byronMinFee
            .~ (defMessage & #txFeeA .~ rat a & #txFeeB .~ rat b & #size .~ size)
        )
        ^. #byronMinFee
    fee a b size =
      let r = minFee a b size in if null (failures r) then Just (r ^. #fee) else Nothing
    failures r = [f ^. #name | f <- r ^. #rejection . #failures]
    pparams :: S.ByronProtocolParameters
    pparams =
      defMessage
        & #txFeeA .~ rat ("155381", "1")
        & #txFeeB .~ rat ("43946", "1000000")
        & #maxTxSize .~ 4096
        & #maxBlockSize .~ 2000000
        & #maxHeaderSize .~ 2000000
    utxoEntry =
      defMessage
        & #txin .~ (defMessage & #txId .~ BS.pack (0x11 : replicate 31 0) & #index .~ 0)
        & #txout .~ unhex "8282d818582183581cec0e23340c62b5e0d64ed01278769291f3ee83e721ea3375d8830d7ca0001aed599b891a00989680"
    runTx tx adj =
      call
        ( #byronTxValidate
            .~ ( defMessage
                  & #protocolMagic .~ 764824073
                  & #pparams .~ adj pparams
                  & #utxo .~ [utxoEntry]
                  & #tx .~ tx
               )
        )
        ^. #byronTxValidate
    txResp name adj = runTx (unhex (txHex name)) adj
    txName name = failures (txResp name id)
    txValid =
      let r = txResp "valid" id
       in ( if null (failures r) then Nothing else Just (failures r)
          , r ^. #fee
          , length (r ^. #utxoAdded)
          , length (r ^. #utxoRemoved)
          )

block3Hex, prev3Hex, payload2Hex, issuer2Hex, votesOnlyHex :: String
block3Hex = "820183850158209972ffaee13b4afcf1a133434161ce25e8ecaf34b7a76e06b0c642125cf911a984830058200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b82035820d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c5820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b58204e66280cd94d591072349bec0a3090a53aa945562efb6d08d56e53654b0e40988482001910e55840bf29db977abf045bf07a180cce9a7736858eb8de51c0351809b368a74451d239445e89a03ceae55c2c0c4ebc2b23c0dc7f3ab9ab57caff584e7eff48c0330f00810382028284005840bf29db977abf045bf07a180cce9a7736858eb8de51c0351809b368a74451d239445e89a03ceae55c2c0c4ebc2b23c0dc7f3ab9ab57caff584e7eff48c0330f005840d4dd69a41071bc2dc8e64a97f4bd6379524ce0c2b665728043a067e34d3e218af89a1e334d87220ac4c94f2bd8f0828804111c4f71985ba665698cbb5db63992584082b10bbea2dd80441315230354e3e4af0ac87d02c9e6424fc9432be21a6183c81555d3a1b373566aef7a8065a126d57c42629be780becc61bc1b8f3ce5b5170d584069720ed5efd64d7009c7bc657b2a452af3f9fb6081a4242a79907b1439d69ad2af09bbc26f8b751e305baad4f770fec1c4a73f2833edfec2b7ad714ac06e19038483000000826a63617264616e6f2d736c01a058204ba92aa320c60acc9ad7b9a64f2eda55c4d2ec28e604faf186708b4f0c4e8edf849fff8203d90102809fff82809fff81a0"
prev3Hex = "9972ffaee13b4afcf1a133434161ce25e8ecaf34b7a76e06b0c642125cf911a9"
payload2Hex = "828187830100008e8080808080808080808080808080826a63617264616e6f2d736c01a1656c696e757884582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c11131458200fd923ca5e7218c4ba3c3801c26a617ecdbfdaebb9c76ce2eca166e7855efbb8582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314a058408ef320c2df6654a6188c45e9c639c0a686bf5a865295587d399dfeb05fe74ab67961a696b268c8bcec9372bc73aa9b1365ec007b3dfa0539eac00d154a31518c58408ac677b7ff7875d6d7c174969d19eaacb5fefe11c6a44bc678b4ae06885f3906a6f94d3d18a26698f13658570be23b95e553d59bcd0939db505b4822dbf3ff049f8458408ef320c2df6654a6188c45e9c639c0a686bf5a865295587d399dfeb05fe74ab67961a696b268c8bcec9372bc73aa9b1365ec007b3dfa0539eac00d154a31518c5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584074902a37be48431a6470ea8dfebce7070d2de278854ca391d48a5f48e859ba302256221597224e4fb12d155626fa415b6696eefe80e3e43609a76a51c5cfba008458408b0960d234bda67d52432c5d1a26aca2bfb5b9a09f966d9592a7bf0c728a1ecd840eac5c3fe8a7edda024a7403b6d37705990828b3548332a3f850221c2098bf5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584092347c2c8332ed692b0efd5ad0ce53063f4a8d75de4e36b4170c6501069a2b85f07172e1bec628dc4ca6ad44a98e971db9f5b2660c2a952c199bde9fb61e400b845840d4dd69a41071bc2dc8e64a97f4bd6379524ce0c2b665728043a067e34d3e218af89a1e334d87220ac4c94f2bd8f0828804111c4f71985ba665698cbb5db639925820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f558402b18ff7dc379889c4042ab9bc66b9f0b8138802e2faa699bad1c4887ffe76151de78c52edf548d14a5191b83bd2fbc421a3736e82711650b11fceba5868d510c845840618b625df30de53895ff29e7a3770dca56c2ff066d4aa05a6971905deecef6dbb7dd10ea1f9175e5293eadec97bf16b167af379a7b3ed4af032cd07b99ecc1ea5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584008fe8e6d92d460bb65a2a49b26aa9113c708fb9ec56e6f482060e774254ea1ec0fc8f8ad77be7f1baf35ccfb9de508f4a6c63f0af59d7ae2bb745f2f40dd0e0e8458409aae625d4d15bcb3733d420e064f1cd338f386e0af049fcd42b455a69d28ad366483d177ba2b801b4136e0d6662e5e9e0a24f2c80a0e78d4c235b4c08f201f4c5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f55840000d9297b45ba4d0bc33d3aa4e0a1c87d09d5773fc15f57e0741b791c5ab1c357184089b1364873c4925d3f99eb48af9c9d0592d8c91bea1187a981e74cebd0c845840d1a8de6caa8fd9b175c59862ecdd5abcd0477b84b82a0e52faecc6b3c85100a475216ffb64ea74537021405bb328b0f706e4aad7157795e316781cca120dac965820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f558403ea4f111ce4dd159ee116c3c5aa03384cae87a0afd2ad9dbb742f6fafd03c03b2f469cf1d4718948b296dba621a564255859ab1d7854b4fb77d89fcb1e508a01845840942bb3aaab0f6442b906b65ba6ddbf7969caa662d90968926211a3d56532f11d8e8cb4adc3f5034fbc6257fa5b1086689dc7d024df7226aa501fc28eba2d1f635820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f55840b99c70fc219bf98a1822ca1a322458818baf3a2a6ff9e7dc18b3adf4f80528e29dd0fc1df6c141dd02d2dc19489edb7635755da79ddcff11742ed245c9d33f0cff"
issuer2Hex = "8b0960d234bda67d52432c5d1a26aca2bfb5b9a09f966d9592a7bf0c728a1ecd840eac5c3fe8a7edda024a7403b6d37705990828b3548332a3f850221c2098bf"
-- Block 2's payload without its proposal: an empty proposal list and its votes.
votesOnlyHex = "82809f8458408ef320c2df6654a6188c45e9c639c0a686bf5a865295587d399dfeb05fe74ab67961a696b268c8bcec9372bc73aa9b1365ec007b3dfa0539eac00d154a31518c5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584074902a37be48431a6470ea8dfebce7070d2de278854ca391d48a5f48e859ba302256221597224e4fb12d155626fa415b6696eefe80e3e43609a76a51c5cfba008458408b0960d234bda67d52432c5d1a26aca2bfb5b9a09f966d9592a7bf0c728a1ecd840eac5c3fe8a7edda024a7403b6d37705990828b3548332a3f850221c2098bf5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584092347c2c8332ed692b0efd5ad0ce53063f4a8d75de4e36b4170c6501069a2b85f07172e1bec628dc4ca6ad44a98e971db9f5b2660c2a952c199bde9fb61e400b845840d4dd69a41071bc2dc8e64a97f4bd6379524ce0c2b665728043a067e34d3e218af89a1e334d87220ac4c94f2bd8f0828804111c4f71985ba665698cbb5db639925820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f558402b18ff7dc379889c4042ab9bc66b9f0b8138802e2faa699bad1c4887ffe76151de78c52edf548d14a5191b83bd2fbc421a3736e82711650b11fceba5868d510c845840618b625df30de53895ff29e7a3770dca56c2ff066d4aa05a6971905deecef6dbb7dd10ea1f9175e5293eadec97bf16b167af379a7b3ed4af032cd07b99ecc1ea5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f5584008fe8e6d92d460bb65a2a49b26aa9113c708fb9ec56e6f482060e774254ea1ec0fc8f8ad77be7f1baf35ccfb9de508f4a6c63f0af59d7ae2bb745f2f40dd0e0e8458409aae625d4d15bcb3733d420e064f1cd338f386e0af049fcd42b455a69d28ad366483d177ba2b801b4136e0d6662e5e9e0a24f2c80a0e78d4c235b4c08f201f4c5820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f55840000d9297b45ba4d0bc33d3aa4e0a1c87d09d5773fc15f57e0741b791c5ab1c357184089b1364873c4925d3f99eb48af9c9d0592d8c91bea1187a981e74cebd0c845840d1a8de6caa8fd9b175c59862ecdd5abcd0477b84b82a0e52faecc6b3c85100a475216ffb64ea74537021405bb328b0f706e4aad7157795e316781cca120dac965820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f558403ea4f111ce4dd159ee116c3c5aa03384cae87a0afd2ad9dbb742f6fafd03c03b2f469cf1d4718948b296dba621a564255859ab1d7854b4fb77d89fcb1e508a01845840942bb3aaab0f6442b906b65ba6ddbf7969caa662d90968926211a3d56532f11d8e8cb4adc3f5034fbc6257fa5b1086689dc7d024df7226aa501fc28eba2d1f635820561a907d9d313c7a3b769db775eb9f2a00af344952d8ae906a5f4e363d6ed3e2f55840b99c70fc219bf98a1822ca1a322458818baf3a2a6ff9e7dc18b3adf4f80528e29dd0fc1df6c141dd02d2dc19489edb7635755da79ddcff11742ed245c9d33f0cff"

unhex :: String -> BS.ByteString
unhex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []

txHex :: String -> String
txHex name = case name of
  "valid" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840d2c7222bf8a3587ffd93f1197e34a1e3bf1b9a5bd6f6c7ed81e6a764defc10b7a32eedff530f90c87daa928882d5880bcc77355ca11b1e1ee8acc76a1245a206"
  "fee" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00989298ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840ded369471ebe409d1c6b4bb8dd7943ffa5f4b364599b380864a736f120fe56c28f253d63ce47c43ba31f3b1cca01e15fad52c50fb686f68b7e1148ab38077203"
  "exceed" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00989681ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840ee17dfbe0df1eb3d0abcc5506e7c9105c879f5181bf3d64c4bb28fae61a7426cba51c6794d0fdc9ba89f96f19c6bac381d8fdd6c1a9c595f92435d537b1de30b"
  "badsig" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b584012b5036022288e1e752761cd8db9bafe1d27a436c6c579503c368d2206736f60fa2506f97e88890f754bf439b0c1cfcf3b9e5e9ee676111cb7a37cb9ed2fcb0d"
  "wrongkey" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b394585858585858585858585858585858585858585858585858585858585858585858409aedfda9055a7ce6c070d5e1d7ee51399f11a1507208285b444573eb1d2124f1538bbcc24295da770a0d937117b5733f8a8bb88fcd7c066e933817020e4c1003"
  "network" -> "82839f8200d8185824825820110000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582583581cb48bb6d3e2be6deda4c4cdbe535f406bdb0129f072ba16a0fb1cb30aa10242182a001a7a66285c1a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5840864ab158f3f635d8cc9053a60825617f2e22274010701cc993eb269d87555de1ce9d96656c90b92d037a359ea0b9d549690ebc80d78a90454abded2a8e35e601"
  _ -> "82839f8200d8185824825820990000000000000000000000000000000000000000000000000000000000000000ff9f8282d818582183581c24f0b0bee33829cb726dc6002c3873cc2029cc74304299b8c111fbf4a0001a4ab2a4291a00895440ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b58407c34fd86ace87e2ca05e7e89695228fe430f929ac086a0afd2333a4412e36397dbd5faf027c74ce17188a47728e9a2fca37bbc3e07f3aa9a70ae2d8caf492b06"

-- Preprod Byron main block 3 (header and a copy with one previous-hash bit
-- flipped) and the preprod genesis delegations, as (issuer, delegate) xpubs.
headerHex, badHeaderHex :: String
headerHex = "85015820f3d7cd6f93cb4c59b61b28ac974f4a4dccfc44a4c83c1998aad17bb6b7b0344684830058200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b82035820d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c5820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b58204e66280cd94d591072349bec0a3090a53aa945562efb6d08d56e53654b0e4098848200191950584063e55a8f421a31eab4fa85a342be61884978124f9c5ac2aaee6b9f4cee30b9ed91e80e80325a840c857bbd8b1ddcd656a261b90c6730480c3612fd4ccf6e8b2081048202828400584063e55a8f421a31eab4fa85a342be61884978124f9c5ac2aaee6b9f4cee30b9ed91e80e80325a840c857bbd8b1ddcd656a261b90c6730480c3612fd4ccf6e8b205840618b625df30de53895ff29e7a3770dca56c2ff066d4aa05a6971905deecef6dbb7dd10ea1f9175e5293eadec97bf16b167af379a7b3ed4af032cd07b99ecc1ea5840ca38e688e759183b57d08ca6568248b91c8ddd81e56d0d31c3f3a26eca35754387c5f301ef017dafae453a93757d39a0e2d326ebe59cf31d5c1b22fc8616ac005840c714da33d72c7a69029402283ae5e76be27b3e87072ac207256543749e0616fc465699ede3b9d78010605469a533c886be8c8b73d966fb9f1a4ceea950fa7e0d8483010000826a63617264616e6f2d736c01a058204ba92aa320c60acc9ad7b9a64f2eda55c4d2ec28e604faf186708b4f0c4e8edf"
badHeaderHex = "85015820f3d7cd6f93ca4c59b61b28ac974f4a4dccfc44a4c83c1998aad17bb6b7b0344684830058200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b82035820d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c5820afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b58204e66280cd94d591072349bec0a3090a53aa945562efb6d08d56e53654b0e4098848200191950584063e55a8f421a31eab4fa85a342be61884978124f9c5ac2aaee6b9f4cee30b9ed91e80e80325a840c857bbd8b1ddcd656a261b90c6730480c3612fd4ccf6e8b2081048202828400584063e55a8f421a31eab4fa85a342be61884978124f9c5ac2aaee6b9f4cee30b9ed91e80e80325a840c857bbd8b1ddcd656a261b90c6730480c3612fd4ccf6e8b205840618b625df30de53895ff29e7a3770dca56c2ff066d4aa05a6971905deecef6dbb7dd10ea1f9175e5293eadec97bf16b167af379a7b3ed4af032cd07b99ecc1ea5840ca38e688e759183b57d08ca6568248b91c8ddd81e56d0d31c3f3a26eca35754387c5f301ef017dafae453a93757d39a0e2d326ebe59cf31d5c1b22fc8616ac005840c714da33d72c7a69029402283ae5e76be27b3e87072ac207256543749e0616fc465699ede3b9d78010605469a533c886be8c8b73d966fb9f1a4ceea950fa7e0d8483010000826a63617264616e6f2d736c01a058204ba92aa320c60acc9ad7b9a64f2eda55c4d2ec28e604faf186708b4f0c4e8edf"

dlgs :: [(String, String)]
dlgs =
  [ ("505b237a1cb140020cc442f6e51c6a0f48ad3344427735a9ad5b755bece8a4ce5bed4f6bbbd00caffcdff6502359a16534ea5182badbcfbaf9b516edf305264a", "8b0960d234bda67d52432c5d1a26aca2bfb5b9a09f966d9592a7bf0c728a1ecd840eac5c3fe8a7edda024a7403b6d37705990828b3548332a3f850221c2098bf")
  , ("a4020008c1ba5826b89b3d2d56879e90b5d8b9691dc64feb803b99ff1eee821dc2bc4ba1a467f62d14515a295a19dada1f5b026e7b8c0a6007ac14db3c3166c4", "d1a8de6caa8fd9b175c59862ecdd5abcd0477b84b82a0e52faecc6b3c85100a475216ffb64ea74537021405bb328b0f706e4aad7157795e316781cca120dac96")
  , ("3fb2637923c78dec4d5b932c09ac92ad445aa95da00a05edb447b40a07c59d5645187692f40e31114ad0f375532cf8e4f875740e2d7d0e343e0e14b533a31fec", "8ef320c2df6654a6188c45e9c639c0a686bf5a865295587d399dfeb05fe74ab67961a696b268c8bcec9372bc73aa9b1365ec007b3dfa0539eac00d154a31518c")
  , ("5eba3a05b57a84c877453667b2de00061b50dafafcdd83d7a0b7d0f0959eba7bef72eb9d18142f2deab055f197ac15a830e38aae8155e3cca07d212adb185110", "9aae625d4d15bcb3733d420e064f1cd338f386e0af049fcd42b455a69d28ad366483d177ba2b801b4136e0d6662e5e9e0a24f2c80a0e78d4c235b4c08f201f4c")
  , ("7aa000b6ed57efd3310926bf8bf4520292c6b2784362a2c92eef777f7c12cd1e7f33b71a9594bc0c716b02d260cd86e483d9f0285d47410f1b3bfd4b389f4ee3", "942bb3aaab0f6442b906b65ba6ddbf7969caa662d90968926211a3d56532f11d8e8cb4adc3f5034fbc6257fa5b1086689dc7d024df7226aa501fc28eba2d1f63")
  , ("63e55a8f421a31eab4fa85a342be61884978124f9c5ac2aaee6b9f4cee30b9ed91e80e80325a840c857bbd8b1ddcd656a261b90c6730480c3612fd4ccf6e8b20", "618b625df30de53895ff29e7a3770dca56c2ff066d4aa05a6971905deecef6dbb7dd10ea1f9175e5293eadec97bf16b167af379a7b3ed4af032cd07b99ecc1ea")
  , ("bf29db977abf045bf07a180cce9a7736858eb8de51c0351809b368a74451d239445e89a03ceae55c2c0c4ebc2b23c0dc7f3ab9ab57caff584e7eff48c0330f00", "d4dd69a41071bc2dc8e64a97f4bd6379524ce0c2b665728043a067e34d3e218af89a1e334d87220ac4c94f2bd8f0828804111c4f71985ba665698cbb5db63992")
  ]
