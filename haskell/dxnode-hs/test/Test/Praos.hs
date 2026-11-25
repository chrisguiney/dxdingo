{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Praos (tests) where

import Cardano.Crypto.Hash (Blake2b_256, hashFromBytes, hashToBytes, hashWith)
import Cardano.Ledger.BaseTypes (Nonce (..), (⭒))
import qualified Data.ByteString as BS
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..), Reply)
import Proto.Dxdingo.V1.Praos (EnvelopeTip, OverlaySlotKind (..))
import qualified Proto.Dxdingo.V1.State as S
import Proto.Dxdingo.V1.Types (Era (..), Rejection)
import Test.Util

tests :: [Check]
tests =
  [ ("leader check: praos lowest value leads", leader ERA_BABBAGE (BS.replicate 32 0) ^. #praosLeaderCheck . #isLeader)
  , ("leader check: praos highest value does not", not (leader ERA_BABBAGE (BS.replicate 32 255) ^. #praosLeaderCheck . #isLeader))
  , ("leader check: tpraos lowest value leads", leader ERA_SHELLEY (BS.replicate 64 0) ^. #praosLeaderCheck . #isLeader)
  , ("leader check: 64-byte praos value is UNSUPPORTED", leader ERA_BABBAGE (BS.replicate 64 0) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("nonce epoch: candidate only", nonceOf a "" "" == a)
  , ("nonce epoch: neutral candidate returns the lab", nonceOf "" b "" == b)
  , ("nonce epoch: lab and extra entropy mix the same way", nonceOf a b "" == nonceOf a "" b && BS.length (nonceOf a b "") == 32)
  , ( "overlay: active slot names genesis key 0"
    , overlay 1000 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_ACTIVE
        && overlay 1000 ^. #tpraosOverlaySchedule . #genesisKeyHash == BS.replicate 28 1
    )
  , ("overlay: praos slot", overlay 1001 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_PRAOS)
  , ("overlay: non-active slot", overlay 1002 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_NON_ACTIVE)
  , ("overlay: slot before epoch is UNSUPPORTED", overlay 999 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , -- Divergence confirmations call the reference directly, bypassing
    -- dxnode-hs, so a reported mismatch cannot come from request
    -- translation. praos.nonce-epoch, neutral candidate: dingo encodes it as
    -- 32 zero bytes and hashes it when a lab is present
    -- (dxnode-go TestDivergenceNeutralCandidateIsHashed).
    -- praos.nonce-rolling and praos.nonce-candidate, neutral evolving nonce:
    -- the reference returns the contribution, gouroboros hashes it
    -- (dxnode-go TestDivergenceNeutralEvolvingIsHashed).
    ( "divergence: a neutral evolving nonce yields the contribution itself"
    , neutralRolling == b2b (b2b ("N" <> vrfOut))
    )
  , ("divergence: NeutralNonce is the identity of the nonce operator", (NeutralNonce ⭒ labN) ⭒ NeutralNonce == labN)
  , ("divergence: a real all-zero nonce is not neutral in the reference", zeroN /= NeutralNonce && (zeroN ⭒ labN) /= labN)
  , -- Real mainnet headers (gouroboros test data). conway_a: header 859
    -- bytes, declared body 50666, counter 10, epoch nonce known. babbage_m8:
    -- slot 129124527, counter 18, KES start period 975, so evolution 21 at
    -- 129600 slots per period.
    ("envelope: at both bounds", rejName (envelope 859 50666) == "")
  , ("envelope: header one over", rejName (envelope 858 90112) == "HeaderSizeTooLarge")
  , ("envelope: body one over", rejName (envelope 1100 50665) == "BlockSizeTooLarge")
  , ("envelope: header is checked first", rejName (envelope 858 50665) == "HeaderSizeTooLarge")
  , ("envelope: truncated header is DeserialiseFailure", rejName (envelopeOf (BS.take 100 conwayA) 1100 90112) == "DeserialiseFailure")
  , ("envelope: header bound above Word16 is UNSUPPORTED", errCode (envelope 65536 1) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("envelope: byron is UNSUPPORTED", errCode (call (#praosHeaderEnvelope .~ (defMessage & #era .~ ERA_BYRON & #header .~ conwayA & #pparams .~ pp 1100 90112))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("vrf check: valid header, leader and nonce values", vrfOk && vrfRes ^. #praosVrfCheck . #leaderValue == unhex leaderHex && vrfRes ^. #praosVrfCheck . #nonceValue == unhex nonceHex)
  , ("vrf check: wrong epoch nonce is VRFKeyBadProof", vrfRej (vrf (BS.replicate 32 1) (unhex vrfKeyHashHex)) == "VRFKeyBadProof")
  , ("vrf check: wrong key hash is VRFKeyWrongVRFKey", vrfRej (vrf (unhex eta0Hex) (BS.replicate 32 2)) == "VRFKeyWrongVRFKey")
  , ("vrf check: key hash is checked before the proof", vrfRej (vrf (BS.replicate 32 1) (BS.replicate 32 2)) == "VRFKeyWrongVRFKey")
  , ("kes check: valid header, evolution 21", rejName' (kes babbageM8 129600 62) == "" && kes babbageM8 129600 62 ^. #praosKesCheck . #kesPeriod == 21)
  , ("kes check: evolution below the maximum", rejName' (kes babbageM8 129600 22) == "")
  , ("kes check: evolution equal to the maximum", rejName' (kes babbageM8 129600 21) == "KESAfterEndOCERT")
  , ("kes check: period before the start", rejName' (kes babbageM8 135000 62) == "KESBeforeStartOCERT")
  , ("kes check: tampered cold signature", rejName' (kes (flipAt coldIdx babbageM8) 129600 62) == "InvalidSignatureOCERT")
  , ("kes check: tampered KES signature", rejName' (kes (flipAt (BS.length babbageM8 - 1) babbageM8) 129600 62) == "InvalidKesSignatureOCERT")
  , ("kes check: wrong evolution", rejName' (kes babbageM8 129000 62) == "InvalidKesSignatureOCERT")
  , ("kes check: the window is checked before the cold signature", rejName' (kes (flipAt coldIdx babbageM8) 129600 21) == "KESAfterEndOCERT")
  , ("kes check: the cold signature is checked before the KES signature", rejName' (kes (flipAt (BS.length babbageM8 - 1) (flipAt coldIdx babbageM8)) 129600 62) == "InvalidSignatureOCERT")
  , ("kes check: zero slots per period is UNSUPPORTED", errCode (call (#praosKesCheck .~ (defMessage & #era .~ ERA_BABBAGE & #header .~ babbageM8 & #maxKesEvolutions .~ 62))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("opcert: same counter", opName (Just 10) True == ("", 10))
  , ("opcert: next counter", opName (Just 9) False == ("", 10))
  , ("opcert: skips one", opName (Just 8) True == ("CounterOverIncrementedOCERT", 0))
  , ("opcert: lower than last", opName (Just 11) True == ("CounterTooSmallOCERT", 0))
  , ("opcert: unseen registered pool counts from zero", opName Nothing True == ("CounterOverIncrementedOCERT", 0))
  , ("opcert: unseen unregistered pool", opName Nothing False == ("NoCounterForKeyHashOCERT", 0))
  , ("opcert: a recorded counter ignores the registered flag", opName (Just 10) False == ("", 10))
  , ("opcert: TPraos protocol version is UNSUPPORTED", errCode (call (#praosOpcertCheck .~ (defMessage & #protocolVersion .~ (defMessage & #major .~ 6) & #header .~ conwayA))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("vrf key registered: pool and key hash match", keyReg [(unhex poolHex, unhex vrfKeyHashHex)] ^. #praosVrfKeyRegistered . #poolId == unhex poolHex && rejName'' (keyReg [(unhex poolHex, unhex vrfKeyHashHex)]) == "")
  , ("vrf key registered: unknown pool", rejName'' (keyReg [(BS.replicate 28 9, unhex vrfKeyHashHex)]) == "VRFKeyUnknown")
  , ("vrf key registered: empty distribution", rejName'' (keyReg []) == "VRFKeyUnknown")
  , ("vrf key registered: wrong key hash", rejName'' (keyReg [(unhex poolHex, BS.replicate 32 1)]) == "VRFKeyWrongVRFKey")
  , ("nonce rolling: tpraos hashes the VRF output once", all (\e -> rolling e == b2b (ev <> b2b vrfOut)) [ERA_SHELLEY, ERA_ALLEGRA, ERA_MARY, ERA_ALONZO])
  , ("nonce rolling: praos range-extends it", all (\e -> rolling e == b2b (ev <> b2b (b2b ("N" <> vrfOut)))) [ERA_BABBAGE, ERA_CONWAY])
  , ("nonce rolling: byron is UNSUPPORTED", errCode (call (#praosNonceRolling .~ (defMessage & #era .~ ERA_BYRON & #evolving . #hash .~ ev & #vrfOutput .~ vrfOut))) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("nonce candidate: no blocks", cand ERA_SHELLEY 0 == (ev0, cand0))
  , ("nonce candidate: tpraos window 60, 1039 moves it and 1040 freezes it", cand ERA_SHELLEY 4 == (etp !! 3, etp !! 1))
  , ("nonce candidate: tpraos one block", cand ERA_ALONZO 1 == (head etp, head etp))
  , ("nonce candidate: babbage keeps 3k/f", cand ERA_BABBAGE 4 == (epr !! 3, epr !! 1))
  , ("nonce candidate: conway uses 4k/f, cutoff 1020", cand ERA_CONWAY 4 == (epr !! 3, head epr))
  , ("nonce candidate: conway before the cutoff", cand ERA_CONWAY 1 == (head epr, head epr))
  , ("nonce candidate: slot outside the epoch is UNSUPPORTED", errCode (call (#praosNonceCandidate .~ candReq ERA_CONWAY [(1100, BS.replicate 64 1)])) == HARNESS_ERROR_CODE_UNSUPPORTED)
  ]
    ++ concatMap tpraosChecks tpraosHeaders
    ++ overlayChecks
    ++ positionChecks
  where
    a = BS.replicate 32 0xaa
    b = BS.replicate 32 0xbb
    leader era v =
      call (#praosLeaderCheck .~ (defMessage & #era .~ era & #leaderValue .~ v
        & #poolStake .~ 1 & #totalStake .~ 2 & #activeSlotCoeff .~ rat ("1", "20")))
    nonceOf c l e =
      call (#praosNonceEpoch .~ (defMessage & #era .~ ERA_SHELLEY
        & #candidate . #hash .~ c & #previousLab . #hash .~ l & #extraEntropy . #hash .~ e))
        ^. #praosNonceEpoch . #epochNonce . #hash
    keys = [defMessage & #genesisKeyHash .~ BS.replicate 28 i | i <- [1 .. 7]]
    overlay slot =
      call (#tpraosOverlaySchedule .~ (defMessage & #slot .~ slot & #epochFirstSlot .~ 1000
        & #d .~ rat ("1", "2") & #activeSlotCoeff .~ rat ("1", "20") & #genesisDelegations .~ keys))
    nonceFrom = maybe NeutralNonce Nonce . hashFromBytes
    labN = nonceFrom (BS.replicate 32 0xbb)
    zeroN = nonceFrom (BS.replicate 32 0)
    conwayA = unhex "828a1a00a60faf1a0817580c58204eac1e7264c0e80436b04687e75d46d6a0d6b2338c2abb73a14fafbd689f69b2582012209e0b93f0128f670c9a02781c5466c4c4be003da3a51344b6a94f709ce51f58209c1a5fc5dec0a4b822d5a3b254ce9b168299479127aadcf97506ef257517fff682584023c2d70c24c44041644f5152f7e8a1bb580e516eb8e73c7df287116adb5f009c0c001feccfeebdf34c2275d1fce859c6c46182631b6306d5fd2724ac7ab1c6be58500dbe31ef7c00c34b6522e983d223e05075359cb170668d960b8cebfced178287ee6ca5cfc6e8e60aec97fd197aebfefc24aae695680631d575c6dacdfd9efc5687e46eb2a5c04a755c7f260af9ef830819c5ea5820d2b74b6333637801f2e9c7265792d5b8fc1647f9056d67c769dbac27f25f2fd08458200946347d22a3b6da29d79102424973c932b898808ff2436fa138df102484230a0a1904165840c75619c3ebad0758349eb1dedc154a8cd280d8189d6da973b4a147b0cdb0f60442d493feeba64167a05b5fc40bc695192bf1c08afad3c07ebd33cb5925f378018209015901c00a8442332bd3f33a4d78fe2736a75110b528a1e7501bc7887910d1475fc0e425f49a84f94e98f87047916cf622f3db1f61b60c5f06709769f98c4cc67de8f50c320c6772b647ac9916765b6985d4eafccb54e71064d01df41f8d0638ed5cd62b7b6e49ba15dd87cc687ab87d3fb22490d355e8fa9c5f7c24ed88b800fcc4cb1f1b54e65b5ba82c442f4643caadc86583072b8b6956f4f9a4530c29873f7231605efd7a7f961a863530512ef86b50f9b1004748c31fa07978f2ece7d8e76ffde67d713015824b28e19f05f0383c2def3cdeb67247f33f5eae329c38a375b2eb06a586dcc2e102a776a6deaad1741f2a7f5aa604074698e876afab4455278fd84a1db5768078e2848cc85e3c8a0b48630a2622832ecd2dbb3c505df2a70b93b49ce99616f601e5e2004a8ce8926319c23f2a26ac8550cb1c05c9d2d25fc5fcd122fc35b057a71d6e961250c99b19a7bfd9acdc60a8151d6c81ef2d7d69a62fd0f17d184dd753cce9a2e9c32b53baf317e31c6c5e3cf8ea8b203b413ae8b0253db53d0cbe19b0f0547a0e67d3591d1cade6ceb4a47779ba4a09e7526280acb62200f42c98f6185ea9da3daf47aa3d10ffe5307331fa3430af6c6361154943c39375"
    babbageM8 = unhex "828a1a00a11c251a07b248af582012e0d55c96d9c197fa3cc265ba81df9b9bd435d32e839b436fbec9623799145558206c6aeb4c58918e71403d8a9bee01e39ac8c8c4ffc52bf58b46beb7a7d121a05b5820e44615156d5140f59b218cbcc64ed195c23f0ddc3d80e4f63541f96082e80871825840f29a8c0373c08f8e19adbd02d8179560a32d3e7de1985499c9e250379c6f5b46db84e875d703f7239c786cad5807ca6ad16c8d08f92360a28dfd9e354fe1b13b58509f99f42454b9cdde9f5559560f9baa7a5a6f79f97659ec56e6d81ddd4e5c3e64e27ca2e0804b2456c0b59050b6d994fb60b1f8c0518c031d1f5c36b5c5e08db18afb055fe6d41252b7b0b42510d8b50c1935f658202e293f1d9aed556b51dd1eee374e3899124c8a3f9b88bd3d9c5a1e3d3b5960778458206e31b612cbbbdc6e539ebb175835424b4fa608c4937ea42c761ef7e84f25c5a1121903cf584091bafbbc5b280c96d1bc2b71cfaee13be1146c2b980ed2851bdf3f2dbb880a3efd7fd3ba0c982c0a9e27b8552a7540108e4c8f239807c44b956e1d978b93130b8208005901c034ad8775bc3af6c8fbcec1e704dc53439de0c0f86cccd79d84fd7788bf009d976960b8b291bca6e250ef1c8cd3f8c8c064c94afe039eb006e5247b8288a7340911674523ab058fded91b667a21d8b1cb1a4c2305b9f80ff03ed7ea036b87ec3185356bcd8e425811d88265ef5ec42d4aeaa526772a0e57bddf202008df47374cc144b1f08b688b27884385ca2a7d44e52e7ff96b9589ed3238eec73de6718023127e84ede20c9092c592dfaa735d64cde007416c58db0afafb311857cdb7103ebce8d311c7c2a5f52151b1a2c07fc8319f51bf3b9f2953030520b064c369518ade6696519d2ca7865483e29aa07b8cf8fa838c965b1b9d5ee55e5f25927d2f29a294a8f578045a34d71e8cbede10ca7805e9c21dd5889d05a4306a026691922803a869d6ac214698fd0778c37a383766e2000a67341ae11e0e21506f65fd1b8782472b2923318b0d466240de0d0292a08e6770c859679049e89527dfe468a5878bebee1e1dc1b4bd763f90f76cf3b1847f9a172f42b9cf81959f23bc5fe530ff573778ca1ead972c51d2f7bf1fbd01e2471533ef85f5404d64648fcfb86a118b91faee8d922eb30436a1d02d1ff106f64ef3205990d919e9cfed53f610321f87"
    eta0Hex :: String
    eta0Hex = "4ef95a10f639d0cf16bb963c3a580d4bf2a95b6ae7848702665884843e3c661d"
    poolHex :: String
    poolHex = "db49cc2df6435f7c4a53cf4a52ef3469801c847521dbcac379427716"
    vrfKeyHashHex :: String
    vrfKeyHashHex = "e73e21246d4fffd38d24b4e77b643aeae35a700763fc1106d910293ec054aa12"
    leaderHex :: String
    leaderHex = "0000a8e701b9d8472fb979cbc5190fd93c9a5c7a4bc44963ef94cb02c35eeb90"
    nonceHex :: String
    nonceHex = "73018c6dec48e9eeefdcb6e989f3128321af0a67702bfe523d92222ac3f9bde6"
    coldIdx = 341 :: Int
    pp :: Word32 -> Word32 -> S.ProtocolParameters
    pp hs bs = defMessage & #maybe'maxBlockHeaderSize .~ Just hs & #maybe'maxBlockBodySize .~ Just bs
    envelopeOf hdr hs bs = call (#praosHeaderEnvelope .~ (defMessage & #era .~ ERA_CONWAY & #header .~ hdr & #pparams .~ pp hs bs))
    envelope hs bs = envelopeOf conwayA hs bs
    rejName r = failName (r ^. #praosHeaderEnvelope . #rejection)
    rejName' r = failName (r ^. #praosKesCheck . #rejection)
    vrfRej r = failName (r ^. #praosVrfCheck . #rejection)
    rejName'' r = failName (r ^. #praosVrfKeyRegistered . #rejection)
    failName rej = case rej ^. #failures of
      [f] -> f ^. #name
      _ -> ""
    vrf nonce keyHash =
      call (#praosVrfCheck .~ (defMessage & #era .~ ERA_CONWAY & #header .~ conwayA & #epochNonce . #hash .~ nonce & #poolVrfKeyHash .~ keyHash))
    vrfRes = vrf (unhex eta0Hex) (unhex vrfKeyHashHex)
    vrfOk = failName (vrfRes ^. #praosVrfCheck . #rejection) == ""
    kes hdr spkp maxEvo =
      call (#praosKesCheck .~ (defMessage & #era .~ ERA_BABBAGE & #header .~ hdr & #slotsPerKesPeriod .~ spkp & #maxKesEvolutions .~ maxEvo))
    opName last' reg =
      let r = call (#praosOpcertCheck .~ (defMessage & #protocolVersion .~ (defMessage & #major .~ 10) & #header .~ conwayA
                & #maybe'lastCounter .~ (last' :: Maybe Word64) & #poolRegistered .~ reg & #slotsPerKesPeriod .~ 129600 & #maxKesEvolutions .~ 62))
          resp = r ^. #praosOpcertCheck
       in (failName (resp ^. #rejection), resp ^. #newCounter)
    keyReg dist =
      call (#praosVrfKeyRegistered .~ (defMessage & #header .~ conwayA
        & #poolDistribution .~ [defMessage & #poolId .~ pid & #vrfKeyHash .~ vh | (pid, vh) <- dist]))
    b2b bs = hashToBytes (hashWith @Blake2b_256 id bs)
    ev = BS.replicate 32 0xaa
    vrfOut = BS.replicate 64 0x11
    rolling e =
      call (#praosNonceRolling .~ (defMessage & #era .~ e & #evolving . #hash .~ ev & #vrfOutput .~ vrfOut))
        ^. #praosNonceRolling . #evolving . #hash
    neutralRolling =
      call (#praosNonceRolling .~ (defMessage & #era .~ ERA_CONWAY & #vrfOutput .~ vrfOut))
        ^. #praosNonceRolling . #evolving . #hash
    ev0 = BS.replicate 32 0xaa
    cand0 = BS.replicate 32 0xcc
    outs = [BS.replicate 64 (fromIntegral i) | i <- [1 .. 4 :: Int]]
    slots = [1010, 1039, 1040, 1099] :: [Word64]
    evolve contrib = tail (scanl (\prev o -> b2b (prev <> contrib o)) ev0 outs)
    etp = evolve b2b
    epr = evolve (\o -> b2b (b2b ("N" <> o)))
    candReq e blocks =
      defMessage & #era .~ e & #evolving . #hash .~ ev0 & #candidate . #hash .~ cand0
        & #epochFirstSlot .~ 1000 & #nextEpochFirstSlot .~ 1100 & #securityParam .~ 10
        & #activeSlotCoeff .~ rat ("1", "2")
        & #blocks .~ [defMessage & #slot .~ sl & #vrfOutput .~ o | (sl, o) <- blocks]
    cand e n =
      let r = call (#praosNonceCandidate .~ candReq e (take n (zip slots outs))) ^. #praosNonceCandidate
       in (r ^. #evolving . #hash, r ^. #candidate . #hash)

-- | A TPraos mainnet header and the facts the checks derive from it.
data TP
  = TP String Era Word32 BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString Word64 Word64 Word32 Word32 Word64 Word64 Int Int Int

-- | The TPraos mainnet headers of the gouroboros test data: era, protocol
-- major version, header, epoch nonce, pool id, VRF key hash, previous hash,
-- leader value, nonce value, KES evolution at 129600 slots per period, counter,
-- body size, header size, block number, slot, and the byte offsets of the nonce
-- proof, the leader proof and the cold signature.
tpraosHeaders :: [TP]
tpraosHeaders =
  [ TP "shelley" ERA_SHELLEY 2 (unhex "828f1a004d4a6f1a00f6892c5820bc4766a289bb5d8ec86647a0aeed5dc3521f43db7a72abef3eed1debbfa9412f5820ddba672a2abc65da47537df8f190ba376512bc0283e731e3934b52f1a0abc1a558206abad5392188997c5e14b9f6e581129ec141cec17d1d2f0cfc1e9192d464716e825840df9856406b92387c9b138ef24a58d8c48a37bf5be8721eaec4702ca98df48edd2678f0a8c722982d354582b5da65d849f72077c880778982f71de60536c3ed4a5850edc5d9f0ea6965edf729548ff7cba2cea82425ceb4e42d1074a726cc9812eb5fcabb4aefa7f049bcc5209edbbe18763a080eb1a0bdfd8fb933a1763813dd3314c229847a880116bee73c97e24130f1058258400009fa6d8e070a89d1ce903f49868682ff8dec52a0756ef60cb99226b36ab781fa8e0fa296b286b964d9cac56517071501716b752dc11636a5f9b8c29193012d58505a32d1a3ce3a8a89f17c73aedbab2c299a52f90fd78adf78fe6b5ac8cfded66ebd6d2fb238a770b6d0f022954f59855138c8ec47dc1021b8b69dabf26592076ffd612d04bfbc8fbcfdaade7d7eda670f19089958204847b0787633b18df8724c920c7cc35172531f2756fe4d6ab17489e019c2c4f358201ee99223a40caf33ca63758235af29c1d53066a423249f5690c8f9e664924dcf00186158404e4b58717c4f894e34c41d5b3d68de6676f78d9a5d719a077e282974cb136bcc6287d3fbf76e175a76edce3eb934afcf7005f5a7cc9204adf14fa9d01c123a0902005901c09b1bb23288fa4ba5f8ad23d00d0d945e89958057c17df6557a37a331867a6ef252a7893bfcf7006e32a614a1ff9787e7af3575cfdeb5054214ded324fa850b090f292bd54599e88bd4efe12b49d520eb7a8e57270dbd532dc567fdb8e5cfc386e1e28c1812734dfcde9dede86845d8e66966089f47b410225886e125942b445f912d70e517d175e9ee6f4de13f0aadf3a16e01911875df1710b49ea063d17228297c9eabf6cf663e7f5f5e58f6e86bfe7117381070c86b42981bcc7d405a49108fdd760f61815e4957ad256136706e94587ccac9ded8f9459b9433d0b6d06df5e3a821a715ee5fd912bb63d26aa1365ddf0dbff435ab42252cb2e9549f41bda074201f824cd48b6dbc8c9d79961b9c7f0cab85caa2bedef282c585ee0cad01b771a48f6740e0115acc27b4a8bb998b0f05de1cfef0b2507cd2f4d65120389fb88513e03c8a1709628faf3e48bb3956de316198ee06d45ce84b218632dd53f3ab0087681c90287c14c37ad76b17bbbeb68bb4924363987355859d6b6098418b2117b01bab13a786555d4e996dff6e1510f2e11070aa9223a229f69f5fd503de22b09052cb6e4f371f8a8858258df1cc5ebde829a8befce981f4fbaae621f8917e") (unhex "829749cb2701843214ae3aee67ae12ec9bdb3502e060ac0b75275d0f52af349c") (unhex "021e8d5d8acd1eecb2c0946a87e09daa4bde370be90413b80eb996d9") (unhex "1832401dbd892da8f6282d8fe52604abc7381aef56a3e4e763f97c036e222ce0") (unhex "bc4766a289bb5d8ec86647a0aeed5dc3521f43db7a72abef3eed1debbfa9412f") (unhex "0009fa6d8e070a89d1ce903f49868682ff8dec52a0756ef60cb99226b36ab781fa8e0fa296b286b964d9cac56517071501716b752dc11636a5f9b8c29193012d") (unhex "9e740e91a348d79d6f5fc88e6c4929ea9d19d38e118cda16f90b90ef79fcdb86") 27 0 2201 1005 5065327 16156972 183 332 488
  , TP "allegra" ERA_ALLEGRA 3 (unhex "828f1a005280141a015fff9d58206c75ffd5efb79a234d49e9e279be21f7f9b73a5b9db6cd5271f5db00bb8cc1085820e35049a00c155dc46c09d1c2838cda7b8b2bc68f5134fd4be528fc388363d43658200ca8bfb99d223616e305f5839cff11a9c6dc637d2d9a3cda6531be1a842259a7825840bf1d6983a8e3fd881f8ae17fa168957e9fdc1d0d26acc13b2afdf3ff4f212f16f4be0bbf3a6ab4c1ffaff62e8ff4ac76ae3cccb845a73b4460617aa9bc4e4c555850aae23d05e6e52e9241ce52db8f1274a4cbbff19f1d9f854cc072701bb8c640f6fa25ef8132e2e721380051a7ee0cdb91fe9b9cbd30f28f278964462150c328984a88f4a2d3775518b9d106d91484580882584000048a1dfc14dfb36fe05a78fd7d4b15f91ca0c05b79977170ee616147e8d113e42fca6b97816ccb93af5339255d3beb04d2b7767d80e31306018e06bbe406cd58507c6e72d25832f5a30052f1a48fc188624d8b501a2b4178169b59999edfb14b9e50fed1ed8b76ef90d489a9ad4ba6afca79e0c8717e336fc0c3e5e29ddecbed3cb067d74868abc24763be13d1d7d9a70f1914e6582017c0799bb8e4aeb6e106f30b7032af347e1b1cfef38f45edcee333e18256c47c5820d259e09394ecabd3e7de126b4da365afcd556185807ab41db1189781b617490f0118ae584081a2a058d9d036e3e243e0a1b9f989796de96258f6308ecfef8708770f7ac2e746fee8583d962f1a7f2a161ada1daf627340ceb88d32d957990992afbc7d270804005901c0deb4e2adbe562bb76dda9d085fbff084a3da7a48164fc394ce6caae005aee7d80e51767ef329a809abb780c815afadaaf60cb4cc451654a641fe57f03266f40af55307c888af9dd02669af94647d371de1ed688c4e7c84d8f6065dd9dfdcae4d8599ead8bb39d70ce536e760464e07924e5d53dfba388ebe5370cdcb94ac19dedb31f9d48dd8bcda6b4717db02f52ab7cd2a4e92dde744f81dadd35b6c6bd66eb7185b49edb6fb394c74af90ccd5d815249a5546b511e77301051328f4bc39dc0d0c48002f042a9959fee8fcba68c45116eb3fccfd1b0c736069208ed8ff787dc6ae2f665866a8e55d08792591d789ced9860eb52d5642c5ec269551baf0042892edf489e5231904e510db7a5d639b5732999360a61485d800fd0315cf9b3467b8a98dc99e13f9bd587a487e278d2012e5e5a11477fedb80b93f9e62f2816db0fd625f8786dc2e44b9669496b6be1dd46f5b3dadbca2f236ae5fe0aac0528891d27f56ec7837e71a6e9c2d4d66fe2b9758a7734e0ca66bf02059302a9e0620e92431c06ea9642c6b4abdbbd02d84d5e0f69d2fa1b3574796f3df12ffdca65764b1216b0dcc47ac840296ac2c923ec4ba2febafc2ed4c5b183695c0b4b8b59619") (unhex "8d60fab791ae353486f176318d52501dc8f2d1843bfd69492b52ae724d2cb33d") (unhex "b83ec9c4136a2d3cd15cbe691afa7f2437bce3d1240ac6f2c5d0e159") (unhex "e305b2c8902605ebce6b16fc135b1807eaa4114bd815c76519eb527cfb36cfc3") (unhex "6c75ffd5efb79a234d49e9e279be21f7f9b73a5b9db6cd5271f5db00bb8cc108") (unhex "00048a1dfc14dfb36fe05a78fd7d4b15f91ca0c05b79977170ee616147e8d113e42fca6b97816ccb93af5339255d3beb04d2b7767d80e31306018e06bbe406cd") (unhex "4fbfa75375eb235609fc2f67eb10a902a0412966c4135eca8970f0a05ed46d6f") 3 1 5350 1005 5406740 23068573 183 332 488
  , TP "mary" ERA_MARY 4 (unhex "828f1a005f27931a0261147e582084d910a9b03102f4d5d1cfa175595e0671fa16f9018ff1679750efc3d056e9595820f62edd10f9c922efcc58d7a7e939fc0493c006fff17d1d5789d314d7a43530725820ac6051aab9b68f68871774b599e697a400a98336a23075db99262f1d5e28d17a825840381bc4e03e170f96f0a0d5ff40a5c4544fed7f44d5da0970e5efe7ac6686e74faf7c655349ceab6b2306d5af201d7943e7dd026ca8e8c4d32bd7953566d29f8f585069cf8f936bb4eb58d67a70e9bdfd45cf8f9d85e0dc836347046217a43ea6c79ee10856ad39163490e30033ed57c847024365c85713319c7d1d18e21234d6271615930d4963f42fd280caa450e6d83d0b8258400006ce96fc4c0171d0478eca955a4fba452f93b2c09ffb85fbe78d5f2a40f017bdddd2dec6f11620aa54736226ff7731719ffdab4f0657ef777115559d375df25850c78ceafa9c9cbad538c13219807f2b9867672bbcc665268813025e6cbad72da40288bc4a7e96e29fa441827f801c6f804b121d6f3a99f7b660c2fe44f0ee1d402228ace7a5cfc5362d4de38b52498a0b1910125820867488a75d35cf4c06efb6d214697ad56b0237f956c75cc7e5f1ab89f5acef17582010f60a7519103220ab76085ca89609e229c60a39f200341675ac97c887287d640519010b584030e52646ec81ad60fc8cd7cf1813604dce4d7d9995621a2f5f7ffb82b1f1d2a8c21b18f0e732ac5da76790d2ac5a4da592da4d77708aec67868013e15673ab0b05005901c07ac5ad01d682b2abfcf6d2be5993a0b9d286e9723a853ebd7a64c973a70ed4c48f4bd644f4387ea9685be096df83e17bdb7b64f5f53019aef6b68d1164315b096e2809cdc43c7a3e83e68e49133209a4b035238940cd41a4211cd70ee8a1d289ed0037eeefca1926d48e6fdbe8ad069247b59c27f949656dfa3a7e155d6b704bdc1774fdb215f0680e109f51f2c302432fe8fa431c1e19b3cdcd5765a198d95cb80968b627ef000e5c7bfd836c1ac625fe318390241a2d78e7bf5c516284a5cf3569d3df6581b145d0391f7be2fa1a5f0ec6428f69b9919a46112ce26bc8db1098f1388c7708d3633bf7215fbbab9c1ea6186fc6a8a9016f567e2076a29310d4954230f0f00116b122867db4cdf59cf28b894d2b19dc547815c41b93359588ddb59f493ee86058bfdfa4c4dcbec705c6f8cdfb190ccd4a84381af7e2ad8995533bea20849aedcce82a7d1bb17919a883a9dfcd3bb0a02ebcbb9515d9ef1732097d9dbd0461df19b23640638b14d10142031af9f77e74839ad4f437d2aa8290f8b4b2e510a78c16722f50a2c71a2b9a4d0fc82ad0361d0aa1068f1f2cb8374abc17205e188d3876e6bb496ea123e8bbcf1e4287cb506db3d55013073c9425d4b7") (unhex "b9c6e9073b124c26f89556d2d7fbb99d8047787517c0ed5e4f0d323b480444b5") (unhex "a3ac6ca0694fc5825f831c8d0d97f202c4f298741ae60874730fafb8") (unhex "31247e9eeeebb67a90d1761bd8bc54b0ba8ec4f6bef6f5daa0f37908ed4826b9") (unhex "84d910a9b03102f4d5d1cfa175595e0671fa16f9018ff1679750efc3d056e959") (unhex "0006ce96fc4c0171d0478eca955a4fba452f93b2c09ffb85fbe78d5f2a40f017bdddd2dec6f11620aa54736226ff7731719ffdab4f0657ef777115559d375df2") (unhex "29d8e26b266e0e335d2b9cea85c6a7d7318ca433443044f2411a8acc54597fdb") 40 5 4114 1006 6236051 39916670 183 332 489
  , TP "alonzo" ERA_ALONZO 5 (unhex "828f1a0076e44f1a044f775f5820f289b983294053fa79e182cc464608c8b0839501aa7f60ead08992446b74ff5a582054a08fb24331f75a0ef1ed4918bdea3f6cc7a943f620f2dfd6fddde7a7b3386e58206d1b4ca9f3e109ac01bba976867c85e50e521df6e44610a7ef63167184adf43b82584041623ec7ed8f02d3fc41494333a1a2e2d920d4ab1052cb43fec4da1c2b0feb95e3eaa96f114ffeb5e0b0ed45dadf5000db0f8b6c56e006a6d9f672f5612ab5345850d0a37a9510d9aaaa2d98dafac2baf544f4d7dfef4982167c7375c9a0279e740a2536f9296d69b848e60578950b52d4062f14c5b79367c4add4cb8ff40d285d01bfe7635bf551a43e545097178d7ac10b825840000065f35d36a8af91cf946b6343b895e7ba1e2ab16fcf5626662ff87ba2bb59f815299db1c4365d03d1b004a0379c154d16e2ddf062b127b0c9a01679674a505850cf97a889d4a9e49e6c0336f1afd076b2ad3a2f24a094a62da33d11eeb4a13e7e549afcef1357dddf9fea024cc68bd93857397aee46df40d203c4082ce34a50582852923308652bd69430f3e0ba7a060a191c5458205054611d3aa3d83caa5409b9ff6283659672298423e4d86938993aa5891b4ac158204d87ad159b4d15510164ee138e6949f96ec33b3b8fb19194f19eca852c2764a90119022358404989cb6f6908f43c0d2939ddfb8b8cb0514b4ade1a741c86acdb8774b53252b7626d780dc08d586f5fc6ff06649e41b835a07d7b8761518584e3f80ed2bec80907025901c009e03bea22014744a96f5d9eac6b1aa40d2a04cc86ac534a3d8c8b3c5c46e8a6dab35fbb580d0e9b7b5b5ec217ad44457a5c392f6f1d2fb660a7664a6d6caf06d25aa9661adc4b0a7115495448176ec8d3c63c19e0949a20407a7b7787107e40e3ccefe0704f09a5871466b197d5aca1866f20502820a2bebe7e36826dbe067e3dd934c1077f14d2f7f602b53a265fd54b73556cc50db813ba710b5a81ece98098c850dc1321ecf64464d6b14d90465a70149b1a03249ac0ec555684da175eee5a12549cec498ff4eb15cc475e8a6429ab35ba88e9d03e4c64a341e7401347ed1d052d04f39577a92afe6768d3093cb2ca76d391deaac18d3253998a636aef7c0b4c7ae46f774e3e17643ce24f4c1c0605c7b5d434a1475e1508efd5f3e0cf649553fe6956b89107e255c23f7fa01d48c2e6641fe28c916dd407d44b7d308622a51c397974c4f46fedea8b259aefdd5d045c3c8a1bd999eb5feae51ad66a62dd357f3915f0706d12b85607f4603cfd196ffba7e3b52950f93e3ca368bb4283e59599bb9a442d1576e0be6f542f81ec9eebb0a5bcb94200d30ce336cf96df9fafdbc32d66c996f7fcb876d7cbbe2ca754ca1669559f917a42de85d600574a073c") (unhex "d932efa15a43b37c7d3389bed5711b1ff4d1038ce9a864cb66d8ed0fe9129852") (unhex "3c81cdc939611c4c4a54aef4a47aa8bb615def8401261726ddcede6b") (unhex "5e1740f5bf5db418c1d975d7b6035c9062757c3b9d35db5ab897ea769d11af2f") (unhex "f289b983294053fa79e182cc464608c8b0839501aa7f60ead08992446b74ff5a") (unhex "000065f35d36a8af91cf946b6343b895e7ba1e2ab16fcf5626662ff87ba2bb59f815299db1c4365d03d1b004a0379c154d16e2ddf062b127b0c9a01679674a50") (unhex "aad2f7b68690f894a6aa84dc89ba2fbd9c3ead00f89d21c263fd114278c4ed38") 10 1 7252 1006 7791695 72316767 183 332 489
  ]

unhex :: String -> BS.ByteString
unhex = BS.pack . go
  where
    go (x : y : rest) = fromIntegral (digitToInt x * 16 + digitToInt y) : go rest
    go _ = []

flipAt :: Int -> BS.ByteString -> BS.ByteString
flipAt i bs = BS.take i bs <> BS.singleton (BS.index bs i `xor` 1) <> BS.drop (i + 1) bs

failNames :: Maybe Rejection -> [Text]
failNames Nothing = []
failNames (Just r) = sort [(f ^. #rule) <> "/" <> (f ^. #name) | f <- r ^. #failures]

noRej :: Rejection -> Maybe Rejection
noRej r = if null (r ^. #failures) then Nothing else Just r

-- | TPraos checks, hand-derived from Rules/OCert.hs and Rules/Overlay.hs. The
-- certificate is part of the KES-signed header body, so a tampered cold
-- signature fails both signature checks; OCERT records every failure; the
-- counter rule has no upper bound.
tpraosChecks :: TP -> [Check]
tpraosChecks (TP name era major hdr eta0 pool keyHash _prev leader nonceV evo counter bodySize hdrSize _no _slot nonceIdx leaderIdx coldIdx) =
  [ (name <> " envelope: at both bounds", envRej hdrSize bodySize == [])
  , (name <> " envelope: header one over", envRej (hdrSize - 1) bodySize == ["Envelope/HeaderSizeTooLarge"])
  , (name <> " envelope: body one over", envRej hdrSize (bodySize - 1) == ["Envelope/BlockSizeTooLarge"])
  , (name <> " kes: evolution", kesRes hdr 129600 62 ^. #praosKesCheck . #kesPeriod == evo && kesNames hdr 129600 62 == [])
  , (name <> " kes: a bound above the evolution", kesNames hdr 129600 (evo + 1) == [])
  , (name <> " kes: evolution equals the bound", kesNames hdr 129600 evo == ["OCERT/KESAfterEndOCERT"])
  , (name <> " kes: tampered cold signature", kesNames (flipAt coldIdx hdr) 129600 62 == ["OCERT/InvalidKesSignatureOCERT", "OCERT/InvalidSignatureOCERT"])
  , (name <> " kes: tampered KES signature", kesNames (flipAt (BS.length hdr - 1) hdr) 129600 62 == ["OCERT/InvalidKesSignatureOCERT"])
  , (name <> " kes: window and signatures", kesNames (flipAt (BS.length hdr - 1) (flipAt coldIdx hdr)) 129600 evo == ["OCERT/InvalidKesSignatureOCERT", "OCERT/InvalidSignatureOCERT", "OCERT/KESAfterEndOCERT"])
  , (name <> " kes: before the start period", kesNames hdr 1000000 62 == ["OCERT/InvalidKesSignatureOCERT", "OCERT/KESBeforeStartOCERT"])
  , (name <> " opcert: same counter", opNames (Just counter) False False == ([], counter))
  , (name <> " opcert: unseen registered pool counts from zero", opNames Nothing True False == ([], counter))
  , (name <> " opcert: unseen genesis delegate counts from zero", opNames Nothing False True == ([], counter))
  , (name <> " opcert: unseen unregistered", fst (opNames Nothing False False) == ["OCERT/NoCounterForKeyHashOCERT"])
  , (name <> " opcert: lower than the last", fst (opNames (Just (counter + 1)) True False) == ["OCERT/CounterTooSmallOCERT"])
  , (name <> " opcert: a skipped counter is allowed", counter == 0 || fst (opNames (Just 0) False False) == [])
  , (name <> " vrf: valid, leader and nonce values", vrfRes eta0 keyHash Nothing ^. #praosVrfCheck . #leaderValue == leader && vrfRes eta0 keyHash Nothing ^. #praosVrfCheck . #nonceValue == nonceV && vrfNames eta0 keyHash Nothing == [])
  , (name <> " vrf: wrong key hash", vrfNames eta0 (BS.replicate 32 3) Nothing == ["OVERLAY/VRFKeyWrongVRFKey"])
  , (name <> " vrf: wrong epoch nonce", vrfNames (BS.replicate 32 1) keyHash Nothing == ["OVERLAY/VRFKeyBadNonce"])
  , (name <> " vrf: key hash first", vrfNamesH (BS.replicate 32 1) (BS.replicate 32 3) Nothing hdr == ["OVERLAY/VRFKeyWrongVRFKey"])
  , (name <> " vrf: tampered nonce proof", vrfNamesH eta0 keyHash Nothing (flipAt (nonceIdx + 5) hdr) == ["OVERLAY/VRFKeyBadNonce"])
  , (name <> " vrf: tampered leader proof", vrfNamesH eta0 keyHash Nothing (flipAt (leaderIdx + 5) hdr) == ["OVERLAY/VRFKeyBadLeaderValue"])
  , (name <> " vrf: nonce proof before leader proof", vrfNamesH eta0 keyHash Nothing (flipAt (nonceIdx + 5) (flipAt (leaderIdx + 5) hdr)) == ["OVERLAY/VRFKeyBadNonce"])
  , (name <> " key registered: pool and key hash match", keyRegNames [(pool, keyHash)] == [] && keyRegPool [(pool, keyHash)] == pool)
  , (name <> " key registered: unknown pool", keyRegNames [(BS.replicate 28 9, keyHash)] == ["OVERLAY/VRFKeyUnknown"])
  , (name <> " key registered: empty distribution", keyRegNames [] == ["OVERLAY/VRFKeyUnknown"])
  , (name <> " key registered: wrong key hash", keyRegNames [(pool, BS.replicate 32 1)] == ["OVERLAY/VRFKeyWrongVRFKey"])
  ]
  where
    pp hs bs = defMessage & #maybe'maxBlockHeaderSize .~ Just hs & #maybe'maxBlockBodySize .~ Just bs :: S.ProtocolParameters
    envRej hs bs = failNames (noRej (call (#praosHeaderEnvelope .~ (defMessage & #era .~ era & #header .~ hdr & #pparams .~ pp hs bs)) ^. #praosHeaderEnvelope . #rejection))
    kesRes h spkp maxEvo = call (#praosKesCheck .~ (defMessage & #era .~ era & #header .~ h & #slotsPerKesPeriod .~ spkp & #maxKesEvolutions .~ maxEvo))
    kesNames h spkp maxEvo = failNames (noRej (kesRes h spkp maxEvo ^. #praosKesCheck . #rejection))
    opNames last' reg gen =
      let r = call (#praosOpcertCheck .~ (defMessage & #protocolVersion .~ (defMessage & #major .~ major) & #header .~ hdr
                & #maybe'lastCounter .~ (last' :: Maybe Word64) & #poolRegistered .~ reg & #issuerIsGenesisDelegate .~ gen
                & #slotsPerKesPeriod .~ 129600 & #maxKesEvolutions .~ 62)) ^. #praosOpcertCheck
       in (failNames (noRej (r ^. #rejection)), r ^. #newCounter)
    vrfRes eta k ov = vrfResH eta k ov hdr
    vrfResH eta k ov h =
      call (#praosVrfCheck .~ (defMessage & #era .~ era & #header .~ h & #epochNonce . #hash .~ eta & #poolVrfKeyHash .~ k & #maybe'overlay .~ ov))
    vrfNames eta k ov = vrfNamesH eta k ov hdr
    vrfNamesH eta k ov h = failNames (noRej (vrfResH eta k ov h ^. #praosVrfCheck . #rejection))
    keyRegRes dist =
      call (#praosVrfKeyRegistered .~ (defMessage & #header .~ hdr & #era .~ era
        & #poolDistribution .~ [defMessage & #poolId .~ pid & #vrfKeyHash .~ vh | (pid, vh) <- dist])) ^. #praosVrfKeyRegistered
    keyRegNames dist = failNames (noRej (keyRegRes dist ^. #rejection))
    keyRegPool dist = keyRegRes dist ^. #poolId

-- | OVERLAY in an overlay slot: the shelley header is at slot 16156972, 172972
-- into a 432000-slot epoch. With d = 1 and f = 1 every slot is an active
-- overlay slot and the producing key is position mod count of the sorted
-- genesis keys: 172972 mod 3 = 1, the second of three.
overlayChecks :: [Check]
overlayChecks =
  [ ("overlay vrf: the scheduled delegate", names one one 432000 keys == [])
  , ("overlay vrf: wrong delegate", names one one 432000 (setAt 1 (#delegateKeyHash .~ BS.replicate 28 0xaa) keys) == ["OVERLAY/WrongGenesisColdKeyOVERLAY"])
  , ("overlay vrf: wrong VRF key hash", names one one 432000 (setAt 1 (#vrfKeyHash .~ BS.replicate 32 0xaa) keys) == ["OVERLAY/WrongGenesisVRFKeyOVERLAY"])
  , ("overlay vrf: both wrong, the cold key does not stop the VRF key check", names one one 432000 (setAt 1 (\g -> g & #delegateKeyHash .~ BS.replicate 28 0xaa & #vrfKeyHash .~ BS.replicate 32 0xbb) keys) == ["OVERLAY/WrongGenesisColdKeyOVERLAY", "OVERLAY/WrongGenesisVRFKeyOVERLAY"])
  , ("overlay vrf: another key's slot, 172972 mod 2 = 0", names one one 432000 (take 2 keys) == ["OVERLAY/WrongGenesisColdKeyOVERLAY", "OVERLAY/WrongGenesisVRFKeyOVERLAY"])
  , ("overlay vrf: non-active overlay slot, 172972 mod 20 = 12", names one ("1", "20") 432000 keys == ["OVERLAY/NotActiveSlotOVERLAY"])
  , ("overlay vrf: non-overlay slot with f < 1 is UNSUPPORTED", errCode (run ("1", "2") ("1", "20") 432001 keys (BS.replicate 32 9)) == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("overlay vrf: non-overlay slot under the pool registration", failNames (noRej (run ("1", "2") one 432001 keys keyHash ^. #praosVrfCheck . #rejection)) == [])
  ]
  where
    TP _ era _ hdr eta0 delegate keyHash _ _ _ _ _ _ _ _ _ _ _ _ = head tpraosHeaders
    one = ("1", "1")
    genesis i = defMessage & #genesisKeyHash .~ BS.replicate 28 i & #delegateKeyHash .~ BS.replicate 28 (0xd0 + i) & #vrfKeyHash .~ BS.replicate 32 (0xe0 + i) :: S.GenesisDelegation
    keys = setAt 1 (\g -> g & #delegateKeyHash .~ delegate & #vrfKeyHash .~ keyHash) (map genesis [1, 2, 3])
    setAt i f xs = [if j == i then f x else x | (j, x) <- zip [0 :: Int ..] xs]
    run d f len ks k =
      call (#praosVrfCheck .~ (defMessage & #era .~ era & #header .~ hdr & #epochNonce . #hash .~ eta0 & #poolVrfKeyHash .~ k
        & #overlay .~ (defMessage & #d .~ rat d & #activeSlotCoeff .~ rat f & #epochLength .~ len & #genesisDelegations .~ ks)))
    names d f len ks = failNames (noRej (run d f len ks (BS.replicate 32 9) ^. #praosVrfCheck . #rejection))

errCode :: Reply -> HarnessErrorCode
errCode r = r ^. #error . #code

-- | validateEnvelope's position checks through the envelope function: block
-- number, slot, previous hash, in that order, before the sizes.
positionChecks :: [Check]
positionChecks = concatMap forHeader headers
  where
    headers =
      [ ("shelley", ERA_SHELLEY, hdr, no, slot, prev) | TP _ _ _ hdr _ _ _ prev _ _ _ _ _ _ no slot _ _ _ <- take 1 tpraosHeaders ]
        ++ [("conway_a", ERA_CONWAY, conwayAHdr, 10882991, 135747596, unhex "4eac1e7264c0e80436b04687e75d46d6a0d6b2338c2abb73a14fafbd689f69b2")]
    forHeader (name, era, hdr, no, slot, prev) =
      [ (name <> " position: consecutive parent", rej (tip 0 0 prev) == [])
      , (name <> " position: same block number", rej (tip 1 0 prev) == ["Envelope/UnexpectedBlockNo"])
      , (name <> " position: gap in block numbers", rej (tip (-1) 0 prev) == ["Envelope/UnexpectedBlockNo"])
      , (name <> " position: same slot", rej (tip 0 1 prev) == ["Envelope/UnexpectedSlotNo"])
      , (name <> " position: parent after the header", rej (tip 0 5 prev) == ["Envelope/UnexpectedSlotNo"])
      , (name <> " position: other parent hash", rej (tip 0 0 (flipAt 31 prev)) == ["Envelope/UnexpectedPrevHash"])
      , (name <> " position: number before slot", rej (tip 1 1 (flipAt 0 prev)) == ["Envelope/UnexpectedBlockNo"])
      , (name <> " position: slot before hash", rej (tip 0 1 (flipAt 0 prev)) == ["Envelope/UnexpectedSlotNo"])
      , (name <> " position: before the sizes", failNames (noRej (env (Just (tip 1 0 prev)) False 0 0 ^. #praosHeaderEnvelope . #rejection)) == ["Envelope/UnexpectedBlockNo"])
      , ("divergence: " <> name <> " on the empty chain is refused at its block number (dingo queues it, TestDivergenceOriginHasNoPositionCheck)", failNames (noRej (env Nothing True 1100 90112 ^. #praosHeaderEnvelope . #rejection)) == ["Envelope/UnexpectedBlockNo"])
      ]
      where
        tip dNo dSlot h = defMessage & #blockNo .~ (no - 1 + fromInteger dNo) & #slot .~ (slot - 1 + fromInteger dSlot) & #hash .~ h :: EnvelopeTip
        rej t = failNames (noRej (env (Just t) False 1100 90112 ^. #praosHeaderEnvelope . #rejection))
        env parent origin hs bs =
          call (#praosHeaderEnvelope .~ (defMessage & #era .~ era & #header .~ hdr & #maybe'parent .~ parent & #origin .~ origin
            & #pparams .~ (defMessage & #maybe'maxBlockHeaderSize .~ Just hs & #maybe'maxBlockBodySize .~ Just bs :: S.ProtocolParameters)))

conwayAHdr :: BS.ByteString
conwayAHdr = unhex "828a1a00a60faf1a0817580c58204eac1e7264c0e80436b04687e75d46d6a0d6b2338c2abb73a14fafbd689f69b2582012209e0b93f0128f670c9a02781c5466c4c4be003da3a51344b6a94f709ce51f58209c1a5fc5dec0a4b822d5a3b254ce9b168299479127aadcf97506ef257517fff682584023c2d70c24c44041644f5152f7e8a1bb580e516eb8e73c7df287116adb5f009c0c001feccfeebdf34c2275d1fce859c6c46182631b6306d5fd2724ac7ab1c6be58500dbe31ef7c00c34b6522e983d223e05075359cb170668d960b8cebfced178287ee6ca5cfc6e8e60aec97fd197aebfefc24aae695680631d575c6dacdfd9efc5687e46eb2a5c04a755c7f260af9ef830819c5ea5820d2b74b6333637801f2e9c7265792d5b8fc1647f9056d67c769dbac27f25f2fd08458200946347d22a3b6da29d79102424973c932b898808ff2436fa138df102484230a0a1904165840c75619c3ebad0758349eb1dedc154a8cd280d8189d6da973b4a147b0cdb0f60442d493feeba64167a05b5fc40bc695192bf1c08afad3c07ebd33cb5925f378018209015901c00a8442332bd3f33a4d78fe2736a75110b528a1e7501bc7887910d1475fc0e425f49a84f94e98f87047916cf622f3db1f61b60c5f06709769f98c4cc67de8f50c320c6772b647ac9916765b6985d4eafccb54e71064d01df41f8d0638ed5cd62b7b6e49ba15dd87cc687ab87d3fb22490d355e8fa9c5f7c24ed88b800fcc4cb1f1b54e65b5ba82c442f4643caadc86583072b8b6956f4f9a4530c29873f7231605efd7a7f961a863530512ef86b50f9b1004748c31fa07978f2ece7d8e76ffde67d713015824b28e19f05f0383c2def3cdeb67247f33f5eae329c38a375b2eb06a586dcc2e102a776a6deaad1741f2a7f5aa604074698e876afab4455278fd84a1db5768078e2848cc85e3c8a0b48630a2622832ecd2dbb3c505df2a70b93b49ce99616f601e5e2004a8ce8926319c23f2a26ac8550cb1c05c9d2d25fc5fcd122fc35b057a71d6e961250c99b19a7bfd9acdc60a8151d6c81ef2d7d69a62fd0f17d184dd753cce9a2e9c32b53baf317e31c6c5e3cf8ea8b203b413ae8b0253db53d0cbe19b0f0547a0e67d3591d1cade6ceb4a47779ba4a09e7526280acb62200f42c98f6185ea9da3daf47aa3d10ffe5307331fa3430af6c6361154943c39375"
