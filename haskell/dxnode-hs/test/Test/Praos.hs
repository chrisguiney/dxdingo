{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Praos (tests) where

import Cardano.Crypto.Hash (Blake2b_256, hashFromBytes, hashToBytes, hashWith)
import Cardano.Ledger.BaseTypes (Nonce (..), (⭒))
import qualified Data.ByteString as BS
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.Word (Word32, Word64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Praos (OverlaySlotKind (..))
import qualified Proto.Dxdingo.V1.State as S
import Proto.Dxdingo.V1.Types (Era (..))
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
    errCode r = r ^. #error . #code
    vrf nonce keyHash =
      call (#praosVrfCheck .~ (defMessage & #era .~ ERA_CONWAY & #header .~ conwayA & #epochNonce . #hash .~ nonce & #poolVrfKeyHash .~ keyHash))
    vrfRes = vrf (unhex eta0Hex) (unhex vrfKeyHashHex)
    vrfOk = failName (vrfRes ^. #praosVrfCheck . #rejection) == ""
    kes hdr spkp maxEvo =
      call (#praosKesCheck .~ (defMessage & #era .~ ERA_BABBAGE & #header .~ hdr & #slotsPerKesPeriod .~ spkp & #maxKesEvolutions .~ maxEvo))
    flipAt i bs = BS.take i bs <> BS.singleton (BS.index bs i `xor` 1) <> BS.drop (i + 1) bs
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
    unhex :: String -> BS.ByteString
    unhex = BS.pack . go
      where
        go (x : y : rest) = fromIntegral (digitToInt x * 16 + digitToInt y) : go rest
        go _ = []
