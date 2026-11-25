{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Dxdingo.Dispatch (handle, handleBytes)
import Dxdingo.Wire
import Lens.Micro ((&), (.~), (^.))
import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.BaseTypes (Nonce (..), (⭒))
import Proto.Dxdingo.V1.Crypto (AddressKind (..))
import Proto.Dxdingo.V1.Envelope (Call, HarnessErrorCode (..), Reply)
import Proto.Dxdingo.V1.Praos (OverlaySlotKind (..))
import Proto.Dxdingo.V1.Types (Era (..))
import System.Exit (exitFailure)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do
        putStrLn ("FAIL: " <> name)
        modifyIORef' failures (+ 1)

  check "header is big-endian uint32 length" $
    encodeFrame "abc" == BS.pack [0, 0, 0, 3, 97, 98, 99]
  check "consecutive frames round-trip" $
    let buf = encodeFrame "one" <> encodeFrame "" <> encodeFrame "three"
        go b = case decodeFrame b of
          Right (Just (p, rest)) -> p : go rest
          _ -> []
     in go buf == ["one", "", "three"]
  check "empty buffer is clean end of stream" $ decodeFrame "" == Right Nothing
  check "truncated header" $ decodeFrame (BS.pack [0, 0]) == Left Truncated
  check "truncated body" $ decodeFrame (BS.pack [0, 0, 0, 5, 97]) == Left Truncated
  check "oversized length rejected" $
    decodeFrame (BS.pack [255, 255, 255, 255]) == Left (TooLarge 4294967295)

  let feeCall =
        (defMessage :: Call)
          & #id .~ 42
          & #feeScriptFee .~ (defMessage & #exMem .~ 7)
      unimpl = handle feeCall
  check "unhandled function replies UNIMPLEMENTED with the call id" $
    unimpl ^. #id == 42
      && unimpl ^. #error . #code == HARNESS_ERROR_CODE_UNIMPLEMENTED
  check "meta_info is answered" $
    let r = handle ((defMessage :: Call) & #id .~ 1 & #metaInfo .~ defMessage)
     in r ^. #metaInfo . #implementation == "cardano-ledger"
  check "call without request is BAD_REQUEST" $
    handle ((defMessage :: Call) & #id .~ 9) ^. #error . #code
      == HARNESS_ERROR_CODE_BAD_REQUEST
  check "undecodable bytes reply BAD_REQUEST" $
    case decodeMessage (handleBytes (BS.pack [255, 255, 255])) of
      Right (r :: Reply) -> r ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST
      Left _ -> False
  check "handleBytes round-trips a call" $
    case decodeMessage (handleBytes (encodeMessage feeCall)) of
      Right (r :: Reply) -> r ^. #id == 42
      Left _ -> False

  let memberCall reward cost (mn, md) member pool total =
        (defMessage :: Call)
          & #id .~ 7
          & #rewardsMemberReward
            .~ ( defMessage
                  & #poolReward .~ reward
                  & #cost .~ cost
                  & #margin .~ (defMessage & #numerator .~ mn & #denominator .~ md)
                  & #memberStake .~ member
                  & #poolStake .~ pool
                  & #totalStake .~ total
               )
      rewardOf c = handle c ^. #rewardsMemberReward . #reward
      codeOf c = handle c ^. #error . #code
  -- Same cases as dxnode-go's TestMemberReward.
  check "member reward: half the pool, 1% margin" $
    rewardOf (memberCall 1000000000 340000000 ("1", "100") 25 50 1000) == 326700000
  check "member reward: reward below cost is zero" $
    rewardOf (memberCall 300000000 340000000 ("0", "1") 25 50 1000) == 0
  check "member reward: floors" $
    rewardOf (memberCall 10 0 ("0", "1") 1 3 3) == 3
  check "member reward: zero pool stake is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "2") 1 0 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: zero total stake is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "2") 1 2 0) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: margin above one is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("3", "2") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: negative margin is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("-1", "2") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: margin over 64 bits is UNSUPPORTED" $
    codeOf (memberCall 1000 1 ("1", "18446744073709551617") 1 2 4) == HARNESS_ERROR_CODE_UNSUPPORTED
  check "member reward: malformed margin is BAD_REQUEST" $
    codeOf (memberCall 1000 1 ("x", "2") 1 2 4) == HARNESS_ERROR_CODE_BAD_REQUEST

  let rat (a, b) = defMessage & #numerator .~ a & #denominator .~ b
      call f = handle ((defMessage :: Call) & #id .~ 1 & f)
  -- Same cases as dxnode-go.
  check "leader reward: owner fifth of pool" $
    call (#rewardsLeaderReward .~ (defMessage & #poolReward .~ 1000000000 & #cost .~ 340000000
      & #margin .~ rat ("1", "100") & #ownerStake .~ 10 & #poolStake .~ 50 & #totalStake .~ 1000))
      ^. #rewardsLeaderReward . #reward == 477280000
  check "leader reward: below cost goes to the operator" $
    call (#rewardsLeaderReward .~ (defMessage & #poolReward .~ 300000000 & #cost .~ 340000000
      & #margin .~ rat ("0", "1") & #ownerStake .~ 10 & #poolStake .~ 50 & #totalStake .~ 1000))
      ^. #rewardsLeaderReward . #reward == 300000000
  check "max pool" $
    call (#rewardsMaxPool .~ (defMessage & #pparams .~ (defMessage & #a0 .~ rat ("3", "10") & #nOpt .~ 500)
      & #poolPot .~ 1000000000 & #totalStake .~ 1000000 & #poolStake .~ 1000 & #pledge .~ 500))
      ^. #rewardsMaxPool . #maxPool == 812500
  check "max pool: missing n_opt is UNSUPPORTED" $
    call (#rewardsMaxPool .~ (defMessage & #pparams .~ (defMessage & #a0 .~ rat ("3", "10"))
      & #poolPot .~ 1 & #totalStake .~ 1 & #poolStake .~ 1))
      ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED
  let perf d ps = call (#rewardsApparentPerformance .~ (defMessage & #d .~ rat d & #poolStake .~ ps
        & #totalActiveStake .~ 4 & #blocks .~ 3 & #blocksTotal .~ 10))
        ^. #rewardsApparentPerformance . #performance
      ratIs (a, b) r = r ^. #numerator == a && r ^. #denominator == b
  check "apparent performance: decentralized" $ ratIs ("6", "5") (perf ("0", "1") 1)
  check "apparent performance: federated" $ ratIs ("1", "1") (perf ("9", "10") 1)
  check "apparent performance: no stake" $ ratIs ("0", "1") (perf ("0", "1") 0)

  let leader era v = call (#praosLeaderCheck .~ (defMessage & #era .~ era & #leaderValue .~ v
        & #poolStake .~ 1 & #totalStake .~ 2 & #activeSlotCoeff .~ rat ("1", "20")))
  check "leader check: praos lowest value leads" $
    leader ERA_BABBAGE (BS.replicate 32 0) ^. #praosLeaderCheck . #isLeader
  check "leader check: praos highest value does not" $
    not (leader ERA_BABBAGE (BS.replicate 32 255) ^. #praosLeaderCheck . #isLeader)
  check "leader check: tpraos lowest value leads" $
    leader ERA_SHELLEY (BS.replicate 64 0) ^. #praosLeaderCheck . #isLeader
  check "leader check: 64-byte praos value is UNSUPPORTED" $
    leader ERA_BABBAGE (BS.replicate 64 0) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED

  let a = BS.replicate 32 0xaa
      b = BS.replicate 32 0xbb
      nonceOf c l e = call (#praosNonceEpoch .~ (defMessage & #era .~ ERA_SHELLEY
        & #candidate . #hash .~ c & #previousLab . #hash .~ l & #extraEntropy . #hash .~ e))
        ^. #praosNonceEpoch . #epochNonce . #hash
      ab = nonceOf a b ""
  check "nonce epoch: candidate only" $ nonceOf a "" "" == a
  check "nonce epoch: neutral candidate returns the lab" $ nonceOf "" b "" == b
  check "nonce epoch: lab and extra entropy mix the same way" $ ab == nonceOf a "" b && BS.length ab == 32

  let keys = [defMessage & #genesisKeyHash .~ BS.replicate 28 i | i <- [1 .. 7]]
      overlay slot = call (#tpraosOverlaySchedule .~ (defMessage & #slot .~ slot & #epochFirstSlot .~ 1000
        & #d .~ rat ("1", "2") & #activeSlotCoeff .~ rat ("1", "20") & #genesisDelegations .~ keys))
  check "overlay: active slot names genesis key 0" $
    overlay 1000 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_ACTIVE
      && overlay 1000 ^. #tpraosOverlaySchedule . #genesisKeyHash == BS.replicate 28 1
  check "overlay: praos slot" $ overlay 1001 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_PRAOS
  check "overlay: non-active slot" $ overlay 1002 ^. #tpraosOverlaySchedule . #kind == OVERLAY_SLOT_KIND_NON_ACTIVE
  check "overlay: slot before epoch is UNSUPPORTED" $ overlay 999 ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED

  let pay = BS.replicate 28 0x11
      stake = BS.replicate 28 0x22
      addr raw = call (#cryptoAddressDecode .~ (defMessage & #raw .~ raw)) ^. #cryptoAddressDecode
      ent = addr (BS.cons 0x61 pay)
      rew = addr (BS.cons 0xe1 stake)
      ptr = addr (BS.cons 0x41 pay <> BS.pack [0x81, 0x00, 0x02, 0x03])
  check "address: enterprise" $
    ent ^. #kind == ADDRESS_KIND_ENTERPRISE && ent ^. #networkId == 1
      && ent ^. #payment . #hash == pay && ent ^. #raw == BS.cons 0x61 pay
  check "address: reward" $
    rew ^. #kind == ADDRESS_KIND_REWARD && rew ^. #stake . #hash == stake && rew ^. #raw == BS.cons 0xe1 stake
  check "address: pointer" $
    ptr ^. #kind == ADDRESS_KIND_POINTER && ptr ^. #pointer . #slot == 128
      && ptr ^. #pointer . #txIndex == 2 && ptr ^. #pointer . #certIndex == 3
  check "address: truncated is rejected" $
    not (null (addr (BS.pack [0x01, 0x02]) ^. #rejection . #failures))

  -- Divergence confirmations call the reference directly, bypassing
  -- dxnode-hs, so a reported mismatch cannot come from request translation.
  -- praos.nonce-epoch, neutral candidate: dingo encodes the neutral candidate
  -- as 32 zero bytes and hashes it when a lab is present
  -- (dxnode-go TestDivergenceNeutralCandidateIsHashed).
  let nonceFrom = maybe NeutralNonce Nonce . hashFromBytes
      labN = nonceFrom (BS.replicate 32 0xbb)
      zeroN = nonceFrom (BS.replicate 32 0)
  check "divergence: NeutralNonce is the identity of the nonce operator" $
    (NeutralNonce ⭒ labN) ⭒ NeutralNonce == labN
  check "divergence: a real all-zero nonce is not neutral in the reference" $
    zeroN /= NeutralNonce && (zeroN ⭒ labN) /= labN

  n <- readIORef failures
  unless (n == 0) exitFailure
  putStrLn "all tests passed"
