{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Crypto primitives and address decoding, answered by cardano-base and
-- cardano-ledger-core.
module Dxdingo.Crypto
  ( vkeyWitness
  , vrfVerify
  , kesVerify
  , addressDecode
  ) where

import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (..)
  , Ed25519DSIGN
  )
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, hashToBytes, hashWith)
import Cardano.Crypto.KES (KESAlgorithm (..), Sum6KES)
import qualified Cardano.Crypto.VRF.Praos as Praos
import Cardano.Ledger.Address
  ( AccountAddress (..)
  , AccountId (..)
  , Addr (..)
  , decodeAddrEither
  , deserialiseRewardAccount
  , serialiseAddr
  , serialiseRewardAccount
  )
import Cardano.Ledger.BaseTypes (Network (..), certIxToInt, txIxToInt)
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..), ptrSlotNo)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString (ByteString)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Text as T
import Dxdingo.Domain
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Crypto
import qualified Proto.Dxdingo.V1.Types as P

-- | Ed25519 'verifyDSIGN' over the body hash. The key hash is Blake2b-224 of
-- the raw key, reported only when the key deserialises.
vkeyWitness :: CryptoVkeyWitnessRequest -> Result CryptoVkeyWitnessResponse
vkeyWitness req =
  pure $ case rawDeserialiseVerKeyDSIGN @Ed25519DSIGN (req ^. #vkey) of
    Nothing -> defMessage
    Just vk ->
      let valid = case rawDeserialiseSigDSIGN @Ed25519DSIGN (req ^. #signature) of
            Nothing -> False
            Just sig -> either (const False) (const True) $
              verifyDSIGN () vk (req ^. #bodyHash) sig
       in defMessage
            & #valid .~ valid
            & #keyHash .~ hashToBytes (hashWith @Blake2b_224 id (rawSerialiseVerKeyDSIGN vk))

-- | 'Praos.verify' (libsodium ECVRF draft 03).
vrfVerify :: CryptoVrfVerifyRequest -> Result CryptoVrfVerifyResponse
vrfVerify req =
  pure $ case verified of
    Nothing -> defMessage
    Just out -> defMessage & #valid .~ True & #output .~ Praos.outputBytes out
  where
    verified = do
      vk <- Praos.vkFromBytes (req ^. #vkey)
      proof <- Praos.proofFromBytes (req ^. #proof)
      Praos.verify vk proof (req ^. #alpha)

type CardanoKES = Sum6KES Ed25519DSIGN Blake2b_256

-- | 'verifyKES' for Sum6KES, the only depth Cardano uses.
kesVerify :: CryptoKesVerifyRequest -> Result CryptoKesVerifyResponse
kesVerify req
  | req ^. #depth /= 6 = unsupported ("KES depth " <> T.pack (show (req ^. #depth)) <> "; only 6 is modelled")
  | otherwise =
      pure $
        defMessage & #valid .~ maybe False id verified
  where
    verified = do
      vk <- rawDeserialiseVerKeyKES @CardanoKES (req ^. #vkey)
      sig <- rawDeserialiseSigKES @CardanoKES (req ^. #signature)
      pure $ either (const False) (const True) $
        verifyKES () vk (fromIntegral (req ^. #period)) (req ^. #message :: ByteString) sig

-- | Strict 'decodeAddrEither'; a reward (account) address is tried with
-- 'deserialiseRewardAccount' when that fails. A failure is the rejection
-- "DeserialiseFailure", as in dxnode-go.
addressDecode :: CryptoAddressDecodeRequest -> Result CryptoAddressDecodeResponse
addressDecode req = case req ^. #maybe'input of
  Just (CryptoAddressDecodeRequest'Raw raw) -> pure (decodeRaw raw)
  _ -> unsupported "only raw address bytes are modelled"
  where
    decodeRaw raw = case decodeAddrEither raw of
      Right addr -> fromAddr addr
      Left err -> case deserialiseRewardAccount raw of
        Just acct -> fromAccount acct
        Nothing -> reject err
    reject err =
      defMessage
        & #rejection
          .~ ( defMessage
                & #failures
                  .~ [ defMessage
                        & #rule .~ "address"
                        & #name .~ "DeserialiseFailure"
                        & #detail .~ T.pack err
                     ]
             )
    fromAddr addr@(AddrBootstrap _) =
      defMessage & #kind .~ ADDRESS_KIND_BOOTSTRAP & #raw .~ serialiseAddr addr
    fromAddr addr@(Addr net pay ref) =
      let base =
            defMessage
              & #networkId .~ network net
              & #payment .~ credential pay
              & #raw .~ serialiseAddr addr
       in case ref of
            StakeRefBase c -> base & #kind .~ ADDRESS_KIND_BASE & #stake .~ credential c
            StakeRefPtr p -> base & #kind .~ ADDRESS_KIND_POINTER & #pointer .~ pointer p
            StakeRefNull -> base & #kind .~ ADDRESS_KIND_ENTERPRISE
    fromAccount acct@(AccountAddress net (AccountId c)) =
      defMessage
        & #kind .~ ADDRESS_KIND_REWARD
        & #networkId .~ network net
        & #stake .~ credential c
        & #raw .~ serialiseRewardAccount acct
    network Mainnet = 1
    network Testnet = 0
    pointer p@(Ptr _ tx cert) =
      let SlotNo s = ptrSlotNo p
       in (defMessage :: P.Pointer)
            & #slot .~ s
            & #txIndex .~ fromIntegral (txIxToInt tx)
            & #certIndex .~ fromIntegral (certIxToInt cert)

credential :: Credential kr -> P.Credential
credential (KeyHashObj (KeyHash h)) =
  defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
credential (ScriptHashObj (ScriptHash h)) =
  defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h
