{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Crypto primitives and address decoding, answered by cardano-base and
-- cardano-ledger-core.
module Dxdingo.Crypto
  ( dispatch
  , implemented
  , vkeyWitness
  , vrfVerify
  , kesVerify
  , addressDecode
  , scriptHash
  , bootstrapWitness
  , vrfProve
  ) where

import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (..)
  , Ed25519DSIGN
  )
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, hashFromBytes, hashToBytes, hashWith)
import Cardano.Crypto.KES (KESAlgorithm (..), Sum6KES)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.VRF.Class (VRFAlgorithm (genKeyPairVRF))
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
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.BaseTypes (Network (..), certIxToInt, natVersion, txIxToInt)
import Cardano.Ledger.Binary (DecCBOR (..), DecoderError, decodeFull', decodeFullAnnotator, serialize')
import qualified Data.ByteString as BS
import Cardano.Ledger.Core (NativeScript, eraProtVerLow, fromNativeScript, hashScript)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (PlutusScript (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness, bootstrapWitKeyHash, verifyBootstrapWit)
import Cardano.Ledger.Plutus.Language (Plutus (..), PlutusBinary (..))
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..), ptrSlotNo)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Address (BootstrapAddress, bootstrapKeyHash)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Text as T
import Dxdingo.Domain
import Dxdingo.Answer (Answer, respond)
import Proto.Dxdingo.V1.Envelope (Call'Request (..))
import Data.Text (Text)
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Crypto
import qualified Proto.Dxdingo.V1.Types as P
import Cardano.Ledger.Hashes (EraIndependentTxBody, HASH)
import Cardano.Crypto.Hash (Hash)

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

-- | 'Praos.prove' from the 32-byte seed through 'genKeyPairVRF' (which calls
-- 'Praos.keypairFromSeed'); the
-- output is 'Praos.outputFromProof'. Any other seed width is UNSUPPORTED.
vrfProve :: CryptoVrfProveRequest -> Result CryptoVrfProveResponse
vrfProve req
  | BS.length (req ^. #skey) /= 32 =
      unsupported ("VRF seed of " <> T.pack (show (BS.length (req ^. #skey))) <> " bytes; only 32 is modelled")
  | otherwise =
      let (Praos.SignKeyPraosVRF sk, _) = genKeyPairVRF @Praos.PraosVRF (mkSeedFromBytes (req ^. #skey))
       in case Praos.prove sk (req ^. #alpha) of
            Nothing -> pure (failed "prove returned no proof")
            Just proof -> case Praos.outputFromProof proof of
              Nothing -> pure (failed "outputFromProof returned no output")
              Just out ->
                pure $
                  defMessage
                    & #proof .~ Praos.proofBytes proof
                    & #output .~ Praos.outputBytes out
  where
    failed d = defMessage & #rejection .~ rejection "vrf" "DingoError" d

-- | 'hashScript' over the script as the reference decodes it. Native scripts
-- go through the Dijkstra decoder, which accepts every earlier native script
-- shape; Plutus bytes are the 'PlutusBinary', hashed as given. A malformed
-- native script is the rejection "DeserialiseFailure".
scriptHash :: CryptoScriptHashRequest -> Result CryptoScriptHashResponse
scriptHash req = case req ^. #language of
  P.SCRIPT_LANGUAGE_NATIVE ->
    case decodeFullAnnotator (eraProtVerLow @DijkstraEra) "native script" decCBOR (BSL.fromStrict raw) of
      Left err ->
        pure (defMessage & #rejection .~ rejection "script" "DeserialiseFailure" (T.pack (show err)))
      Right (ns :: NativeScript DijkstraEra) -> pure (hashed (fromNativeScript ns))
  P.SCRIPT_LANGUAGE_PLUTUS_V1 -> pure (hashed (PlutusScript (DijkstraPlutusV1 (Plutus bin))))
  P.SCRIPT_LANGUAGE_PLUTUS_V2 -> pure (hashed (PlutusScript (DijkstraPlutusV2 (Plutus bin))))
  P.SCRIPT_LANGUAGE_PLUTUS_V3 -> pure (hashed (PlutusScript (DijkstraPlutusV3 (Plutus bin))))
  P.SCRIPT_LANGUAGE_PLUTUS_V4 -> pure (hashed (PlutusScript (DijkstraPlutusV4 (Plutus bin))))
  other -> badRequest ("script language " <> T.pack (show other))
  where
    raw = req ^. #script
    bin = PlutusBinary (SBS.toShort raw)
    hashed :: AlonzoScript DijkstraEra -> CryptoScriptHashResponse
    hashed s = let ScriptHash h = hashScript @DijkstraEra s in defMessage & #hash .~ hashToBytes h

-- | 'verifyBootstrapWit' over the body hash and 'bootstrapWitKeyHash' against
-- the address root of a Byron address ('bootstrapKeyHash'). The witness is
-- built through its CBOR decoder, so a key or signature of the wrong width is
-- an invalid witness. An empty address does not match; an address that does
-- not decode is the rejection "DeserialiseFailure"; a body hash that is not
-- 32 bytes cannot be a 'Hash' and is UNSUPPORTED.
bootstrapWitness :: CryptoBootstrapWitnessRequest -> Result CryptoBootstrapWitnessResponse
bootstrapWitness req = do
  bodyHash <- maybe (unsupported "body hash is not 32 bytes") pure (hashFromBytes (req ^. #bodyHash) :: Maybe (Hash HASH EraIndependentTxBody))
  addrRoot <- addressRoot
  case addrRoot of
    Left rej -> pure (defMessage & #rejection .~ rej)
    Right root -> pure $ case witness of
      Left _ -> defMessage
      Right w ->
        let KeyHash wh = bootstrapWitKeyHash w
         in defMessage
              & #valid .~ verifyBootstrapWit bodyHash w
              & #matchesAddress .~ (root == Just (hashToBytes wh))
  where
    witness :: Either DecoderError BootstrapWitness
    witness =
      decodeFull' (natVersion @9) $
        serialize' (natVersion @9) (req ^. #vkey, req ^. #signature, req ^. #chainCode, req ^. #attributes)
    addressRoot
      | BS.null (req ^. #address) = pure (Right Nothing)
      | otherwise = pure $ case decodeAddrEither (req ^. #address) of
          Left err -> Left (rejection "address" "DeserialiseFailure" (T.pack err))
          Right (AddrBootstrap ba) -> Right (Just (rootOf ba))
          Right _ -> Right Nothing
    rootOf :: BootstrapAddress -> ByteString
    rootOf ba = let KeyHash h = bootstrapKeyHash ba in hashToBytes h

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

rejection :: Text -> Text -> Text -> P.Rejection
rejection rule name detail =
  defMessage
    & #failures
      .~ [ defMessage
            & #rule .~ rule
            & #name .~ name
            & #detail .~ detail
         ]

credential :: Credential kr -> P.Credential
credential (KeyHashObj (KeyHash h)) =
  defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hashToBytes h
credential (ScriptHashObj (ScriptHash h)) =
  defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hashToBytes h

dispatch :: Call'Request -> Maybe Answer
dispatch = \case
  Call'CryptoVkeyWitness q -> Just (respond #cryptoVkeyWitness (vkeyWitness q))
  Call'CryptoVrfVerify q -> Just (respond #cryptoVrfVerify (vrfVerify q))
  Call'CryptoKesVerify q -> Just (respond #cryptoKesVerify (kesVerify q))
  Call'CryptoAddressDecode q -> Just (respond #cryptoAddressDecode (addressDecode q))
  Call'CryptoScriptHash q -> Just (respond #cryptoScriptHash (scriptHash q))
  Call'CryptoBootstrapWitness q -> Just (respond #cryptoBootstrapWitness (bootstrapWitness q))
  Call'CryptoVrfProve q -> Just (respond #cryptoVrfProve (vrfProve q))
  _ -> Nothing

implemented :: [Text]
implemented =
  [ "crypto_vkey_witness"
  , "crypto_vrf_verify"
  , "crypto_kes_verify"
  , "crypto_address_decode"
  , "crypto_script_hash"
  , "crypto_bootstrap_witness"
  , "crypto_vrf_prove"
  ]
