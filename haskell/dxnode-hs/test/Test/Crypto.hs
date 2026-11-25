{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Crypto (tests) where

import Cardano.Crypto.Hash (Blake2b_224, hashToBytes, hashWith)
import Cardano.Ledger.Binary (DecCBOR (..), DecoderError, decodeFullAnnotator)
import Cardano.Ledger.Core (NativeScript, eraProtVerLow)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Crypto (AddressKind (..), CryptoBootstrapWitnessRequest)
import qualified Proto.Dxdingo.V1.Types as P
import Test.Util

-- The verifiers are cross-checked through the corpus, whose keys, proofs and
-- signatures dxnode-go generates.
tests :: [Check]
tests =
  [ ( "address: enterprise"
    , ent ^. #kind == ADDRESS_KIND_ENTERPRISE && ent ^. #networkId == 1
        && ent ^. #payment . #hash == pay && ent ^. #raw == BS.cons 0x61 pay
    )
  , ("address: reward", rew ^. #kind == ADDRESS_KIND_REWARD && rew ^. #stake . #hash == stake && rew ^. #raw == BS.cons 0xe1 stake)
  , ( "address: pointer"
    , ptr ^. #kind == ADDRESS_KIND_POINTER && ptr ^. #pointer . #slot == 128
        && ptr ^. #pointer . #txIndex == 2 && ptr ^. #pointer . #certIndex == 3
    )
  , ("address: truncated is rejected", not (null (addr (BS.pack [0x01, 0x02]) ^. #rejection . #failures)))
  , ("script hash: native", scriptHash P.SCRIPT_LANGUAGE_NATIVE native ^. #hash == blake 0 native)
  , ("script hash: plutus v1", scriptHash P.SCRIPT_LANGUAGE_PLUTUS_V1 plutus ^. #hash == blake 1 plutus)
  , ("script hash: plutus v2", scriptHash P.SCRIPT_LANGUAGE_PLUTUS_V2 plutus ^. #hash == blake 2 plutus)
  , ("script hash: plutus v3", scriptHash P.SCRIPT_LANGUAGE_PLUTUS_V3 plutus ^. #hash == blake 3 plutus)
  , ("script hash: plutus v4", scriptHash P.SCRIPT_LANGUAGE_PLUTUS_V4 plutus ^. #hash == blake 4 plutus)
  , ( "script hash: malformed native is rejected"
    , (scriptHash P.SCRIPT_LANGUAGE_NATIVE (BS.pack [0xff]) ^. #rejection . #failures) /= []
    )
  , ("divergence: a native script alone decodes in full", nativeDecodes native)
  , ("divergence: the reference rejects a native script with a trailing byte", not (nativeDecodes (native <> BS.pack [0])))
  , ( "vrf prove: standard_10"
    , prove (unhex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60") BS.empty
        == ( unhex "b6b4699f87d56126c9117a7da55bd0085246f4c56dbc95d20172612e9d38e8d7ca65e573a126ed88d4e30a46f80a666854d675cf3ba81de0de043c3774f061560f55edc256a787afe701677c0f602900"
           , unhex "5b49b554d05c0cd5a5325376b3387de59d924fd1e13ded44648ab33c21349a603f25b84ec5ed887995b33da5e3bfcb87cd2f64521c4c62cf825cffabbe5d31cc"
           )
    )
  , ( "vrf prove: generated_1"
    , prove (BS.replicate 32 0) (BS.pack [0])
        == ( unhex "000f006e64c91f84212919fe0899970cd341206fc081fe599339c8492e2cea3299ae9de4b6ce21cda0a975f65f45b70f82b3952ba6d0dbe11a06716e67aca233c0d78f115a655aa1952ada9f3d692a0a"
           , unhex "9930b5dddc0938f01cf6f9746eded569ee676bd6ff3b4f19233d74b903ec53a45c5728116088b7c622b6d6c354f7125c7d09870b56ec6f1e4bf4970f607e04b2"
           )
    )
  , ("bootstrap: valid and matching", bw bootReq == (True, True))
  , ("bootstrap: tampered signature", bw (bootReq & #signature .~ flipFirst (bootReq ^. #signature)) == (False, True))
  , ("bootstrap: wrong body", bw (bootReq & #bodyHash .~ flipFirst (bootReq ^. #bodyHash)) == (False, True))
  , ("bootstrap: other chain code", bw (bootReq & #chainCode .~ flipFirst (bootReq ^. #chainCode)) == (True, False))
  , ("bootstrap: other attributes", bw (bootReq & #attributes .~ unhex "a10140" ) == (True, False))
  , ("bootstrap: no address", bw (bootReq & #address .~ BS.empty) == (True, False))
  , ( "bootstrap: bad address is rejected"
    , not (null (bootCall (bootReq & #address .~ BS.pack [1, 2]) ^. #rejection . #failures))
    )
  ]
  where
    nativeDecodes :: BS.ByteString -> Bool
    nativeDecodes bs =
      either (const False) (const True) $
        (decodeFullAnnotator (eraProtVerLow @DijkstraEra) "native script" decCBOR (BSL.fromStrict bs) :: Either DecoderError (NativeScript DijkstraEra))
    pay = BS.replicate 28 0x11
    stake = BS.replicate 28 0x22
    addr raw = call (#cryptoAddressDecode .~ (defMessage & #raw .~ raw)) ^. #cryptoAddressDecode
    ent = addr (BS.cons 0x61 pay)
    rew = addr (BS.cons 0xe1 stake)
    ptr = addr (BS.cons 0x41 pay <> BS.pack [0x81, 0x00, 0x02, 0x03])
    native = BS.pack [0x82, 0x00, 0x58, 0x1c] <> pay
    plutus = BS.pack [1, 2, 3]
    blake tag bs = hashToBytes (hashWith @Blake2b_224 id (BS.cons tag bs))
    scriptHash lang script =
      call (#cryptoScriptHash .~ (defMessage & #language .~ lang & #script .~ script)) ^. #cryptoScriptHash
    prove sk alpha =
      let r = call (#cryptoVrfProve .~ (defMessage & #skey .~ sk & #alpha .~ alpha)) ^. #cryptoVrfProve
       in (r ^. #proof, r ^. #output)
    bootCall req = call (#cryptoBootstrapWitness .~ req) ^. #cryptoBootstrapWitness
    bw req = let r = bootCall req in (r ^. #valid, r ^. #matchesAddress)
    flipFirst b = BS.cons (BS.head b + (if even (BS.head b) then 1 else -1)) (BS.tail b)
    -- Same fixture as TestBootstrapWitness in dxnode-go: a Byron address whose
    -- root is the witness key hash.
    bootReq :: CryptoBootstrapWitnessRequest
    bootReq =
      defMessage
        & #bodyHash .~ BS.replicate 32 0xcc
        & #vkey .~ unhex "6e7a1cdd29b0b78fd13af4c5598feff4ef2a97166e3ca6f2e4fbfccd80505bf1"
        & #chainCode .~ BS.replicate 32 0x33
        & #attributes .~ BS.pack [0xa0]
        & #signature .~ unhex "7ce4872e122a7c260e49e214fdb638b0db347afac790ceb94f6163c15f3dc624e7dc9283d88e3961b2e58213f1e2749ece225456b79b3aa7215cfdb5da477701"
        & #address .~ unhex "82d818582183581c72a446c7cc324f0b9dd4888bfe1a00f05ad139d94786b8b92b7dfb75a0001a73e4624d"

unhex :: String -> BS.ByteString
unhex = BS.pack . go . filter (/= ' ')
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []
