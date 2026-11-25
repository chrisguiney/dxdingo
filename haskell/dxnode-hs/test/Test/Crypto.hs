{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Crypto (tests) where

import qualified Data.ByteString as BS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Crypto (AddressKind (..))
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
  ]
  where
    pay = BS.replicate 28 0x11
    stake = BS.replicate 28 0x22
    addr raw = call (#cryptoAddressDecode .~ (defMessage & #raw .~ raw)) ^. #cryptoAddressDecode
    ent = addr (BS.cons 0x61 pay)
    rew = addr (BS.cons 0xe1 stake)
    ptr = addr (BS.cons 0x41 pay <> BS.pack [0x81, 0x00, 0x02, 0x03])
