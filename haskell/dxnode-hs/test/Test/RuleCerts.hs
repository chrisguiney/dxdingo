{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Certificate rules. The same scenarios run in dxnode-go
-- (cmd/dxnode-go/rule_certs_test.go): Conway transactions with hand-built
-- certificates over a small ledger state, expecting the constructors the
-- reference reports.
module Test.RuleCerts (tests) where

import qualified Data.ByteString as BS
import Data.List (sort)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Numeric (readHex)
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode, Reply)
import qualified Proto.Dxdingo.V1.Rule as R
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Test.RuleCertsData (scenarios)
import Test.Util

unhex :: String -> BS.ByteString
unhex (a : b : rest) = BS.cons (fst (head (readHex [a, b]))) (unhex rest)
unhex _ = BS.empty

hash28 :: Word -> BS.ByteString
hash28 b = BS.replicate 28 (fromIntegral b)

key, script :: Word -> P.Credential
key b = defMessage & #kind .~ P.CREDENTIAL_KIND_KEY & #hash .~ hash28 b
script b = defMessage & #kind .~ P.CREDENTIAL_KIND_SCRIPT & #hash .~ hash28 b

poolId :: BS.ByteString
poolId = hash28 0x51

pparams :: Word -> S.ProtocolParameters
pparams pv =
  defMessage
    & #keyDeposit .~ 2000000 & #poolDeposit .~ 500000000 & #maxEpoch .~ 18
    & #minPoolCost .~ 170000000 & #drepDeposit .~ 500000000 & #drepActivity .~ 20
    & #committeeMaxTermLength .~ 146
    & #protocolVersion .~ (defMessage & #major .~ fromIntegral pv)

context :: Word -> S.LedgerContext
context pv =
  defMessage
    & #era .~ P.ERA_CONWAY & #epoch .~ 600 & #slot .~ 100000000 & #networkId .~ 1
    & #protocolVersion .~ (defMessage & #major .~ fromIntegral pv)

state :: S.LedgerState
state =
  defMessage
    & #accounts
      .~ [ defMessage & #credential .~ key 0xa1 & #deposit .~ 2000000 & #balance .~ 5000000 & #poolId .~ poolId
         , defMessage & #credential .~ key 0xa2 & #deposit .~ 2000000
         ]
    & #pools
      .~ [ defMessage
             & #poolId .~ poolId & #stage .~ S.POOL_STAGE_CURRENT & #vrfKeyHash .~ BS.replicate 32 0x61
             & #pledge .~ 100 & #cost .~ 170000000 & #margin .~ rat ("1", "20")
             & #rewardAccount .~ key 0xa1 & #owners .~ [hash28 0xa1] & #deposit .~ 500000000
         ]
    & #dreps .~ [defMessage & #credential .~ key 0xd1 & #deposit .~ 500000000 & #expiry .~ 650]
    & #committee
      .~ ( defMessage
             & #threshold .~ rat ("2", "3")
             & #members
               .~ [ defMessage & #cold .~ key 0xc1 & #expiry .~ 700
                  , defMessage & #cold .~ key 0xc2 & #hot .~ key 0xe2 & #expiry .~ 700
                  , defMessage & #cold .~ key 0xc3 & #expiry .~ 700 & #resigned .~ True
                  ]
         )

digest :: S.LedgerDelta -> String
digest d =
  unwords
    [ "A+" <> show (length (d ^. #accountsAdded))
    , "A~" <> show (length (d ^. #accountsChanged))
    , "A-" <> show (length (d ^. #accountsRemoved))
    , "P~" <> show (length (d ^. #poolsChanged))
    , "P-" <> show (length (d ^. #poolsRemoved))
    , "D~" <> show (length (d ^. #drepsChanged))
    , "D-" <> show (length (d ^. #drepsRemoved))
    , "C" <> maybe "0" (const "1") (d ^. #maybe'committeeAfter)
    ]

-- | The failures and the delta digest of a reply, or the harness error.
result :: Reply -> Maybe P.Rejection -> Maybe S.LedgerDelta -> Either HarnessErrorCode ([String], String)
result r rej delta = case r ^. #maybe'error of
  Just e -> Left (e ^. #code)
  Nothing ->
    Right
      ( sort [T.unpack (f ^. #rule) <> "/" <> T.unpack (f ^. #name) | f <- maybe [] (^. #failures) rej]
      , maybe "" digest delta
      )

outcome :: String -> Maybe Int -> Word -> BS.ByteString -> Either HarnessErrorCode ([String], String)
outcome rule idx pv tx = case rule of
  "deleg" ->
    let r = call (#ruleDeleg .~ (defMessage & #context .~ context pv & #pparams .~ pparams pv & #state .~ state & #tx .~ tx & #maybe'certIndex .~ ix :: R.RuleDelegRequest))
     in result r (r ^. #ruleDeleg . #maybe'rejection) (r ^. #ruleDeleg . #maybe'delta)
  "pool" ->
    let r = call (#rulePool .~ (defMessage & #context .~ context pv & #pparams .~ pparams pv & #state .~ state & #tx .~ tx & #maybe'certIndex .~ ix :: R.RulePoolRequest))
     in result r (r ^. #rulePool . #maybe'rejection) (r ^. #rulePool . #maybe'delta)
  "gov" ->
    let r = call (#ruleGovCert .~ (defMessage & #context .~ context pv & #pparams .~ pparams pv & #state .~ state & #tx .~ tx & #maybe'certIndex .~ ix :: R.RuleGovCertRequest))
     in result r (r ^. #ruleGovCert . #maybe'rejection) (r ^. #ruleGovCert . #maybe'delta)
  _ ->
    let r = call (#ruleCerts .~ (defMessage & #context .~ context pv & #pparams .~ pparams pv & #state .~ state & #tx .~ tx :: R.RuleCertsRequest))
     in result r (r ^. #ruleCerts . #maybe'rejection) (r ^. #ruleCerts . #maybe'delta)
  where
    ix = fromIntegral <$> idx

tests :: [Check]
tests =
  [ ( "certs: " <> name <> " (got " <> show got <> ", want " <> show (sort want, delta) <> ")"
    , case got of
        Right (failures, d) -> failures == sort want && (not (null want) || d == delta)
        Left _ -> False
    )
  | (name, rule, idx, pv, txHex, want, delta) <- scenarios
  , let got = outcome rule idx pv (unhex txHex)
  ]
