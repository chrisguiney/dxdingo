{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The composed ledger rules (PPUP, GOV, LEDGER, BBODY). The same scenarios
-- run in dxnode-go (cmd/dxnode-go/rule_ledger_*_test.go): hand-built
-- transactions over small ledger states, expecting the constructors the
-- reference reports and a digest of the accepted delta.
module Test.RuleLedger (tests) where

import qualified Data.ByteString as BS
import Data.List (sort)
import Data.ProtoLens (decodeMessage)
import Data.ProtoLens.Labels ()
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Numeric (readHex)
import Proto.Dxdingo.V1.Envelope (Call, HarnessErrorCode, Reply)
import qualified Proto.Dxdingo.V1.State as S
import qualified Proto.Dxdingo.V1.Types as P
import Data.List (isInfixOf)
import Dxdingo.Rule.Ledger (babbageLedgerDirectFailures, legacyBbodyDirectFailures, ledgersDirectFailures, ppupDirectFailures, reencodeBabbageOutput, withdrawalsDelegatedDirect)
import Test.RuleLedgerData (divergenceScenarios, legacyDivergenceScenarios, scenarios)
import Test.Util

unhex :: String -> BS.ByteString
unhex (a : b : rest) = BS.cons (fst (head (readHex [a, b]))) (unhex rest)
unhex _ = BS.empty

-- | The digest 'Digest' in internal/ledgerfixture computes: the non-empty
-- parts of a delta.
digest :: S.LedgerDelta -> String
digest d =
  unwords
    [ k <> "=" <> show v
    | (k, v) <-
        [ ("U+", length (d ^. #utxoAdded))
        , ("U-", length (d ^. #utxoRemoved))
        , ("A+", length (d ^. #accountsAdded))
        , ("A-", length (d ^. #accountsRemoved))
        , ("A~", length (d ^. #accountsChanged))
        , ("P~", length (d ^. #poolsChanged))
        , ("P-", length (d ^. #poolsRemoved))
        , ("D~", length (d ^. #drepsChanged))
        , ("D-", length (d ^. #drepsRemoved))
        , ("C", maybe 0 (const 1) (d ^. #maybe'committeeAfter))
        , ("G+", length (d ^. #proposalsAdded))
        , ("G-", length (d ^. #proposalsRemoved))
        , ("V+", length (d ^. #votesAdded))
        , ("PP", length (d ^. #pparamProposalsChanged))
        , ("res", fromIntegral (d ^. #reservesDelta))
        , ("tre", fromIntegral (d ^. #treasuryDelta))
        , ("fee", fromIntegral (d ^. #feesDelta))
        , ("dep", fromIntegral (d ^. #depositsDelta))
        , ("don", fromIntegral (d ^. #donationsDelta))
        , ("enact", maybe 0 (const 1) (d ^. #maybe'enactStateAfter))
        , ("pparams", maybe 0 (const 1) (d ^. #maybe'pparamsAfter))
        ]
    , v /= (0 :: Int)
    ]

-- | The failures and the delta digest of a reply, or the harness error.
result :: Reply -> Maybe P.Rejection -> Maybe S.LedgerDelta -> Either (HarnessErrorCode, T.Text) ([String], String)
result r rej delta = case r ^. #maybe'error of
  Just e -> Left (e ^. #code, e ^. #message)
  Nothing ->
    Right
      ( sort [T.unpack (f ^. #rule) <> "/" <> T.unpack (f ^. #name) | f <- maybe [] (^. #failures) rej]
      , maybe "" digest delta
      )

outcome :: String -> BS.ByteString -> Either (HarnessErrorCode, T.Text) ([String], String)
outcome fn raw = case decodeMessage raw of
  Left _ -> Right (["undecodable scenario"], "")
  Right c -> case fn of
    "rule_ppup" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #rulePpup . #maybe'rejection) (r ^. #rulePpup . #maybe'delta)
    "rule_gov" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #ruleGov . #maybe'rejection) (r ^. #ruleGov . #maybe'delta)
    "rule_ledger" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #ruleLedger . #maybe'rejection) (r ^. #ruleLedger . #maybe'delta)
    "rule_deleg" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #ruleDeleg . #maybe'rejection) (r ^. #ruleDeleg . #maybe'delta)
    "rule_pool" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #rulePool . #maybe'rejection) (r ^. #rulePool . #maybe'delta)
    "rule_certs" -> let r = call (const (c & #id .~ 1)) in result r (r ^. #ruleCerts . #maybe'rejection) (r ^. #ruleCerts . #maybe'delta)
    _ ->
      let r = call (const (c & #id .~ 1))
          b = r ^. #ruleBlockBody
          totals = "ex=" <> show (b ^. #exMemTotal) <> "/" <> show (b ^. #exStepsTotal) <> " ref=" <> show (b ^. #refScriptBytes)
       in fmap (\(fs, d) -> (fs, unwords (filter (not . null) [d, totals]))) (result r (b ^. #maybe'rejection) (b ^. #maybe'delta))

tests :: [Check]
tests = divergences ++ blockDivergences ++ legacyDivergences ++
  [ ( "ledger rules: " <> name <> " (got " <> show got <> ", want " <> show (sort want, delta) <> ")"
    , case got of
        Right (failures, d) -> failures == sort want && (not (null want) || d == delta) && (fn /= "rule_block_body" || d == delta)
        Left _ -> False
    )
  | (name, fn, callHex, want, delta) <- scenarios
  , let got = outcome fn (unhex callHex)
  ]

-- | The reference's own answers on the two points where dxnode-go differs,
-- from the library functions with no adapter in between.
divergences :: [Check]
divergences =
  [ ( "divergence: PPUP reports PPUpdateWrongEpoch without any witness"
    , ppupDirectFailures == Right ["PPUpdateWrongEpoch"]
    )
  , ( "divergence: a withdrawal from an unregistered account also fails the DRep delegation check"
    , case [c | (name, "rule_ledger", c, _, _) <- scenarios, name == "ledger withdrawal drains the account"] of
        (c : _) -> case (decodeMessage (unhex c) :: Either String Call) of
          Right msg -> case msg ^. #maybe'ruleLedger of
            Just rq -> case withdrawalsDelegatedDirect (rq ^. #tx) of
              Right out -> "ConwayWdrlNotDelegatedToDRep" `isInfixOf` out
              Left _ -> False
            Nothing -> False
          Left _ -> False
        [] -> False
    )
  ]

-- | The reference's answers on the BBODY scenarios where dxnode-go differs:
-- through the adapter, and from the LEDGERS rule alone.
blockDivergences :: [Check]
blockDivergences =
  concat
    [ [ ( "divergence: " <> name <> " (reference answer through the adapter)"
        , outcome fn (unhex callHex) == Right (sort want, delta)
        )
      , ( "divergence: " <> name <> " (LEDGERS alone reports every failing transaction)"
        , case (decodeMessage (unhex callHex) :: Either String Call) of
            Right msg -> case msg ^. #maybe'ruleBlockBody of
              Just rq -> ledgersDirectFailures rq == Right (sort [T.pack (drop 1 (dropWhile (/= '/') w)) | w <- want])
              Nothing -> False
            Left _ -> False
        )
      ]
    | (name, fn, callHex, want, delta) <- divergenceScenarios
    ]

-- | The reference's answers on the mainnet Babbage LEDGER cases where dxnode-go
-- differs: through the adapter, from LEDGER alone for the unresolved input,
-- and from the reference's output encoder for the created outputs.
legacyDivergences :: [Check]
legacyDivergences =
  concat
    [ [ ( "divergence: " <> name <> " (reference answer through the adapter)"
        , outcome fn (unhex callHex) == Right (sort want, delta)
        )
      ]
        ++ [ ( "divergence: " <> name <> " (BBODY alone accepts the block)"
             , case (decodeMessage (unhex callHex) :: Either String Call) of
                 Right msg -> case msg ^. #maybe'ruleBlockBody of
                   Just rq -> legacyBbodyDirectFailures rq == Right []
                   Nothing -> False
                 Left _ -> False
             )
           | fn == "rule_block_body"
           ]
        ++ [ ( "divergence: " <> name <> " (LEDGER alone reports no MissingRedeemers for an unresolved input)"
             , case (decodeMessage (unhex callHex) :: Either String Call) of
                 Right msg -> case msg ^. #maybe'ruleLedger of
                   Just rq -> babbageLedgerDirectFailures rq == Right (sort [T.pack (drop 1 (dropWhile (/= '/') w)) | w <- want])
                   Nothing -> False
                 Left _ -> False
             )
           | fn == "rule_ledger" && not (null want)
           ]
    | (name, fn, callHex, want, delta) <- legacyDivergenceScenarios
    ]
    ++ [ ( "divergence: the reference re-encodes asset names in byte order"
         , reencodeBabbageOutput shortlexOutput == Right sortedOutput && shortlexOutput /= sortedOutput
         )
       ]

-- | A Babbage output holding two assets of one policy, named "z" and "aa":
-- the names in canonical CBOR order (shortest first), and in byte order.
shortlexOutput, sortedOutput :: BS.ByteString
shortlexOutput = outputWith [(BS.pack [0x41, 0x7a]), (BS.pack [0x42, 0x61, 0x61])]
sortedOutput = outputWith [(BS.pack [0x42, 0x61, 0x61]), (BS.pack [0x41, 0x7a])]

outputWith :: [BS.ByteString] -> BS.ByteString
outputWith names =
  BS.concat
    ( [BS.pack [0x82, 0x58, 0x1d, 0x61], BS.replicate 28 0x77, BS.pack [0x82, 0x1a, 0x00, 0x2d, 0xc6, 0xc0, 0xa1, 0x58, 0x1c], BS.replicate 28 0x11, BS.pack [0xa2]]
        ++ concatMap (\n -> [n, BS.pack [0x01]]) names
    )
