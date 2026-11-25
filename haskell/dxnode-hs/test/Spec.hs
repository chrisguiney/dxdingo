module Main (main) where

import Control.Monad (unless)
import qualified Test.Wire
import qualified Test.Codec
import qualified Test.Rule
import qualified Test.Fee
import qualified Test.Plutus
import qualified Test.Rewards
import qualified Test.Stake
import qualified Test.Epoch
import qualified Test.Gov
import qualified Test.Praos
import qualified Test.Chainsel
import qualified Test.Time
import qualified Test.Byron
import qualified Test.Crypto
import qualified Test.Net
import System.Exit (exitFailure)

main :: IO ()
main = do
  let failed = [name | (name, ok) <- checks, not ok]
  mapM_ (putStrLn . ("FAIL: " <>)) failed
  unless (null failed) exitFailure
  putStrLn ("all " <> show (length checks) <> " tests passed")
  where
    checks =
      concat
      [ Test.Wire.tests
      , Test.Codec.tests
      , Test.Rule.tests
      , Test.Fee.tests
      , Test.Plutus.tests
      , Test.Rewards.tests
      , Test.Stake.tests
      , Test.Epoch.tests
      , Test.Gov.tests
      , Test.Praos.tests
      , Test.Chainsel.tests
      , Test.Time.tests
      , Test.Byron.tests
      , Test.Crypto.tests
      , Test.Net.tests
      ]
