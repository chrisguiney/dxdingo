{-# LANGUAGE LambdaCase #-}

-- | dxnode-hs: the Haskell reference node behind the dxdingo stdin/stdout
-- transport.
module Main (main) where

import Dxdingo.Dispatch (handleBytes)
import Dxdingo.Wire (readFrame, writeFrame)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)
  loop
  where
    loop =
      readFrame stdin >>= \case
        Right Nothing -> pure ()
        Right (Just call) -> writeFrame stdout (handleBytes call) >> loop
        Left err -> hPutStrLn stderr ("dxnode-hs: " <> show err) >> exitFailure
