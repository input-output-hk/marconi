{-# LANGUAGE NumericUnderscores #-}

module Spec.Marconi.Sidechain.Utils where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import GHC.Conc.IO qualified as IO
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Helpers qualified as Help
import System.IO qualified as IO

-- | Wait up to a specific duration (seconds) for text file to contain all string values
waitForLog :: FilePath -> Int -> [String] -> IO Bool
waitForLog logFile t expectedStrings = go (t + 1) (t + 1)
  where
    go t1 t2
      | t1 <= 0 = return False
      | t1 == t2 = do
          IO.threadDelay 100_000 -- wait 100ms before initial check
          go (t1 - 1) t2
      | otherwise = do
          contents <- IO.readFile logFile
          if all (`isInfixOf` contents) expectedStrings
            then return True
            else do
              IO.threadDelay 1_000_000 -- wait 1 second after each subsequent check
              waitForLog logFile (t1 - 1) expectedStrings

integrationTest :: H.Integration () -> H.Property
integrationTest =
  H.withShrinks 0
    . H.withTests 1
    . H.propertyOnce
    . (liftIO Help.setDarwinTmpdir >>)
    . H.runFinallies
