module Test.Main where

import Prelude

import BridgeTests as BridgeTests
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import FileEvents (FILE)
import GistsTests as GistsTests
import MainFrameTests as MainFrameTests
import Node.FS (FS)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import TypesTests as TypesTests

foreign import forDeps :: forall a. Eff a Unit

main :: forall eff. Eff (testOutput :: TESTOUTPUT, file :: FILE, exception :: EXCEPTION, fs :: FS, avar :: AVAR, console :: CONSOLE, random :: RANDOM | eff) Unit
main = runTest do
  BridgeTests.all
  TypesTests.all
  GistsTests.all
  MainFrameTests.all
