{-# LANGUAGE TemplateHaskell #-}
module Haskell.Blueprint.FileContents
   ( testHelperContents
   , testHarnessContents
   ) where

import Language.Haskell.TH.All (lift)
import System.IO.Unsafe (unsafePerformIO)

testHelperContents  = $(lift $ unsafePerformIO $ readFile "Haskell/Blueprint/TestHelper.hs")
testHarnessContents = $(lift $ unsafePerformIO $ readFile "Haskell/Blueprint/TestHarness.hs")
