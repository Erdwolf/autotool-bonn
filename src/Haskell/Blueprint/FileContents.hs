{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}
module Haskell.Blueprint.FileContents
   ( testHelperContents
   , testHarnessContents
   ) where

import Language.Haskell.TH
import System.FilePath

-- {- Not defined as a function here because of the stage restriction -}
-- file name = do
--    Loc path _ _ _ _ <- location
--    content <- runIO $ readFile $ dropFileName path </> name
--    stringE content

testHelperContents  = $(location >>= \(Loc path _ _ _ _) -> (runIO $ readFile $ dropFileName path </> "TestHelper.hs") >>= stringE)
testHarnessContents = $(location >>= \(Loc path _ _ _ _) -> (runIO $ readFile $ dropFileName path </> "TestHarness.hs") >>= stringE)
