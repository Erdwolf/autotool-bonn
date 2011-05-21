{-# LANGUAGE ScopedTypeVariables #-}
module TestHelper (qc, mustFail) where
import Prelude
import Data.List

import Test.QuickCheck as QC
import Test.HUnit as HU

import Test.QuickCheck.Monadic (monadicIO, run)
import Control.Exception (try, evaluate, SomeException)


qc :: QC.Testable prop => prop -> HU.Assertion
qc test = do
   result <- quickCheckWithResult (stdArgs {chatty=False, maxSuccess=1000}) test
   case result of
      Success _ _ _ -> return ()
      _  -> assertFailure $ output result

mustFail :: a -> String -> Property
mustFail x msg =
   monadicIO $ run $ do
      resultOrError <- try (evaluate x)
      case resultOrError of
         Left (_::SomeException) -> return () -- expected failure occured.
         Right _ -> error msg

