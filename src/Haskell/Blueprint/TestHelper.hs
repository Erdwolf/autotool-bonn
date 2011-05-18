module TestHelper (qc) where
import Prelude
import Data.List

import Test.QuickCheck as QC
import Test.HUnit as HU


qc :: QC.Testable prop => prop -> HU.Assertion
qc test = do
   result <- quickCheckWithResult (stdArgs {chatty=False}) test
   case result of
      Success _ _ _ -> return ()
      _  -> assertFailure $ output result
