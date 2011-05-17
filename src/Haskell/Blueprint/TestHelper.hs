module TestHelper (qc) where
import Prelude

import Test.QuickCheck as QC
import Test.HUnit as HU

qc :: QC.Testable prop => prop -> HU.Assertion
qc test = do
   result <- quickCheckResult test
   case result of
      Success _ _ _ -> return ()
      _             -> assertFailure (show result)
