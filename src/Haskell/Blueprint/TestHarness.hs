{-# LANGUAGE ScopedTypeVariables #-}
module TestHarness
   ( run
   , syntaxCheck, findTopLevelDeclsOf, contains, ident
   , allowFailures
   ) where
import Prelude

import Language.Haskell.Exts
import Data.Generics
import Information (filePath)

import System.IO.Unsafe (unsafePerformIO) -- We need to run the tests inside the interpreter
import qualified Test.HUnit as HU
import System.IO.UTF8 as UTF8 {- needed to avoid encoding problems -}

import Test.HUnit (Test(TestLabel,TestCase), assertFailure)
import Control.Exception (try, SomeException)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Control.Monad (when)


{- Function called by the interpreter, getting the tests to run as the argument. -}

run :: HU.Testable t => t -> (HU.Counts, ShowS)
run testable =
   unsafePerformIO $ HU.runTestText HU.putTextToShowS (HU.test testable)


{- Common test patterns -}

allowFailures :: Int -> [HU.Test] -> HU.Assertion
-- Detailed output of correct/incorrect Tests in case of failure
-- with the option to allow a fixed number of tests to fail.
allowFailures limit testCases = do
   (successes, failures) <- collectResults [(l,a) | TestLabel l (TestCase a) <- testCases]
   when (length failures > limit) $ do
      assertFailure $ intercalate "\n\n"
         [ "Es sind zu viele Fehler aufgetreten. Es gibt immer noch Funktionen, die sich wie gefordert implementieren lassen."
         , "Korrekt sind: " ++ intercalate ", " (map show successes)
         , "Folgende Fehler sind aufgetreten:"
         , intercalate "\n---\n" $ [ "# " ++ l ++ ":\n" ++ show f | (l,f) <- failures ]
         ]
 where
   collectResults = fmap groupIntoTwoLists
                  . fmap concat
                  . sequence
                  . map (\(l, action) -> either (\(e :: SomeException) -> [(l,Just e)]) (\_ -> [(l,Nothing)]) <$> try action)

   groupIntoTwoLists :: [(a,Maybe b)] -> ([a],[(a,b)])
   groupIntoTwoLists = foldr (\(a,mb) (ns,js) -> maybe (a:ns,js) (\b -> (ns,(a,b):js)) mb) ([],[])



{- Syntax predicates -}

contains pred = everything (||) (mkQ False pred)

ident name (Ident name')  | name == name'           = True
ident name (Symbol name') | name == "("++name'++")" = True
ident _    _                                        = False


syntaxCheck check = do
   contents <- UTF8.readFile filePath
   let ParseOk mod = parseFileContents contents
   check mod

findTopLevelDeclsOf name (Module _ _ _ _ _ _ decls) = filter matches decls
   where
     matches (PatBind _ (PVar (Ident name')) _ _ _) | name == name' = True
     matches (FunBind (Match _ (Ident name') _ _ _ _:_)) | name == name' = True
     matches _ = False


{-
-- Usage: let [PatBind _ _ _ rhs _] = decls
--        leftmostIdentifier rhs @?= "foldr"
--
leftmostIdentifier = maybe (error "Declaration does not contain identifier!") id . something (mkQ Nothing matchIdent)

matchIdent (Ident name) = Just name
matchIdent _            = Nothing
-}
