module Haskell.Blueprint.TestHelperContents (testHelperContents) where

testHelperContents = "{-# LANGUAGE ScopedTypeVariables #-}
module TestHelper
   ( syntaxCheck
   , findTopLevelDeclsOf
   , qc
   , ok
   , run
   )
where

import Language.Haskell.Exts
import Information (filePath)
import Prelude

import Test.QuickCheck
import Test.HUnit

import System.IO.Unsafe (unsafePerformIO) -- We need to run the tests inside the interpreter
import Test.HUnit.Lang (performTestCase)

run action = unsafePerformIO $ performTestCase action



syntaxCheck check = do
   ParseOk mod <- parseFile filePath
   check mod
   --putStrLn desc
   --putStrLn (\"   \" ++ if check mod then \"Ja.\" else \"Nein!\")

findTopLevelDeclsOf name (Module _ _ _ _ _ _ decls) = filter matches decls
   where
     matches (PatBind _ (PVar (Ident name')) _ _ _) | name == name' = True
     matches (FunBind (Match _ (Ident name') _ _ _ _:_)) | name == name' = True
     matches _ = False

qc test = do
   result <- quickCheckResult test
   case result of
      Success _ _ _ -> return ()
      _             -> assertFailure (show result)

ok = putStrLn \"ok\""
