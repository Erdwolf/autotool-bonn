{-# LANGUAGE ScopedTypeVariables #-}
module TestHarness (run, syntaxCheck, findTopLevelDeclsOf, contains, ident) where
import Prelude

import Language.Haskell.Exts
import Data.Generics
import Information (filePath)

import System.IO.Unsafe (unsafePerformIO) -- We need to run the tests inside the interpreter
import qualified Test.HUnit as HU
import System.IO.UTF8 as UTF8 {- needed to avoid encoding problems -}

run :: HU.Testable t => t -> (HU.Counts, ShowS)
run testable =
   unsafePerformIO $ HU.runTestText HU.putTextToShowS (HU.test testable)


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
