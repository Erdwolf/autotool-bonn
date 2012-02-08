{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving #-}

module Haskell.Blueprint.Central where

import Debug ( debug )

import Haskell.Blueprint.Data
import Haskell.Blueprint.Match
import Haskell.Blueprint.FileContents (testHelperContents, testHarnessContents)

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (srcLine, srcColumn)

import Language.Haskell.Interpreter
import System.Directory -- (setCurrentDirectory, getTemporaryDirectory)
import System.FilePath (takeBaseName, takeExtension)
import qualified System.FilePath as Path
import Data.List (union, isPrefixOf, groupBy)
import Control.Monad (when, forM)
import Control.Exception (evaluate)
import System.IO.Temp (withTempDirectory)

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import System.IO.UTF8 as UTF8 {- needed to avoid encoding problems -}
import Test.HUnit (Counts(..))

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data Haskell_Blueprint = Haskell_Blueprint deriving Typeable

$(derives [makeReader, makeToDoc] [''Haskell_Blueprint])

make_fixed = direct Haskell_Blueprint $ Code $ unlines
    [ "module Blueprint where"
    , "import Prelude"
    , ""
    , "r :: [a] -> [a]"
    , "r = undefined"
    , ""
    , "{- You can add additional modules separated by lines of three or more dashes: -}"
    , "----------"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "module Test (test) where"
    , "import Prelude"
    , "{-"
    , " - If this module is present, Test.test is used to check the submission."
    , " - Otherwise, Blueprint.test is used."
    , " - "
    , " - 'test' has to be Test.HUnit.Testable, so assertions build with (@?=) will work,"
    , " - as do plain booleans."
    , " - If your testsuite comprises more than a single assertion, you should use a list"
    , " - of named test cases (see (~:)) to provide better feedback."
    , " - "
    , " - Example:"
    , " -}"
    , "import TestHelper (qc)"
    , "import TestHarness (syntaxCheck, contains, ident, findTopLevelDeclsOf)"
    , "import Test.HUnit ((@?=),(~:))"
    , ""
    , "import qualified Blueprint"
    , ""
    , "test ="
    , "  [ \"Test with QuickCheck (random input)\" ~:"
    , "      qc $ \\(xs :: [Int]) ->"
    , "        Blueprint.r xs == Prelude.reverse xs"
    , "  , \"Contains foldl?\" ~:"
    , "      syntaxCheck $ \\mod -> do"
    , "        contains (ident \"foldl\") (findTopLevelDeclsOf \"reverse\" mod) @?= True"
    , "  ]"
    , "----------"
    , "module SomeHiddenModule where"
    , "import Prelude"
    , "{- This module is also not shown to the student but is available to the code -}"
    , "{-"
    , " - Also available are the following modules:"
    , " - "
    , " -     TestHelper   (Import this in Blueprint or Test)"
    , " -        qc :: Testable prop => prop -> Assertion"
    , " -          (Use this instead of 'quickCheck' to turn a property into an HUnit assertion.)"
    , " - "
    , " -     TestHarness  (Import this in Test)"
    , " -        syntaxCheck :: (Module -> Assertion) -> Assertion"
    , " -        findTopLevelDeclsOf :: String -> Module -> [Decl]"
    , " -        contains"
    , " -        ident"
    , " -          (Used to implement syntax checks. Example usage: see above)"
    , " - "
    , " -        allowFailures :: Int -> [Test] -> Assertion"
    , " -          (Detailed output of correct/incorrect Tests in case of failure,"
    , " -          with the option to allow a fixed number of tests to fail.)"
    , " - "
    , " - "
    , " -}"
    ]

instance OrderScore Haskell_Blueprint where
    scoringOrder h = Increasing

instance Verify Haskell_Blueprint Code where
    verify _ ( Code i ) = do
        --Haskell.Blueprint.Central.parse i
        return ()

instance Partial Haskell_Blueprint Code Code where
    describe p (Code i) = toDoc $ Code $ blueprintSegment i
    initial  p (Code i) =         Code $ blueprintSegment i

    partial p ( Code i ) ( Code b ) = do
        mi <- parseM (blueprintSegment i)
        mb <- parseM b
        inform $ text "paßt Ihr Quelltext zum Muster?"
        case Haskell.Blueprint.Match.test mi mb of
            Fail loc ->  reject_parse b loc "Nein"
            Haskell.Blueprint.Match.Ok _ -> inform $ text "Ja."

    totalIO p (Code i) (Code b) = do
        result <- liftIO $ do
            tmp <- getTemporaryDirectory
            withTempDirectory tmp "Blue" $ \dirname -> do
               let ParseOk namedModules = fmap (map (\(k,v) -> (k,if k == "Blueprint" then b else v))) $ withNames (splitModules i)
               files <- namedModules `forM` \(name,contents) -> do
                  let fname = dirname Path.</> (name ++ ".hs")
                  UTF8.writeFile fname contents
                  return fname

               UTF8.writeFile (dirname Path.</> "Information.hs") $ "module Information where filePath = \"Blueprint.hs\""
               let existingModules = map takeBaseName $ filter ((".hs"==).takeExtension) $ filter (`notElem` [".",".."]) files
                   modules = union ["Test"] existingModules
               when (notElem "Test" existingModules) $ do
                  UTF8.writeFile (dirname Path.</> "Test.hs") $ "module Test (test) where\nimport qualified Blueprint (test)\ntest = Blueprint.test"
               UTF8.writeFile (dirname Path.</> "TestHelper.hs") testHelperContents
               UTF8.writeFile (dirname Path.</> "TestHarness.hs") testHarnessContents
               setCurrentDirectory dirname -- will at least mess up relative links
               result <- runInterpreter (interpreter modules)
               evaluate $
                  case result of
                     Right (Counts {errors=0, failures=0},_) -> informIO $ text "ok." -- success
                     Right (_,showS) -> rejectIO $ text $ showS ""                    -- test failure / test error
                     Left (WontCompile (GhcError msg:_)) -> rejectIO $ text msg       -- compilation error(s), only showing the first one
                     Left err ->                                                      -- unexpected error (our fault)
                        rejectIO $ vcat [ text "Es ist ein unerwarteter Fehler aufgetreten. Dies liegt im Allgemeinen nicht an Ihrer Lösung. Bitte kontaktieren Sie diesbezüglich die zuständigen Personen."
                                        , text "Die Fehlermeldung lautet wie folgt:"
                                        , nest 4 (text (show err))
                                        ]
        result
deriving instance Typeable Counts
interpreter modules = do
                  set [languageExtensions := map read ["NPlusKPatterns","ScopedTypeVariables"]]

                  reset -- Make sure nothing is available
                  set [installedModulesInScope := False]

                  loadModules ("TestHarness" : modules)
                  setTopLevelModules modules

                  setImports ["Prelude", "TestHarness", "Test.HUnit.Base"]

                  interpret "TestHarness.run Test.test" (as :: (Counts, ShowS))



blueprintSegment :: String -> String
blueprintSegment i =
   case withNames (splitModules i) of
         ParseFailed _ msg -> error msg
         ParseOk namedModules -> maybe (error "No \"Blueprint\" module in config.") id $ lookup "Blueprint" namedModules

parseM m =
    case parse m of
        ParseOk (a :: Module) -> return a
        ParseFailed loc msg -> reject_parse m loc msg

reject_parse m loc msg =
    let ( lpre, lpost ) = splitAt ( srcLine loc  ) $ lines m
        lpre' = reverse $ take 3 $ reverse lpre
        tag = replicate ( srcColumn loc ) '.' ++ "^"
    in  reject $ vcat ( map text lpre' ++ [ text tag, text msg ] )



splitModules =
   map unlines . splitBy (isPrefixOf "---")  . lines

splitBy p = dropOdd . groupBy (\l r -> not (p l) && not (p r))
  where
   dropOdd [] = []
   dropOdd [x] = [x]
   dropOdd (x:y:xs) = x:dropOdd xs


withNames mods =
   fmap (flip zip mods) $ sequence $ map (fmap moduleName . parseFileContents) mods

moduleName (Module _ (ModuleName name) _ _ _ _ _) = name
