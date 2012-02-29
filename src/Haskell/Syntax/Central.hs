{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving #-}

module Haskell.Syntax.Central where

import Debug ( debug )

 -- Reuse the functionality from Haskell.Blueprint
import Haskell.Blueprint.Data
import Haskell.Blueprint.Central (splitModules, parseM)
import Haskell.Blueprint.Match (test, Result(Ok))

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (srcLine, srcColumn)

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Control.Monad (unless)

import Language.Haskell.Interpreter
import System.Directory -- (setCurrentDirectory, getTemporaryDirectory)
import Control.Exception (evaluate)
import System.IO.UTF8 as UTF8 {- needed to avoid encoding problems -}
import System.IO.Temp (withTempDirectory)
import qualified System.FilePath as Path
import qualified Autolib.Reporter.IO.Type (reject, inform)

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform


data Haskell_Syntax = Haskell_Syntax deriving Typeable

$(derives [makeReader, makeToDoc] [''Haskell_Syntax])

make_fixed = direct Haskell_Syntax $ Code $ unlines
   [ "{-"
   , " - Put the syntactically *incorrect* Haskell code here."
   , " -}"
   , "-------------"
   , "{-"
   , " - Put a correct version of the code here."
   , " - Its abstract syntax tree will be compared the the one of the submission."
   , " -}"
   ]

instance OrderScore Haskell_Syntax where
    scoringOrder h = Increasing

instance Partial Haskell_Syntax Code Code where
    describe p (Code cfg) = vcat [ text "Benennen und korrigieren Sie die Syntax-Fehler in:"
                                 , nest 4 $ toDoc $ Code $ splitModules cfg !! 0
                                 ]
    initial  p (Code cfg) = Code $ splitModules cfg !! 0

    totalIO p (Code cfg) (Code src) = do
      case parse src of
         ParseFailed loc msg -> do
            let (lpre, lpost ) = splitAt (srcLine loc) $ lines src
                lpre' = reverse $ take 3 $ reverse lpre
                tag   = replicate ( srcColumn loc ) '.' ++ "^"
            rejectIO $ vcat $ map text $ ["Fehler beim Parsen!", ""] ++ lpre' ++ [tag, msg]
         ParseOk (a :: Module) -> do
            result <- liftIO $ do
               tmp <- getTemporaryDirectory
               withTempDirectory tmp "Syn" $ \dirname -> do
                  UTF8.writeFile (dirname Path.</> "Main.hs") src
                  setCurrentDirectory dirname -- will at least mess up relative links
                  result <- runInterpreter $ do
                     reset
                     set [installedModulesInScope := False]
                     loadModules ["Main"]
                     setTopLevelModules ["Main"]
                     return ()
                  evaluate $
                     case result of
                        Left (WontCompile (GhcError msg:_)) -> rejectIO $ text msg       -- compilation error(s), only showing the first one
                        Left err ->                                                      -- unexpected error (our fault)
                             rejectIO $ vcat [ text "Es ist ein unerwarteter Fehler aufgetreten. Dies liegt im Allgemeinen nicht an Ihrer Lösung. Bitte kontaktieren Sie diesbezüglich die zuständigen Personen."
                                             , text "Die Fehlermeldung lautet wie folgt:"
                                             , nest 4 (text (show err))
                                             ]
                        Right () -> do
                           let ParseOk (b :: Module) = parse (splitModules cfg !! 1)
                           case test b a of
                              Ok _ -> informIO $ vcat [ text "Ja, Ihre Einsendung ist richtig."
                                                      , text ""
                                                      , text "Ignorieren Sie die unten angezeigte Bewertung."
                                                      ]
                              _ ->  rejectIO $ vcat [ text "Ihre Lösung ist syntaktisch korrekt."
                                                    , text ""
                                                    , text "Sie entspricht allerdings nicht mehr der Vorgabe. Es sollen nur die mindestens nötigen Veränderungen vorgenommen werden, um den Quelltext syntaktisch korrekt zu machen."
                                                    ]
            result

