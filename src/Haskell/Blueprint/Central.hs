{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables, OverlappingInstances, DeriveDataTypeable #-}

module Haskell.Blueprint.Central where

import Debug ( debug )

import Haskell.Blueprint.Data
import Haskell.Blueprint.Match

import Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.SrcLoc as S

import qualified Mueval.ArgsParse as M
import qualified Mueval.Interpreter
import qualified Language.Haskell.Interpreter as Hint
import qualified Mueval.Context

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader hiding (parse)
import Autolib.Reporter.IO.Type
import qualified Autolib.Reporter as R
import Inter.Types
import Inter.Quiz
import Data.Typeable (Typeable)

import qualified Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans (MonadIO)
import Test.SmallCheck
import System.IO.Temp
import System.IO.UTF8
import System.Random ( randomRIO )
import qualified System.FilePath as Path
import System.FilePath.Posix (takeFileName)
import qualified System.IO.Strict
import qualified System.IO
import qualified System.Directory
import System.Directory (copyFile, makeRelativeToCurrentDirectory, removeFile, setCurrentDirectory)
import qualified System.Posix.Directory as SPD

import Data.List
import Control.Monad


--import Control.Monad (guard,mplus,unless,when)
import qualified Control.Exception.Extensible as E (evaluate,catch,SomeException(..))

import Language.Haskell.Interpreter (eval, set, reset, setImportsQ, loadModules, liftIO,
                                     installedModulesInScope, languageExtensions,
                                     typeOf, setTopLevelModules, runInterpreter, glasgowExtensions,
                                     OptionVal(..), Interpreter,
                                     InterpreterError(..),GhcError(..))

import qualified Mueval.Context (qualifiedModules)


data Haskell_Blueprint = Haskell_Blueprint deriving Typeable

$(derives [makeReader, makeToDoc] [''Haskell_Blueprint])

instance OrderScore Haskell_Blueprint where
    scoringOrder h = Increasing

instance Verify Haskell_Blueprint Code where
    verify _ ( Code i ) = do
        --Haskell.Blueprint.Central.parse i
        return ()

instance Partial Haskell_Blueprint Code Code where
    describe p i = vcat
        [ text "Vervollständigen Sie das Haskell-Programm."
        , text "Ersetzen Sie jedes 'undefined',"
        , text "so daß die Tests erfolgreich sind."
        , nest 4 $ toDoc i
        ]
    initial p (Code i) = Code $ blueprintSegment i

    partial p ( Code i ) ( Code b ) = do
        mi <- parseM (blueprintSegment i)
        mb <- parseM b
        --R.inform $ text $ show mi
        --R.inform $ text $ show mb
        R.inform $ text "paßt Ihr Quelltext zum Muster?"
        case Haskell.Blueprint.Match.test mi mb of
            Fail loc ->  reject_parse b loc "Nein"
            Haskell.Blueprint.Match.Ok _ -> R.inform $ text "Ja."

    totalIO p (Code i) (Code b) = do
        r <- liftIO $ do
            tmp <- System.Directory.getTemporaryDirectory
            withTempDirectory tmp "Blue" $ \dirname -> do
               let ParseOk namedModules = fmap (map (\(k,v) -> (k,if k == "Blueprint" then b else v))) $ withNames (splitModules i)
               files <- namedModules `forM` \(name,contents) -> do
                  let fname = dirname Path.</> (name ++ ".hs")
                  System.IO.UTF8.writeFile fname contents
                  return fname
               flip Control.Exception.catch (\( e :: Control.Exception.SomeException ) -> do
                        debug $ "interpreter got exception " ++ show e
                        return $ Left $ Hint.UnknownError ( show e )) $ do
                  Hint.runInterpreter $ do
                        let exts = []
                        let modules = ["Prelude"]
                        let expr = "test"

                        set [languageExtensions := (glasgowExtensions ++ map read exts)]

                        reset -- Make sure nothing is available
                        set [installedModulesInScope := False]
                        liftIO $ setCurrentDirectory dirname -- will at least mess up relative links

                        loadModules [ name ++ ".hs" | (name,_) <- namedModules ]
                        setTopLevelModules (map fst namedModules)

                        let unqualModules = zip modules (repeat Nothing)
                        setImportsQ (unqualModules ++ Mueval.Context.qualifiedModules)

                        -- we don't check if the expression typechecks
                        -- this way we get an "InterpreterError" we can display
                        etype <- typeOf expr
                        result <- eval "test"

                        return (expr, etype, result)
        case r of
            Left (Hint.WontCompile err) -> reject $ vcat $ map (text . Hint.errMsg) err
            Left err -> reject $ text $ show err
            Right ( e, et, val ) -> do
                inform $ vcat
                       [ text "expression" </> text e
                       , text "type" </> text et
                       ]
                assert ( et == "Bool" ) $ text "richtiger Typ?"

                v <- liftIO  ( ( do Control.Exception.evaluate val ; return ( Right val ) )
                  `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
                        return $ Left ( show e ) )

                case v of
                     Right val -> assert ( val == "True" ) $ text "richtiger Wert?"
                     Left ex -> reject $ text "Exception" </> text ex


make_fixed = direct Haskell_Blueprint code_example

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
    let ( lpre, lpost ) = splitAt ( S.srcLine loc  ) $ lines m
        lpre' = reverse $ take 3 $ reverse lpre
        tag = replicate ( S.srcColumn loc ) '.' ++ "^"
    in  R.reject $ vcat ( map text lpre' ++ [ text tag, text msg ] )



splitModules =
   map unlines . splitBy (isPrefixOf "---")  . lines

splitBy p = dropOdd . groupBy (\l r -> not (p l) && not (p r))

dropOdd [] = []
dropOdd [x] = [x]
dropOdd (x:y:xs) = x:dropOdd xs


withNames mods =
   fmap (flip zip mods) $ sequence $ map (fmap moduleName . parseFileContents) mods

moduleName (Module _ (ModuleName name) _ _ _ _ _) = name

