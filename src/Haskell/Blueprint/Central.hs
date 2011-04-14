{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-} 
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-} 
{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-} 

module Haskell.Blueprint.Central where

import Debug ( debug )

import Haskell.Blueprint.Data
import Haskell.Blueprint.Match 

import qualified Language.Haskell.Exts.Parser as P
-- import qualified Language.Haskell.Syntax as S
import qualified Language.Haskell.Exts.SrcLoc as S

import qualified Mueval.ArgsParse as M
import qualified Mueval.Interpreter
import qualified Language.Haskell.Interpreter as I
import qualified Mueval.Context

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter.IO.Type
import qualified Autolib.Reporter as R
import Inter.Types
import Inter.Quiz
import Data.Typeable

import qualified Control.Exception
import Control.Monad.IO.Class
import Test.SmallCheck
import System.IO.Temp
import System.IO.UTF8
import System.Random ( randomRIO )
import qualified System.IO.Strict
import qualified System.IO
import qualified System.Directory
import qualified System.Posix.Directory as SPD

data Haskell_Blueprint = Haskell_Blueprint deriving Typeable

$(derives [makeReader, makeToDoc] [''Haskell_Blueprint])

instance OrderScore Haskell_Blueprint where
    scoringOrder h = Increasing

instance Verify Haskell_Blueprint Code where
    verify _ ( Code i ) = do
        Haskell.Blueprint.Central.parse i
        return ()

instance Partial Haskell_Blueprint Code Code where
    describe p i = vcat
        [ text "Vervollständigen Sie das Haskell-Programm."
        , text "Ersetzen Sie jedes 'undefined',"
        , text "so daß die Tests erfolgreich sind."
        , nest 4 $ toDoc i
        ]
    initial p i = i

    partial p ( Code i ) ( Code b ) = do
        mi <- Haskell.Blueprint.Central.parse i
        mb <- Haskell.Blueprint.Central.parse b
        R.inform $ text "paßt Ihr Quelltext zum Muster?"
        case Haskell.Blueprint.Match.test mi mb of
            Fail loc ->  reject_parse b loc "Nein"
            Haskell.Blueprint.Match.Ok _ -> R.inform $ text "Ja."

    totalIO p (Code i) (Code b) = do
        r <- liftIO $ withTempDirectory "/tmp" "Blue" $ \ d -> do
            let f = d ++ "/" ++ "Blueprint.hs"
            System.IO.UTF8.writeFile f b
            ( I.runInterpreter $ Mueval.Interpreter.interpreter $ M.Options
                    { M.timeLimit = 1
                    , M.modules = Just [ "Prelude" ]
                    , M.expression = "test"
                    , M.loadFile =  f
                    , M.user = "" -- WHAT?
                    , M.printType = False -- printed to where?
                    , M.extensions = False
                    , M.namedExtensions = []
                    , M.noImports = False
-- http://httpd.apache.org/docs/1.3/misc/FAQ-F.html#premature-script-headers 
-- Another cause for the "premature end of script headers" message 
-- are the RLimitCPU and RLimitMEM directives. 
-- You may get the message if the CGI script was killed due to a resource limit.
                    , M.rLimits = False
                    } ) 
              `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
                        debug $ "interpreter got exception " ++ show e
                        return $ Left $ I.UnknownError ( show e )
            -- debug $ "after runInterpreter"
            -- length ( show r ) `seq` 
            -- return r
            -- System.Directory.removeFile f
            -- return r
        -- liftIO $ debug "outside runInterpreter"
        case r of
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

parse m = 
    case P.parseModule m of
        P.ParseOk a -> return a
        P.ParseFailed loc msg -> reject_parse m loc msg

reject_parse m loc msg =
    let ( lpre, lpost ) = splitAt ( S.srcLine loc  ) $ lines m
        lpre' = reverse $ take 3 $ reverse lpre
        tag = replicate ( S.srcColumn loc ) '.' ++ "^"
    in  R.reject $ vcat ( map text lpre' ++ [ text tag, text msg ] )