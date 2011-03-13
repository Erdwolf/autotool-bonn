{-# LANGUAGE TemplateHaskell #-}

module Haskell.Central where

import Haskell.Data

import qualified Mueval.ArgsParse as M
import qualified Mueval.Interpreter
import qualified Language.Haskell.Interpreter as I
import qualified Mueval.Context

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter.IO.Type
import Inter.Types
import Inter.Quiz
import Data.Typeable

import Control.Monad.IO.Class
import Test.SmallCheck

data Haskell = Haskell deriving Typeable

$(derives [makeReader, makeToDoc] [''Haskell])

instance OrderScore Haskell where
    scoringOrder h = Increasing

instance Partial Haskell Instance Code where
    describe p i = vcat
        [ text "Gesucht ist ein Haskell-Ausdruck,"
        , text "der diese Spezifikation erfüllt:"
        , nest 4 $ toDoc $ specification i
        , text "Dabei gelten die Definitionen:"
        , nest 4 $ toDoc $ global i 
        ]
    initial p i = code_example
    totalIO p i b = do
        let Code glob = global i
            Code fun = specification i
            Code arg = b
        r <- liftIO $ do 
            -- FIXME use unique tmp file name, clean up after use
            -- OR: have Mueval read from String instead of from File
            -- writeFile "/tmp/autotool-mueval.hs" glob
            I.runInterpreter $ Mueval.Interpreter.interpreter $ M.Options
                    { M.timeLimit = 1
                    , M.modules = Just [ "Prelude", "Haskell.SmallCheck", "Haskell.Data" ]
                    , M.expression = show $ hsep
                          [ text "Haskell.SmallCheck.run" 
                          , parens ( toDoc i )
                          , parens ( text fun ) 
                          , parens ( text arg )
                          ]
                    , M.loadFile =  ""
                    , M.user = "" -- WHAT?
                    , M.printType = True -- printed to where?
                    , M.extensions = False
                    , M.namedExtensions = []
                    , M.noImports = False
                    , M.rLimits = True
                    }

        case r of
            Left err -> reject $ text $ show err
            Right ( e, et, val ) -> do
                inform $ vcat
                       [ text "expression" </> text e
                       , text "type" </> text et
                       , text "value" </> text val
                       ]       
                assert ( et == "[[String]]" ) $ text "richtiger Typ?"
                let argvs = read val :: [[String]]
                if null argvs 
                    then  inform $ text "alle Tests erfolgreich"
                    else reject $ vcat
                            [ text "Fehler für diese Argumente:"
                            , nest 4 $ vcat $ do
                                 argv <- argvs
                                 return $ hsep $ map text argv
                            ]
                


make_fixed = direct Haskell instance_example

        