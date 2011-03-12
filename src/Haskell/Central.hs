{-# LANGUAGE TemplateHaskell #-}

module Haskell.Central where

import Haskell.Data

import qualified Language.Haskell.Interpreter as I

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Inter.Types
import Inter.Quiz

data Haskell = Haskell
$(derives [makeReader, makeToDoc] [''Haskell])

instance Partial Haskell Instance Code where
    describe p i = vcat
        [ text "Gesucht ist ein Haskell-Ausdruck,"
        , text "der diese Spezifikation erfüllt:"
        , nest 4 $ toDoc $ specification i
        ]
    initial p i = code_example
    total p i b = do
        