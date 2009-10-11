{-# language TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Exp.Shortest_Missing where

import Data.DeriveTH
import Data.Derive.ToDoc
import Data.Derive.Reader

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

import Autolib.Exp.Example
import Autolib.NFA.Minus
import Autolib.NFA.Basic
import Autolib.NFA.Shortest
import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.Size
import Autolib.Dot ( peng )

import Exp.Property
import Exp.Test

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Informed

data Exp_Shortest_Missing = Exp_Shortest_Missing
    deriving ( Eq, Ord, Show, Read, Typeable )

data Config =
     Config { properties :: [ Property Char ]
            , minimal_length_of_missing_word :: Int
            }
    deriving Typeable

default_config :: Config
default_config = Config
          { properties = [ Simple, Max_Size 10, Alphabet $ mkSet "01" ]
          , minimal_length_of_missing_word = 20
          }

$(derive makeToDoc ''Config)
$(derive makeReader ''Config)

instance C.Partial Exp_Shortest_Missing Config ( RX Char )  where

    report _ conf = do
        inform $ vcat
            [ text "Gesucht ist ein regulärer Ausdruck X"
            , text "mit diesen Eigenschaften:"
            , nest 4 $ toDoc $ properties conf
            , text "L(X) soll nicht alle Wörter aus Sigma^* enthalten,"
            , text "jedes fehlende Wort soll wenigstens die Länge" 
                       <+> toDoc (minimal_length_of_missing_word conf)
                       <+> text "haben."
            ]

    initial _ conf =
        let [ alpha ] = do Alphabet a <- properties conf ; return a
	in  Autolib.Exp.Example.example alpha

    partial _ conf exp = do
        inform $ text "Sind alle Eigenschaften erfüllt?"
        nested 4 $ mapM_ ( flip test exp ) $ properties conf
        inform $ text "Ja."

    total _ conf exp = do
        let [ alpha ] = do Alphabet a <- properties conf ; return a
        let missing = minus ( sigmastar $ setToList alpha )
                            ( inter ( std_sigma $ setToList alpha ) exp )
        case some_shortest missing of
            [] -> reject $ text "Der Ausdruck ist äquivalent zu Sigma^*."
            w : _ -> do
                inform $ vcat [ text "Ein kürzestes fehlendes Wort ist"
                              , toDoc w 
                              , text "mit Länge" <+> toDoc ( length w )
                              ]
                when ( length w < minimal_length_of_missing_word conf ) 
                     $ reject $ text "Das ist zu kurz."

instance C.Measure Exp_Shortest_Missing Config ( RX Char )
  where
    measure _ _ exp = fromIntegral $ size exp

make :: Make
make = direct Exp_Shortest_Missing default_config


