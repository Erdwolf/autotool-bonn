module Convert.Main where

--  $Id$

import Convert.Type

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

import Autolib.NFA
import Autolib.NFA.Example
import Autolib.NFA.Eq
import Autolib.NFA.Restrict
import Autolib.Size

import NFA.Property
import NFA.Test
import Convert.Input

data Convert_To_NFA = Convert_To_NFA 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Convert_To_NFA 
                   ( Convert , [ Property Char ] ) 
		   ( NFA Char Int ) 
  where

    describe Convert_To_NFA ( from, props ) = vcat
        [ text "Gesucht ist ein endlicher Automat,"
	, text "der die Sprache" <+> form from <+> text "akzeptiert"
	, text "und folgende Eigenschaften hat:"
	, nest 4 $ toDoc props
        ]

    initial Convert_To_NFA ( from, props ) = 
        let [ alpha ] = do Alphabet a <- props ; return a
	in  Autolib.NFA.Example.example_sigma alpha

    partial Convert_To_NFA ( from, props ) aut = do
        let [ alpha ] = do Alphabet a <- props ; return a
        restrict_states aut
        restrict_alpha alpha aut
        inform $ text "Sind alle Eigenschaften erfüllt?"
        nested 4 $ mapM_ ( flip test aut ) props

    total Convert_To_NFA ( from, props ) aut = do
        inform $ text "Akzeptiert der Automat die richtige Sprache?"
        let [ alpha ] = do Alphabet a <- props ; return a
        flag <- nested 4 
             $ equ ( informed ( form from ) $ eval alpha from ) 
                   ( informed ( text "Sprache Ihres Automaten" ) aut )
        when (not flag) $ reject $ text ""


instance C.Measure Convert_To_NFA 
                   ( Convert, [ Property Char ] ) 
		   ( NFA Char Int ) 
  where
    measure _ _ aut = fromIntegral $ size aut

make :: Make
make = direct Convert_To_NFA 
     ( Convert { name = Nothing
	       , input = Exp $ read "a (a+b)^* b"
	       }
     , NFA.Property.example
     )


