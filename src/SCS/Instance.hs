{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module SCS.Instance 

( make_fixed
, make_quiz
)

where


import LCS.Code
import SCS.Data
import SCS.Quiz
import SCS.Config
import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Typeable

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Xml
import Autolib.Reporter

data SCS = SCS deriving ( Show, Read, Typeable )

instance OrderScore SCS where
    scoringOrder _ = Increasing

instance 
     ( InstanceC a ) => Partial SCS ( Instance a ) [ a ] 
    where

    describe SCS i =
        vcat [ text "Bestimmen Sie eine Folge der Länge <=" <+> toDoc ( max_length i )
             , text "die jede dieser Folgen als eingebettete Teilfolge enthält:"
             , nest 4 $ toDoc $ contents i
	     ]
	
    initial SCS ( i ) =
        concat $ contents i

    partial SCS i w = forM_ ( contents i ) $ \ c -> do
        assert ( c `is_embedded_in` w )
	       ( fsep [ text "ist", toDoc c, text "Teilfolge von", toDoc w ])
	
    total SCS i w = do
        inform $ text "Länge Ihrer Folge:" <+> toDoc ( length w )
        when ( length w > max_length i ) $ reject $ text "zu groß"

instance Measure SCS ( Instance a ) [a] where
    measure SCS i zs = fromIntegral $ length zs

make_fixed :: Make
make_fixed = ( direct :: SCS -> Instance Char -> Make ) SCS SCS.Data.example


instance ( Show a, Read a, InstanceC a )
        => Generator SCS (Config a) ( [a], Instance a ) where
    generator _ conf key = roll conf
instance Project SCS  ( [a], Instance a ) ( Instance a ) where
    project _ ( l, i ) = i

make_quiz :: Make
make_quiz = quiz SCS SCS.Config.example


