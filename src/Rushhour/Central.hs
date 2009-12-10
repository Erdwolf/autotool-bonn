module Rushhour.Central where

--  $Id$

import Rushhour.Data
-- import Rushhour.Config
import Rushhour.Move
-- import Rushhour.Quiz
-- import Rushhour.Generator

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Maybe ( isJust )
import Data.List ( partition )

instance Partial Rushhour 
		 Instance
		 [ Zug ]
	 where

    describe Rushhour k = vcat
        [ text "Rushhour (TM):"
        , text "Geben Sie eine Zugfolge an,"
	, text "die das Auto" <+> toDoc ( target k ) <+> text "befreit"
	, text "(nach rechts bzw. oben)"
	, nest 4 $ nice k
	]

    initial Rushhour k = 
        let a : b : _ = keysFM $ cars k
	in  [ (b, 2), (a,-1) ]

    partial Rushhour k zs = do
        executes k zs
	return ()

    total Rushhour k zs = do
        k' <- silent $ executes k zs
	final k'

make :: Make
make = direct Rushhour Rushhour.Data.example

-- qmake :: Make
-- qmake = quiz Rushhour Rushhour.Quiz.rc

