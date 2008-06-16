{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Robots2.Interface where

--  $Id$

import Robots2.Data
import Robots2.Config
import Robots2.Move
import Robots2.Nice
import Robots2.Final
import Robots2.Examples
import Robots2.Quiz
import Robots2.Generator

import Autolib.Reporter
import Autolib.ToDoc

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Maybe ( isJust )
import Data.List ( partition )

instance Partial Robots 
		 Config
		 [ Zug ]
	 where

    describe Robots k = vcat
        [ text "Solar Lockout"
        , text "Geben Sie eine Zugfolge an,"
	, text "die die Roboter (*)"
	, text "auf die Zielfelder (+) bringt."
	, text "(# bezeichnet ein belegtes Zielfeld)"
	, nest 4 $ nice k
	]

    initial Robots k = 
        let a : b : _ = positions k
	in  [ ( a, N ), ( b, O ) ]

    partial Robots k zs = do
        executes k zs
	return ()

    total Robots k zs = do
        k' <- silent $ executes k zs
	final k'

make :: Make
make = direct Robots Robots2.Examples.alex

qmake :: Make
qmake = quiz Robots Robots2.Quiz.rc

