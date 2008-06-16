{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Robots3.Interface where

--  $Id$

import Robots3.Data
import Robots3.TH
import Robots3.Config
import Robots3.Move
import Robots3.Nice
import Robots3.Final
import Robots3.Examples
import Robots3.Quiz
import Robots3.Generator

import Autolib.Reporter
import Autolib.ToDoc

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Maybe ( isJust )
import Data.List ( partition )

instance Partial Robots3 
		 Config
		 (Folge Zug)
	 where

    describe Robots3 k = vcat
        [ text "Lunar Lockout (c) Binary Arts:"
        , text "Geben Sie eine Zugfolge an,"
	, text "nach der alle Zielfelder (*)"
	, text "mit Robotern (Großbuchstaben) belegt sind."
	, nest 4 $ nice k
	]

    initial Robots3 k = 
        let a : b : _ = robots k
	in  Folge [ Zug ( name a) N , Zug ( name b) O  ]

    partial Robots3 k (Folge zs) = do
        executes k zs
	return ()

    total Robots3 k (Folge zs) = do
        k' <- silent $ executes k zs
	final k'

make :: Make
make = direct Robots3 Robots3.Examples.fourty

qmake :: Make
qmake = quiz Robots3 Robots3.Quiz.rc

