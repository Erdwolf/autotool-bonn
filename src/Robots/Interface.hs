module Robots.Interface where

--  $Id$

import Robots.Data
import Robots.Config
import Robots.Move
import Robots.Nice
import Robots.Final
import Robots.Examples
import Robots.Quiz
import Robots.Generator

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
        [ text "Lunar Lockout (c) Binary Arts:"
        , text "Geben Sie eine Zugfolge an,"
	, text "die den Roboter (Großbuchstabe)"
	, text "ins Ziel (entsprechender Kleinbuchstabe) bringt:"
	, nest 4 $ nice k
	]

    initial Robots k = 
        let a : b : _ = robots k
	in  [ ( name a, N ), ( name b, O ) ]

    partial Robots k zs = do
        executes k zs
	return ()

    total Robots k zs = do
        k' <- silent $ executes k zs
	final k'

make :: Make
make = direct Robots Robots.Examples.fourty

qmake :: Make
qmake = quiz Robots Robots.Quiz.rc

