module Robots.Inverse where

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

instance Partial Robots_Inverse 
		 [ Zug ]
		 Config
	 where

    describe _ zs = vcat
        [ text "Lunar Lockout (c) Binary Arts:"
        , text "Geben Sie eine Startkonfiguration"
	, text "mit folgender Lösungsfolge an:"
	, nest 4 $ toDoc zs
        , text "Das Ziel ist dem zuletzt bewegten Roboter zuzuordnen."
	]

    initial _ zs = 
        Robots.Examples.e10

    partial _ zs k = do
        valid k
        last_moved_has_goal zs k

    total _ zs k = do
        k' <- executes k zs
	final k'

last_moved_has_goal zs k = 
    let goals = do
          r <- robots k
          guard $ isJust $ ziel r
          return r
    in case goals of
        [r] -> do
          let ( n, d ) = last zs
          assert ( n == name r )
                 $ text "Ziel-Roboter wurde zuletzt bewegt?"
        _ -> reject $ text "es soll genau einen Roboter mit einem Ziel geben."

make :: Make
make = direct Robots_Inverse 
     [("C",W),("E",W),("D",S),("A",O),("D",N),("B",N),("E",N),("D",W),("A",S)]

qmake :: Make
qmake = quiz Robots_Inverse Robots.Quiz.rc

