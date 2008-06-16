{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Robots3.Inverse where

--  $Id$

import Robots3.Data
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
import Data.List ( partition, inits )

instance Partial Robots3_Inverse 
		 [ Zug ]
		 Config
	 where

    describe _ zs = vcat
        [ text "Inverse Solar Lockout:"
        , text "Geben Sie eine Startkonfiguration"
	, text "mit folgender minimaler Lösungsfolge an:"
	, nest 4 $ toDoc zs
	, text "alle Züge sollen ausführbar sein"
	, text "und erst der letzte Zug löst die Aufgabe."
	]

    initial _ zs = 
        Robots3.Examples.fourty

    partial _ zs k = do
        silent $ sequence $ do
	    zs' <- inits zs
	    guard $ zs' /= zs	    
	    return $ do
	        k' <- executes k zs'
		when ( is_final k' ) 
		     $ reject $ text "Zugfolge ist keine minimale Lösung."
        valid k

    total _ zs k = do
        k' <- executes k zs
	final k'

make :: Make
make = direct Robots3_Inverse 
     [("C",W),("E",W),("D",S),("A",O),("D",N),("B",N),("E",N),("D",W),("A",S)]

qmake :: Make
qmake = quiz Robots3_Inverse Robots3.Quiz.rc

