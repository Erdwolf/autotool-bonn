module Shift.Check where

import Shift.Type
import Shift.Computer
import Shift.Verify

import Reporter
import Size
import ToDoc




check :: Int -> Int -> Shift -> Reporter Int
check m limit sh = do
    inform $ text "Sie haben eingesandt:" <+> toDoc sh
    newline

    inform $ fsep [ text "Das Schaltnetz darf höchstens"
			, toDoc m, text "Ein/Ausgänge haben." ]
    let n = maximum $ 0 : pins sh
    when (n > m) $ reject 
		 $ fsep [ text "aber Sie benutzen", toDoc n ]
    inform $ text "OK"
    newline

    let c = 50
    inform $ text "Die Folge y_1(0), y_1(1), ... beginnt so:"
    inform $ toDoc $ take c $ map fromEnum $ folge $ pins sh
    newline

    let dots = text "..."

    verify limit sh
