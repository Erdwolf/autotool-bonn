module Shift.Check where

import Shift.Type
import Shift.Computer

import Reporter
import Size
import ToDoc


check :: Int -> Shift -> Reporter Int
check m sh = do
    inform $ text "Sie schicken mir dieses NAND-Schiebe-Register:"
	   <+> toDoc ( pins sh )
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

    let zs = zustands_folge $ pins sh
    let (q, p) = find zs

    inform $ text "Sie behaupten, diese Folge hat Vorperiode" 
	   <+> toDoc (vorperiode sh)
	   <+> dots
    when ( vorperiode sh /= q ) $ reject $ text "das ist falsch."
    inform $ text "OK"
    newline

    inform $ text "Sie behaupten, diese Folge hat Periode" 
	   <+> toDoc (periode sh)
	   <+> dots
    when ( periode sh /= p ) $ reject $ text "das ist falsch."
    inform $ text "OK"
    newline

    return $ size sh
