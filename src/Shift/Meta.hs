module Shift.Meta 

( Meta (..)
, meta
)

where

-- $Id$

import Util.Size
import ToDoc

import Shift.Type
import Shift.Verify

import Shift.Computer (find, zustands_folge)

import Reporter

vector :: Meta -> Int -> Pins
vector me k = do
    (x, d) <- zip (start me) (diff me)
    return $ x + k * d

meta :: Int -> Meta -> Reporter Int 
meta limit me = do
    inform $ text "Sie haben eingesandt:" <+> toDoc me
    newline

    let shs = do 
          (k, (q,p)) <- zip [0..] $ qps me
	  return $ Shift { pins = vector me k
			 , vorperiode = q
			 , periode = p
			 }
    
    inform $ text "Ich prüfe die Shift-Instanzen"
    inform $ toDoc shs
    sequence_ $ map (silent . verify limit) shs
    inform $ text "OK"
    newline    

    let ps = map periode shs
    inform $ text "Ich untersuche das Wachstum der Periodenlängen."

    delta 0 ps


delta :: Int -> [Int] -> Reporter Int
delta deg ps = do
    inform $ fsep [ text "Betrachte die", toDoc deg <> text "-te"
		  , text "Differenzenfolge", toDoc ps
		  ]
    when ( null ps )
         $ reject $ text "Die Folge ist zu kurz,"
		    <+> text "ich kann das Wachstum nicht bestimmen."

    if ( all (== 0) ps )
       then do
	    inform $ text "Diese Folge ist konstant 0,"
	    inform $ text "deswegen ist die Original-Folge wahrscheinlich"
	    inform $ text "ein Polynom vom Grad" <+> toDoc (deg-1)
	    return (deg-1)
       else do
	    when ( any ( < 0 ) ps )
		 $ reject $ text "Einige Elemente sind < 0"
	    delta (deg + 1) $ zipWith (-) (tail ps) ps

