-- $Id$

module Pump.Central

( central
)

where

import Pump.Type

import Pump.Positiv 
import Pump.Negativ 

import Language.Type
import Util.Seed
import List (nub, sort)
import Reporter

central :: Pumping z 
	=> String -> Language -> Pump z -> z
	-> IO String

central mat l p @ Nein {} fodder = do
    reporter $ negativ l p fodder

central mat l p @ Ja   {} fodder = do
    seed $ read mat
    ws1 <- samples l 3 (n p    )
    ws2 <- samples l 2 (n p + 3)
    ws3 <- samples l 2 (n p + 6)
    let ws = sort $ nub $ ws1 ++ ws2 ++ ws3
    reporter $ positiv l p $ take 5 ws



	   

       
