-- -- $Id$

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
	=> String -> Language -> Pump z 
	-> IO String

central mat l p @ Nein {} = do
    reporter $ negativ l p 

central mat l p @ Ja   {} = do
    seed $ read mat
    ws1 <- samples l 3 (n p    )
    ws2 <- samples l 2 (n p + 3)
    ws3 <- samples l 2 (n p + 6)
    let ws = sort 
	   $ nub 
	   $ filter ( (n p <= ) . length ) 
	   $ ws1 ++ ws2 ++ ws3
    reporter $ positiv l p $ take 5 ws




	   

       
