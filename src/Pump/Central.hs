--  $Id$

module Pump.Central {-# DEPRECATED #-}

( central
)

where

import Pump.Type

import Pump.Positiv 
import Pump.Negativ 

import Language.Type
import Util.Seed
import Data.List (nub, sort)
import Reporter

central :: Pumping z 
	=> String -> Language -> Pump z 
	-> IO String

central mat l p @ Nein {} = do
    reporter $ negativ l p 

central mat l p @ Ja   {} = do
    seed $ read mat
    ws1 <- samples l 5 (    n p    )
    ws2 <- samples l 5 (2 * n p + 3)
    ws3 <- samples l 5 (3 * n p + 6)
    let ws = sort 
	   $ nub 
	   $ filter ( (n p <= ) . length ) 
	   $ take 4 ws1 ++ take 3 ws2 ++ take 2 ws3
    reporter $ positiv l p $ ws




	   

       
