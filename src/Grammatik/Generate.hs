module Grammatik.Generate 

-- $Id$

( generate )

where

import Grammatik.Type
import Grammatik.Trace

import Util.Zufall
import Set
import Reporter

gens :: Int -> Int -> Grammatik -> IO Track
    -- erzeugt eine Ableitung mit höchstens `steps' meilensteinen
    -- und jeder abstand ist <= `width' 
    -- d. h. jeder ausgegebene track
    -- läßt sich mit `trace b g' verifizieren
gens s b g = gen s b g [ startsymbol g ]

gen s b g u | s <= 0 = return [ u ]
gen s b g u = 
    case setToList $ nachfolger b g u of
	 [] -> return [u]
	 vs -> do v <- eins vs
		  rest <- gen (s-1) b g v
		  return $ u : rest

generate :: Int -> Int -> Int -> Grammatik -> IO (Set String)
generate words steps width g = do
    ws0 <- mapM ( \ s ->  gens s width g ) [ 1 .. steps ]
    return $ mkSet $ take words $ map last $ ws0

