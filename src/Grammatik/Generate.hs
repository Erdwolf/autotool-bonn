module Grammatik.Generate 

-- -- $Id$

( generate )

where

import Grammatik.Type
import Grammatik.Trace

import qualified Grammatik.Trace.Config as T
import qualified Grammatik.Ableitung.Config as A

import Autolib.Util.Zufall
import Autolib.Set
import Autolib.Reporter

-- | erzeugt eine Ableitung mit höchstens `steps' meilensteinen
-- und jeder abstand ist <= `width' 
-- d. h. jeder ausgegebene track
-- läßt sich mit `trace b g' verifizieren
gens :: T.Config -> IO Track
gens conf = gen conf [ startsymbol $ T.grammatik conf ]

gen :: T.Config -> String -> IO Track
gen conf u | T.steps conf <= 0 = return [ u ]
gen conf u = 
    case setToList $ nachfolger conf u of
	 [] -> return [u]
	 vs -> do v <- eins vs
		  rest <- gen ( conf { T.steps = T.steps conf - 1 } ) v
		  return $ u : rest

generate :: T.Config -> IO [ String ]
generate conf = do
    ws0 <- mapM ( \ s ->  gens $ conf { T.steps = s } ) 
           [ 1 .. T.steps conf ]
    return $ map last $ ws0

