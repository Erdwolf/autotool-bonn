module RM.Machine where

import Machine.Class

import RM.Type
import RM.Memory
import RM.State
import RM.Step

import Autolib.Set
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc

instance Compute Program State where
    next _ = mkSet . step
    accepting _ = ( == Nothing ) . program
    depth _ = schritt

instance In Program Memory State where
    input p m = State 
	      { schritt = 0
	      , program = Just p
	      , memory  = m
	      , past    = []
	      }

instance Out Program Memory State where
    output_reporter _ s = do 

        let m  = memory s
        let m0 = memory $ last $ past s

        inform $ nest 4 $ text "Sind die Eingabewerte erhalten?"

        let ok = and $ do (r,v) <- fmToList m0
			  return $ lookupFM m r == Just v

        when ( not ok ) $ reject $ nest 4 $ text "Nein."

        inform $ nest 4 $ text "Ja."

        return m

instance Encode Memory where
    encode xs = fromList $ zip [1..] xs

instance Decode Memory where
    decode m = get m 0
