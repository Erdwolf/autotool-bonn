module RM.Machine where

--  $Id$

import Machine.Class

import RM.Type
import RM.Memory
import RM.State
import RM.Step

import Autolib.Set
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc

import Control.Monad ( guard )

instance Compute Program State where
    next _ = mkSet . step
    accepting _ = ( == Nothing ) . program
    depth _ = schritt

instance In Program Memory State where
    input_reporter p m = return $ State 
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

        inform $ nest 4 $ text "Sind alle benutzten Register zurückgesetzt?"

        let rs  = 0 : keysFM m0
        let bad = do b@(r,v) <- fmToList m
		     guard $ not $ elem r rs
		     guard$ v /= 0
		     return b

        when ( not $ null bad ) $ reject $ nest 4 $ vcat
		 [ text "Nein. Diese Register enthalten noch Werte:"
		 , toDoc bad
		 ]

        inform $ nest 4 $ text "Ja."

        return m

instance Encode Memory where
    encode xs = RM.Memory.fromList $ zip [1..] xs

instance Decode Memory where
    decode m = get m 0
