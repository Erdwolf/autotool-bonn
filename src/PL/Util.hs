module PL.Util where

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reporter

find_or_complain tag fm this = 
    case lookupFM fm this of
        Just x -> return x
	Nothing -> reject $ fsep 
            [ text tag, toDoc this, text "not bound/defined" ]

