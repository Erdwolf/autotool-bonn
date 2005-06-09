module Language.RX where

-- $Id$

import Language.Type

import Autolib.Exp ( Exp )
import Autolib.Exp.Inter ( inter , std_sigma )
import Autolib.NFA ( is_accepted_by )
import Autolib.Set ( mkSet )

regular :: String -> Exp -> Language
regular sigma rx 
    = let fa = inter (std_sigma sigma) rx
          element = flip is_accepted_by fa
          l = Language 
	    { nametag = "Regular"
	    , abbreviation = foldl1 (++) [ "die durch den Ausdruck "
					 , show rx
					 , " beschriebene Sprache"
					 ]
	    , alphabet = mkSet sigma
	    , contains = element
	    , sample = random_sample l
	    , anti_sample = random_sample (komplement l)
	    }
      in l