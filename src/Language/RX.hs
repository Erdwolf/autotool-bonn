module Language.RX where

-- $Id$

import Language.Type

import Autolib.Exp ( Exp )
import Autolib.Exp.Inter ( inter , std_sigma )
import Autolib.NFA ( is_accepted_by, accepted )
import Autolib.NFA.Minus
import Autolib.Set ( mkSet )
import Autolib.Util.Zufall
import Autolib.ToDoc

regular :: String -> Exp -> Language
regular sigma rx 
    = let fa = inter (std_sigma sigma) rx
          cfa = complement sigma fa
          element = flip is_accepted_by fa
          sampler a = \ num len -> do
                  let ws = takeWhile ( (<= len) . length )
                         $ take 1000 -- FIXME: arbitrary constant
                         $ accepted fa
                  sequence $ replicate num $ eins ws
          l = Language 
	    { nametag = "Regular"
	    , abbreviation = show $ vcat
                  [ text "die durch den Ausdruck"
                  , toDoc rx
	          , text "beschriebene Sprache"
	          ]
	    , alphabet = mkSet sigma
	    , contains = element
	    , sample = sampler fa
	    , anti_sample = sampler cfa
	    }
      in l

