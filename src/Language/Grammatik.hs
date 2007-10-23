module Language.Grammatik where

import Language.Type

import Grammatik.Type

import qualified Grammatik.CF.Chomsky as C
import qualified Grammatik.CF.DPL_CYK as D


import Autolib.Exp ( Exp )
import Autolib.Exp.Inter ( inter , std_sigma )
import Autolib.NFA ( is_accepted_by, accepted )
import Autolib.NFA.Minus
import Autolib.Set ( mkSet )

import Autolib.Util.Zufall


import Autolib.ToDoc

grammatik :: Grammatik -> Language
grammatik g =
    let ch = C.make g
        l = Language
            { nametag = "Grammatik"
            , abbreviation = show $ vcat
                  [ text "die durch die Grammatik"
                  , nest 4 $ toDoc g
                  , text "erzeugte Sprache"
                  ]
            , alphabet = terminale g
            , contains = D.accepted ch
            , sample = \ num len -> 
                random_sample l (5 * num) len
            , anti_sample = \ num len -> 
	        random_sample (komplement l) (5 * num) len
            }
    in l
