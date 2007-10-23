module Language.Grammatik where

import Language.Type

import Grammatik.Type

import qualified Grammatik.CF.Chomsky as C
import qualified Grammatik.CF.DPL_CYK as D


import Autolib.Util.Zufall

import Autolib.ToDoc

grammatik :: Grammatik -> Language
grammatik g =
    let ch = C.make g
        l = Language
            { nametag = "Grammatik"
            , abbreviation = show $ vcat
                  [ text "die durch"
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
