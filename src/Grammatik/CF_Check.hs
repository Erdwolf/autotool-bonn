module Grammatik.CF_Check where

-- $Id$

import Language.Type

import Grammatik.Trace

import Grammatik.Type
import Grammatik.Epsfrei
import Grammatik.Kettenfrei

import Grammatik.Hierarchie ( typ2 )

import qualified Grammatik.Chomsky as C


-- eins der beiden imports auswählen:

-- für jedes wort einzeln das CYK-array ausrechnen
-- (besser für hugs??)
-- import CYK

-- gemeinsamen speicher für alle teilwörter
-- (müßte eigentlich hashtabelle benutzen, FiniteMaps sind zu langsam?)
import Grammatik.DPL_CYK

-- end auswahl

import Size
import Set
import Wort


import List (partition, nub, sortBy)
import FilterBound

import ToDoc
import Random

import Reporter

cf_check :: Language 
	 -> ( Grammatik -> Reporter () )
	 -> Int -> Int -> Int
	 -> ( Grammatik, Tracks ) 
	 -> IO ( Reporter Int )
cf_check l typ w n t ( g, ts ) = do
    here   <- samples      l w n
    there  <- anti_samples l w n
    let (yeah, noh) = partition (contains l) $ nub $ here ++ there
    return $ cf_yeah_noh ( toDoc l ) yeah noh typ t ( g, ts )


cf_yeah_noh :: Doc 
	    -> [ String ] -> [ String ] 
	    -> ( Grammatik -> Reporter () )
	    -> Int
	    -> ( Grammatik, Tracks ) 
	    -> Reporter Int
cf_yeah_noh doc yeah0 noh0 typ t gts @ ( g, ts ) = do
    inform $ text "Gesucht sind Grammatik und Beispiel-Ableitungen für" <+> doc
    inform $ text "Sie haben eingesandt:" <+> toDoc gts

    typ2 g
    typ g

    let arrange = sortBy (\ x y -> compare (length x, x) (length y, y))
    let [ yeah, noh ] = map arrange [ yeah0, noh0 ]

    let demo = mkSet $ take 2 yeah -- FIXME: arbitrary constant
    trace t ( g, demo ) ts 

    -- wenn wir hier ankommen, sind die ableitungen OK

    let ch = C.chomsky $ kettenfrei $ epsfrei $ g
    let snip = take 5 -- ??

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter der Zielsprache"
	   , toDoc yeah
	   , text "von Ihrer Grammatik erzeugt werden."
	   ]

    let achyeah = accepteds ch yeah
    let yeah_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> not f ) 
		    $ achyeah

    when ( not $ null yeah_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter NICHT:"
	 , toDoc yeah_falsch
	 ]
    inform $ text "OK."

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter aus dem Komplement der Zielsprache"
	   , toDoc noh
	   , text "tatsächlich NICHT von Ihrer Grammatik erzeugt werden."
	   ]

    let achnoh =  accepteds ch noh
    let noh_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> f ) 
		    $ achnoh

    when ( not $ null noh_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter DOCH:"
	 , toDoc noh_falsch
	 ]
    inform $ text "OK."

    -- wenn wir hier sind, ist alles OK
    return $ size g







