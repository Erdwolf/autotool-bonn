module Grammatik.CF_Check where

-- $Id$

import Language.Type

import Grammatik.Trace

import Grammatik.Type
import Grammatik.Epsfrei
import Grammatik.Kettenfrei

import Grammatik.Hierarchie ( typ2 )

import qualified Grammatik.Chomsky as C


-- eins der beiden imports ausw�hlen:

-- f�r jedes wort einzeln das CYK-array ausrechnen
-- (besser f�r hugs??)
-- import CYK

-- gemeinsamen speicher f�r alle teilw�rter
-- (m��te eigentlich hashtabelle benutzen, FiniteMaps sind zu langsam?)
import Grammatik.DPL_CYK

-- end auswahl

import Size
import Set
import Wort
import Edit

import List (partition, nub, sortBy)
import FilterBound

import ToDoc
import Random

import Reporter

cf_check :: Language 
	 -> ( Grammatik -> Reporter () )
	 -> Int -> Int
	 -> ( Grammatik, Tracks ) 
	 -> IO ( Reporter Int )
cf_check l typ w n ( g, ts ) = do
    ws <- samples l w n
    let large = maximum [ length w | w <- ws ]
    ms <- mapM edits $ ws ++ ws
    let mutants = filter ( \ w -> length w <= large ) ms
    let (yeah, noh) = partition (contains l) $ nub $ ws ++ mutants
    return $ cf_yeah_noh ( toDoc l ) yeah noh typ ( g, ts )


cf_yeah_noh :: Doc 
	    -> [ String ] -> [ String ] 
	    -> ( Grammatik -> Reporter () )
	    -> ( Grammatik, Tracks ) 
	    -> Reporter Int
cf_yeah_noh doc yeah0 noh0 typ gts @ ( g, ts ) = do
    inform $ text "Gesucht sind Grammatik und Beispiel-Ableitungen f�r" <+> doc
    inform $ text "Sie haben eingesandt:" <+> toDoc gts

    typ2 g
    typ g

    let arrange = sortBy (\ x y -> compare (length x, x) (length y, y))
    let [ yeah, noh ] = map arrange [ yeah0, noh0 ]
    let demo = mkSet $ take 3 yeah

    trace 4 ( g, demo ) ts -- FIXME: arbitrary constant

    -- wenn wir hier ankommen, sind die ableitungen OK

    let ch = C.chomsky $ kettenfrei $ epsfrei $ g
    let snip = take 5 -- ??

    inform $ vcat
	   [ text "Ich pr�fe, ob diese W�rter der Zielsprache"
	   , toDoc yeah
	   , text "von Ihrer Grammatik erzeugt werden."
	   ]

    let achyeah = accepteds ch yeah
    let yeah_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> not f ) 
		    $ achyeah

    when ( not $ null yeah_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese W�rter NICHT:"
	 , toDoc yeah_falsch
	 ]
    inform $ text "OK."

    inform $ vcat
	   [ text "Ich pr�fe, ob diese W�rter aus dem Komplement der Zielsprache"
	   , toDoc noh
	   , text "tats�chlich NICHT von Ihrer Grammatik erzeugt werden."
	   ]

    let achnoh =  accepteds ch noh
    let noh_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> f ) 
		    $ achnoh

    when ( not $ null noh_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese W�rter DOCH:"
	 , toDoc noh_falsch
	 ]
    inform $ text "OK."

    -- wenn wir hier sind, ist alles OK
    return $ size g







