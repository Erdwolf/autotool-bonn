module Grammatik.CF.Yeah_Noh where

-- -- $Id$

import Grammatik.Type
import qualified Grammatik.Ableitung as A

import Grammatik.CF.Instance.Config
import Grammatik.Check

import qualified Grammatik.CF.Chomsky as C

import Grammatik.CF.Epsfrei
import Grammatik.CF.Kettenfrei

import Grammatik.Reduziert

import FilterBound
import Util.Sort
import ToDoc
import Reporter

-- eins der beiden imports ausw�hlen:

-- f�r jedes wort einzeln das CYK-array ausrechnen
-- (besser f�r hugs??)
-- import CYK

-- gemeinsamen speicher f�r alle teilw�rter
-- (m��te eigentlich hashtabelle benutzen, FiniteMaps sind zu langsam?)
import Grammatik.CF.DPL_CYK

-- end auswahl


cf_yeah_noh :: Config -> Grammatik -> Reporter ()
cf_yeah_noh c g = do

    -- first, some simple checks (not CF-specific)
    let conf = A.Config
	  { A.max_length = 12
	  , A.max_depth = 7
	  , A.max_width = 100
	  } 
    check ( lang c ) conf g

    let arrange = sortBy length
    let [ yeahs, nohs ] = map arrange [ yeah c, noh c ]

    -- erstmal keine ableitungen:
    -- trace t ( g, demos ) ts 
    -- wenn wir hier ankommen, sind die ableitungen OK

    let ch = C.make g
    -- inform $ text "DEBUG: chomsky nf" <+> toDoc ch

    let snip = take 5 -- ??

    inform $ vcat
	   [ text "Ich pr�fe, ob diese W�rter der Zielsprache"
	   , nest 4 $ toDoc yeahs
	   , text "von Ihrer Grammatik erzeugt werden."
	   ]

    let achyeah = accepteds ch yeahs
    let yeah_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> not f ) 
		    $ achyeah

    when ( not $ null yeah_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese W�rter NICHT:"
	 , nest 4 $ toDoc yeah_falsch
	 ]
    inform $ text "OK."

    inform $ vcat
	   [ text "Ich pr�fe, ob diese W�rter aus dem Komplement der Zielsprache"
	   , nest 4 $ toDoc nohs
	   , text "tats�chlich NICHT von Ihrer Grammatik erzeugt werden."
	   ]

    let achnoh =  accepteds ch nohs
    let noh_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> f ) 
		    $ achnoh

    when ( not $ null noh_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese W�rter DOCH:"
	 , nest 4 $ toDoc noh_falsch
	 ]
    inform $ text "OK."

    -- wenn wir hier sind, ist alles OK

