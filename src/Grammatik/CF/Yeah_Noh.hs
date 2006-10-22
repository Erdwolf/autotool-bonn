-- | context free parsing (using CYK)

module Grammatik.CF.Yeah_Noh where

-- -- $Id$

import Language.Inter
import Language.Type
import Language.Syntax

import Grammatik.Type
import qualified Grammatik.Property 
import qualified Grammatik.Ableitung as A

import Grammatik.CF.Instance.Config

import Grammatik.CF.Zeige
import Grammatik.Check

import qualified Grammatik.CF.Chomsky as C

import Grammatik.CF.Epsfrei
import Grammatik.CF.Kettenfrei

import Grammatik.Reduziert

import Autolib.FilterBound
import Autolib.Util.Sort
import Autolib.ToDoc
import Autolib.Reporter



-- eins der beiden imports auswählen:

-- für jedes wort einzeln das CYK-array ausrechnen
-- (besser für hugs??)
-- import CYK

-- gemeinsamen speicher für alle teilwörter
-- (müßte eigentlich hashtabelle benutzen, FiniteMaps sind zu langsam?)
import Grammatik.CF.DPL_CYK

-- end auswahl


cf_yeah_noh :: Config -> Grammatik -> Reporter ()
cf_yeah_noh c g = do

    ws <- zeige 100 g
    check ( inter $ lang c ) ws

    let arrange = sortBy length
        unl :: Long (Long a) -> [[a]]
        unl = map unLong . unLong 
    let [ yeahs, nohs ] = map arrange [ unl $ yeah c, unl $ noh c ]

    -- erstmal keine ableitungen:
    -- trace t ( g, demos ) ts 
    -- wenn wir hier ankommen, sind die ableitungen OK

    let ch = C.make g

    let snip = take 5 -- ??

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter der Zielsprache"
	   , nest 4 $ toDoc yeahs
	   , text "von Ihrer Grammatik erzeugt werden."
	   ]

    let achyeah = accepteds ch yeahs
    let yeah_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> not f ) 
		    $ achyeah

    when ( not $ null yeah_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter NICHT:"
	 , nest 4 $ toDoc yeah_falsch
	 ]
    inform $ text "OK."

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter aus dem Komplement der Zielsprache"
	   , nest 4 $ toDoc nohs
	   , text "tatsächlich NICHT von Ihrer Grammatik erzeugt werden."
	   ]

    let achnoh =  accepteds ch nohs
    let noh_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> f ) 
		    $ achnoh

    when ( not $ null noh_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter DOCH:"
	 , nest 4 $ toDoc noh_falsch
	 ]
    inform $ text "OK."

    -- wenn wir hier sind, ist alles OK

--------------------------------------------------------------------

test = do
     let s = Gleich "ab" [2,1]
         l = inter s
     y <-      samples l 50 4
     n <- anti_samples l 50 4
     let conf = Config 
		 { lang = s
		 , properties  = [ Grammatik.Property.Eindeutig 500 ]
		 , yeah = Long $ map Long y
		 , noh  = Long $ map Long n
                 }
     let g = Grammatik 
                 { terminale = mkSet "ab", variablen = mkSet "S"
	         , start = 'S'
	         , regeln = mkSet [ ( "S", "" )    ,("S","aSbSbS")
				  , ( "S","bSaSbS"),("S","bSbSaS") 
				  ]
	         }

     let ch = C.make g
     mapM_ print $ accepteds ch ( y ++ n )

     print $ repo $ cf_yeah_noh conf g
