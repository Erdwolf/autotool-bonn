module Grammatik.Eindeutig 

( eindeutig
)

where

import Grammatik.Type
import Grammatik.Ableitung
import Grammatik.Links_Ableitung
import Grammatik.CF.Reduziert
import qualified Grammatik.Checker as C

import Reporter
import ToDoc

eindeutig :: Maybe Int 
	  -> C.Type
eindeutig schranke = 
    C.make "Ein" ( text "Die Grammatik soll eindeutig sein." ) $ \ g0 -> do
        let g = reduktion g0
        inform $ vcat 
	       [ text "Hier sind einige Links-Ableitungen:"
	       , nest 4 $ toDoc $ take 4 $ links_ableitungen schranke g
	       ]
        let wrong = mehrdeutige_links_ableitungen schranke g 
	let interesting (x, y) = cadr x /= cadr y -- aber car x == car y
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese Grammatik ist nicht eindeutig."
	     , text "Die folgenden Wörter haben mehrere Links-Ableitungen:"
	     , text "(Die eventuell noch vorkommenden Variablen sind produktiv,"
	     , text "d. h. die Ableitungen können zu Terminalwörtern fortgesetzt werden.)"
	     , nest 4 $ vcat $ do
	           (x , y) <- take 4 $ filter interesting $ wrong
	           return $ vcat 
	                  [ toDoc ( car x )
			  , nest 4 $ vcat $ map toDoc [ x, y ]
			  ]
	     ]
        inform $ text "OK" 
	       <+> parens ( fsep [ text "für Wörter der Länge <="
				 , toDoc schranke
				 ]
			  )
