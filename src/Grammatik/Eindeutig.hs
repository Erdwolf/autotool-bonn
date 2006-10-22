module Grammatik.Eindeutig 

( eindeutig
)

where

import Grammatik.Type
import Grammatik.Ableitung
import Grammatik.Links_Ableitung
import Grammatik.Reduziert
import qualified Autolib.Reporter.Checker as C

import Autolib.Reporter
import Autolib.ToDoc

eindeutig :: Config
	  -> C.Type Grammatik
eindeutig conf = 
    C.make "Ein" ( text "Die Grammatik soll eindeutig sein." ) $ \ g0 -> do
        let g = reduktion g0
        inform $ vcat 
	       [ text "Hier sind einige Links-Ableitungen:"
	       , nest 4 $ toDoc $ take 4 $ links_ableitungen conf g
	       ]
        let wrong = mehrdeutige_links_ableitungen conf g 
	let interesting (x, y) =
		mcadr x /= mcadr y -- aber car x == car y
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
				 , toDoc ( max_length conf )
				 ]
			  )
