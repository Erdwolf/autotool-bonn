module Grammatik.Eindeutig 

( eindeutig
)

where

import Grammatik.Type
import Grammatik.Ableitung
import Grammatik.Links_Ableitung
import qualified Grammatik.Checker as C

import Reporter
import ToDoc

eindeutig :: Maybe Int 
	  -> C.Type
eindeutig schranke = 
    C.make "Ein" ( text "Die Grammatik soll eindeutig sein." ) $ \ g -> do
        let wrong = mehrdeutige_links_ableitungen schranke g 
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese Grammatik ist nicht eindeutig."
	     , text "Wenigstens folgende Wörter haben mehrere Links-Ableitungen:" 
	     , nest 4 $ vcat $ do
	           xs @ (x : _) <- wrong
	           return $ vcat 
	                  [ toDoc ( car x )
			  , nest 4 $ vcat $ map toDoc xs
			  ]
	     ]
        inform $ text "OK (soweit getestet)"

