-- -- $Id$

module Pump.Positiv

( positiv )

where

import Pump.Type
import Language.Type

import Util.Seed
import ToDoc
import Size
import Control.Monad ( guard )

import Reporter
import FiniteMap
import Set
import List ( sort, nub )
import Maybe ( isNothing )

positiv :: Pumping z
	=> Language -> Pump z -> [ String ]
	-> Reporter Int

positiv l ( p @ Ja {} :: Pump z ) ws = do
    let fodder = undefined :: z
	
    inform $ vcat $ map text
	     [ "Guten Tag. Sie m�chten nachweisen, da� die Sprache  L = "
	     , show l
	     , "die " ++ tag fodder ++ " erf�llt."
	     , ""
	     , "Sie behaupten, JEDES Wort  p  in  L  mit  |p| >= " ++ show (n p) 
	     , "besitzt eine Zerlegung  p = " ++ tag_show fodder ++ ","
	     , "so da� f�r alle i:  " ++ inflate_show_i fodder ++ " in L."
	     ]
    newline

    when ( n p < 1 ) $ reject $ text "Es soll aber n >= 1 sein."

    inform $ vcat 
	   [ text "Ich pr�fe jetzt, ob die von Ihnen angegebenen Zerlegungen f�r die W�rter"
	   , nest 4 $ toDoc ws
	   , text "tats�chlich die geforderten Eigenschaften besitzen."
	   ]
    newline
	 
    mapM_ ( report l p ) ws
    return $ size p

----------------------------------------------------------------------------

report :: Pumping z 
       => Language -> Pump z -> String 
       -> Reporter ()
report l p w = do
    inform $ text $ "Ich w�hle  p = " ++ show w 
    let mz @ ~ (Just z) = lookupFM (zerlege p) w
    when ( isNothing mz ) 
	 $ reject $ text "Sie haben gar keine Zerlegung angegeben."
    inform $ text "Sie w�hlen" <+> toDoc z
    admissable (n p) z
    when ( w /= inflate 1 z )
	 $ reject $ text "Das ist gar keine Zerlegung von p."

    let check i = do
	   let w' = inflate i z
	   when ( not $ contains l w' ) $ reject $ text
		$ "aber " ++ inflate_show i z ++ " = " ++ show w'
		  ++ " ist nicht in " ++  show l 
    mapM_ check [ 0 .. 20 ]
    inform $ text "OK"
    newline

	   

