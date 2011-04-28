{-# LANGUAGE ScopedTypeVariables #-}
-- -- $Id$

module Pump.Positiv

( positiv )

where

import Pump.Type
import Language.Type

import Autolib.Util.Seed
import Autolib.ToDoc
import Autolib.Size
import Control.Monad ( guard )

import Autolib.Reporter
import Autolib.FiniteMap
import Autolib.Set
import Data.List ( sort, nub )
import Data.Maybe ( isNothing )

positiv :: Pumping z
	=> Language -> Pump z -> [ String ]
	-> Reporter Int

positiv l ( p @ Ja {} :: Pump z ) ws = do
    let fodder = undefined :: z
	
    inform $ vcat $ map text
	     [ "Sie möchten nachweisen, daß die Sprache  L = "
	     , show l
	     , "die " ++ tag fodder ++ " erfüllt."
	     , ""
	     , "Sie behaupten, JEDES Wort  p  in  L  mit  |p| >= " ++ show (n p) 
	     , "besitzt eine Zerlegung  p = " ++ tag_show fodder ++ ","
	     , "so daß für alle i:  " ++ inflate_show_i fodder ++ " in L."
	     ]
    newline

    when ( n p < 1 ) $ reject $ text "Es soll aber n >= 1 sein."

    inform $ vcat 
	   [ text "Ich prüfe jetzt, ob die von Ihnen angegebenen Zerlegungen für die Wörter"
	   , nest 4 $ toDoc ws
	   , text "tatsächlich die geforderten Eigenschaften besitzen."
	   ]
    newline
	 
    mapM_ ( report l p ) ws
    return $ size p

----------------------------------------------------------------------------

report :: Pumping z 
       => Language -> Pump z -> String 
       -> Reporter ()
report l p w = do
    inform $ text $ "Ich wähle  p = " ++ show w 
    let mz @ ~ (Just z) = lookupFM (zerlege p) w
    when ( isNothing mz ) 
	 $ reject $ text "Sie haben gar keine Zerlegung angegeben."
    inform $ text "Sie wählen" <+> toDoc z
    admissable (n p) z
    when ( w /= inflate 1 z )
	 $ reject $ text "Das ist gar keine Zerlegung von p."

    let check i = do
	   let w' = inflate i z
	   when ( not $ contains l w' ) $ reject $ text
		$ "aber " ++ inflate_show i z ++ " = " ++ show w'
		  ++ " ist nicht in " ++  show l 
    mapM_ check [ 0 .. 100 ] -- FIXME
    inform $ text "OK"
    newline

	   


