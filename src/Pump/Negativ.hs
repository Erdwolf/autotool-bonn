-- $Id$

module Pump.Negativ

( negativ )

where


import Pump.Type
import Language.Type


import ToDoc
import Reporter
import Monad (guard)
import FiniteMap
import Maybe ( isNothing )
import List ( nub, sort )

import Size

negativ :: Pumping z 
	=> Language -> Pump z 
        -> Reporter Int
negativ l ( p @ Nein {} :: Pump z ) = do

    let checked =  [ 1, 2, 4, 8, 10 ]

    inform $ vcat $ map text
	     [ "Guten Tag. Sie m�chten nachweisen, da� die Sprache "
	     , show l
	     , "die " ++ tag ( undefined :: z ) ++ " NICHT erf�llt."
	     , ""
	     , "Sie behaupten, zu jeder Zahl n > 0"
	     , "gibt es ein Wort  p  in L  mit  |p| >= n,"
	     , "so da� zu jeder g�ltigen Zerlegung  p = " 
	       ++ tag_show ( undefined :: z )
	     , "ein  i  existiert mit  " 
	       ++ inflate_show_i ( undefined :: z ) ++ " not in L."
	     , ""
	     , "Dazu sollen Sie mir f�r jedes  n  aus " ++ render (toDoc checked)
	     , "ein solches Wort  p  angeben."
	     , ""
	     , "Sie haben eingesandt:"
	     , render $ toDoc $ wort p
	     ]

    newline
    mapM_ ( report l p ) checked


    inform $ vcat $ map text
	   [ "OK, Ihre Liste ist korrekt." ]

    return $ size p

----------------------------------------------------------------------------

report :: Pumping z 
       => Language -> Pump z -> Int 
       -> Reporter ()
report l ( neg @ Nein {} :: Pump z ) n = do
    let pre1 = "Ich w�hle  n = " ++ show n
	mw @ ~ (Just w) = lookupFM (wort neg) n
	pre2 = "Sie w�hlen  p = " ++ show w
    inform $ text pre1
    when ( isNothing mw ) $ reject 
	 $ text "Sie haben f�r dieses  n  gar kein  p  angegeben."
    inform $ text pre2
    when ( not $ contains l w ) $ reject 
	 $ text "p  ist aber gar nicht in L"  
    when ( length w < n ) $ reject
	 $ text  "es gilt nicht: |p| >= n"            

    let zs =  zerlegungen w n :: [ z ]
    let m = length zs
    let zzs = sort $ nub $ do 
	    guard $ m > 0
            k <- take 5 [ m `div` 2 .. ]
	    let i = ( k^3 + k + 13 ) `mod` m
	    return $ zs  !! i
    inform $ vcat
	   [ text $ "p  hat  " ++ show (length zs) ++ "  erlaubte Zerlegungen, unter anderem:"
	   , toDoc zzs
	   ]

    mapM_ ( \ z ->  when ( null $ expos l n z ) $ do
                newline 
		reject $ vcat $ map text
		  [ "Die Zerlegung " ++ show z 
                  , "erf�llt die Pump-Eigenschaft DOCH,"
		  , "denn soweit ich sehe, gilt f�r alle  i : "
		    ++ inflate_show_i z ++ " in  L."
		  ]
          ) zs
    inform $ text $ "OK. F�r jede Zerlegung gibt es ein  i  mit  " 
	       ++ inflate_show_i ( undefined :: z )  ++ " not in L."
    newline


expos :: Pumping z
      => Language -> Int -> z
      -> [ Int ]
-- die exponenten, deren inflation nicht in der sprache ist
expos l n z = do
     i <- [ 0 .. 20 ] -- ARBITRARY
     let w' = inflate i z
     guard $ not $ contains l w'
     return i



	   

       
