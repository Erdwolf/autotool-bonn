module Main where

import IO
import HTMLMonad 
import CGI
import Char -- toLower

-- autoan-modules
import HTMLshortcuts
import SQLqueries
import Helper

--  
-- TODO 
-- SQL-Exception fangen
-- Seiten Struktur rausziehen: stdpage ttl bdy menu
main :: IO ()
main = 
	do
	run [] $ showGruppenPage F0


showGruppenPage F0 = do
	  (h,freegrps)	<- io $ getFreeGruppenDB 
	  (h,allgrps)   <- io $ getAllGruppenDB
	  mglAufgs  <- io $ mglNextAufgabenDB []
	  let	{allgrps'= [  v ++ ", " ++  g ++ ", " ++ r 
			   |  (gnr , [v,g,r]) <- allgrps , not ( gnr `elem` ( Prelude.map fst freegrps ))
			   ]
		;freegrps'= [ (gnr , c ++ "/" ++ m ++ " " ++ v ++ ", " ++  g ++ ", " ++ r) 
			    |  (gnr , [v,g,r,m,c]) <- freegrps , length m < 5
			    ]
		}
	  standardQuery "Uebungsgruppen Uebersicht" $ 
			table $ do
				th3 "Freie Gruppen"
				mapM_ ttxt (Prelude.map snd freegrps' )
				spacerow
				th3 "Volle Gruppen"
				mapM_ ttxt allgrps'
				spacerow
				th3 "Mgl. Aufgaben"
				showAsTable2 mglAufgs
				hrrow 
				smallSubButton F0 showGruppenPage "Update"			
				
