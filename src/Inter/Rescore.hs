-- module main where

{-

input: log file name with lines like:

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) Ein-Gleich : OK # Size: 7 
( all other lines are ignored )

action: read the corresponding input file and re-do the evaluation

output: ?

-}

import Scorer.Einsendung ( Einsendung (..), slurp )

import Inter.Store ( location, load, store )
import Inter.Bank ( logline )
import Inter.Boiler ( boiler )
import Inter.Types
import Inter.Evaluate
import Inter.Timer 
import qualified Inter.Param as P

import ToDoc
import Reporter

import Control.Monad ( guard )
import System

patience :: Int
patience = 60 -- seconds

main :: IO ()
main = do
    variants <- boiler
    args <- getArgs
    contents <- mapM readFile args
    let einsendungen = slurp $ concat contents
    mapM_ ( rescore $ blank { P.variants = variants } ) einsendungen
    
blank = P.Param 
	   { P.matrikel = error "Rescore.matrikel"
	   , P.passwort = error "Rescore.passwort"
	   , P.problem  = error "Rescore.problem"
	   , P.aufgabe  = error "Rescore.aufgabe"
	   , P.version  = error "Rescore.version"
	   , P.input    = error "Rescore.input"
	   , P.ident    = error "Rescore.ident"
	   , P.highscore = error "Rescore.highscore"
	   , P.anr      = error "Rescore.anr"
	   , P.variants = error "Rescore.variants"
	   , P.input_width = 80
	   , P.variante = error "Rescore.variante"
	   }

rescore :: P.Type
	-> Einsendung 
	-> IO ()
rescore p0 e = do
    let ( aufg , '-' : vers  ) = span (/= '-') $ auf e

    let vs = do
	    vv @ ( Variant v ) <- P.variants p0
	    guard $ ( aufgabe v == aufg ) && ( version v == vers )
	    return vv
    case vs of
	 [ vv @ ( Variant v ) ] -> do
	     let p1 = p0
	 	   { P.matrikel = show $ matrikel e
	 	   , P.problem  = show $ problem v
	 	   , P.aufgabe  = aufgabe v
	 	   , P.version  = version v
	 	   , P.variante = vv
	 	   }

             cs <- load p1 ( pid e ) True
	     let p2 = p1 { P.input = cs }

	     k <- key  v $ P.matrikel p2
	     generator <- gen v k
             let ( Just i, com :: Doc ) = export generator

	     ( res :: Maybe Int , com :: Doc ) 
	          <- timed_run patience ( reject $ text "timer expired" ) $ do
	                 evaluate ( problem v ) i p2

	     putStr $ logline (time e) (pid e) p2 res




