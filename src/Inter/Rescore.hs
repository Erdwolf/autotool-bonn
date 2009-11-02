-- module main where

{-

input: log file name with lines like:

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) Ein-Gleich : OK # Size: 7 
( all other lines are ignored )

action: read the corresponding input file and re-do the evaluation

output: ?

-}

import Scorer.Einsendung 

import Inter.Recommon

import Inter.Store ( location, load, store )
import Inter.Bank ( logline )
-- import Inter.Boiler ( boiler )

import Inter.Collector

import Inter.Types
import Inter.Evaluate
import qualified Inter.Param as P

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Vorlesung as V
import Control.Types


import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Timer 

import Control.Monad ( guard, forM )
import System

patience :: Int
patience = 60 -- seconds

main :: IO ()
main = do
    args <- getArgs
    contents <- mapM readFile args
    let ms = Inter.Collector.makers 
    forM_ ( slurp $ concat contents ) $ rescore ms

    
rescore :: [ Make ]
	-> Einsendung 
	-> IO ()
rescore mks e = do
    let m = internal $ matrikel e
    us <- U.get
    forM us $ \ u -> do
        studs <- S.get_unr_mnr ( U.unr u, m )
        forM studs $ \ stud -> do
            aufs <- A.get_this $ auf e
            forM aufs $ \ auf -> do
                -- print $ A.typ auf
                staufs <- SA.get_snr_anr 
                     ( S.snr stud ) ( A.anr auf )
                forM staufs $ \ stauf -> do
                   let [ mk ] = filter 
                                ( \ m -> show m == toString (A.typ auf)
                                ) mks
                   recompute_for_einsendung mk auf stauf
    return ()

{-


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

-}



