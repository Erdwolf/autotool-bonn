-- module main where

{-

input: log file name with lines like:

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) VNr-ANr : OK # Size: 7 
Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) VNr-ANr : NO 


action: read the corresponding input file and re-do the evaluation

output: ?

-}

import Scorer.Einsendung 

import Inter.Recommon

import Inter.Store ( location, load, store )
import Inter.Bank ( logline )
-- import Inter.Boiler ( boiler )

import Inter.Collector
import Inter.Common

import Inter.Types
import Inter.Evaluate
import qualified Inter.Param as P

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Vorlesung as V
import Control.Types

import Util.Datei

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Timer 

import Control.Monad ( guard, forM )
import Data.Maybe ( isJust )
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
    let mat = internal $ matrikel e 
    let infile = Datei { pfad  = [ "done", toString ( vor e )
                                , toString ( auf e )
                                , toString mat
                                , if isJust ( msize e )
                                  then "OK" else "NO"
                                ]
                      , name = pid e
                      , extension = "input"
                      }
    input <- Util.Datei.lesen infile
    [ aufgabe ] <- A.get_this $ auf e

    let [ mk ] = filter ( \ m -> show m == toString (A.typ aufgabe) ) mks

    mres :: Maybe Wert <- case mk of 
      Make p tag fun verify conf -> do
        ( p, instant, icom ) <-  
            make_instant_common_with (A.vnr aufgabe) (Just $ A.anr aufgabe) 
                   ( error "S.Student" )
                   ( fun $ read $ toString $ A.config aufgabe ) 
                   ( toString mat )
        let ( res :: Maybe Wert , com :: Doc ) 
	        = export $ evaluate p instant input
        return res

    let param = P.Param { P.mmatrikel = Just mat
                            , P.vnr = A.vnr aufgabe
                            , P.anr = A.anr aufgabe
                            }
    case mres of
       Just res -> do
           putStr $ Inter.Bank.logline ( time e ) ( pid e ) param res


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



-}



