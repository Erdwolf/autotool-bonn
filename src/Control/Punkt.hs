-- | werden von Face.cgi benutzt: login zum aufgabenl�sen und scoring

module Control.Punkt where

--  $Id$

import Control.SQL
import Control.Types
import Control.Passwort
import Inter.Crypt

import Control.Monad
import Data.Maybe

-- | Login des Studenten Version 2
--
-- Input:   Matrikelnr., Passwort
-- Output:  IO Just SNr zur�ck, wenn (mnr,pass) in DB
--
loginDB :: MNr -> Control.Passwort.Type -> IO (Maybe SNr)
loginDB mnr pass =
    do
       conn <- myconnect
       stat <- squery conn $ Query
	       ( Select $ map reed [ "SNr", "Passwort" ] )
	       [ From $ map reed [ "student" ]
	       , Where $ equals ( reed "student.MNr" ) ( toEx mnr )
	       ]
       inhs <- collectRows ( \ state -> do
                            s <- getFieldValue state "SNr"
                            p <- getFieldValue state "Passwort"
                            return (s, reed p) -- FIXME: instance Sqlbind Crypt
                          ) stat
       disconnect conn

       logged $ "toString pass: " ++ show (toString pass) 

       return $ case inhs of
           [ (s, p) ] -> do
		 guard $ Inter.Crypt.compare p (toString pass)
		 return s
           _ -> Nothing

set :: SNr -> ANr -> Wert 
    -> Maybe File -- ^ Eingabe des Studenten
    -> Maybe File -- ^ Bewertung (automatische)
    -> IO ()
set snr anr wert minput mreport = 
    bepunkteStudentDB snr anr wert Keine minput mreport

-- | erh�ht von Student, f�r Aufgabe (Ok,Size) \/ No 
--
-- Input: (SNr,ANr,{No,Ok size}, {High,Low,Keine} )
-- Output: IO ()
--
-- bepunkteStudentDB :: String -> String -> ATBewertung -> ATHighLow -> IO ()
-- bepunkteStudentDB snr anr bewert highlow = return ()

bepunkteStudentDB :: SNr 
		  -> ANr 
		  -> Wert 
		  -> HiLo 
		  -> Maybe File -- ^ input
		  -> Maybe File -- ^ report
		  -> IO ()
bepunkteStudentDB snr anr bewert highlow minput mreport = do
   conn <- myconnect
   stat <- squery conn $ Query 
	   ( Select [ reed "SNr" ] )
	   [ From [ reed "stud_aufg" ]
	   , Where $ ands
	           [ equals (reed "SNr") ( toEx snr )
		   , equals (reed "ANr") ( toEx anr )
		   ]
	   ]
   inh <- collectRows ( \ state -> do
                        ( b :: SNr ) <- getFieldValue state "SNr"
                        return b
                      ) stat

   -- prepare for scoring: make empty entry
   when ( null inh ) $ do 
       stat <- squery conn $ Query 
	   ( Insert (reed "stud_aufg") 
	            [ ( reed "SNr", toEx snr )
		    , ( reed "ANr", toEx anr )
		    , ( reed "Ok" , reed "0" ) 
		    , ( reed "No" , reed "0" )
		    , ( reed "Result" , ENull )
		    ] 
	   ) 
	   [] 
       return ()

   let okno = case bewert of
           Reset -> [ ( reed "Ok", reed "0" )      ]
           No    -> [ ( reed "No", reed "No + 1" ) ]
	   Ok s  -> [ ( reed "Ok", reed "Ok + 1" ) ]
	   Pending -> [ ]
       rest = return $ ( reed "Result", toEx bewert ) 
       inpt = maybeToList $ do 
                inp <-  minput
		return ( reed "Input", toEx inp )
       rept = maybeToList $ do
	        rpt <- mreport
		return ( reed "Report", toEx rpt )

   squery conn $ Query 
	   ( Update ( reed "stud_aufg" )
	            ( okno ++ rest ++ inpt ++ rept )
	   ) 
           [ Where $ ands
	           [ equals ( reed "SNr" ) ( toEx snr )
		   , equals ( reed "ANr" ) ( toEx anr )
                   ]
	   ]
   disconnect conn 
   return ()


{- -- historic code:
     where sizetime s =
             case highlow of 
             Keine   -> " " 
             High    -> " "
                        ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" - 1) < " ++ show s 
                       ++ " , Now(), Scoretime )"
                       ++ ", Size = GREATEST( Size," ++ show s ++ ")" 
             Low -> " "
                    ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" + 1 ) > " ++ show s
                   ++ " ,Now(), Scoretime )"
                   ++ ", Size = LEAST( Size," ++ show s ++ ")" 
-}


