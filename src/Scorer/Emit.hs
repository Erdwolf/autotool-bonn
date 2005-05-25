module Scorer.Emit where

--   $Id$

import Scorer.Config
import Scorer.Einsendung
import Scorer.Aufgabe
import Scorer.Util hiding ( size )

import Control.Types hiding ( size )
import Control.Vorlesung.DB
import Control.Aufgabe.DB
import qualified Control.Vorlesung.Typ as V
import qualified Control.Aufgabe.Typ as A
import qualified Control.Student.DB

import Autolib.FiniteMap hiding ( collect )
import Autolib.Set
import Autolib.Util.Sort

import Control.Monad ( guard , liftM )
import System.IO ( hFlush, stdout )


-- | druckt Auswertung für alle Aufgaben einer Vorlesung
emit :: Bool -> VNr -> DataFM -> IO ()
emit deco vnr fm0 = do
    vs <- Control.Vorlesung.DB.get_this vnr
    let name = case vs of
          [v] -> toString $ V.name v 
          _   -> show vnr

    mnrs <- Control.Vorlesung.DB.teilnehmer vnr
    let smnrs = mkSet $ map ( \ (_,(mnr,_,_)) -> mnr ) mnrs
    let fm = mapFM ( \ key val -> do
		  e <- val
		  guard $ matrikel e `elementOf` smnrs
		  return e
	      ) fm0

    putStrLn $ unlines
	     [ "", ""
	     ,  unwords [ "Auswertung für Lehrveranstaltung", name, ":" ] 
	     ]

    mapM_ (single deco) $ fmToList fm
    totalize deco fm
    inform


inform :: IO ()
inform = do
    putStrLn $ unlines 
	     [ unwords
	       [ "Dabei gibt es pro Score" , show scorePoints, "Punkte"
	       , "für die Plätze [1 ..", show scoreItems, "]" 
	       ]
	     , ""
	     ]

realize :: [ Einsendung ] -> [ Einsendung ]
realize es = take scoreItems -- genau 10 stück
	   $ filter ( (> 1023) . read . toString . matrikel) -- keine admins
	   $ es
    
-- | druckt Auswertung einer Aufgabe
single :: Bool -> ( ANr, [ Einsendung ] ) -> IO ()
single deco arg @( anr, es ) = do
    aufs <- Control.Aufgabe.DB.get_this anr
    let name = case aufs of
          [a] -> toString $ A.name a 
          _   -> show anr
    let header = unwords 
	       [ "Aufgabe" , name
	       , unwords $ if null es then [] else
	         [ "( beste bekannte Lösung", show (size $ head es), ")" ]
	       ]
	strich = replicate (length header) '-'

    let realized = realize es

    decorated <- if deco then mapM (liftM show . decorate) realized 
		         else return $ map show realized

    putStrLn $ unlines $ [ header , strich ] ++ decorated

decorate :: Einsendung -> IO ( SNr , Einsendung )
decorate e = do

   xs <- Control.Student.DB.snr_by_mnr ( matrikel e ) 

   case xs of []    -> return ( read "SNr 0" , e )
              (s:_) -> return (            s , e )

totalize :: Bool -> DataFM -> IO ()
totalize deco fm = do

    infos <- collect deco fm

    putStrLn $ unlines
	     $ [ "Top Ten"
	       , "-----------------------------------"
	       ] ++ do (i,p) <- infos
		       return $ unwords [ stretch 10 $ show p
					, ":"
					, stretch 10 i
					]

-- | gesamtliste der highscore
collect :: Bool 
	-> DataFM 
	->  IO [ ( String , Int ) ] -- ^ ( Matrikel, Punkt )
collect deco fm = do

    let nice (e,p) = if deco then do (s,_) <- decorate e
				     return (show s,p)
			     else return (show $ matrikel e,p)

    infos <- mapM nice $ do
	     (auf,es) <- fmToList fm
	     (e,p) <- zip (realize es) scorePoints
	     return (e,p)

    return $ take scoreItems
	   $ sortBy ( \ (_, p) -> negate p ) -- größten zuerst
	   $ fmToList
	   $ addListToFM_C (+) emptyFM infos

{-
collect :: DataFM 
	 ->  [ ( MNr, Int ) ] -- ^ ( Matrikel, Punkt )
collect fm = take scoreItems
	   $ sortBy ( \ (m, p) -> negate p ) -- größten zuerst
	   $ fmToList
	   $ addListToFM_C (+) emptyFM
	   $ do  ( auf, es ) <- fmToList fm
		 ( e, p ) <- zip ( realize es ) scorePoints 
		 return ( matrikel e, p )
-}
