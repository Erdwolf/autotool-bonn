module Scorer.Emit where

--   $Id$

import Scorer.Config
import Scorer.Einsendung
import Scorer.Aufgabe
import Scorer.Util hiding ( size )

import Control.Types hiding ( size )

import qualified Control.Vorlesung as V
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U

import Autolib.FiniteMap hiding ( collect )
import Autolib.Set
import Autolib.Util.Sort

import Autolib.ToDoc
import Autolib.Output ( Output )
import qualified Autolib.Output as O

import Control.Monad ( guard , liftM, when, forM )
import System.IO ( hFlush, stdout )
import Data.Char


-- | druckt Auswertung für alle Aufgaben einer Vorlesung
emit :: Bool -> U.Schule -> V.Vorlesung -> DataFM -> IO ( Maybe Output )
emit deco u vor fm0 = do

    studs <- V.steilnehmer $ V.vnr vor
    let smnrs = mkSet $ map S.mnr studs
    let fm = mapFM ( \ key val -> do
		  e <- val
		  guard $ internal ( matrikel e ) `elementOf` smnrs
		  return e
	      ) fm0
                                  
    if ( 0 < sizeFM fm )
       then do
          let header = O.Doc $ vcat
                  [ text $ unwords [ toString $ U.name u ]
	          , text $ unwords [ "Auswertung für Lehrveranstaltung"
                               , toString $ V.name vor, ":" 
                               ] 
	          ]
          out <- forM ( fmToList fm ) $ single deco (V.unr vor)
          to <- totalize deco (V.unr vor) fm
          return $ Just $ O.lead header $ foldr1 O.Above [ O.Itemize out, to, inform ]
       else return Nothing


inform :: Output
inform = O.Doc $ text $ unwords
	       [ "Dabei gibt es pro Score" , show scorePoints, "Punkte"
	       , "für die Plätze [1 ..", show scoreItems, "]" 
	       ]


realize :: [ Einsendung ] -> [ Einsendung ]
realize es = take scoreItems -- genau 10 stück
	   $ filter ( not . isadmin . matrikel) -- keine admins
	   $ es
    
-- | FIXME: this is badly broken
-- und zwar für Matrikelnummern, die keine Zahlen sind
isadmin m = 
    let cs = toString $ internal m
    in  if all isDigit cs
        then 1023 > read cs
        else False

-- | druckt Auswertung einer Aufgabe
single :: Bool -> UNr -> ( ANr, [ Einsendung ] ) -> IO Output
single deco u arg @( anr, es ) = do
    [ auf ] <- A.get_this anr
    let header = O.Text $ unwords 
	       [ "Aufgabe" , toString $ A.name auf
	       , unwords $ if null es then [] else
	         [ "( beste bekannte Lösung", show (size $ head es), ")" ]
	       ]

    let realized = realize es

    decorated <- if deco then mapM (liftM show . decorate u) realized 
		         else return $ map show realized
    let scored = O.Itemize $ map O.Text decorated


    let try = O.Named_Link  "Aufgabe ausprobieren (ohne Wertung)"
                $ "/cgi-bin/Trial.cgi?problem=" ++ Control.Types.toString ( A.anr auf )

    return $ O.lead (O.lead header try ) scored 


decorate :: UNr -> Einsendung -> IO SE
decorate u e = do

   studs <- S.get_unr_mnr ( u , internal $ matrikel e ) 

   case studs of 
       []    -> return $ SE ( read "SNr 0" ) e 
       (s:_) -> return $ SE ( S.snr s      ) e 

totalize :: Bool -> UNr -> DataFM -> IO Output
totalize deco u fm = do

    infos <- collect deco u fm

    return $ O.lead (O.Text "Top Ten") $ O.Doc $ vcat $ do 
                       (i,(p,ps)) <- infos
		       return $ text $ unwords [ stretch 10 $ show p
					, ":"
					, stretch 10 $ toString i
					, ":" 
					, pshow ps
					]

pshow ps = unwords $ [ stretch 4 $ show (length ps)
		     , "Platzierungen"
		     , ":"
		     , cshow ps
		     ]

cshow ps = fshow $ addListToFM_C (+) emptyFM $ zip ps $ repeat 1

fshow pfm = unwords $ do
	    p <- [1..10]
	    return $ stretch 3 $ case lookupFM pfm p of
	     Just k -> show k
	     _ -> []

-- | gesamtliste der highscore
collect :: Bool 
        -> UNr
	-> DataFM 
	->  IO [ ( Obfuscated MNr , (Int , [Int] ) ) ] -- ^ ( Matrikel, Punkt, Plätze )
collect deco u fm = do

    let nice (e,p) = if deco then do SE s _ <- decorate u e
				     return ( error "Scorer.Emit.collect") -- ( show s , p )
			     else return ( matrikel e , p )

    infos <- mapM nice $ do
	     (auf,es) <- fmToList fm
	     (e,p,k) <- zip3 (realize es) scorePoints [1..]
	     return (e,(p,[k]))

    return $ take scoreItems
	   $ sortBy ( \ (_,(p,_)) -> negate p ) -- größten zuerst
	   $ fmToList
	   $ addListToFM_C ( \ (x,xs) (y,ys) -> (x+y,xs++ys) ) emptyFM 
	   $ infos

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
