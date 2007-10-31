module Robots.Move where

--  $Id$

import Robots.Data
import Robots.Config
import Robots.Nice

import Autolib.Set
import Autolib.Schichten
import Autolib.FiniteMap
import Data.Maybe
import Data.List (sortBy)
import Control.Monad ( foldM )

import Autolib.Reporter
import Autolib.ToDoc

offset :: Richtung -> ( Integer, Integer )
offset N = ( 0, 1 )
offset O = ( 1, 0 )
offset S = ( 0,-1 )
offset W = (-1, 0 )

gegen d = case d of
    N -> S; S -> N; W -> O; O -> W

blocking d (px,py) (qx,qy) = 
    let (dx, dy) = offset d
    in	if 0 == dx 
	then px == qx && (dy * py < dy * qy)
	else py == qy && (dx * px < dx * qx)

-- | alle, die in dieser Richtung im Weg sind
blocks :: Config -> Position -> Richtung -> [ Position ]
blocks k p d = filter ( blocking d p ) 
	     $ map position
	     $ robots k

-- | letzter freier platz in dieser richtung
slide :: Config -> Position -> Richtung -> Maybe Position
slide k p d =
    let bs = sortBy ( \ q r -> if blocking d q r then LT else GT )
	   $ blocks k p d
	( dx, dy ) = offset d
    in	case bs of
	     [] -> Nothing
	     (qx, qy) : rest -> Just (qx-dx, qy-dy)

execute :: Config -> Zug -> Maybe Config
execute k z @ ( n, d ) = do
    r <- look k n
    case slide k ( position r ) d of
	 Nothing -> case ziel r of
			 Nothing -> return $ addZug z $ remove n k  
			 -- Roboter mit Ziel darf nicht verschwinden
			 Just z  -> Nothing  
	 Just p  -> return $ addZug z $ move   (n, p) k

direct_predecessors :: Config -> [ Config ]
direct_predecessors k = do
    ( n, r ) <- fmToList $ inhalt k
    d <- richtungen
    reverse_executes k ( n, d )
    
predecessors :: Config -> [[ Config ]]
predecessors k = 
    map setToList $ schichten ( mkSet . direct_predecessors ) k

-- | alle möglichen Vorgänger einer Konfiguration
reverse_executes :: Config -> Zug -> [ Config ]
reverse_executes k z @ (n, d) = do
    r <- maybeToList $ look k n
    let p @ (x,y) = position r
        d' = gegen d
	(dx', dy') = offset d'
    case slide k p d of
         Just q | q == p -> do
             -- ist blockiert in richtung d,
	     -- könnte also von gegenüber gekommen sein
             let st = case slide k p d' of
                   Nothing -> ( if 0 == dx' then x else breit k * dx'
			      , if 0 == dy' then y else breit k * dy'
			      )
		   Just q  -> q
	     zw <- zwischen d' p st 
             return $ move (n, zw) k
         _ -> []

zwischen d p @ (x,y) st = drop 1 $ takeWhile (/= st) $ do
    let (dx, dy) = offset d
    k <- [ 0 .. ]
    return (x + k*dx, y + k*dy)

executes :: Config -> [ Zug ] -> Reporter Config    
executes k [] = do
    inform $ nice k
    return k
executes k (z : zs) = do
    inform $ vcat [ nice k , nest 4  $ text "nächster Zug:" <+> toDoc z ] 
    case Robots.Move.execute k z of
	   Nothing -> reject $ text "nicht erlaubt" 
	   Just k' -> executes k' zs



