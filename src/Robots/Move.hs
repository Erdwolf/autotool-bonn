module Robots.Move where

--  $Id$

import Robots.Data
import Robots.Config
import Robots.Nice

import Autolib.Set
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
	

executes :: Config -> [ Zug ] -> Reporter Config    
executes k [] = do
    inform $ nice k
    return k
executes k (z : zs) = do
    inform $ vcat [ nice k , nest 4  $ text "nächster Zug:" <+> toDoc z ] 
    case Robots.Move.execute k z of
	   Nothing -> reject $ text "nicht erlaubt" 
	   Just k' -> executes k' zs

