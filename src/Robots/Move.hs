module Robots.Move where

import Robots.Data
import Robots.Konfig

import Set
import FiniteMap
import Maybe
import List (sortBy)
import Monad ( foldM )
import Boc

remove :: String -> Konfig -> Konfig
-- fort damit (into outer space)
remove n ( Konfig k ) = Konfig $ delFromFM k n

move :: (String, Position) -> Konfig -> Konfig
-- auf neue position
move (n, p) ( Konfig k ) = Konfig $ addToFM k n 
	      $ let r = fromMaybe ( error "Robots.Move.move" ) ( lookupFM k n )
		in  r { position = p }

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

blocks :: Konfig -> Position -> Richtung -> [ Position ]
-- alle, die in dieser Richtung im Weg sind
blocks k p d = filter ( blocking d p ) 
	     $ map position
	     $ robots k

slide :: Konfig -> Position -> Richtung -> Maybe Position
-- letzter freier platz in dieser richtung
slide k p d =
    let bs = sortBy ( \ q r -> if blocking d q r then LT else GT )
	   $ blocks k p d
	( dx, dy ) = offset d
    in	case bs of
	     [] -> Nothing
	     (qx, qy) : rest -> Just (qx-dx, qy-dy)

execute :: Konfig -> Zug -> Maybe Konfig
execute k ( n, d ) = do
    r <- look k n
    return $ case slide k ( position r ) d of
	 Nothing -> remove n      k
	 Just p  -> move   (n, p) k
	
executes :: Konfig -> [ Zug ] -> Boc
executes k [] = 
    final k
executes k (z : zs) = 
    explain 0 ( vcat [ toDoc k , text "Zug:" <+> toDoc z ] )
    $ case execute k z of
	   Nothing -> ( False , text "nicht erlaubt" )
	   Just k' -> executes k' zs

