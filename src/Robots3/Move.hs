module Robots3.Move where

--  $Id$

import Robots3.Data
import Robots3.Config
import Robots3.Nice

import Autolib.Set
import Autolib.Schichten
import Autolib.FiniteMap
import Data.Maybe
import Data.List (sortBy)
import Control.Monad ( foldM )

import Autolib.Reporter
import Autolib.ToDoc

offset :: Richtung -> Position
offset N = Position  0 1 
offset O = Position 1 0 
offset S = Position  0 (negate 1) 
offset W = Position (negate 1)  0

gegen d = case d of
    N -> S; S -> N; W -> O; O -> W

blocking dir p q = 
    let d = offset dir
    in	if 0 == x d 
	then x p == x q && (y d * y p  < y d * y q)
	else y p == y q  && (x d * x p < x d * x q )

-- | alle, die in dieser Richtung im Weg sind
blocks :: Config -> Position -> Richtung -> [ Position ]
blocks k p d = filter ( blocking d p ) 
	     $ map position
	     $ robots k

-- | letzter freier platz in dieser richtung
slide :: Config -> Position -> Richtung -> Maybe Position
slide k p dir =
    let bs = sortBy ( \ q r -> if blocking dir q r then LT else GT )
	   $ blocks k p dir
	d = offset dir
    in	case bs of
	     [] -> Nothing
	     q : rest -> Just $ q - d

execute :: Config -> Zug -> Maybe Config
execute k z @ ( n, d ) = do
    r <- look k n
    p <- slide k ( position r ) d 
    return $ addZug z $ move   (n, p) k

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
reverse_executes k z @ (n, dir) = do
    r <- maybeToList $ look k n
    let p = position r
        dir' = gegen dir
	d = offset dir'
    case slide k p dir of
         Just q | q == p -> do
             -- ist blockiert in richtung d,
	     -- könnte also von gegenüber gekommen sein
             let st = case slide k p dir' of
                   Nothing -> Position ( if 0 == x d then x p else breit k * x d
			      ) ( if 0 == y d then y p else breit k * y d
			      )
		   Just q  -> q
	     zw <- zwischen dir' p st 
             return $ move (n, zw) k
         _ -> []

zwischen dir p  st = drop 1 $ takeWhile (/= st) $ do
    let d = offset dir
    k <- [ 0 .. ]
    let dk = scalar k d
    return $ p + dk

executes :: Config -> [ Zug ] -> Reporter Config    
executes k [] = do
    inform $ nice k
    return k
executes k (z : zs) = do
    inform $ vcat [ nice k , nest 4  $ text "nächster Zug:" <+> toDoc z ] 
    case Robots3.Move.execute k z of
	   Nothing -> reject $ text "nicht erlaubt" 
	   Just k' -> executes k' zs



