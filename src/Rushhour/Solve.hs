module Rushhour.Solve where


import Rushhour.Data
import Rushhour.Move ( solved )

import Robots.QSearch ( search )

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Set
import Autolib.Util.Hide 
import Autolib.Schichten

import Data.Array
import Control.Monad ( guard )

solutions ::  Int -- ^  max length (depth)
          -> Int -- ^  max width (of search tree level)
	  -> Instance 
	  -> [(Instance, [Zug])]
solutions depth width i = do
    ( k, zs ) <- reachables1 depth width i
    guard $ solved k
    return ( k, zs )

reachables1 depth width i = do 
    (d, k, zs ) <- take ( depth * width ) $ search next obstructions i
    return ( k, reverse zs )

obstructions i = sum $ do
    let occ = occupied i
        Just t = lookupFM ( cars i ) ( target i )
        (x, y) = position t
        (dx, dy) = offset $ orientation t
    p <- takeWhile ( inRange ( bounds occ ) ) $ do
        d <- [ extension t .. ]
        return ( x+d*dx, y+d*dy )
    guard $ not $ null $ occ ! p
    return 1


reachables0 :: Int -- ^  max length (depth)
          -> Int -- ^  max width (of search tree level)
	  -> Instance 
	  -> [(Instance, [Zug])]
reachables0 depth width i = do
    let nach (k, m) = mkSet $ do
	    (z, k') <- next k
	    return ( k', Hide $ z : unHide m )
    kms <- take depth 
         $ takeWhile ( \ s -> cardinality s <= width )
         $ schichten nach ( i, Hide [] )
    (k,m) <- setToList kms
    return (k, reverse $ unHide m )

next :: Instance ->  [ (Zug, Instance) ]
next i = do
    let occ = occupied i
    ( name, car ) <- fmToList $ cars i
    let lo @ ( x, y ) = position car
	( dx, dy ) = offset $ orientation car
	e = extension car - 1
	hi = ( x + e * dx, y + e * dy )
    ((xx,yy), dir) <- [ (lo, -1), (hi, 1) ]
    let ok p = inRange ( bounds occ ) p && null ( occ ! p )
    (o, _) <- takeWhile ( ok . snd ) $ do
            o <- map ( dir * ) [ 1 .. ]
	    return ( o, (xx+o*dx, yy+o*dy) )
    let z = (name, o )
	q = (x + o*dx, y+o*dy )
    let i' = i { cars = addToFM ( cars i ) name ( car { position = q } ) }
    return ( z, i')


    