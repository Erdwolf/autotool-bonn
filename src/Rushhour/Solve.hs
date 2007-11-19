module Rushhour.Solve where


import Rushhour.Data
import Rushhour.Move ( solved )

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Set
import Autolib.Util.Hide 
import Autolib.Schichten

import Data.Array
import Control.Monad ( guard )

solutions ::  Int -- ^  max length (depth)
	  -> Instance 
	  -> [(Instance, [Zug])]
solutions depth i = do
    ( k, zs ) <- reachables depth i
    guard $ solved k
    return ( k, zs )

reachables :: Int -- ^  max length (depth)
	  -> Instance 
	  -> [(Instance, [Zug])]
reachables depth i = do
    let nach (k, m) = mkSet $ do
	    (z, k') <- next k
	    return ( k', Hide $ z : unHide m )
    kms <- take depth $ schichten nach ( i, Hide [] )
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


    