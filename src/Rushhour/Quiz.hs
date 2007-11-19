module Rushhour.Quiz where

import qualified  Rushhour.Data as D
import qualified Rushhour.Config as C
import Rushhour.Solve

import Autolib.Util.Zufall
import Autolib.FiniteMap

create :: RandomC m
       => C.Config 
       -> m D.Instance
create c = do
    let i = D.Instance 
	  { D.width = C.width
	  , D.height = C.height
	  , D.cars = emptyFM
	  }
    i <- add_target_car c i
    i <- add_more_cars c i
    return i    

positions car = do
    let ( dx, dy ) = offset $ orientation car
    d <- [ 0 .. extension car - 1 ]
    let (x,y) = position car
    return (x+d*dx, y+d*dy)

add_car c i name = do
    let occ = occupied i
        free car = and $ do
	    p <- positions car
	    return $ inRange ( bounds occ ) && null ( occ ! p )
    car <- do
        e <- randomRIO ( C.min_extension c, C.max_extension c )
        o <- eins [ Horizontal, Vertical ]
        let ( dx, dy ) = offset o
        let ( ex, ey ) = ( e * dx, e * dy )
        x <- randomRIO ( negate ( D.width i ), D.width i - ex + 1 )
        y <- randomRIO ( negate ( D.height i ), D.height i - ey + 1 )
        return $ Car { orientation = o, extension = e, position = (x,y) }
      `repeat_until` free
    return $ i { cars = addToFM ( cars i ) name car }

    