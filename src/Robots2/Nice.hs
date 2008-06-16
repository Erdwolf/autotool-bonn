module Robots2.Nice where

-- -- $Id$

import Robots2.Data
import Robots2.Config
import Robots2.Hull

import Autolib.ToDoc
import Autolib.Set

import Data.Array
import Data.Maybe
import Data.Char

-- | hübsches layout
instance Nice Config where
  nice k = vcat $ do
    let bereich @ ((l,u), (r,o)) = hull_with_goals k
                                     --  this is important here, since...
    let f = array bereich $ do p <- range bereich
			       return ( p, '.' )
    let a = if show_hull k
	    then f // do p <- setToList $ c_hull k 
		         return ( p, 'h' )
	    else f
    let b = a // do r <- positions k
		    return ( r, '*' )
    let c = b // do r <- goals k
		    let c = if r `elementOf` inhalt k then '#' else '+'
                                    --  ... we depend on it :-)
		    return ( r, c )
    y <- reverse [ u .. o ]
    return $ text $ do x <- [ l .. r ] ; [ c ! (x, y) , ' ' ]

