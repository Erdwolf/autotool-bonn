module Robots.Nice where

-- -- $Id$

import Robots.Data
import Robots.Config
import Robots.Hull

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
		         return ( p, '*' )
	    else f
    let b = a // do r <- robots k
		    return ( position r, head $ name r )
    let c = b // do r <- robots k
                                    --  ... we depend on it :-)
		    z <- maybeToList $ ziel r
		    return ( z, toLower $ head $ name r )
    y <- reverse [ u .. o ]
    return $ text $ do x <- [ l .. r ] ; [ c ! (x, y) , ' ' ]

