module Robots.Nice where

-- -- $Id$

import Robots.Data
import Robots.Konfig
import Robots.Hull

import ToDoc
import Array
import Maybe
import Char

nice :: Konfig -> Doc
-- hübsches layout
nice k = vcat $ do
    let bereich @ ((l,u), (r,o)) = hull k
    let a = array bereich $ do p <- range bereich
			       return ( p, '.' )
    let b = a // do r <- robots k
		    return ( position r, head $ name r )
    let c = b // do r <- robots k
		    z <- maybeToList $ ziel r
		    return ( z, toLower $ head $ name r )
    y <- reverse [ u .. o ]
    return $ text $ do x <- [ l .. r ] ; [ c ! (x, y) , ' ' ]

