module Hanoi.Quiz where

--  $Id$


import Hanoi.Type
import Hanoi.Semantik

import Inter.Types
import Inter.Quiz

import Autolib.Util.Zufall
import Autolib.FiniteMap

import Control.Monad ( guard )

roll :: Conf -> IO HI
roll conf = do
    let turms = take ( turme conf ) [ A .. ]
        dist = sequence $ replicate (scheiben conf) $ eins turms
	collect dist = listToFM $ do 
	    t <- turms
	    return ( t , do 
                ( k, d ) <- zip [ 1 .. ] dist
		guard $ t == d
		return k
              )
    from <- dist
    to <- dist
    return $ HI { start = collect from
		, ziel = collect to
		}

instance Generator Hanoi Conf HI where
    generator p conf key = roll conf

instance Project Hanoi HI HI where
    project p hi = hi

make :: Make
make = quiz Hanoi $ Conf { scheiben = 6 , turme = 3 }
