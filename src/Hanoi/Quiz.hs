{-# LANGUAGE MultiParamTypeClasses #-}
module Hanoi.Quiz where

--  $Id$


import Hanoi.Type
import qualified Hanoi.Config as C
import Hanoi.Semantik

import Inter.Types
import Inter.Quiz

import Autolib.Util.Zufall
import Autolib.FiniteMap

import Control.Monad ( guard )

roll :: C.Config -> IO HI
roll conf = do
    let turms = take ( C.turme conf ) [ A .. ]
        dist = sequence $ replicate (C.scheiben conf) $ eins turms
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
		, restriction = C.restriction conf
		}

instance Generator Hanoi C.Config HI where
    generator p conf key = roll conf

instance Project Hanoi HI HI where
    project p hi = hi

make :: Make
make = quiz Hanoi $ C.example 

