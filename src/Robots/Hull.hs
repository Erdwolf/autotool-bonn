module Robots.Hull where

--  $Id$

import Robots.Data
import Robots.Config
import Data.Maybe

-- | kleinstes überdeckendes rechteck ( unten links, oben rechts )
hull :: Config -> (Position, Position)
hull k = 
    let ps = map position $ robots k
	xs = map fst ps ; ys = map snd ps
    in	( ( minimum xs, minimum ys ) , ( maximum xs, maximum ys) )

inrange ::  (Position, Position) -> Position -> Bool
inrange ((l,u),(r,o)) (x,y) = 
	l <= x && x <= r && u <= y && y <= o

-- | ist jedes Ziel im Hüll-rechteck?
covered :: Config -> Bool
covered k = 
    let h = hull k
    in	and $ map (inrange h) $ goals k

    