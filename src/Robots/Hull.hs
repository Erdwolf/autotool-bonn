module Robots.Hull where

import Robots.Type
import Maybe

hull :: Konfig -> (Position, Position)
-- kleinstes überdeckendes rechteck ( unten links, oben rechts )
hull k = 
    let ps = map position $ robots k
	xs = map fst ps ; ys = map snd ps
    in	( ( minimum xs, minimum ys ) , ( maximum xs, maximum ys) )

inrange ::  (Position, Position) -> Position -> Bool
inrange ((l,u),(r,o)) (x,y) = 
	l <= x && x <= r && u <= y && y <= o

covered :: Konfig -> Bool
-- ist jedes Ziel im Hüll-rechteck?
covered k = 
    let h = hull k
    in	and $ map (inrange h) $ goals k

    