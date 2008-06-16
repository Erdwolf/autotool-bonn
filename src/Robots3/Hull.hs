module Robots3.Hull where

--  $Id$

import Robots3.Data
import Robots3.Config
import Robots3.Exact

import Autolib.Set

import Data.Maybe

hull = rectangle




exact_hull :: Config -> Set Position
exact_hull k = exact_hull_points $ mkSet $ map position $ robots k


-- | kleinstes überdeckendes rechteck ( unten links, oben rechts )
rectangle :: Config -> (Position, Position)
rectangle k = 
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

    
-- | kleinstes überdeckendes rechteck, basierend auch auf den Positionen
-- | der ziele
hull_with_goals :: Config -> (Position,Position)
hull_with_goals k =     
    let ps = goals k ++ ( map position $ robots k )
	xs = map fst ps ; ys = map snd ps
    in	( ( minimum xs, minimum ys ) , ( maximum xs, maximum ys) )
