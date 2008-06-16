module Robots3.Hull where

--  $Id$

import Robots3.Data
import Robots3.Config
import Robots3.Exact

import Autolib.Set

import Data.Maybe
import Data.Ix

hull = rectangle




exact_hull :: Config -> Set Position
exact_hull k = exact_hull_points $ mkSet $ map position $ robots k


-- | kleinstes überdeckendes rechteck ( unten links, oben rechts )
rectangle :: Config -> (Position, Position)
rectangle k = 
    let ps = map position $ robots k
	xs = map x ps ; ys = map y ps
    in	( Position ( minimum xs) ( minimum ys ) 
	, Position ( maximum xs) ( maximum ys) 
	)


-- | ist jedes Ziel im Hüll-rechteck?
covered :: Config -> Bool
covered k = 
    let h = hull k
    in	and $ map (inRange h) $ goals k

    
-- | kleinstes überdeckendes rechteck, basierend auch auf den Positionen
-- | der ziele
hull_with_goals :: Config -> (Position,Position)
hull_with_goals k =     
    let ps = goals k ++ ( map position $ robots k )
	xs = map x ps ; ys = map y ps
    in	( Position ( minimum xs) ( minimum ys ) 
	, Position ( maximum xs) ( maximum ys) 
	)

