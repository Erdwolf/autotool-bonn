{-# OPTIONS -fglasgow-exts #-}

module Robots2.Config 

( Config -- abstract
, c_hull, cluster_of
, showing_hull, show_hull
, breit
, make, make_with_hull, geschichte
, move, addZug
, inhalt
, positions, goals
, valid
, bounds, area
)

where

--  $Id$

import Robots2.Data
import Robots2.Exact

import Autolib.FiniteMap
import Autolib.Set

import Autolib.ToDoc
import Autolib.Hash
import Autolib.Reader
import Autolib.Size
import Autolib.Set
import Autolib.FiniteMap
import Autolib.Xml
import Autolib.Reporter

import Data.List (partition)
import Control.Monad ( guard )
import Data.Maybe ( isJust, maybeToList, fromMaybe )
import Data.Typeable
import Data.Int

data Config = Config { c_hash :: Int32
		     , inhalt :: Set Position
		     , targets :: Set Position
		     , breit :: Integer
		     , geschichte :: [ Zug ]
		     , c_hull :: Set Position
		     , c_clusters :: FiniteMap Position Int
		     , show_hull :: Bool
		     }
     deriving ( Typeable )

cluster_of k p = lookupFM ( c_clusters k ) p 

make :: [ Position ]
     -> [ Position ]
     -> Config
make starts goals = hulled $ Config 
	{ c_hash = hash $ mkSet starts
	, inhalt = mkSet starts
	, targets = mkSet goals
	, breit = maximum $ do 
               (x,y) <- starts 
	       map abs [x,y]
	, geschichte = []
	, show_hull = False
	}

make_with_hull :: [ Position ] -> [ Position ] -> Config
make_with_hull starts goals  = showing_hull $ make starts goals

-- | recompute hull (from scratch)
hulled k = 
    let ps = inhalt k
	fm = listToFM $ do
	       ( i, cl ) <- zip [ 0 .. ] $ clusters ps
	       p <- setToList cl
	       return ( p, i )
    in  k { c_hull = exact_hull_points ps
	  , c_clusters = fm
	  }

showing_hull k = k { show_hull = True }

bounds k = 
    let ps =  positions k 
        xs = map fst ps ; ys = map snd ps
    in  ( ( minimum xs, minimum ys )
        , ( maximum xs, maximum ys )
        )

area k = 
    let ((a,b),(c,d)) = bounds k
    in  (c-a +1) * (d-b+1)


-- | auf neue position
move :: (Position, Position) -> Config -> Config
move (from, to) k = 
    let j = addToSet (delFromSet (inhalt k) from) to
    in hulled $ k { inhalt = j
	  , c_hash = hash j
	  }

addZug :: Zug -> Config -> Config
addZug z k = k { geschichte = z : geschichte k }

instance ToDoc Config where
    toDoc k = text "make" <+> toDoc ( positions k ) <+> toDoc ( goals k )

instance Reader Config where
    atomic_readerPrec p = do
        guard $ p < 9
        my_reserved "make"
        arg1 <- reader
        arg2 <- reader
        return $ make arg1 arg2

instance Container Config ( [Position], [Position] ) where
    label _ = "Config"
    pack k = ( positions k, goals k )
    unpack ( i, t ) = make i t

instance Hash Config where hash = c_hash

essence :: Config -> ( Int32, Set Position )
essence k = ( hash k, inhalt k )

instance Ord Config where
    compare k l = compare (essence k) (essence l)
instance Eq Config where
    (==) k l = (==)  (essence k) (essence l)

instance Size Config where
    size = fromIntegral . area

-----------------------------------------------------------------

positions :: Config -> [ Position ]
positions = setToList . inhalt

goals :: Config -> [ Position ]
goals = setToList . targets

valid :: Config -> Reporter ()
valid k = return () 


