{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Robots.Config 

( Config -- abstract
, c_hull, cluster_of
, showing_hull, show_hull
, breit
, make, make_with_hull, geschichte
, move, remove, addZug
, look, robots, inhalt
, positions, goals
, valid
, bounds, area
)

where

--  $Id$

import Robots.Data
import Robots.Exact

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
		     , inhalt :: FiniteMap String Robot
		     , breit :: Integer
		     , geschichte :: [ Zug ]
		     , c_hull :: Set Position
		     , c_clusters :: FiniteMap Position Int
		     , show_hull :: Bool
		     }
     deriving ( Typeable )

cluster_of k p = lookupFM ( c_clusters k ) p 

make :: [ Robot ] -> Config
make rs = 
    let i = listToFM $ do 
	      r <- rs
	      return ( name r, r )
    in  hulled $ Config 
	{ c_hash = hash i
	, inhalt = i
	, breit = maximum $ do 
               r <- rs 
	       let (x,y) = position r
	       map abs [x,y]
	, geschichte = []
	, show_hull = False
	}

make_with_hull :: [ Robot ] -> Config
make_with_hull rs = showing_hull $ make rs

-- | recompute hull (from scratch)
hulled k = 
    let ps = mkSet $ map position $ robots k 
	fm = listToFM $ do
	       ( i, cl ) <- zip [ 0 .. ] $ clusters ps
	       p <- setToList cl
	       return ( p, i )
    in  k { c_hull = exact_hull_points ps
	  , c_clusters = fm
	  }

showing_hull k = k { show_hull = True }

bounds k = 
    let ps = do r <- robots k ; return $ position r
        xs = map fst ps ; ys = map snd ps
    in  ( ( minimum xs, minimum ys )
        , ( maximum xs, maximum ys )
        )

area k = 
    let ((a,b),(c,d)) = bounds k
    in  (c-a +1) * (d-b+1)

-- | fort damit (into outer space)
remove :: String -> Config -> Config
remove n k = 
    let i = delFromFM (inhalt k) n
    in  hulled $ k { inhalt = i
	  , c_hash = hash i
	  }

-- | auf neue position
move :: (String, Position) -> Config -> Config
move (n, p) k = 
    let i = inhalt k
        j = addToFM i n 
	      $ let r = fromMaybe ( error "Robots.Move.move" ) ( lookupFM i n )
		in  r { position = p }
    in  hulled $ k { inhalt = j
	  , c_hash = hash j
	  }

addZug :: Zug -> Config -> Config
addZug z k = k { geschichte = z : geschichte k }

instance ToDoc Config where
    toDoc k = text "make" <+> toDoc ( robots k )

instance Reader Config where
    atomic_readerPrec p = do
        guard $ p < 9
        my_reserved "make"
        arg <- reader
        return $ make arg

instance Container Config [ Robot ] where
    label _ = "Config"
    pack = robots
    unpack = make

instance Hash Config where hash = c_hash

-- | die mit ziel werden echt verglichen,
-- | von den anderen nur die positionen
essence :: Config -> ( Int32, Set Robot, Set Position )
essence k = 
    let rs = robots k
	(zs, ns) = partition ( isJust . ziel ) rs
    in	(hash k, mkSet zs, mkSet $ map position ns)

instance Ord Config where
    compare k l = compare (essence k) (essence l)
instance Eq Config where
    (==) k l = (==)  (essence k) (essence l)

instance Size Config where
    size = fromIntegral . area

-----------------------------------------------------------------

look :: Config -> String -> Maybe Robot
look c n = lookupFM (inhalt c) n

robots :: Config -> [ Robot ]
robots c = eltsFM (inhalt c) 

positions :: Config -> [ Position ]
positions = map position . robots

goals :: Config -> [ Position ]
goals k = do r <- robots k ; maybeToList ( ziel r )

valid :: Config -> Reporter ()
valid k = do
    let mappe = addListToFM_C (++) emptyFM $ do
	      r <- robots k
	      return ( position r, [ name r ] )
    let mehrfach = do 
	      ( p, rs ) <- fmToList mappe
	      guard $ length rs > 1
	      return ( p , rs )
    inform $ text "Stehen alle Roboter auf verschiedenen Positionen?"
    if ( null mehrfach )
	   then inform $ text "Ja."
	   else reject $ text "Nein, diese nicht:" <+> toDoc mehrfach 




