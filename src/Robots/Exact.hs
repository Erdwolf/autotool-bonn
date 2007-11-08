module Robots.Exact where

import Robots.Data
import Autolib.Set
import Control.Monad ( guard )
import Data.List ( tails )

clusters :: Set Position -> [ Set Position ]
clusters ps = fixpoint cmeet 
	    $ do p <- setToList ps ; return $ unitSet p

cmeet :: [ Set Position ] -> [ Set Position ]
cmeet [] = []
cmeet [c] = [c]
cmeet ( c : ds ) = 
    let ds' :: [ Set Position ]
	ds' = cmeet ds
        news = do
	    d <- ds'
	    let new :: [ Position ]
		new = do
	            x <- setToList c
		    y <- setToList d
		    guard $ same_line x y
		    interval_rectangle x y
            return $ case new of
		 [] -> Left d
		 _  -> Right (d, mkSet new )
        combined :: [ Position ]
        combined = setToList c ++ do
	    Right (d, new) <- news
	    setToList new ++ setToList d
        solitary :: [ Set Position ]
	solitary = do
	    Left old <- news
	    return old
    in  if null combined 
	then c : ds'
	else mkSet combined : solitary


exact_hull_points :: Set Position -> Set Position
exact_hull_points ps = fixpoint meet ps 

fixpoint f x = 
    let y = f x  in  if x == y then x else fixpoint f y

meet ps = mkSet $ do
    (p,q) <- all_pairs ps
    if same_line p q
       then interval_rectangle p q
       else [p,q]

same_line (a,b) (c,d) = a == c || b == d

interval_rectangle (a,b) (c,d) = do
    x <- [ min a c .. max a c ]
    y <- [ min b d .. max b d ]
    return (x, y)

all_pairs ps = do
    p : qs <- tails $ setToList $ ps
    q <- qs
    return (p,q)
