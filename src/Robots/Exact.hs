module Robots.Exact where

import Robots.Data
import Autolib.Set
import Control.Monad ( guard )
import Data.List ( tails )


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
