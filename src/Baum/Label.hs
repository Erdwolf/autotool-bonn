module Baum.Label 

( label
, Order (..)
)

where

--  $Id$

import Baum.Type
import Baum.Traverse
import Control.Monad.State

-- | inlabel t cs = t'  
-- such that  (in)order t' = cs
-- and t' has same shape as t
label :: Order -> Term a b -> [ c ] -> Term d c
label o t cs = evalState ( work o t ) cs

type ST c = State [ c ]

pop :: ST c c
pop = do
   c : cs <- get
   put cs
   return c

work :: Order -> Term a b -> ST c ( Term d c )
work Pre ( Node f xs ) = do
    g <- pop
    ys <- mapM ( work Pre ) xs
    return $ Node g ys
