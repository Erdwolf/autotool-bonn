module Term.Rewrite where

--   $Id$

import Term.Type
import Term.Match

import Maybe
import Schichten
import Data.Set


next :: TRS a -> Term a -> [ Term a ]
next trs t = do
    p <- positions t
    let s = peek t p
    (l, r) <- trs
    sub <- maybeToList $ match l s
    return $ poke t p $ apply sub r

successors :: Ord a => TRS a -> Term a -> [ Term a ]
-- 0, 1, .. schritte (evtl unendliche liste!)
successors trs t = do
    ts <- schichten ( mkSet . next trs ) t
    setToList ts

