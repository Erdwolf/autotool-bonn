module Grammatik.Erreichbar

-- -- $Id$

( erreichbar
)

where

import Control.Monad (guard)

import Set
import Fix
import Grammatik.Type

-- eigentlich ist das nur f�r CFG definiert.

-- wir verallgemeinern auf beliebige Grammatiken:
-- jede Vars. der rechten seite hei�t hier erreichbar,
-- falls alle Vars der linken seite erreichbar sind.

-- das ist sichere Approximation, d. h. 
-- L�schen aller nicht erreichtb. Var. �ndert die Sprache nicht

erreichbar :: Grammatik -> Set Char
erreichbar g = fix ( \ qs -> union qs $ mkSet $ do
    ( lhs , rhs ) <- rules g
    guard $ and $ do p <- lhs ; return $ p `elementOf` qs 
    q <- rhs
    guard $ q `elem` vars g
    return q
  ) ( unitSet $ startsymbol g )


    
