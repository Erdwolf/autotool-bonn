module Grammatik.CF.Erreichbar

-- $Id$

( erreichbar
)

where

import Control.Monad (guard)

import Set
import Fix
import Grammatik.Type


erreichbar :: Grammatik -> Set Char
erreichbar g = fix ( \ qs -> union qs $ mkSet $ do
    ( [lhs] , rhs ) <- rules g
    guard $ lhs `elementOf` qs   
    p <- rhs
    guard $ p `elem` vars g
    return p
  ) (unitSet $ startsymbol g)


    
