module Grammatik.CF.Produktiv 

-- $Id$

( produktiv
)

where

import Control.Monad (guard)

import Fix
import Set
import Grammatik


produktiv :: Grammatik -> Set Char
produktiv g = fix ( \ qs -> union qs $ mkSet $ do
    ( [lhs] , rhs ) <- rules g
    guard $ and [    x `elementOf` terminale g 
		  || x `elementOf` qs
		| x <- rhs ]
    return lhs
  ) emptySet


    
