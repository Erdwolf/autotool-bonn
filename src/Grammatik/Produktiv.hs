module Grammatik.Produktiv 

-- -- $Id$

( produktiv
)

where

-- verallgemeiner von CFG auf beliebige Grammatiken
-- produktivit�t ist nat�rlich nicht entscheidbar
-- hier ist sichere Approximation in diesem sinne:
-- nach l�schen aller nicht produktiven variablen
-- gibts immer noch die gleiche sprache

import Control.Monad (guard)

import Fix
import Data.Set
import Grammatik

-- jede Variable der linken seite nennen wir produktiv,
-- wenn rechts alle Variablen produktiv sind.
-- zur vereinfachung z�hlen wir auch alle terminale als produktiv

produktiv :: Grammatik -> Set Char
produktiv g = fix ( \ qs -> union qs $ mkSet $ do
    ( lhs , rhs ) <- rules g
    guard $ and $ do
        x <- rhs
	return $  ( x `elementOf` qs )
    lhs
  ) ( terminale g )



    
