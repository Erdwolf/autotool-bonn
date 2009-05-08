module Grammatik.CF.Nullable where

--  $Id$

import Grammatik.Type
import Autolib.Set
import Autolib.Util.Fix
import Control.Monad ( guard )


-- |  alle Variablen V mit V ->> Eps
nullable :: Grammatik -> Set Char
nullable g = fix ( \ ns -> mkSet $ do
        ( [ lhs ] , rhs ) <- rules g
	guard $ and [ x `elementOf` ns | x <- rhs ]
	return lhs ) emptySet

-- | Grammatik erzeugt leeres Wort?
creates_epsilon :: Grammatik -> Bool
creates_epsilon g = startsymbol g `elementOf` nullable g
