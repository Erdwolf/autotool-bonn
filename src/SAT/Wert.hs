module SAT.Wert where

-- -- $Id$

import SAT.Types
import Autolib.FiniteMap
import Control.Monad ( mzero )

class Wert a where 
      wert :: a -> Belegung -> Maybe Bool

instance Wert Variable where
    wert v b = lookupFM b v

instance Wert Literal where
    wert (Pos v) b = wert v b
    wert (Neg v) b = fmap not $ wert v b

instance Wert Klausel where
    wert (Or lits) b = do
        let ws = map (flip wert b) lits
        if Just True `elem` ws
	   then return True
	   else if Nothing `elem` ws
		then mzero
		else return False

instance Wert Formel where
    wert (And cls) b = do
        ws <- mapM (flip wert b) cls
        return $ and ws


