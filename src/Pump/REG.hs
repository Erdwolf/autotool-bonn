module Pump.REG 

-- -- $Id$

( Zerlegung (..)
, Pump (..)
)

where

-------------------------------------------------------------------

import Pump.Type
import Pump.REG.Type

import Util.Splits
import Data.FiniteMap
import Reporter
import ToDoc


instance Pumping Zerlegung where
    tag z = "Pump-Eigenschaft für reguläre Sprachen"
    tag_show z = "u v w"

    inflate_show i z = "u v^" ++ show i ++ " w"
    inflate_show_i z = "u v^i w"

    admissable n z = do
        let [ lu, lv, lw ] = map length [ u z, v z, w z ]
	when ( lu + lv > n ) 
	     $ reject $ text $ "es gilt nicht: |u| + |v| <= " ++ show n
	when ( lv < 1 ) 
	     $ reject $ text $ "es gilt nicht: |v| >= 1"
        return ()

    inflate i z = u z ++ concat ( replicate i $ v z ) ++ w z

    zerlegungen p n = do
        ( ab, c ) <- takeWhile ( \ (ab,c) -> length ab <= n) 
		  $ splits p
        ( a, b  ) <- splits ab
        guard $ not (null b)
        return $ Zerlegung { u = a, v = b, w = c }

    exempel = Zerlegung { u = "", v = "", w = "" }




    
