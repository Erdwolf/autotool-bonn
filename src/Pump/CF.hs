module Pump.CF

-- -- $Id$

( Zerlegung (..)
, Pump (..)
, module Autolib.FiniteMap
)

where

-------------------------------------------------------------------

import Pump.Type
import Pump.CF.Type

import Autolib.Util.Splits
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc


instance Pumping Zerlegung where
    tag z = "Pump-Eigenschaft für kontextfreie Sprachen"
    tag_show z = "u v x y z"

    exempel = Zerlegung "" "" "" "" ""

    exem z = let h = length z `div` 5
	         ( a, bcde ) = splitAt h z
		 ( b, cde ) = splitAt h bcde
		 ( c, de ) = splitAt h cde
		 ( d, e ) = splitAt h de
	     in  Zerlegung { u = a, v = b, x = c, y = d, z = e }

    inflate_show i z = "u v^" ++ show i ++ " x y^" ++ show i ++ " z"
    inflate_show_i z = "u v^i x y^i z"

    admissable n g = do
        let [ lu, lv, lx, ly, lz ] = map length [ u g, v g, x g, y g, z g ]
	when ( lv + lx + ly > n ) 
	     $ reject $ text $ "es gilt nicht: |vxy| <= " ++ show n
	when ( lv + ly < 1 ) 
	     $ reject $ text $ "es gilt nicht: |vy| >= 1"
        return ()

    inflate i g =  u g
	 ++ concat ( replicate i $ v g )
	 ++ x g
	 ++ concat ( replicate i $ y g )
	 ++ z g

    zerlegungen w n = do
        ( links, rest ) <- splits w
	( mitte, rechts ) <- takeWhile ( \ (m,r) -> length m <= n ) 
			   $ splits rest
	( a, bc ) <- splits mitte
	( b, c ) <- splits bc
	guard $ not $ null $ a ++ c
	return $ Zerlegung { u = links, v = a, x = b, y = c, z = rechts }





    
