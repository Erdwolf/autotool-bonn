module Pump.CF

-- $Id$

( Zerlegung (..)
, Pump (..)
, module FiniteMap
)

where

-------------------------------------------------------------------

import Pump.Type

import Util.Splits
import FiniteMap
import Reporter
import ToDoc


data Zerlegung = Zerlegung
	       { u :: String, v :: String
	       , x :: String, y :: String, z :: String }
     deriving (Read, Eq, Ord)

instance Pumping Zerlegung where
    tag z = "Pump-Eigenschaft für kontextfreie Sprachen"
    tag_show z = "u v x y z"

    exempel = Zerlegung "" "" "" "" ""

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

-------------------------------------------------------------------------

instance ToDoc Zerlegung where
    toDoc g = 
	 let teile = do
	         (name, wert) <- [ ("u", u g), ("v", v g), ("x", x g), ("y", y g), ("z", z g) ]
		 return $ text name <+> equals <+> toDoc wert
	 in      text "Zerlegung" 
	     <+> braces ( fsep $ punctuate comma $ teile )

instance Show Zerlegung where show = render . toDoc


    
