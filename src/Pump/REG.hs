module Pump.REG 

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
               { u :: String, v :: String, w :: String }
     deriving (Read, Eq, Ord)

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
        ( ab, c ) <- take n $ splits p
        ( a, b  ) <- splits ab
        guard $ not (null b)
        return $ Zerlegung { u = a, v = b, w = c }

    exempel = Zerlegung { u = "", v = "", w = "" }

instance ToDoc Zerlegung where
    toDoc g =
         let teile = do
                 (name, wert) <- [ ("u", u g), ("v", v g), ("w", w g) ]
                 return $ text name <+> equals <+> toDoc wert
         in      text "Zerlegung"
             <+> braces ( fsep $ punctuate comma $ teile )

instance Show Zerlegung where show = render . toDoc


    
