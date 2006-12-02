module PL.Tree 

( peng )

where

import PL.Data
import PL.ToDoc
import Tree
import Autolib.ToDoc

instance ToTree Formel where
    toTree f = case f of
        Operation op xs -> 
            Node ( show op ) 
                 $ map toTree xs
        Quantified q x f ->
            Node ( show $ toDoc q <+> toDoc x )
                 [ toTree f ]
        f -> Node ( show $ toDoc f )
                 []

