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

instance ToTree Term where
    toTree t = case t of
        Variable v -> Node ( show v ) []
        Apply f args -> Node ( show f ) ( map toTree args )
        
