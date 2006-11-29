module Lambda.Tree 

( toTree, display, peng )

where

import Lambda.Data
import Tree
import Autolib.Dot.Dotty

instance ToTree Lambda where
    toTree t = case t of
        Variable v -> Node ( show v ) []
        Apply fun arg -> Node "@" [ toTree fun, toTree arg ]
        Abstract var body -> Node ( "\\" ++ show var ) [ toTree body ]


