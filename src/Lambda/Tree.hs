module Lambda.Tree 

( toTree, display )

where

import Lambda.Data
import Tree

instance ToTree Lambda where
    toTree t = case t of
        Variable v -> Node ( show v ) []
        Apply fun arg -> Node "@" [ toTree fun, toTree arg ]
        Abstract var body -> Node ( "\\" ++ show var ) [ toTree body ]


