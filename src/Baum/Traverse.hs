module Baum.Traverse where

--  $Id$

import Baum.Type

preorder :: Term a c -> [c]
preorder (Node f args) = [ f ] ++ concat ( map preorder args )

postorder :: Term a c -> [c]
postorder (Node f args) = concat ( map preorder args ) ++ [ f ]

-- | only for binary trees
inorder :: Term a c -> [c]
inorder (Node f []) = [ f ]
inorder (Node f [l,r]) = inorder l ++ [ f ] ++ inorder r

levelorder :: Term a c -> [c]
levelorder t = 
    let ts = t : concat ( map children ts )
    in    take (size t) -- to avoid the black hole at the end
	$ map top ts

