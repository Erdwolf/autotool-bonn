module Baum.Traverse where

--  $Id$

import Baum.Type
import Baum.Order
import Autolib.ToDoc


traverse :: Order -> Term a c -> [ c ]
traverse o = case o of
    Pre   -> preorder
    In	  -> inorder
    Post  -> postorder
    Level -> levelorder

announce :: Order -> Doc -> Doc
announce o d =  hsep [ toDoc o <> text "order", parens d , equals ]

preorder :: Term a c -> [c]
preorder (Node f args) = [ f ] ++ concat ( map preorder args )

postorder :: Term a c -> [c]
postorder (Node f args) = concat ( map postorder args ) ++ [ f ]

-- | only for binary trees
inorder :: Term a c -> [c]
inorder (Node f []) = [ f ]
inorder (Node f [l,r]) = inorder l ++ [ f ] ++ inorder r

levelorder :: Term a c -> [c]
levelorder t = 
    let ts = t : concat ( map children ts )
    in    take (size t) -- to avoid the black hole at the end
	$ map top ts

