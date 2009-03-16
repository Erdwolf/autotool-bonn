module Strings.Reverse.Data where

import Autolib.Size
import Autolib.ToDoc

data Exp a = Reverse ( Exp a )
           | Plus ( Exp a ) ( Exp a )
           | Empty
           | Item a

instance ToDoc a => ToDoc ( Exp a ) where
    toDoc x = case x of
        Reverse y -> parens (toDoc y) <> text "^R"
        Plus y z -> toDoc y <+> toDoc z
        Empty -> empty
        Item i -> toDoc i

instance ToDoc a => Show ( Exp a ) where
    show = render . toDoc

instance Size ( Exp a ) where
    size x = 1 + case x of
        Reverse y -> size y
        Plus y z -> size y + size z
        _ -> 0

make :: [a] -> [Int] -> Exp a
make items [] = Empty
make items [x] = Item $ items !! (x `mod` length items)
make items (x : y : []) = Plus ( make items [x] ) ( make items [y] )
make items (x : xs) = case odd x of
    False -> let  k = x `div` 2
                  ( pre, post ) = splitAt ( 1 + k `mod` (length xs -1) ) xs
             in   Plus ( make items pre ) ( make items post )
    True  -> Reverse ( make_no_r items xs )

make_no_r items [] = Empty
make_no_r items [x] = Item $ items !! (x `mod` length items)
make_no_r items (x : y : []) = Plus ( make items [x] ) ( make items [y] )
make_no_r items (k : xs) = 
    let  ( pre, post ) = splitAt ( 1 + k `mod` (length xs -1) ) xs
    in   Plus ( make items pre ) ( make items post )



original :: Exp a -> [a]
original x = case x of
    Reverse y -> original y
    Plus y z -> original y ++ original z
    Empty -> []
    Item i -> [i]

semantics :: Exp a -> [a]
semantics x = case x of
    Reverse y -> reverse $ semantics y
    Plus y z -> semantics y ++ semantics z
    Empty -> []
    Item i -> [i]


