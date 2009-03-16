module Strings.Reverse.Data where

import Autolib.Size

data Exp a = Reverse ( Exp a )
           | Plus ( Exp a ) ( Exp a )
           | Empty
           | Item a
    deriving Show

instance Size ( Exp a ) where
    size x = 1 + case x of
        Reverse y -> size y
        Plus y z -> size y + size z
        _ -> 0

make :: [a] -> [Int] -> Exp a
make items [] = Empty
make items [x] = Item $ items !! (x `mod` length items)
make items (x : xs) = case odd x of
    False -> let  k = x `div` 2
                  ( pre, post ) = splitAt ( k `mod` length xs ) xs
             in   Plus ( make items pre ) ( make items post )
    True  -> Reverse ( make items xs )

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


