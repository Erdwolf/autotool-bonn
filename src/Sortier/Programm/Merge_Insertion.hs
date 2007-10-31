module Sortier.Programm.Merge_Insertion where

import Sortier.Programm.Type
import Autolib.TES.Identifier

type Stack = [ Identifier ]

sort :: [ Stack ] -> [ Statement ]
sort xs | length xs < 2 = []
sort xs =
    let ( ps, rest ) = pairs xs
        mkpairs = do 
            [ y, x ] <- ps
            return $ If_Greater_Else ( head x ) ( head y ) 
                       ( Sequence $ swaps x y )
                       ( Sequence [] )
    in     mkpairs
        ++ sort ps
        ++ insertions ps rest
        
insertions ps rest = 
    

pairs [] = ( [], [] )
pairs [x] = ( [], [x] )
pairs x : y : zs =
    let ( ps, rest ) = pairs zs
    in  ( [y,x] : ps, rest )

-- | result is in x : ys (ascending)
insert :: Stack -> [ Stack ] -> [ Statement ]
insert x [] = []
insert x ys = 
    let ( pre, mid : post ) = splitAt ( length ys `div` 2 ) ys
        ripple = do 
            ( x, y ) <- zip (x : pre) ( pre ++ [mid] )
            swaps x y
    in  return $ If_Greater_Else ( head x ) ( head mid )
             ( Sequence $ ripple ++ insert mid post )
             ( Sequence $ insert x pre )

swaps x y = do
    ( xx, yy ) <- zip x y
    return $ Swap xx yy

             
        