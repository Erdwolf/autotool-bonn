module Sortier.Programm.Example where

import Sortier.Programm.Type

import Autolib.TES.Identifier

import Data.List

bubble :: [ Identifier ] -> Program
bubble xs = Sequence $ do
    ys <- reverse $ inits xs
    x : y : _ <- tails ys
    return $ If_Greater_Else x y ( Sequence [ Swap x y ] ) ( Sequence [] ) 

nonsense :: [ Identifier ] -> Program
nonsense xs @ (x: y : zs) = Sequence 
    $ unSeq ( line $ reverse xs )
    ++ [ If_Greater_Else x y ( line xs ) ( line zs ) ]

unSeq ( Sequence xs ) = xs

line :: [ Identifier ] -> Program
line (x : ys) = Sequence $ do
    (y, z) <- take ( length ys `div` 2 ) $ zip ys $ reverse ys
    return $ If_Greater_Else x y ( Sequence [ Swap x z ] ) ( Sequence [] ) 
