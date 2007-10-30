module Sortier.Programm.Merge_Insertion where

import Prelude hiding ( insert )

import Sortier.Programm.Type
import Autolib.TES.Identifier

import Control.Monad.State

type Stack = [ Identifier ]



insert :: Stack -> [ Stack ] -> State [ Statement ] [ Stack ]
insert x [] = return [x]
insert x ys = do
    let ( pre, mid : post ) = splitAt ( length ys `div` 2 ) ys
    
        