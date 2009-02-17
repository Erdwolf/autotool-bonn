{-# language DeriveDataTypeable #-}

module Brainfuck.Syntax.Data where

import Autolib.Size
import Data.Typeable

data Statement 
    = Block [ Statement ]  
    | Plus
    | Minus
    | MRight
    | MLeft
    | Input
    | Output
    | Loop [ Statement ]
    | Null                 -- kommentare und co

    deriving ( Eq, Ord, Typeable )

instance Size Statement where
    size s = 1 -- FIXME
    
