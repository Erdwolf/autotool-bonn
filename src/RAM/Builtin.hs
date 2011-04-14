-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

-- | Eingebaute Standard Funktionen
module RAM.Builtin 

( Table, Entry, Builtin (..)
, table, get
, none, every
)

where

import Autolib.ToDoc
import Autolib.Reader
import Data.Array
import Autolib.Set
import Data.Typeable

data Builtin = Copy 
             | Plus  | Minus
             | Times | Div | Mod
             | Paar  | Links | Rechts
             | Suc   | Pre
     deriving ( Eq, Ord, Enum, Ix, Typeable )

$(derives [makeReader, makeToDoc] [''Builtin])


none :: Set Builtin
none = mkSet [ ]

-- | ohne Suc !
every :: Set Builtin
every = mkSet [ Copy .. Pre ]


-- | Tabellen Eintrag (arity, meaning)
type Entry =  ( Int 
              , [ Integer ] -> Integer  -- meaning
              )

type Table = Array Builtin Entry

table :: Table
table = array ( Copy, Pre )
              [ ( Copy , ( 1, \ [x   ] -> x ) )
              , ( Plus , ( 2, \ [x, y] -> x + y ) )
              , ( Minus, ( 2, \ [x, y] -> max 0 $ x - y ) )
              , ( Times, ( 2, \ [x, y] -> x * y ) )
              , ( Div  , ( 2, \ [x, y] -> if y == 0 then 0 else x `div` y ) )
              , ( Mod  , ( 2, \ [x, y] -> if y == 0 then 0 else x `mod` y ) )

              , ( Paar , ( 2, \ [x, y] -> 2^x * (2*y + 1) ) )
              , ( Links, ( 1, \ [x]    -> let (l,r) = decode x in l ) )
              , ( Rechts, ( 1, \ [x]    -> let (l,r) = decode x in r ) )

              , ( Suc , ( 1, \ [x   ] -> succ x ) )
              , ( Pre , ( 1, \ [x   ] -> max 0 $ pred x ) )
              ]


-- | bijektive Kodierung von N^2 <-> N
-- gegeben durch (x,y) <-> 2^x * (2*y + 1)
decode :: Integer -> ( Integer, Integer )
decode n = if odd n 
           then ( 0, n `div` 2 ) -- wird abgerundet 
           else let ( x, y ) = decode ( n `div` 2 )
                in  ( succ x, y )

get :: Builtin -> Entry
get b = table ! b
