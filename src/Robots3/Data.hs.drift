{-# OPTIONS -fglasgow-exts  #-}

module Robots3.Data where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Data.Typeable
import Data.Ix

data Robots3 = Robots3 deriving ( Typeable )
data Robots3_Inverse = Robots3_Inverse deriving ( Typeable )

{-! for Robots3 derive : Reader, ToDoc !-}
{-! for Robots3_Inverse derive : Reader, ToDoc !-}

data Position = Position { x :: Int, y :: Int }
     deriving ( Eq, Ord, Typeable )

{-! for Position derive : Reader, ToDoc !-}


instance Hash Position where hash p = hash (x p, y p)

same_line p q = x p == x q || y p == y q


interval_rectangle p q = do
    a <- [ min (x p) (x q) .. max (x p) (x q) ]
    b <- [ min (y p) (y q) .. max (y p) (y q) ]
    return $ Position { x = a, y = b }

extension p = maximum $ map abs [ x p, y p ]


instance Ix Position where
    range (p, q ) = do a <- [x p .. x q]; b <- [y p .. y q ] ; return $ Position a b
    index (p, q) r = fromIntegral $ (x r - x p) * (y q - y p + 1) + (y r - y p)
    inRange (p, q) r = x p <= x r && x r <= x q && y p <= y r && y r <= y q 
    rangeSize (p,q) = fromIntegral $ (x q - x p + 1) * (y q - y p + 1)

instance Num Position where
    p + q = Position ( x p + x q ) ( y p + y q)
    negate p = Position ( negate $ x p ) ( negate $ y p )

scalar k p = Position ( k * x p ) ( k * y p )

data Robot = Robot { name :: String
		   , position :: Position
		   }
     deriving ( Eq, Ord, Typeable )

{-! for Robot derive : Reader, ToDoc !-}



instance Hash Robot where
    hash r = hash ( name r, position r )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

{-! for Richtung derive : Reader, ToDoc !-}


richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

data Zug = Zug { robot :: String, richtung :: Richtung }
     deriving ( Eq, Ord, Typeable )
     
{-! for Zug derive : Reader, ToDoc !-}

instance Size Zug where size _ = 1

-- local variables:
-- mode: haskell
-- end:
