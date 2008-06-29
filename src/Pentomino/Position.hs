module Pentomino.Position where

data Position = Position { x :: Int, y :: Int }
    deriving ( Eq, Ord, Show )

instance Num Position where 
    p + q = Position { x = x p + x q
                     , y = y p + y q
                     }
    negate p = Position { x = negate ( x p )
                        , y = negate ( y p )
                        }

mirror :: Position -> Position
mirror p = Position { x = x p , y = negate $ y p }

turn :: Position -> Position
turn p = Position { x = negate $ y p, y = x p }
