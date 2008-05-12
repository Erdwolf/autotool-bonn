module WFA.Semiring where

data Semiring a = 
     Semiring { zero :: a
              , one  :: a
              , plus :: a -> a -> a
              , times :: a -> a -> a
              }

