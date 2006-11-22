module Suggest where

class Suggest a where 
      suggest :: [a]

instance Suggest a where
      suggest = []


