module Finite.Wort where

import Finite.Run
import ToDoc


data Direction = L | R
     deriving (Eq, Ord, Show, Read)
instance ToDoc Direction where toDoc = text . show

data Question = Exists Direction
	      | Read
     deriving (Eq, Ord, Show, Read)
instance ToDoc Question where toDoc = text . show


data Action = Go Direction
     deriving (Eq, Ord, Show, Read)
instance ToDoc Action where toDoc = text . show

data State = State { contents :: String
		   , position :: Int
		   }
     deriving (Eq, Ord, Show, Read)

instance ToDoc State where
     toDoc s = let ( pre, post ) = splitAt (position s) (contents s)
	       in  text $ show pre ++ " >> " ++ show post


instance World State Question Action where

    ask s (Exists L) = if position s > 0
		       then "true" else "false"
    ask s (Exists R) = if position s < length (contents s) - 1
		       then "true" else "false"
    ask s Read	     = [ contents s !! position s ]

    act s (Go L)     = s { position = pred $ position s }
    act s (Go R)     = s { position = succ $ position s }
