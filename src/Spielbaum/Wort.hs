module Spielbaum.Wort where

-- this file is copied only (source: /autotool/autobahn/Wort.hs)
-- reason for copying: autotool/util contains another Wort.hs

import Spielbaum.Next
import ToDoc

import Data.List (inits, tails)
import Control.Monad (guard)

data Regel a = 
     Regel { from :: [a]
	   , to :: [a]
	   }

data Wort a = Wort { inhalt :: [a]
		   , regeln :: [ Regel a ]
		   }

instance ToDoc [a] => ToDoc (Wort a) where
    toDoc = toDoc . inhalt

instance ToDoc (Wort a) => Show (Wort a) where
    show = render . toDoc

instance Eq [a] => Eq (Wort a) where
    x == y = inhalt x == inhalt y 

instance Ord [a] => Ord (Wort a) where
    x `compare` y = inhalt x `compare` inhalt y 
	
instance Eq a => Next (Wort a) where
    next w = do
	let i = inhalt w
        ( pre, midpost ) <- zip ( inits i ) ( tails i )
	r <- regeln w
	let ( mid, post ) = splitAt (length $ from r) midpost
	guard $ mid == from r
	return $ w { inhalt =  pre ++ to r ++ post }

