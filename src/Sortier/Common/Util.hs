module Sortier.Common.Util where

import qualified Autolib.Util.Wort

import Data.List ( tails ) 

-- | es gilt ja der satz: 
-- alle 0-1-folgen sortiert <=> überhaupt alle folgen sortiert.
-- also generiere ich 0-1-folgen (weil das weniger sind)
-- aber um die studenten nicht unnötig aufzuregen,
-- rechne ich es in folgen aus lauter verschiedenen elementen um
testing :: Int -> [ [Int] ]
testing soll = do
    w <- Autolib.Util.Wort.alle [ 0, 1 ] soll
    return $ umrech w    

-- | 0-1-Folge zu Zahlenfolge
-- mit gleicher Steigung, aber lauter verschiedenen Zahlen
-- dabei alle 0 aufsteigend, dann alle 1 aufsteigend
umrech :: [ Int ] -> [ Int ]
umrech w = um w 1 (1 + length w - sum w ) where
    um [] _ _ = []
    um (0 : xs) low high = low  : um xs (succ low) high
    um (1 : xs) low high = high : um xs low (succ high)

is_increasing :: Ord a => [a] -> Bool
is_increasing xs = and $ do
    x : y : rest <- tails xs
    return $ x <= y
