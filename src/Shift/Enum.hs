module Shift.Enum where

import Data.List ( tails )
import Control.Monad ( guard )

subsets :: Int -> [a] -> [[a]]
subsets 0 xs = return []
subsets s xs = do
    guard $ not $ null $ drop (s-1) xs
    y : rest <- tails xs
    ys <- subsets (s-1) rest
    return $ y : ys
