module Shift.Break

( Break
, grundy
, bond, off
)

where

import Control.Monad
import Array
import Data.Set
import Bits
import Int
import ToDoc

type Break = [[Int]]

bond :: Break
-- .007 = james bond
bond = [[], [], [0,1,2]] 

off :: Break 
-- .6 = officers
off = [[1,2]] 

grundy :: Break -> [ Int16 ]
grundy moves = 
    let b = ( 0, 2^13 )
        a = listArray b $ do 
	      n <- range b
	      let hss = options moves n
		  val hs = foldr xor 0 $ map (a!) hs
	      return $ mex $ map val hss 
    in	elems a

instance ToDoc Int16 where toDoc = text . show

mex :: [Int16] -> Int16
mex xs = head $ do 
    let s = mkSet xs
    x <- [ 0 .. ]
    guard $ not $ elementOf x s
    return x

options :: Break -> Int -> [[Int]]
options moves n = do
    ( k, hs ) <- zip [1..] moves
    h <- hs
    divide h (n - k)

divide :: Int -> Int -> [[Int]]
-- divide n into exactly h non-empty parts
divide h n = helper h n 1 

helper :: Int -> Int -> Int -> [[Int]]
-- as above, but each part must be >= m
helper 0 n m = do
    guard $ n == 0
    return [] -- no heap
helper h n m | h * m > n = mzero -- too small
helper 1 n m = return [ n ] -- exactly one heap
helper h n m = do
    x <- [ m .. n `div` h ]
    xs <- helper (h - 1) (n - x) x
    return $ x : xs

