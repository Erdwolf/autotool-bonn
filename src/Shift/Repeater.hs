module Shift.Repeater ( repeater, clipper, nths ) where

-- import Util
import List
import Monad ( guard )

nths :: Int -> [a] -> [a]
-- take xs!!0, xs!!n, xs!!(2n), ..
nths n [] = []
nths n xs = head xs : nths n ( drop n xs )


-- recognize repetitive patterns in lists

repeater :: Eq a => [a] -> [ (Int, [a]) ]
repeater = collector . cutter

cutter :: Eq a => [a] -> [(Int, [a])]
cutter [] = []
cutter w = 
    let (i, u) = cut w
	rest = drop (i * length u) w
    in	if i > 1 then (i, u) : cutter rest
		 else (1, [head w]) : cutter (tail w)


clipper :: Eq a => a -> [a] -> [[a]]
clipper c [] = []
clipper c xs =
    let (pre, post) = span (/= c) xs
    in	pre : clipper c ( drop 1 post )

cut :: Eq a => [a] -> (Int, [a])
-- find best initial repetition
cut [] = (1, [])
cut w = maximumBy ( \ foo bar -> fst foo `compare` fst bar ) $ (1, w) : do
    u <- take 10 $ inits w
    guard $ not $ null u
    let (c, rest) = zahl u w
    guard $ c > 2
    return (c, u)

collector :: Eq a => [(Int, [a])] ->  [(Int, [a])]
-- collect repetition-free subseqs
collector [] = []
collector [(i,u)] = [(i,u)]
collector ((1,u) : ius) = 
   case collector ius of
	(1, v) : rest -> (1, u ++ v) : rest
	sonst -> (1,u) : sonst
collector ((i,u): rest) = (i,u) : collector rest

zahl :: Eq a => [a] -> [a] -> (Int, [a])
zahl u w = 
    let ( pre, post ) = splitAt (length u) w
	( c, rest ) = zahl u post
    in	if pre == u 
	then ( succ c , rest )
	else (      0 ,    w )

