module Shift.Generate 

-- $Id$

where

import Shift.Type
import Shift.Computer
import Shift.Meta
import Shift.Enum

import Util.Zufall

import IO

import List ( inits, tails, sort )
import Monad ( guard, when, foldM )

type Pool = [(Pins, Int)]

handle :: Pool -> Pins -> IO Pool
handle pool ps = do
    let per = pii ps 
    let it = ( ps, per )
    if per > 100 
       then do
            -- putStr $ show per ++ "," ; hFlush stdout
	    mapM (comp it) pool
	    return $ it : pool
       else return pool

geeceedee xs = foldr gcd 0 $ filter (/= 0) xs

comp (ps, p) (qs, q) | p < q = comp (qs,q) (ps,p)

comp (ps, p) (qs, q) = do
    let ds = zipWith (-) ps qs
    let g = geeceedee ds
    when ( p > q && all (>= 0) ds && g > 1 ) $ do
        case  mf $ Meta { start = qs, diff = ds } of
            i @ ( _, _, ps, ds ) : rest ->  do
                let g = length ds - 1
		putStr $ show g ++ " "
                when ( g > 2 ) $ do
		     putStrLn $ "\n" ++ show g ++ " : " ++ show i
		hFlush stdout
            _ -> return ()

single :: Int -> Int -> IO Pins
single b h = do
    ps <- einige b [ 1 .. h ]
    return $ sort ps

mainf b h = 
    foldM handle []  $ subsets b [ 1 .. h ]

pee ps = 
    let m = maximum $ 0 : ps
    in  ffind (next0 m ps) $ replicate m True

pii ps = 
    let (q, p) = find $ zustands_folge ps
    in p

mf me = do
    let ups = do -- unendlich! 
	    k <- [ 0 .. ] 
	    return $ pii $ vector me k
    (ds, ps) <- take 1
	      $ filter ( \ (ds, ps) -> length ds < length ps )
	      $ takeWhile ( \ ( ds, ps ) -> all (>= 0) ds ) $ do
	             ps <- inits ups
		     return ( diffs ps, ps )
    let l = length ps
    let rs = predict ds
    guard $ take l rs == take l ( drop l ups )
    return ( start me, diff me, take (2*l) ups, ds )

    


diffs :: [Int] -> [Int]
diffs xs = helper [] xs 


helper diffs [] = diffs
helper diffs (x : xs) = 
        let ds = update diffs x
	in  helper ds xs

update [] x = [x | x /= 0]
update (d : ds) x = x : update ds (x - d)
	
predict :: [Int] -> [Int]
predict ds = tail $ map head $ iterate next ds
    where next [d] = [d]
	  next (d : ds) = 
	      let es = next ds
	      in  ( d + head es ) : es
