import Strings.Reverse.Data

import Data.Map ( Map )
import qualified Data.Map as M

import Data.List
import Control.Monad ( guard )

complete :: Ord a => [a] -> Map [a] ( Exp a )
complete xs = M.union
    ( complete_reverse xs )
    ( complete_no_reverse xs )

complete_reverse :: Ord a => [a] -> Map [a] ( Exp a )
complete_reverse xs = M.fromList $ do 
    guard $ length xs > 1
    (k,v) <- M.toList ( complete_no_reverse xs )
    return (reverse k, Reverse v) 

complete_no_reverse :: Ord a => [a] -> Map [a] ( Exp a )
complete_no_reverse [ ] = M.fromList [ ( [], Empty ) ]
complete_no_reverse [x] = M.fromList [ ( [x], Item x ) ]
complete_no_reverse xs = M.fromList $ do
    ( pre, post ) <- splits xs
    guard $ not $ null pre
    guard $ not $ null post
    (k1, v1) <- M.toList $ complete pre
    (k2, v2) <- M.toList $ complete post
    return ( k1 ++ k2, Plus v1 v2 )

splits xs = zip ( inits xs ) ( tails xs )

{-
permutations :: [a] -> [[a]]
permutations [] = return []
permutations (x : xs) = do
    rest <- permutations xs
    ( pre, post ) <- splits rest
    return $ pre ++ x : post
-}

missing :: Int -> [ [Int] ]
missing k = do
    let w = [ 1 .. k ]
        m = complete w
    p <- permutations w
    guard $ not $ M.member p m
    return p
