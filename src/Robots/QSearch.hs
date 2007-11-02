module Robots.QSearch where

import Robots.Solver
import Robots.Config
import Robots.Data
import Robots.Examples

import Autolib.Util.Hide
import Autolib.ToDoc

import Control.Monad ( guard, when )
import Data.Set ( Set )
import Data.Maybe
import qualified Data.Set as S

search :: Ord a
       => ( a -> [ (z, a) ] ) -- ^ ( move, neighbour reached )
       -> ( a -> Double ) -- ^ badness (0 == finished)
       -> a -- ^ start
       -> [(Double, a, [z])] -- ^ list of good move sequences
search neigh badness start = 
    let helper done todo = case S.minView todo of
             Nothing -> []
             Just (t @ ( b, c, Hide zs ), odo ) -> 
                 let next = S.fromList $ do
                        ( z, c' ) <- neigh c
                        guard $ not $ S.member c' done
                        return ( badness c', c', Hide $ z : zs )
                 in  ( b, c, zs ) 
                     : helper ( S.insert c done ) 
                              ( S.union odo next )
    in  helper S.empty $ S.singleton ( badness start, start, Hide [] )
       

badness c = fromIntegral ( goal_distance c * area c ) 
          / ( fromIntegral ( length $ robots c )  )

goal_distance c = sum $ do
    r <- robots c
    return $ case ziel r of
        Nothing -> 0
        Just (x,y)  -> let (a,b) = position r
                       in  abs ( signum (a-x) ) + abs ( signum (b-y) )


    
decreasing [] = []
decreasing ( x @ (b,_,_): rest ) = 
    x : decreasing ( filter ( \ (c,_,_) -> c < b ) rest )

qsolve k = do
    let bczs = takeUntil ( \ (b,_,_) -> b <= 0 )
                  $ decreasing
                  $ search znachfolger_all_onboard badness k
    when False $ mapM_ ( \ (b,c,zs) -> 
        print $ besides [ vcat [ toDoc b, toDoc ( length zs ) ]
                        , nice c, toDoc zs 
                        ]
          ) bczs
    return $ last bczs

ist_final k = and $ do
    r @ Robot { ziel = Just z, position = p } <- robots k
    return $ z == p

takeUntil p  [] = []
takeUntil p (x : xs) = x : if p x then [] else takeUntil p xs