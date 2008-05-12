module WFA.Matrix where

import WFA.Semiring ( Semiring )
import qualified WFA.Semiring 

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set ) 
import qualified Data.Set as S

data Matrix s a = Matrix 
                { semiring :: Semiring a
                , contents :: Map (s,s) a 
                }

make :: Ord s => Semiring a -> [ (s, a, s) ] -> Matrix s a
make s items = Matrix
    { semiring = s
    , contents = M.fromList $ do
         (x,w,y) <- items
         return ( (x,y), w )
    }

unit :: Ord s => Semiring a -> Set s -> Matrix s a
unit s q = Matrix
    { semiring = s
    , contents = M.fromList $ do
         x <- S.toList q
         return ( (x,x), WFA.Semiring.one s )
    }

get :: Ord s => Matrix s a -> (s,s) -> a
get m x = M.findWithDefault ( WFA.Semiring.zero $ semiring m ) x ( contents m )

plus :: Ord s => Matrix s a -> Matrix s a -> Matrix s a
plus a b = 
    let s = semiring a
    in  Matrix { semiring = s
               , contents = M.fromList $ do
                    x <- S.toList $ S.union ( M.keysSet $ contents a ) ( M.keysSet $ contents b ) 
                    return ( x, WFA.Semiring.plus s ( get a x ) ( get b x ) )
               }

times :: Ord s => Matrix s a -> Matrix s a -> Matrix s a
times a b = 
    let s = semiring a
        front = S.map fst $ M.keysSet $ contents a
        mid   = S.intersection ( S.map snd $ M.keysSet $ contents a )
                               ( S.map fst $ M.keysSet $ contents b )
        back  = S.map snd $ M.keysSet $ contents b
    in  Matrix { semiring = s
               , contents = M.fromList $ do
                    x <- S.toList front
                    z <- S.toList back
                    return ( (x,z), foldr ( WFA.Semiring.plus s ) ( WFA.Semiring.zero s ) $ do
                                 y <- S.toList mid
                                 return $ WFA.Semiring.times s  ( get a (x,y) ) ( get b (y,z) )
                           )
               }








