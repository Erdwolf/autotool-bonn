module WFA.Matrix 

( Matrix
, make, get
, zero, unit
, plus, times 
)

where

import WFA.Semiring ( Semiring )
import qualified WFA.Semiring 

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set ) 
import qualified Data.Set as S

data Matrix s t a = Matrix 
                { semiring :: Semiring a
                , domain   :: Set s
                , range    :: Set t
                , contents :: Map (s,t) a 
                }

make :: ( Ord s, Ord t ) => Semiring a -> [ (s, a, t) ] -> Matrix s t a
make s items = Matrix
    { semiring = s
    , domain  = S.fromList $ do (x,w,y) <- items; return x
    , range   = S.fromList $ do (x,w,y) <- items; return y
    , contents = M.fromList $ do
         (x,w,y) <- items
         return ( (x,y), w )
    }

zero :: Ord s => Semiring a -> Set s -> Matrix s s a
zero s q = make s []

unit :: Ord s => Semiring a -> Set s -> Matrix s s a
unit s q = make s $ do
    x <- S.toList q
    return ( x, WFA.Semiring.one s, x )

get :: ( Ord s, Ord t ) => Matrix s t a -> (s,t) -> a
get m x = M.findWithDefault ( WFA.Semiring.zero $ semiring m ) x ( contents m )

plus :: ( Ord s, Ord t ) => Matrix s t a -> Matrix s t a -> Matrix s t a
plus a b = 
    let s = semiring a
    in  make s $ do
            (x,y) <- S.toList $ S.union ( M.keysSet $ contents a ) ( M.keysSet $ contents b ) 
            return ( x, WFA.Semiring.plus s ( get a (x,y )) ( get b (x,y) ) , y )

times :: ( Ord s , Ord t, Ord u ) 
      => Matrix s t a -> Matrix t u a -> Matrix s u a
times a b = 
    let s = semiring a
        front = domain a
        mid   = S.intersection ( range a ) ( domain b )
        back  = range b
    in  make s $ do
            x <- S.toList front
            z <- S.toList back
            let p = foldr ( WFA.Semiring.plus s ) ( WFA.Semiring.zero s ) $ do
                                 y <- S.toList mid
                                 return $ WFA.Semiring.times s  ( get a (x,y) ) ( get b (y,z) )
            return ( x, p, z )
 








