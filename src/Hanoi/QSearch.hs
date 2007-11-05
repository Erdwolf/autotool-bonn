module Hanoi.QSearch where

import Autolib.Util.Hide
import Autolib.ToDoc

import Control.Monad ( guard, when )
import Data.Set ( Set )
import Data.Maybe
import qualified Data.Set as S

search :: Ord a
       => ( a -> [ (z, a) ] ) -- ^ ( move, neighbour reached )
       -> ( [z] -> a -> Double ) -- ^ badness (0 == finished)
       -> a -- ^ start
       -> [(Double, a, [z])] -- ^ list of good move sequences
search neigh badness start = 
    let helper done todo = case S.minView todo of
             Nothing -> []
             Just (t @ ( b, c, Hide zs ), odo ) -> 
                 let next = S.fromList $ do
                        ( z, c' ) <- neigh c
                        guard $ not $ S.member c' done
                        let zzs = z : zs
                        return ( badness zzs c', c', Hide zzs )
                 in  ( b, c, reverse $ zs ) 
                     : helper ( S.insert c done ) 
                              ( S.union odo next )
    in  helper S.empty $ S.singleton ( badness [] start, start, Hide [] )
       

decreasing ( x @ (b,_,_): rest ) = 
    x : decreasing ( filter ( \ (c,_,_) -> c < b ) rest )
decreasing _ = []
