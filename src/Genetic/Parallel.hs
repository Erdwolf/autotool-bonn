module Genetic.Parallel 

( evolve
)

where

import Genetic.Config
import qualified  Genetic.Central as C

import Autolib.ToDoc

import Control.Concurrent

evolve :: (Ord v, ToDoc v, ToDoc a, Ord a)
       => Config a v
       -> IO [(v,a)]
evolve conf = do
    pools <- startup conf
--    ring_buffer pools
    collector <- newChan
    handle pools collector
    result <- sequence $ replicate ( length pools ) $ readChan collector
    return result


handle pools sink = 
    sequence $ do 
        p <- pools
        return $ forkIO $ do
            result <- C.handle p
            mapM_ ( writeChan sink ) $ take 1 $ C.popul result

startup conf =
    sequence $ replicate ( num_parallel conf ) $ C.startup conf

ring_buffer pools = 
    sequence $ do
        (p, q) <- zip ( rotate 1 pools ) pools
        return $ C.connect p q


rotate k xs = 
    let ( pre, post ) = splitAt k xs
    in  post ++ pre
