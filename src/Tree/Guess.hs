{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

module Tree.Guess where

import qualified Autolib.Genetic as G

import Autolib.Util.Zufall
import Autolib.ToDoc
import Tree.Like 

import Control.Monad ( guard )
import System.IO

data Config t a =
     Config { goal :: a
            , eval :: t -> a -- ^ interpretation into domain
            , distance :: a -> a -> Double -- ^ metric (if 0, goal ist reached)
            , weight :: t -> Double -- ^ minimize weight
            , generate :: IO t -- ^ generator
            , mutate :: t -> IO t -- ^ problem specific mutator
            , population :: Int -- ^ size
            }

run conf = G.evolve $ make conf

make :: ( Show t , Tree.Like.Class t b )
     => Config t a -> G.Config t Double
make conf = G.Config
        { G.fitness = \ y -> 
              let b = eval conf y
                  d = distance conf ( goal conf ) b
              in  negate $ weight conf y + 1000 * d
        , G.threshold = 0 -- ?
        , G.present = score conf
        , G.trace   = score conf
        , G.size = population conf
        , G.generate = generate conf
        , G.combine = combine 
        , G.num_combine = population conf
        , G.mutate  = often 5 $ mutation conf
        , G.num_mutate = population conf
        , G.num_compact = 2 -- ?
        }

score :: Show t
      => Config t a -> [ (Double, t) ] -> IO ()
score conf vas = mapM_ printf $ take 5 $ do
    (v, x) <- vas
    return ( weight conf x, v, x ) 

printf x = do
    print x
    hFlush stdout

-----------------------------------------------------------------------------

often 0 action x = return x
often k action x = do y <- action x ; often ( k - 1 ) action y

mutation conf x = do
    action <- eins [ combine x x 
                   , compress x, swap x
		   , eins $ x : smaller x
                   , mutate conf x
		   ]
    action

compress x = do
    p <- eins $ positions x
    let y = peek x p
    q <- eins $ positions y
    return $ poke x p $ peek y q

swap x = do
    p <- eins $ positions x
    s <- subswap $ peek x p
    return $ poke x p s

subswap t = do
    cs <- permutation $ children t
    return $ build ( label t ) cs

combine x y = do
    p <- eins $ positions x
    q <- eins $ positions y
    let z = poke x p $ peek y q
    return z

