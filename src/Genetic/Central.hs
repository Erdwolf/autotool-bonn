{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-incoherent-instances #-}

module Genetic.Central 
( evolve 
, Pool, popul
, startup, handle
, connect
) 

where

--  $Id$

import Autolib.Util.Sort
import Autolib.Util.Uniq
import Autolib.Util.Perm
import Autolib.Util.Zufall
import Autolib.Util.Perm
import Autolib.ToDoc

import Control.Monad
import Autolib.FiniteMap
import Autolib.Set

-- import Autolib.Genetic.Config
import Genetic.Config

import Data.Maybe
import Data.IORef
import Control.Concurrent
import System.IO

data Pool a v =
     Pool { num :: Int
	  , popul :: [(v, a)]
          , previous :: v
          , config :: Config a v
          , input :: MVar a
          , outputs :: IORef [ MVar a ] 
          }

-- | output the best items after some steps of computation
evolve :: (Ord v, ToDoc v, ToDoc a, Ord a)
       => Config a v
       -> IO [(v,a)]
evolve conf = do
    pool <- startup conf
    pool' <- handle pool 
    return $ popul pool'

startup conf = do
    pool <- sequence $ replicate (size conf) (generate conf)
    v <- newEmptyMVar
    o <- newIORef []
    return $ Pool
	   { config = conf
           , num = 0
           , previous = fitness conf $ head pool
           , popul = map ( \ g -> ( fitness conf g, g ) ) pool
           , input = v
           , outputs = o
	   }

handle vpool = do
    let conf = config vpool
    trace conf $ popul vpool
    let tops = filter ( \ (v, g) -> v > previous vpool ) 
	     $ popul vpool
    when ( not $ null tops )
         $ present conf tops
    let good = filter ( \ (v, g) -> v >= threshold conf ) 
	     $ popul vpool
    let continue = case num_steps conf of
            Just bound -> num vpool < bound
            Nothing -> True
    if null good && continue 
       then do
           vpool' <- step vpool
           handle vpool'
       else do
	   return vpool

connect :: Pool a v 
     -> Pool a v
     -> IO ( )
connect p q = do
    modifyIORef ( outputs p ) ( input q : )
    modifyIORef ( outputs q ) ( input p : )

step :: ( Ord a, Ord v, ToDoc v, ToDoc a )
     => Pool a v
     -> IO (Pool a v)
step vpool = do
    let conf = config vpool
        pool = map snd $ popul vpool

    malien <- tryTakeMVar $ input vpool
    let aliens = do
          x <- maybeToList malien
          return ( fitness conf x, x )
    when ( not $ null aliens ) 
	 $ hPutStrLn stderr $ "got aliens " -- ++ show ( toDoc aliens )

    -- generate some new ones (crossing)
    combis <- sequence $ replicate (num_combine conf) $ do
		   [x, y] <- einige 2 pool
		   z <- combine conf x y
		   return (fitness conf z, z)

    -- mutations
    mutants <- sequence $ replicate (num_mutate conf) $ do
            x <- eins pool
	    z <- mutate conf x
	    return $ (fitness conf z, z)

    let vpool' = take (size conf)
	       $ compact (num_compact conf)
	       $ aliens ++ combis ++ mutants ++ popul vpool

    -- transport
    outs <- readIORef $ outputs vpool
    let ( _, top ) = head vpool'
    sequence $ do
        out <- outs
        return $ do 
            hPutStrLn stderr $ "sending alien " -- ++ show ( toDoc top )
            tryPutMVar out top

    return $ vpool
	   { num = succ $ num vpool
	   , popul = vpool'
           , previous = maximum $ map fst $ popul vpool
	   }

-- | keep at most fixed number per v class (and sort)
compact :: (Ord v, Ord a) 
	=> Int
	-> [(v, a)]
        -> [(v, a)]
compact d vas = reverse $ do
    let fm = addListToFM_C (++) emptyFM $ do
             (v, a) <- vas
             return (v, [a])
    (i, (v, as)) <- zip [1..] $ fmToList fm
    a <- take d $ setToList $ mkSet as
    return (v, a)


-- | entferne k zufällig gewählte elemente
remove :: Int -> [a] -> IO [a]
remove k xs = do
    ys <- permIO xs
    return $ drop k ys
    
update :: [a] -> (Int, a -> a) -> [a]
update xs (i, f) = poke xs (i, f $ xs !! i)

poke :: [a] -> (Int, a) -> [a]
poke xs (i, y) = 
    let (pre, _ : post) = splitAt i xs
    in  pre ++ [y] ++ post

pokes :: [a] -> [(Int, a)] -> [a]
pokes = foldl poke
