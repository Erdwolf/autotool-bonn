{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module NFA.Genetic where


import Autolib.Util.Sort
import Autolib.Util.Uniq
import Autolib.Util.Perm
import Autolib.Util.Zufall
import Autolib.Util.Perm
import Autolib.ToDoc

import Control.Monad
import Data.FiniteMap
import Data.Set

-- TODO should not depend on IO that much
data Conf a v = 
     Conf { fitness :: a -> v
          , threshold :: v
	  , present :: Pool a v -> IO ()
	  , trace   :: Pool a v -> IO ()
	  , size    :: Int -- ^ will be fixed
	  , generate :: IO a
	  , combine :: a -> a -> IO a
          , num_combine :: Int
	  , mutate  :: a -> IO a
          , num_mutate :: Int
          , num_compact :: Int
          }

data Pool a v =
     Pool { num :: Int
	  , popul :: [(v, a)]
          , previous :: v
          }

step :: ( Ord a, Ord v )
     => Conf a v 
     -> Pool a v
     -> IO (Pool a v)
step conf vpool = do
    let pool = map snd $ popul vpool

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

    let vpool' = id -- take (size conf)
	       $ compact (num_compact conf)
	       $ combis ++ mutants ++ popul vpool

    return $ Pool
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
    a <- take i $ setToList $ mkSet as
    return (v, a)

evolve :: (Ord v, ToDoc [(v,a)], Ord a)
       => Conf a v
       -> IO [(v,a)]
evolve conf = do
    pool <- sequence $ replicate (size conf) (generate conf)
    let handle vpool = do
            trace conf vpool
            let tops = filter ( \(v,g) -> v > previous vpool ) 
		     $ popul vpool
	    when ( not $ null tops )
                 $ present conf $ vpool { popul = tops }
            let good = filter ( \(v,g) -> v >= threshold conf ) 
		     $ popul vpool
            if null good
	       then do
                   vpool' <- step conf vpool
	           handle vpool'
	       else do
		   return good
    handle $ Pool
	   { num = 0
           , previous = fitness conf $ head pool
           , popul = map ( \ g -> ( fitness conf g, g ) ) pool
	   }


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
