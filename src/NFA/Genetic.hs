{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module NFA.Genetic where

-- | TODO: should not depend on IO that much

import Autolib.Util.Sort
import Autolib.Util.Uniq
import Autolib.Util.Zufall
import Autolib.ToDoc
import Control.Monad

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
    combis <- sequence $ replicate (num_combine conf) $ do
		   [x, y] <- einige 2 pool
		   z <- combine conf x y
		   return (fitness conf z, z)
    mutants <- sequence $ replicate (num_mutate conf) $ do
                   x <- eins pool
                   z <- mutate conf x
		   return (fitness conf z, z)
    let vpool' = reverse
	      $ sort
	      $ uniq -- ??
	      $ combis ++ mutants ++ popul vpool
    return $ Pool
	   { num = succ $ num vpool
	   , popul = take (size conf) vpool'
           , previous = maximum $ map fst $ popul vpool
	   }

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
