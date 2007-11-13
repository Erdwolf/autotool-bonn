module Genetic.Config where

--  $Id$

-- | TODO should not depend on IO that much
data Config a v = 
     Config { fitness :: a -> v
          , threshold :: v
	  , present :: [(v, a)] -> IO ()
	  , trace   :: [(v, a)] -> IO ()
	  , size    :: Int -- ^ will be fixed
	  , generate :: IO a
	  , combine :: a -> a -> IO a
          , num_combine :: Int
	  , mutate  :: a -> IO a
          , num_mutate :: Int
          , num_compact :: Int
          , num_steps :: Maybe Int -- ^  Nothing -> unbounded
          , num_parallel :: Int
          }

