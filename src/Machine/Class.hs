{-# OPTIONS -fglasgow-exts  -fallow-undecidable-instances #-}

module Machine.Class where

import Autolib.Set
import Autolib.Schichten
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Size

import Machine.History

import Data.Typeable

import qualified Data.Set as S -- for priority queue

class ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      , Typeable m, Typeable dat, Typeable conf
      ) => Machine m dat conf | m -> dat,  m -> conf

instance ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      , Typeable m, Typeable dat, Typeable conf
      ) => Machine m dat conf 

--------------------------------------------------------------------

class In m dat conf | m -> dat, m -> conf where -- strong dependencies ??
    -- | startkonf. herstellen (tupel von args)
    input_reporter  :: ( Typeable m, Typeable dat, Typeable conf ) 
                    =>  m -> dat -> Reporter conf

class Ord conf => Compute m conf where
    -- | alle direkten nachfolger ( nichtdeterministisch )
    next   :: m -> conf -> Set conf
    accepting  :: m -> conf -> Bool
    depth :: m -> conf -> Int

    -- | the search could use a priority queue ordered by weights
    -- (configurations with smaller weights are preferred) 
    -- this function is only called once per item
    -- (for comparison in the queue, the value is cached)
    -- default implementation: weight == depth
    -- this leads to a breadth first search
    weight :: m -> conf -> Double 
    weight m conf = fromIntegral $ depth m conf


-- | unendliche liste
nachfolger :: Compute m conf
           => m -> conf -> [ conf ]
nachfolger a k = concat $ map setToList $
    schichten (next  a) k

-- | possibly infinite list of reachable configurations,
-- search prefers smaller weights
weighted_nachfolger :: Compute m conf
                    => m -> conf -> [ conf ]
weighted_nachfolger a k = do
    let lift k = ( weight a k, k )
    let handle done todo = case S.minView todo of
	    Nothing -> []
	    Just ( (w, top) , rest ) ->
                let done' = S.insert top done 
                    succs = map lift
			  $ filter ( \ x -> not $ S.member x done' )
			  $ setToList 
			  $ next a top 
	        in  top : handle done' ( foldr S.insert rest succs )
    handle S.empty ( S.singleton $ lift k )
    

-- | unendliche liste
nachfolger_cut :: Compute m conf
           => Int -> m -> conf -> [ conf ]
nachfolger_cut cut a k = 
    -- concat $ map setToList $ take cut $ schichten (next  a) k
    weighted_nachfolger a k


class Out m dat conf  | m -> dat, m -> conf where
    -- | endkonf. lesen (ein einziges arg)
    output_reporter :: m -> conf -> Reporter dat

class ( In m dat conf, Out m dat conf )
      => InOut m dat conf   | m -> dat, m -> conf

instance ( In m dat conf, Out m dat conf )
      => InOut m dat conf

--------------------------------------------------------------------

class Encode dat where
    encode :: [ Integer ] -> dat

class Decode dat where
    decode :: dat -> Integer

class ( Encode dat, Decode dat ) => Numerical dat 

instance ( Encode dat, Decode dat ) => Numerical dat 


