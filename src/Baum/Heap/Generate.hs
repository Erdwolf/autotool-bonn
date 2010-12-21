module Baum.Heap.Generate where

--  $Id$

import Baum.Heap.Class
import Baum.Heap.Op
import Baum.Heap.Inter
import Baum.Heap.Config

import Autolib.Set
import Autolib.Util.Zufall
import Autolib.Util.Sort ( sortBy )
import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( (\\) )
import Data.Maybe ( isJust )

type Instanz baum a = ( baum a, [ Op a ], baum a )
data OperationType = InsertOp | DecreaseToOp | DeleteMinOp
data FixedOrGuessed = Fixed | Guessed

generated_instances = 100

-- | returns most interesting instance of /generated_instances/ instances
-- * crates /generated_instances/ instances
-- * sorts them by preferring intereseting instances,
-- i.e. 'try_solve_ordered' = 'False' and small values of 'try_solve_randomly'
-- * returns the first (most interesting) instance
generate :: ( Random a, Heap baum, OpC a )
	 => Config a
	 -> IO ( Instanz baum a )
generate conf = do
    wis <- sequence $ replicate generated_instances 
                    $ generate_weighted conf
    let (w,i) = head $ sortBy fst  wis
    return i

attempts_per_instance = 100

-- | creates an instance and evaluates it with 'try_solve_ordered' and 'try_solve_randomly'
generate_weighted  :: ( Random a, Heap baum, OpC a )
	 => Config a
	 -> IO ( (Bool,Int), Instanz baum a )
generate_weighted conf = do
    inst <- generate_once conf
    o <- try_solve_ordered inst 
    r <- try_solve_randomly inst attempts_per_instance
    return ( (o,r), inst )

-- | creates an instance with operations depending on 'Config'
generate_once :: ( Random a, Heap baum, OpC a )
	 => Config a
	 -> IO ( Instanz baum a )
generate_once conf = do
    let key = randomRIO (min_key conf, max_key conf)
    keys <- sequence $ replicate ( start_size conf ) key
    let start = foldl insert Baum.Heap.Class.empty keys
    
    let inserts = replicate (fixed_insert_ops conf) (InsertOp, Fixed)
	       ++ replicate (guessed_insert_ops conf) (InsertOp, Guessed)
        decreases = replicate (fixed_decreaseTo_ops conf) (DecreaseToOp, Fixed)
         ++ replicate (guessed_decreaseTo_ops conf) (DecreaseToOp, Guessed)
        deletes = replicate (fixed_deleteMin_ops conf) (DeleteMinOp, Fixed)
	       ++ replicate (guessed_deleteMin_ops conf) (DeleteMinOp, Guessed)
    codes <- permutation $ inserts ++ decreases ++ deletes

    let gen b [] = return ([], b)
        gen b ((op_type, fix_or_guess) : tvs) = do
          a <- key
          (p, x) <- eins $ toList b
          y <- randomDecreaseToKey x key
            
          let op = case fix_or_guess of
                     Guessed -> Any
                     Fixed -> case op_type of
                       InsertOp     -> Insert a
                       DecreaseToOp -> DecreaseTo p y
                       DeleteMinOp  -> DeleteMin
          let c = case op_type of
                    InsertOp     -> insert b a
                    DecreaseToOp -> decreaseTo b p y
                    DeleteMinOp  -> deleteMin b
          (ops, d) <- gen c tvs
          return ( op : ops, d )
      
    ( ops, end ) <- gen start codes
    return ( start, ops, end )


-- | is the instance solvable with a sequence of operations
-- that has the keys in increasing order?
try_solve_ordered ::  ( Random a, Heap baum, OpC a )
          => Instanz baum a 
          -> IO Bool
try_solve_ordered ( start, plan , end ) = do
    let mops = candidate_ops ( start, end )    
    return $ check ( start, plan, end ) mops 
 

-- | try to solve the instance by random permutations
-- of operations with missing keys.
-- Returns the number of successful attempts.
-- For an interesting instance, this should be low as possible.
try_solve_randomly ::  ( Random a, Heap baum, OpC a )
          => Instanz baum a 
          -> Int
          -> IO Int
try_solve_randomly ( start, plan, end ) num = do
    let attempt = do
            mops <- permutation $ candidate_ops ( start, end )
            return $ check ( start, plan, end ) mops
    attempts <- sequence $ replicate num attempt
    return $ length $ filter id attempts

-- | checks whether /start/ can be transformed into /end/ by applying operations /plan/ and /mops/
check :: (Heap baum, OpC a) => Instanz baum a -> [Op a] -> Bool
check ( start, plan, end ) mops  = 
            let end' = foldl ( \ t op -> case op of
                     Insert x -> insert t x
                     DeleteMin -> deleteMin t
                     _        -> t -- should not happen, but does if #Any > #Insert
                  ) start ( merge plan mops ) 
            in  equal end end'

-- | creates a list of required operations to transform /start/ into /end/
-- WARNING: only 'Insert' operations are created
candidate_ops :: (Heap baum, OpC a) => (baum a, baum a) -> [Op a]
candidate_ops ( start, end ) = 
    let must_be_inserted = contents end \\ contents start
        must_be_deleted  = contents start \\ contents end
    in    map Insert must_be_inserted
       -- FIXME: should be uncomment - and decreseTo should be implemented as well
       -- ++ map DeleteMin must_be_deleted

-- | replaces all 'Any' operations from the first list
-- with 'Insert', 'DeleteMin' or 'DecreaseTo' operations from the second list
merge :: [Op a] -> [Op a] -> [Op a]
merge (Any : rest) (m : ops ) = m : merge rest ops
merge (o : ps) mops = o : merge ps mops
merge _ mops = mops

-- | generate a random value which is smaller than /key/
randomDecreaseToKey :: Ord a => a -> IO a -> IO a
randomDecreaseToKey key keyGenerator = do
  x <- keyGenerator
  if (x <= key) then return x else randomDecreaseToKey key keyGenerator
