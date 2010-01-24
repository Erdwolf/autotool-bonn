module Baum.Such.Generate where

--  $Id$

import Baum.Such.Class
import Baum.Such.Op
import Baum.Such.Inter
import Baum.Such.Config

import Autolib.Set
import Autolib.Util.Zufall
import Autolib.Util.Sort ( sortBy )
import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( (\\) )
import Data.Maybe ( isJust )

type Instanz baum a = ( baum a, [ Op a ], baum a )

generated_instances = 100

generate :: ( Random a, Such baum, OpC a )
	 => Config a
	 -> IO ( Instanz baum a )
generate conf = do
    wis <- sequence $ replicate generated_instances 
                    $ generate_weighted conf
    let (w,i) = head $ sortBy fst  wis
    return i

attempts_per_instance = 100

generate_weighted  :: ( Random a, Such baum, OpC a )
	 => Config a
	 -> IO ( (Bool,Int), Instanz baum a )
generate_weighted conf = do
    inst <- generate_once conf
    o <- try_solve_ordered inst 
    r <- try_solve_randomly inst attempts_per_instance
    return ( (o,r), inst )

generate_once :: ( Random a, Such baum, OpC a )
	 => Config a
	 -> IO ( Instanz baum a )
generate_once conf = do
    let key = randomRIO (min_key conf, max_key conf)
    keys <- sequence $ replicate ( start_size conf ) key
    let start = foldl insert Baum.Such.Class.empty keys
    
    let inserts = replicate (fixed_insert_ops conf) (  True,  True )
	       ++ replicate (guess_insert_ops conf) (  True, False )
        deletes = replicate (fixed_delete_ops conf) ( False,  True )
	       ++ replicate (guess_delete_ops conf) ( False, False )
    codes <- permutation $ inserts ++ deletes

    let gen b [] = return ([], b)
        gen b ( (t, v) : tvs) = do
	    a <- if t then key else eins $ contents b
	    let op = if v then ( if t then Insert a else Delete a )
		          else Any
                c = ( if t then insert else delete ) b a
	    (ops, d) <- gen c tvs
            return ( op : ops, d )
    ( ops, end ) <- gen start codes

    return ( start, ops, end )


-- | is the instance solvable with a sequence of operations
-- that has the keys in increasing order?
try_solve_ordered ::  ( Random a, Such baum, OpC a )
          => Instanz baum a 
          -> IO Bool
try_solve_ordered ( start, plan , end ) = do
    let mops = candidate_ops ( start, end )    
    return $ check ( start, plan, end ) mops 
 

-- | try to solve the instance by random permutations
-- of operations with missing keys.
-- Returns the number of successful attempts.
-- For an interesting, instance, this should be low as possible.
try_solve_randomly ::  ( Random a, Such baum, OpC a )
          => Instanz baum a 
          -> Int
          -> IO Int
try_solve_randomly ( start, plan, end ) num = do
    let attempt = do
            mops <- permutation $ candidate_ops ( start, end )
            return $ check ( start, plan, end ) mops
    attempts <- sequence $ replicate num attempt
    return $ length $ filter id attempts

check ( start, plan, end ) mops  = 
            let end' = foldl ( \ t op -> case op of
                     Insert x -> insert t x
                     Delete x -> delete t x
                     _        -> t -- should not happen
                  ) start ( merge plan mops ) 
            in  equal end end'


candidate_ops ( start, end ) = 
    let must_be_inserted = contents end \\ contents start
        must_be_deleted  = contents start \\ contents end
    in    map Insert must_be_inserted
       ++ map Delete must_be_deleted

merge (Any : rest) (m : ops ) = m : merge rest ops
merge (o : ps) mops = o : merge ps mops
merge _ mops = mops

