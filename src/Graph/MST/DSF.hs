-- | disjoint set forest implementation of union-find structure

module Graph.MST.DSF 
( Store, Graph.MST.DSF.join, run 
) 
where

import qualified Data.Map as M
import Data.Map ( Map )

import Control.Monad.State

-- | root pointer, with rank
data Store a = 
     Store { parent :: Map a a
           , rank :: Map a Int
           }

-- | join the sets that belong to these elements.
-- returns True iff elements were in different sets before.
join :: Ord a 
     => a -> a 
     -> State (Store a) Bool
join x y = do
    s <- root x
    t <- root y
    if s == t 
       then return False
       else do
           s <- get
           if ( M.lookup x $ rank s ) < ( M.lookup y $ rank s ) 
               then link x y 
               else link y x
           return True

-- | these must be roots. let x point to y.
link :: Ord a
     => a -> a 
     -> State (Store a) ()
link x y = do
    s <- get
    let r = M.findWithDefault 0 x $ rank s
    put $ s { parent = M.insert x y $ parent s 
            , rank = M.insertWith max y (succ r) $ rank s
            }

-- | find the root and rank of the set that the argument belongs to.
-- uses path compression, in a crude way.
root :: Ord a
     => a 
     -> State (Store a) a
root x = do
    m <- get
    case M.lookup x $ parent m of
        Just y -> 
            if x == y then return x
            else do
                z <- root y
                modify $ \ s -> s { parent = M.insert x z $ parent s }
                return z
        Nothing -> do
            modify $ \ s -> s { parent = M.insert x x $ parent s }
            modify $ \ s -> s { rank = M.insert x 0 $ rank s }
            return x

-- | execute a sequence of operations.
-- the start state has each element in a singleton set
run :: State (Store a ) b -> b
run action = evalState action 
           $ Store { parent = M.empty, rank = M.empty }



