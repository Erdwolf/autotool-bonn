{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

-- | compute the cyclic tetris graph 
-- corresponding to a (partial) PCP solution
module PCP.Paths where

import Autolib.Util.Splits
import Autolib.NFA.Type
import Autolib.NFA.Dot
import Autolib.NFA.Compact
import Autolib.Informed
import Autolib.Schichten
import Autolib.Util.Sort

import PCP.Type
import PCP.Examples

import Control.Monad.State
import Control.Monad.Identity

--------------------------------------------------------------
-- the leftmost derivation
--------------------------------------------------------------

-- | check dis
check pcp w = runIdentity
      $ evalStateT (spirals w)
      $ blank 'x' pcp 




--------------------------------------------------------------------------

data Config c a = Config 
		{ separator :: c
		, pcp :: PCP c
		, store :: [(a, c, a)]
		, supply :: [a] -- lazy infinite
		, used :: [a]
		, from :: a, to :: a
		, border :: [(a, c, a)]
		, time :: Int
		}

wesen :: Config c a -> (Int, [c])
wesen conf = ( time conf, strip $ border conf )

instance ( Eq c ) => Eq ( Config c a ) where 
    c == d = wesen c == wesen d

instance ( Ord c ) => Ord ( Config c a ) where 
    c `compare` d = wesen c `compare` wesen d


spirals :: Monad m
        => [Int]
        -> StateT (Config c a) m (Config c a)
spirals ks = do
    sequence_ $ map spiral ks
    get

-- | apply one pcp pair
spiral :: Monad m
     => Int
     -> StateT (Config c a) m ()
spiral k = do
    p <- gets pcp
    let (l, r) = p !! k

    bo <- gets border
    -- add to border (on the right), returns top state
    ex <- extend (begin bo, l) 

    bore <- gets border -- new value!
    let (pre, post) = splitAt (length r) bore
    -- assert $ strip pre == r
        
    s <- gets separator
    link (end pre, [s], end ex)

    put_border post

blank :: c -- ^ separator
      -> PCP c -- ^ instance
      -> Config c Int
blank s p = Config { separator = s, pcp = p
	       , store = []
	       , supply = [0 ..]
	       , used = []
	       -- questionable
	       , from = 0 , to = 0
	       , border = []
	       , time = 0
	       }

-- | get next item from supply
next :: Monad m 
     => StateT (Config c a) m a
next = do 
    conf <- get
    let (x : xs) = supply conf
    put $ conf { supply = xs
	       , used = x : used conf 
	       }
    return x



-- | create new path into nothing
extend :: Monad m
       => (a, [c])
       -> StateT (Config c a) m [(a,c,a)]
extend (p, w) = do
     q <- next
     link (p, w, q)
    


-- | create new path between existing states 
-- (and add to store, but not to border)
link :: Monad m
     => (a, [c], a) 
     -> StateT (Config c a) m [(a,c,a)]
link (p, w, q) = do
    this <- sequence $ replicate (pred $ length w) next
    let pqs = p : this ++ [q]
    let path = zip3 pqs w (tail pqs)
    add_store path 

add_store :: Monad m
	  => [(a, c, a)] 
	  -> StateT (Config c a) m [(a,c,a)]
add_store path = do
    conf <- get
    put $ conf { store = path ++ store conf }
    return path


put_border :: Monad m 
	   => [(a, c, a)] 
	   -> StateT (Config c a) m ()
put_border path = do
    conf <- get
    put $ conf { border = path }

start :: Monad m
      => [c] 
      -> StateT (Config c a) m ()
start w =  do
    p <- next
    q <- next
    path <- link (p, w, q)
    put_border path
    conf <- get
    put $ conf { from = p, to = q }

------------------------------------------------------------------------
-- state monad stufff
------------------------------------------------------------------------

-- | output all states (level order)
trace :: Ord s 
      => StateT s [] a 
      -> s 
      -> [[s]]
trace f s = map setToList 
	  $ schichten (mkSet . execStateT f) s

-- | execute compuations in parallel
fork :: [ StateT s [] a ] -> StateT s [] a
fork fs = StateT $ \ s -> do
    f <- fs
    runStateT f s

--------------------------------------------------------------------------

type Rule c = ( [c], [c] )
type SRS  c = [ Rule c ]

type Step c a = ( a, c, a )
type Path c a = [ Step c a ]

strip :: Path c a -> [c]
strip = map ( \ (_,b,_) -> b)

begin :: Path c a -> a
begin path = case head path of (a, _, _) -> a

end :: Path c a -> a
end path = case last path of (_, _, c) -> c

exec :: ( Monad m )
	 => StateT s m Bool 
	 -> StateT s m a
         -> StateT s m a
exec step final = do
    cont <- step 
    if cont 
       then exec step final
       else final
	    
-- | execute one leftmost step
-- return success flag (False = reached normal form)
leftmost :: ( Monad m, Eq c )
	 => Apps c a
         -> StateT (Config c a) m Bool
leftmost apps = StateT $ \ s -> 
    case runStateT (results apps) s of
        []  -> return ( False, s )
	( _, t ) : _ -> return ( True , t )

-- | this function is questionable
everywhere :: ( Eq c )
	 => Apps c a
         -> StateT (Config c a) [] Bool
everywhere apps = StateT $ \ s -> 
    case runStateT (results apps) s of
        []  -> return ( False, s )
	things -> do
	      (_, t) <- things
	      return ( True , t )

-- | execute one step (all possible ways)
results :: ( Eq c )
	 => Apps c a 
         -> StateT (Config c a) [] ()
results apps = do
    bore <- gets border
    fork $ do
        (pre, mid, r, post) <- apps bore
        return $ do
             path <- link (begin mid, r, end mid)
	     put_border $ pre ++ path ++ post

type Apps c a =  Path c a -> [(Path c a, Path c a, [c], Path c a)]

-- | find all possible rule applications (left to right)
applicables :: Eq c
	     => SRS c
	     -> Apps c a
applicables srs w = do
    ( pre, midpost ) <- splits w
    (l, r) <- srs
    let (mid, post) = splitAt (length l) midpost
    guard $ strip mid == l
    return (pre, mid, r, post)

--------------------------------------------------------------------------

automate :: (  NFAC c Int )
    => Config c Int 
    -> NFA c Int
automate conf = 
    let trs = store conf
    in  NFA { nfa_info = funni "automate" [ ] 
		 , states = mkSet $ used conf
		 , alphabet = mkSet $ strip trs
		 , starts = unitSet $ from conf
		 , finals = unitSet $ to conf
		 , trans  = collect trs
		 }

