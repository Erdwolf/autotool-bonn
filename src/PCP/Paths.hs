{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- | compute the cyclic tetris graph 
-- corresponding to a (partial) PCP solution
module PCP.Paths where

import Autolib.Util.Splits
import Autolib.NFA.Type
import Autolib.NFA.Dot
import Autolib.NFA.Compact
import Autolib.Exp.Type
import Autolib.Informed
import Autolib.Schichten
import Autolib.Util.Sort
import Autolib.Util.Size
import Autolib.ToDoc

import PCP.Type
import PCP.Examples

import Control.Monad.State
import Control.Monad.Identity

-- | check dis

test = check simple $ take 4 ssimple

check pcp (k : ks) = runIdentity
      $ evalStateT ( do
            let (l, r) = pcp !! k
            p <- next
            q <- next
	    path <- link (p, drop (length r) l, q)
            put_border path
	    put_cut p
	    spirals ks
      )
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
		, cut :: a
		, time :: Int
		}

wesen :: Config c a -> (Int, [c])
wesen conf = ( time conf, strip $ border conf )

instance ( Eq c ) => Eq ( Config c a ) where 
    c == d = wesen c == wesen d

instance ( Ord c ) => Ord ( Config c a ) where 
    c `compare` d = wesen c `compare` wesen d


spirals :: ( Eq a, Monad m )
        => [Int]
        -> StateT (Config c a) m (Config c a)
spirals ks = do
    sequence_ $ map spiral ks
    get

-- | apply one pcp pair
spiral :: (Eq a, Monad m)
     => Int
     -> StateT (Config c a) m ()
spiral k = do
    p <- gets pcp
    let (l, r) = p !! k

    bore <- gets border

    c <- gets cut
    jack <- if c `elem` states_on ( take (length r) bore )
       then do -- start next level
            jack <- next
            put_cut $ jack
            return jack
       else do -- continue old level
	    return $ end bore

    -- add to border (on the right)
    ext <- extend (jack, l) 

    let (pre, post) = splitAt (length r) (bore ++ ext)
    -- assert $ strip pre == r
    put_border post
        
    s <- gets separator
    link (end pre, [s], end ext)	
    return ()

states_on path = do
    (p, c, q) <- path
    return p

blank :: c -- ^ separator
      -> PCP c -- ^ instance
      -> Config c Int
blank s p = Config { separator = s, pcp = p
	       , store = []
	       , supply = [0 ..]
	       , used = []
	       -- questionable
	       , from = 0 , to = 0, cut = 0
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

put_cut c = do
    conf <- get
    put $ conf { cut = c }

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
begin path = case path of (a, _, _) : _ -> a

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

data Net c = Net { net :: NFA c Int }

instance NFAC c Int => ToDoc (Net c) where
    toDoc = toDoc . net
instance NFAC c Int => Show  (Net c) where
    show = render . toDoc
instance NFAC c Int => ToDot (Net c) where
    toDot = toDot . net
    toDotProgram _ = "neato"

automate :: (  NFAC c Int )
    => Config c Int 
    -> Net (RX c)
automate conf = 
    let trs = store conf
    in  Net $ Autolib.NFA.Compact.make
	    $ NFA { nfa_info = funni "automate" [ ] 
		 , states = mkSet $ used conf
		 , alphabet = mkSet $ strip trs
		 , starts = unitSet $ from conf
		 , finals = unitSet $ to conf
		 , trans  = collect trs
		 }

