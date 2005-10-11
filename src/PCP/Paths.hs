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
import Autolib.Reader
import Autolib.Hash


import PCP.Type
import PCP.Examples

import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe


-- | check dis

test = check simple $ take 4 ssimple

app p xs = 
    let top  = do x <- xs ; fst $ p !! x
	down = do x <- xs ; snd $ p !! x
    in  ( top, down )

emit fname pcp sol 
   = writeFile ( fname ++ ".dot" ) 
   $ show $ toDot $ automate $ check pcp sol

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
		, supply :: [a] -- ^ lazy infinite
		, used :: [a]
		, from :: a, to :: a
		, border :: [(a, c, a)]
		, glue :: [(a, a)] -- ^ states should be identified
		, cut :: a
		, time :: Int
		}
instance ToDoc [c] => ToDoc ( Config c a ) where
    toDoc = toDoc . wesen

wesen :: Config c a -> (Int, [c])
wesen conf = ( time conf, strip $ border conf )

instance ( Eq c ) => Eq ( Config c a ) where 
    c == d = wesen c == wesen d

instance ( Ord c ) => Ord ( Config c a ) where 
    c `compare` d = wesen c `compare` wesen d


spirals :: ( Eq a, Monad m, Eq c )
        => [Int]
        -> StateT (Config c a) m (Config c a)
spirals ks = do
    sequence_ $ map spiral ks
    get

-- | apply one pcp pair
spiral :: (Eq a, Monad m, Eq c)
     => Int
     -> StateT (Config c a) m ()
spiral k = do
    p <- gets pcp
    let (l, r) = p !! k

    s <- gets separator
    bore <- gets border

    c <- gets cut
    jack <- if c `elem` states_on ( take (length r) bore )
       then do -- start next level
            jack <- next
            put_cut $ jack
            add_glue (jack, end bore)

            when ( c == begin bore ) $ do
	        link (c, [s], jack)
                return ()

            return jack
       else do -- continue old level
	    return $ end bore

    -- add to border (on the right)
    ext <- extend (jack, l) 

    let (pre, post) = splitAt (length r) (bore ++ ext)
    if strip pre == r
       then return ()
       else error "PCP.Paths.spiral: does not match"
    put_border post
        
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
	       , glue = []
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

add_glue g = do
    conf <- get
    put $ conf { glue = g : glue conf }

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

data Net c a = Net { net :: NFA c (Label a) }

instance NFAC c a => ToDoc (Net c a) where
    toDoc = toDoc . net
instance NFAC c a => Show  (Net c a) where
    show = render . toDoc
instance NFAC c a => ToDot (Net c a) where
    toDot = toDot . net
    toDotProgram _ = Neato
    toDotOptions _ = unwords 
         $ [ "-Nshape=point",  "-Nfixedsize=true"
	   -- , "-Gsize=9,14", "-Gratio=fill"
           , "-Gstart=10"
	   ]

data Label a = Label { it :: a, tag :: Doc }

instance Eq a => Eq (Label a) where
    x == y = it x == it y
instance Ord a => Ord (Label a) where 
    compare x y = compare (it x) (it y)
instance Hash a => Hash (Label a) where
    hash = hash . it

instance ToDoc (Label a) where toDoc = tag
instance Reader (Label a) -- dummy


{-
automate :: (  NFAC c Int )
    => Config c Int 
    -> Net (RX c) (Label Int)
-}
automate conf = 
    let trs = store conf
        fm = addListToFM_C (error "PCP.Paths.automate") emptyFM $ do
                 (k, (x, y)) <- zip [0 :: Int ..] $ glue conf
                 [ (x, toDoc k), (y, toDoc k) ]
        label q = Label
		{ it = q
		, tag = lookupWithDefaultFM fm empty q
		}
    in  Net $ Autolib.NFA.Compact.make
            $ statemap label
	    $ NFA { nfa_info = funni "automate" [ ] 
		 , states = mkSet $ used conf
		 , alphabet = mkSet $ strip trs
		 , starts = unitSet $ from conf
                 -- so that all glue points remain visible when compacting
		 , finals = mkSet $ keysFM fm
		 , trans  = collect trs
		 }

