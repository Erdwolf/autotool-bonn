module NFA.Synchronize where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Det
import Autolib.NFA.Minimize
import Autolib.NFA.Normalize
import Autolib.NFA.Minus
import Autolib.NFA.Shuffle
import Autolib.NFA.Ops
import Autolib.NFA.Basic
import Autolib.NFA.Shortest

import Autolib.NFA.Dot

import Autolib.ToDoc
import Autolib.Sets
import Autolib.Letters
import Autolib.Symbol

import Control.Monad ( guard )


-- | input: complete and deterministic automaton
-- output: automaton that accepts all synchronizing words
synchro :: NFAC c a 
	=> NFA c a
	-> NFA c (Set a)
synchro a = 
    let b = det0 $ a { starts = states a }
    in  b { finals = sfilter ( \ s -> 1 == cardinality s ) 
		   $ states b
	  }

-- | test case
test :: [ String ]
test = map contents $ loopfree $ uno $ synchro $ bad 4


-- | identify all states of cardinality one
-- by replacing them with just one state "emptySet"
uno :: NFAC c (Set a)
    => NFA c (Set a)
    -> NFA c (Set a)
uno = statemap $ \ s -> if cardinality s > 1 then s else emptySet
	    

-- | factor-minimal words
facmin :: NFAC c a 
       => NFA c a
       -> NFA c Int
facmin a =
    let s = normalize a
	sig = sigma $ setToList $ letters s
    in  minus s
	      ( normalize
	      $ Autolib.NFA.Ops.union ( dot sig s ) 
				     ( dot s sig ) )

-- | minimal words w.r.t. embedding
-- (by Kruskal's theorem, this is a finite set)
-- hint: get list of these words with 'Autolib.NFA.Shortest.accepted'
embmin :: NFAC c a
       => NFA c a
       -> NFA c Int
embmin a = 
    let s = minimize $ normalize a
	sig = sigma $ setToList $ letters s
        sigplus = dot sig ( star sig )
    in  minus s ( minimize $ normalize $ shuffle s sigplus )

-------------------------------------------------------------------
-- TODO: Path related code should go in separate module
-------------------------------------------------------------------

type Step c a = (a, c, a)
type Path c a = [ Step c a ]

begin :: Path c a -> a
begin path = case path of
    [] -> error "NFA.Synchronize.begin: empty path"
    (p, c, q) : rest -> p

end :: Path c a -> a
end path = case reverse path of
    [] -> error "NFA.Synchronize.end: empty path"
    (p, c, q) : rest -> q

contents :: Path c a -> [c]
contents = map $ \ (p, c, q) -> c 

-- | loop-free paths in automaton
loopfree_from :: NFAC c a
	 => NFA c a 
	 -> (a, Set a) -- ^ start from here, avoid these states
	 -> [ Path c a ]
loopfree_from a (p, seen) = [] : do
    c <- setToList $ alphabet a
    q <- setToList $ lookupset (trans a) (p, c)
    guard $ not $ q  `elementOf` seen
    rest <- loopfree_from a (q, Autolib.Sets.union seen (unitSet q))
    return $ (p,c,q) : rest

-- | loop-freee accepting paths
loopfree :: NFAC c a
	 => NFA c a 
	 -> [ Path c a ]
loopfree a = do
    p <- lstarts a
    path <- loopfree_from a (p, unitSet p)
    guard $ not $ null path
    guard $ end path `elementOf` finals a
    return path

-----------------------------------------------------------------------

-- | "bad" examples
bad :: Int -> NFA Char Int
bad n = NFA
      { nfa_info = funni "bad" [ toDoc n ]  
      , states = mkSet [ 0 .. pred n ]
      , alphabet = mkSet "ab"
      , starts = emptySet
      , finals = emptySet
      , trans  = collect $
           do i <- [ 0 .. pred n ]
              return (i, 'a' , if 1 == i then 0 else i)
        ++ do i <- [ 0 .. pred n ]
	      return (i, 'b', pred i `mod` n)
      }
