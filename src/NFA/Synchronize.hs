{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module NFA.Synchronize where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Det
import Autolib.NFA.Minimize
import Autolib.NFA.Normalize
import Autolib.NFA.Minus
import Autolib.NFA.Shuffle
import Autolib.NFA.Ops
import Autolib.NFA.Link
import Autolib.NFA.Basic
import Autolib.NFA.Shortest

import Autolib.NFA.Dot
import qualified Autolib.NFA.Compact 

import Autolib.ToDoc
import Autolib.Sets
import Autolib.Letters
import Autolib.Symbol
import Autolib.Util.Wort ( alle )
import Autolib.Util.Splits
import Autolib.Exp.Type 

import Control.Monad ( guard )
import Data.Array

----------------------------------------------------------------------------

-- | build automaton from transition tables given as lists
make :: [[Int]] -> NFA Char Int
make xss = a where 
  a = NFA
    { nfa_info = funni "make" [ info xss ]
    , alphabet = mkSet $ take (length xss) [ 'a' .. ]
    , states   = mkSet [ 0 .. pred $ length $ head xss ]
    , starts   = mkSet [ 0 ]
    , finals   = states a 
    , trans    = collect $ do
        ( c, qs ) <- zip [ 'a' .. ] xss
        (p, q) <- zip [0..] qs
        return (p, c, q)
    }        


-- | all permutations 
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = do
    ys <- perms xs
    ( pre, post ) <- splits ys
    return ( pre ++ [x] ++ post )

type Perm = Array Int Int

-- | make array from list permutation
mka :: [Int] -> Perm
mka xs = array (0, pred $ length xs)
       $ zip [ 0 .. ] xs

-- | apply to both index and value (?)
apply :: Perm -> Perm -> Perm
apply p f = array (bounds f) $ do 
	  i <- indices f
	  return ( p ! i, p ! (f ! i) )

-- | all isomorphic automata
isos :: ([Int],[Int]) 
     -> [([Int],[Int])]
isos (xs, ys) = do
    let x = mka xs ; y = mka ys
    p <- map mka $ perms [ 0 .. pred $ length xs ]
    return ( elems $ apply p x
	   , elems $ apply p y 
	   )

-- | is lex. smallest among iso?
lexi :: ([Int],[Int]) -> Bool
lexi xy = xy == minimum (isos xy)
    

-- | list of all (trans tables) of automata with two letters, n states
-- where first one is permutaion
-- second one is no permuation
gen :: Int -> [([Int],[Int])]
gen n = do
    xs <- perms [0 .. n-1] 
    ys <- alle [0 .. n-1] n
    guard $ not $ mkSet ys == mkSet xs
    let xy = (xs, ys)
    -- guard $ lexi xy
    return xy

genau :: Int -> [ NFA Char Int ]
genau n = do
    ys <- alle [0 .. n-1] n
    guard $ n > cardinality (mkSet ys)
    let b = make [ys]
    guard $ null $ shosyn b
    xs <- perms [0 .. n-1] 
    guard $ lexi (xs, ys)
    return $ make [xs, ys]

extreme :: Int -> [ ( String, NFA Char Int ) ]
extreme n = do
    a <- genau n
    w <- shosyn a
    guard $ length w == pred n ^ 2
    return (w, a)


run :: Int -> IO ()
run n = sequence_ $ do 
    it @ (i, (w, a)) <- zip [0 :: Int ..] $ extreme n
    let fname = "auto_" ++ show n ++ "_" ++ show i
    return $ do
        print $ toDoc it
        writeFile ( fname ++ ".text" ) ( show $ toDoc it )
        writeFile ( fname ++ ".dot" ) ( show $ toDot a )


-- emit :: Int -> [ NFA Char Int ]
emit (w, a) = 
    let b = finite $ takeWhile (\ v -> length v == length w) 
	           $ syn a
        m = Autolib.NFA.Compact.make
    in  glue (m b ) ( alphamap Letter a ) 

glue a b = 
    let u = Autolib.NFA.Ops.union a b
    in  normalize
	$ link u ( Left $ head $ lfinals a 
		 , Letter 'x'
		 , Right $ head $ lstarts b 
		 )

finite :: NFAC c Int
       => [[c]]
       -> NFA c Int
finite ws = foldr1 (\ a b -> tighten $ normalize_union a b) 
	  $ map word ws

tighten a = 
    let s = head $ lstarts a ; f = head $ lfinals a
        h x =      if x `elementOf` starts a then s
	      else if x `elementOf` finals a then f
	      else x
    in  statemap h a

-- | return at most one sync word (the shortest one)
shosyn :: NFAC c a
       => NFA c a
       -> [[ c ]]
shosyn = take 1 . syn

-- | all non-looping sync words (in order of increasing length)
syn ::  NFAC c a
       => NFA c a
       -> [[ c ]]
syn =    map contents 
	 . loopfree 
	 . uno 
	 . synchro


----------------------------------------------------------------------------

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
