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
import Autolib.NFA.Trim

import Autolib.NFA.Dot
import qualified Autolib.NFA.Compact 

import Autolib.ToDoc
import Autolib.Sets
import Autolib.Letters
import Autolib.Symbol
import Autolib.Util.Wort ( alle )
import Autolib.Util.Splits
import Autolib.Util.Sort
import Autolib.Util.Size
import Autolib.Exp.Type 

import Autolib.Reporter
import Autolib.Output
import Autolib.Dot.Dotty

import Control.Monad ( guard )
import Data.Array
import Data.List ( intersperse )
import qualified Text.Html

import NFA.Funk
import Random

----------------------------------------------------------------------------

funki :: Funk -- ^ control expression
     -> Int  -- ^ modulus
     -> Int  -- ^ argument
     -> (Int, Int) -- ^ target of a and b
funki f m k = 
    let paired fun (x, y) = (fun x, fun y)
	shift k e = case e of
	      Absolute a -> a
	      Relative r -> k + r
	big = length $ fixed f
    in    paired ( `mod` m )
        $ if k < big
 	  then fixed f !! k
 	  else paired ( shift k ) ( linear f )

unzip2 :: [(a,b)] -> ([a], [b])
unzip2 xys = ( map fst xys, map snd xys )

funk :: Funk -- ^ control expression
     -> Int  -- ^ modulus
     -> ([Int],[Int])
funk f m = unzip2
	 $ do k <- [0 .. pred m] ; return $ funki f m k

haus :: Funk -- ^ control expression
     -> (Int, Int) -- ^ bounds
     -> [[Int]] -- ^ list of lengths of min sync words
haus f bnd = 
    let eval m = map length
	       $ shosyn
	       $ auto
	       $ funk f m
    in  mapM eval $ range bnd

auto = make
     . ( \ (xs, ys) -> [xs, ys] )

ein :: Int -> IO Funk
ein g = do
    let zahl = randomRIO (-g, g)
        beide act = do 
		  x <- act
	          y <- act
	          return (x, y)
        entry = do f <- randomRIO (False, True)
                   d <- zahl
		   return $ ( if f then Absolute else Relative ) d
    fs <- sequence $ replicate g $ beide zahl
    li <- beide entry
    return $ Sizecase { fixed = fs , linear = li }

looks :: Int -> Int -> IO ()
looks g top = do
    e <- ein g
    case haus e (g, 10) of
         hs : _ -> do
               let s = last hs
               when ( s > top ) $ do
	           print $ toDoc ( hs, e )
                   looks g s
	       looks g top
         _ ->  looks g top

f1 = Sizecase { fixed = [ ( -2, -3 ), ( -2, 1 ), ( -2, -1 ) ]
           , linear = ( Relative (-1), Relative (-1) )
           }


f6 = Sizecase { fixed = [ ( 2,1 ), ( 3,2 ), ( 4,1 ) ]
           , linear = ( Relative 2, Relative 0 )
           }


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
lexi xy = 
   -- the following is (quadratically ?) inefficient
   -- xy == minimum (isos xy)
   and $ do z <- isos xy ; return $ xy <= z


-- | letter a is doubly cyclic perm
-- letter b is *no* perm
-- double_circle :: Int -> Int -> [ NFA Char Int ]
double_circle f g = do
    let rotate (x : xs) = xs ++ [x]
        n = f+g
        xs = rotate [0 .. f-1] ++ rotate [f .. n-1]


    -- no permutation: cannot be a mapping onto
    top <- [1 .. n-1]
    ys <- alle [0 .. top] n
    guard $ maximum ys == top
    guard $ n > cardinality (mkSet ys)

    let zs = do 
	     (i,y) <- zip [0..] ys
	     return $ (i+y) `mod` n

    return [xs, zs]

dorun f g = do
   print $ "double circle" ++ show (f, g)
   runit $ do
       trip @ (l, m, w) <- ups $ do
	   m <-  double_circle f g 
           let ws = shosyn $ make m 
           guard ( not ( null ws)) 
           let w = head ws 
           return ( length w, m, w)
       return $ make m



ups [] = []
ups (x : xs) = x : ups (filter (>= x) xs)

-- | list of all automata with two letters, n states
-- where first one is permutaion
-- second one is no permuation
genau :: Int -> [ NFA Char Int ]
genau n = do

    -- no permutation: cannot be a mapping onto
    ys <- alle [0 .. n-1] n
    guard $ n > cardinality (mkSet ys)

    -- should not be synchronizing for letter b alone
    let b = make [ys]
    guard $ null $ shosyn b

    -- should be permutation (mapping onto)
    xs <- perms [0 .. n-1] 
    -- only if smallest from iso class
    guard $ lexi (xs, ys)

    -- xs <- alle [0 .. n-1] n
    -- guard $ n > cardinality (mkSet xs)

    -- should not be synchronizing for letter a alone
    -- let a = make [xs]
    -- guard $ null $ shosyn a

    -- only if a lex b (because of symmetry)
    -- guard $ xs <= ys	  

    return $ make [xs, ys]

extreme :: Int -> [ NFA Char Int ]
extreme n = do
    a <- genau n
    w <- shosyn a
    guard $ length w == ( n - 1 ) ^ 2 
    return a

run n = runit $ extreme n

runit :: [ NFA Char Int ] -> IO ()
runit auts = sequence_ $ do 
    it @ (i, a) <- zip [0 :: Int ..] $ auts
    return $ do
        ( _ , out :: Text.Html.Html ) <- Autolib.Reporter.run $ handle it
        let fname = concat 
		  $ intersperse "-"
		  $ [ "auto", show $ size a, show i ]
	writeFile ( fname ++ ".html" ) $ show out

handle it @ (i, a) = do
    let ws = take 5 $ syn a
    inform $ vcat
	   [ text "synch:" <+> toDoc ws
	   , text "lengths:" <+> toDoc (map length ws)
	   -- , text "aut:" <+> toDoc a
	   ]
    dotty "dot" a
    sequence_ $ do
        x <- setToList $ alphabet a
        return $ do
            inform $ text "Automaton for letter" <+> toDoc x
	    dotty "dot" $ alphafilter ( == x ) a

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

-- | 
issyn :: NFAC c a
      => NFA c a 
      -> Bool
issyn a = not $ null $ syn a


-- | return at most one sync word (the shortest one)
shosyn :: NFAC c a
       => NFA c a
       -> [[ c ]]
shosyn = take 1 . syn

-- | all non-looping sync words (in order of increasing length)
syn ::  NFAC c a
       => NFA c a
       -> [[ c ]]
syn = some_shortest . trim . normalize . synchro


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
      , alphabet = mkSet "ab"
      , states = mkSet [ 0 .. pred n ]
      , starts = emptySet
      , finals = emptySet
      , trans  = collect $
           do i <- [ 0 .. pred n ]
              return (i, 'a' , if 1 == i then 0 else i)
        ++ do i <- [ 0 .. pred n ]
	      return (i, 'b', pred i `mod` n)
      }
