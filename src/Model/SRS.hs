module Model.SRS where

-- $Id$

-- semantic labeling into a finite domain

-- ideas communicated by Hans Zantema
-- implementation by Johannes Waldmann

-- brute force search
-- for all (monotonic quasi-) models of an SRS

import Letters
import SRS.Type
import SRS.Aged ( Aged (..) )
import qualified SRS.Aged ( combine )

import qualified Relation
import Data.Array
import Size
import Sets
import Monad ( guard )

type Function a b = Array a b

combine :: ( Ix a, Ix b )
	=> Function a b 
	-> Function b c 
	-> Function a c
combine f g = array (bounds f) $ do
    (x, y) <- assocs f
    return (x, g ! y)

functions :: Ix a 
	  =>  Relation.Type a a
	  -> [ Function a a ]
-- restriction: only for consecutive letters
functions rel = do
    let xs = setToList $ Relation.source rel
        bnd = (minimum xs, maximum xs) -- used only for indexing in arrays
        
    ys <- lists (length xs) xs
    let f = listArray bnd ys
    guard $ is_monotonic rel f
    return f

is_monotonic :: Ix a
	     => Relation.Type a a -> Function a a 
             -> Bool
is_monotonic rel f = and $ do
    (x, y) <- Relation.pairs rel
    return $ Relation.holds rel (f ! x) (f ! y)

identity :: Ix a 
	 => (a, a) -> Function a a
identity bnd = array bnd $ do x <- range bnd ; return (x, x)

lists :: Int -> [a] -> [[a]]
lists 0 xs = return []
lists n xs = do y <- xs ; ys <- lists (pred n) xs ; return $ y : ys

type Model c a = Array c ( Function a a )

labeled :: ( Ix c )
	=> SRS c
        -> Relation.Type Int Int
	-> [ ( Model c Int , SRS (Aged c) ) ]
labeled (srs :: SRS c) rel = do
    m <- models srs rel
    let srs' = labeled_srs m srs :: SRS (Aged c)
    -- TODO: add decreasing rules
    guard $ not $ contains_a_copy srs srs'

    let e = essential srs'
    -- guard $ letters e == ( letters srs' :: Set (Aged c))

    -- mal sehen
    -- guard $ length e < length srs'

    return ( m, e )

ex =  [("ab","a2b"),("ba","b1a")]



essential :: Ord c => SRS c -> SRS c
-- remove rules that have a weight decreasing letter
essential srs = do
    let cs = non_rising_letters srs
    lr <- srs
    guard $ not $ falling cs lr
    return lr

falling nr lr = or $ do
    c <- nr
    return $ 0 > diff c lr

non_rising_letters srs = do
    c <- setToList $ letters srs
    guard $ not $ rising srs c
    return c

rising :: Eq c => SRS c -> c -> Bool
rising srs c = or $ do
    lr <- srs
    return $ 0 < diff c lr 

diff :: Eq c => c -> Rule c -> Int
-- if < 0, then letter vanishes (weight decreases)
diff c (l, r) = count c r - count c l

count :: Eq c => c -> [c] -> Int
count c w = length $ filter ( == c ) w


form ( srs' :: SRS (Aged Char) ) = unwords
	  $ do (l, r) <- srs'
	       w <- [l, r] 
	       return $ do x <- w ; filter (/= ':') $ show x

contains_a_copy :: ( Ord c ) 
	=> SRS c -> SRS (Aged c) -> Bool
contains_a_copy ( srs :: SRS c ) ( srs' :: SRS (Aged c ) ) = or $ do
    let alpha = letters srs' :: Set (Aged c)
    a <- setToList $ smap age alpha
    return $ and $ do
        (l, r) <- srs
	let beef a w = SRS.Aged.combine ( repeat a ) w
        return $ (beef a l, beef a r) `elem` srs'
    
models :: ( Ix c )
       => SRS c 
       -> Relation.Type Int Int  -- intended domain
       -> [ Model c Int ]
models srs rel = do
    let funs  = functions rel

    let alpha = setToList $ letters srs
	c = length alpha
	bnd   = (minimum alpha, maximum alpha)

    fs <- lists c funs
    let m = listArray bnd fs

    guard $ not $ trivial m

    guard $ check rel m srs
    return m

trivial :: ( Ix c , Ix a )
	 => Model c a -> Bool
trivial m =  constant ( elems m )
	  || all identic  ( elems m )

constant :: Ix a
	 => [ Function a a ] -> Bool
-- alle den gleichen wert?
constant fs = 1 >= ( cardinality $ mkSet $ concat $ map elems fs )

identic :: Ix a 
        => Function a a -> Bool
identic f = indices f == elems f


check :: (Ix a, Ix c) 
      => Relation.Type a a -> Model c a -> SRS c -> Bool
check rel m srs = all ( check_rule rel m ) srs

check_rule :: ( Ix a, Ix c ) 
	   => Relation.Type a a -> Model c a  -> Rule c -> Bool
check_rule rel m (l, r) = and $ do
    let ir = inter m r
	il = inter m l
    x <-  indices ir
    return $ Relation.holds rel (ir ! x) (il ! x)

inter :: ( Ix a, Ix c ) 
      => Model c a -> [c] -> Function a a
-- compute interpretation
inter m xs = foldr ( \ x v -> combine (m ! x) v ) 
	           ( identity $ bounds $ head $ elems m )
		   xs



labeled_srs :: ( Ix c )
        => Model c Int -> SRS c 
	-> SRS (Aged c)
labeled_srs m srs = do
    (l, r) <- srs
    a <- indices $ head $ elems m
    let la = labeled_string m a l
	ra = labeled_string m a r
    return (la, ra)


labeled_string :: Ix c 
	       => Model c Int -> Int -> [c] -> [Aged c]
labeled_string m a [] = []
labeled_string m a (x : xs) = 
    Aged { age = a, it = x } : labeled_string m ((m ! x) ! a) xs
 

hans :: SRS Char
hans = -- [ ("f", "nc"), ("cf", "fc"), ("ns", "fs"), ("nf", "fn") ]
       [ ("a", "bc"), ("ca", "ac"), ("bd", "ad"), ("ba", "ab") ]
