module Model.SRS where

-- $Id$

-- brute force search
-- for all (exact) models of an SRS

import Letters
import SRS.Type
import SRS.Aged ( Aged (..) )
import qualified SRS.Aged ( combine )

import Data.Array
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
	  => (a, a) -> [ Function a a ]
-- restriction: only for consecutive letters
functions bnd = do
    let xs = range bnd
    ys <- lists (length xs) xs
    return $ listArray bnd ys

identity :: Ix a 
	 => (a, a) -> Function a a
identity bnd = array bnd $ do x <- range bnd ; return (x, x)

lists :: Int -> [a] -> [[a]]
lists 0 xs = return []
lists n xs = do y <- xs ; ys <- lists (pred n) xs ; return $ y : ys

type Model c a = Array c ( Function a a )

labeled :: ( Ix c )
	=> SRS c
	-> Int
	-> [ ( Model c Int , SRS (Aged c) ) ]
labeled srs n = do
    m <- models srs n
    let srs' = labeled_srs m srs
    guard $ not $ contains_a_copy srs srs'
    return ( m, srs' )

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
       -> Int   -- intended size
       -> [ Model c Int ]
models srs n = do

    let funs  = functions (1, n)

    let alpha = setToList $ letters srs
	c = length alpha
	bnd   = (minimum alpha, maximum alpha)

    fs <- lists c funs
    let m = listArray bnd fs

    guard $ not $ trivial m

    guard $ check m srs
    return m

trivial :: ( Ix c , Ix a )
	 => Model c a -> Bool
trivial m =  all constant ( elems m )
	  || all identic  ( elems m )

constant :: Ix a
	 => Function a a -> Bool
constant f = 1 >= ( cardinality $ mkSet $ elems f )

identic :: Ix a 
        => Function a a -> Bool
identic f = indices f == elems f


check :: (Ix a, Ix c) 
      => Model c a -> SRS c -> Bool
check m srs = all ( check_rule m ) srs

check_rule :: ( Ix a, Ix c ) 
	   => Model c a  -> Rule c -> Bool
check_rule m (l, r) = inter m l == inter m r

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
       [ ("a", "bc"), ("ca", "ac"), ("bd", "ab"), ("ba", "ab") ]
