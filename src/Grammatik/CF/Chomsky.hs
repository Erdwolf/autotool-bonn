module Grammatik.CF.Chomsky 

( Chomsky (..)
, Rules 
, make
, vars
)

where

-- -- $Id$

import qualified Grammatik.Type as G
import Grammatik.CF.Chomsky.Type

import Grammatik.CF.Epsfrei
import Grammatik.CF.Kettenfrei
import Grammatik.Reduziert
import Grammatik.CF.Nullable

import Autolib.Set

import Autolib.FiniteMap
import Control.Monad (guard)
import Data.List (nub)
import Autolib.ToDoc
import Autolib.Size
import Autolib.Fix



---------------------------------------------------------------------------

make :: G.Grammatik -> Chomsky Int
make g =     
    let h = kettenfrei 
	   $ epsfrei 
	   $ reduktion
	   $ g 
    in	chomsky ( creates_epsilon g ) h

chomsky :: Bool -- ^ erzeugt epsilon?
	-> G.Grammatik 
	-> Chomsky Int
-- eingabe: CF-grammatik ohne Eps und ohne Ketten
-- ausgabe: Chomsky-Normalform
chomsky eps = normalize . construct2 eps


construct2 :: Bool -- ^ erzeugt Epsilon?
	   -> G.Grammatik 
	   -> Chomsky String
construct2 epsflag g = 
    let rs = do 
	   ( lhs, rhs ) <- G.rules g
	   rewrite lhs rhs
	rewrite top [] = error "epsilon-regel in chomsky-arg"
	rewrite top [ x ] = 
		if x `elementOf` G.terminale g 
		then return ( top, Left x )
		else error "ketten-regel in chomsky-arg"
	rewrite top [x, y] = return ( top, Right ( [x], [y] ) )
	rewrite top (x : xs) =
		( top, Right ([x], xs) ) : rewrite xs xs
	alphas = do
	       ( top, Right ( x, y )  ) <- rs
	       [ z ] <- [ x, y ]
	       guard $ z `elementOf` G.terminale g
	       return ( [z], Left z )
    in	Chomsky { start = [ G.startsymbol g ]
		, rules = nub (alphas ++ rs)	
		, eps = epsflag
		}


---------------------------------------------------------------------------

normalize :: Ord a => Chomsky a -> Chomsky Int
normalize ch = 
    let vs = unitSet (start ch) `union`  vars ( rules ch )
	fm = listToFM $ zip ( setToList vs ) [1..] 
	look = lookupWithDefaultFM fm (error "normalize")
    in	fmap look ch

vars :: Ord a => Rules a -> Set a
vars rules = mkSet $ do 
	     ( lhs, rhs ) <- rules
	     lhs : case rhs of Right (x,y) -> [x,y] ; Left c -> []

-------------------------------------------------------------------------

compact :: Ord a => Chomsky a -> Chomsky Int
compact = fix comp . normalize

comp :: Chomsky Int -> Chomsky Int
comp ch = 
    let -- zu fester lhs  alle  rhs  aufsammeln
	gr = addListToFM_C union emptyFM $ do
	   ( lhs, rhs ) <- rules ch
	   return ( lhs, unitSet rhs )
	-- alle gleichen rhs-mengen feststellen
	fm = addListToFM_C union emptyFM $ do
	   ( lhs, rhss ) <- fmToList gr
	   return ( rhss, unitSet lhs )
	-- durchzählen
	tr = listToFM $ do
	   ( k, ( rhss, lhss ) ) <- zip [1 ..] $ fmToList fm
	   lhs <- setToList lhss
	   return ( lhs, k )
	-- umnumerieren
	dh = fmap ( lookupWithDefaultFM tr ( error "dh" ) ) ch
	-- kürzen
    in	dh { rules = nub $ rules dh }






