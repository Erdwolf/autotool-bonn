module Grammatik.CF.Chomsky where

-- $Id$

import qualified Grammatik.Type as G

import Sets

import FiniteMap
import Monad (guard)
import List (nub)
import ToDoc
import Size
import Fix


type Rules a   = [ ( a, Either Char (a, a) ) ]
data Chomsky a = Chomsky 
	       { start :: a
	       , rules :: Rules a
	       }
     deriving (Eq, Ord)

instance Size (Chomsky a) where size = length . rules

instance ToDoc a => ToDoc (Chomsky a) where
    toDoc ch = 
	 let pstart = text "start" <+> equals <+> toDoc (start ch)
	     pregeln  = text "rules" <+> equals <+> toDoc (rules ch)

	 in      text "Chomsky" 
	     <+> braces ( fsep $ punctuate comma [ pstart, pregeln] )

instance ToDoc a => Show (Chomsky a) where show = render . toDoc


---------------------------------------------------------------------------
    

chomsky :: G.Grammatik -> Chomsky Int
-- eingabe: CF-grammatik ohne Eps und ohne Ketten
-- ausgabe: Chomsky-Normalform
chomsky = normalize . construct2


construct2 :: G.Grammatik -> Chomsky String
construct2 g = 
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
		}


---------------------------------------------------------------------------

instance Functor Chomsky where
    fmap f ch = 
	 Chomsky { start = f $ start ch
		 , rules = do
		      ( lhs, rhs ) <- rules ch 
		      return ( f lhs 
			     , case rhs of 
				    Left c -> Left c
				    Right (x,y) -> Right (f x, f y) 
		             )
		 }

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






