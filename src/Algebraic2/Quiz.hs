{-# OPTIONS -fglasgow-exts #-}

module Algebraic2.Quiz where

import Algebraic2.Central
import Algebraic2.Class
import qualified Algebraic2.Config as C
import qualified Algebraic2.Instance as I

import Inter.Types
import Inter.Quiz

import Condition
import Debug

import qualified Autolib.TES.Binu as B
import Autolib.TES.Type
import Autolib.TES.Position
import Autolib.Choose
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Util.Zufall
import Data.Maybe (isJust )

import Data.IORef
import System.Random

roll binu0 s bel = do
    let new_nulls = do
           ( b, v ) <- fmToList bel
	   return $ Op { name = show b, arity = 0, precedence = Nothing
		       , assoc = AssocNone
		       , inter = \ any -> return v
		       }
	binu = binu0 { B.nullary = new_nulls ++ B.nullary binu0 }
    let no_unaries = binu { B.unary = [] }
    num_uns <- randomRIO ( 0, min 2 s )
    t <- choose no_unaries ( s - num_uns )
    case B.unary binu of
        [] -> return t
	us -> do
	    uns <- sequence $ replicate num_uns $ eins us
	    poke_unaries t uns

poke_unaries t [] = return t
poke_unaries t (u : us) = do
    ( p, s ) <- eins $ positions t
    let t' = poke t ( p, Node u [s] )
    poke_unaries t' us

------------------------------------------------------------------------

internal_roller tag context ops beleg size rests rounds = do
        counter <- newIORef rounds
        ( t, Just x, c )  <- do
	      t <- roll  ops size beleg
	      c <- readIORef counter
	      writeIORef counter $ pred c
	      debug $ unlines 
		    [ show c, show t ]			   
	      return ( t, result $ evaluateC tag context beleg t, c )
	  `repeat_until` \ ( t, mx, c ) -> case mx of
	      _  | c < 0 -> error $ unlines
		     [ "generator could not produce instance,"
		     , "Tutor: perhaps remove some restrictions."
		     , "Student: report error to Tutor."
		     ] 
	      Nothing -> False
	      Just x  -> isJust 
                  $ result $ investigate rests x
	return ( t, x )

instance ( Condition c a,  Algebraic tag context a )
    => Generator tag ( C.Type context c a ) ( I.Type context a , Exp a ) where
    generator tag conf key = do
        pre <- fmap listToFM $ sequence $ do
	    ( b, mv ) <- fmToList $ C.predefined conf
	    return $ case mv of
		    Just v -> return ( b, v )
		    Nothing -> do
			 ( t, x ) <- internal_roller tag ( C.context conf )
			     ( C.operators_in_instance conf ) emptyFM
			     ( C.max_formula_size_for_predefined conf )
			     ( C.restrictions_for_predefined conf )
			     10000
			 return ( b, x )
        
        ( t, x ) <- internal_roller tag ( C.context conf )
	       ( C.operators_in_instance conf ) pre
	       ( C.max_formula_size_for_instance conf )
	       ( C.restrictions_for_instance conf ) 
	       10000

	return ( I.Make
		         { I.target = x
			 , I.context = C.context conf
		         , I.description = case C.information conf of
			     C.Value -> Nothing
			     C.Formula -> Just $ show t
		         , I.operators = C.operators_in_solution conf
			 , I.predefined = pre
		         , I.max_size = max ( size t )
				  ( C.max_formula_size_for_solution conf )
		         }
		       , t
		       )


instance Algebraic tag context a 
      =>  Project tag ( I.Type context a, Exp a ) ( I.Type context a ) where
    project tag (i, f) = i

make :: ( Condition c a, Algebraic tag context a )
     => tag -> Make
make tag = quiz tag $ C.Make
     { C.context = default_context tag
     , C.max_formula_size_for_instance = 6
	 , C.operators_in_instance = default_operators tag
	 , C.operators_in_solution = default_operators tag
	 , C.restrictions_for_instance = suggest
	 , C.restrictions_for_predefined = suggest
	 , C.predefined = listToFM [ ( read "foo", Nothing ) ]
	 , C.max_formula_size_for_predefined = 3
         , C.information = C.Value
	 , C.max_formula_size_for_solution = 10
	 }

