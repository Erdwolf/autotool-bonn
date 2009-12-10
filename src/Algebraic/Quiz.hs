module Algebraic.Quiz where

import Algebraic.Central
import Algebraic.Class
import qualified Algebraic.Config as C
import qualified Algebraic.Instance as I

import Inter.Types
import Inter.Quiz

import Condition
import Debug

import qualified Autolib.TES.Binu as B
import Autolib.TES.Type
import Autolib.TES.Position
import Autolib.Choose
import Autolib.Size
import Autolib.Util.Zufall
import Data.Maybe (isJust )

import Data.IORef

roll binu s = do
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

instance ( Condition c a,  Algebraic tag a )
    => Generator tag ( C.Type c a ) ( I.Type a , Exp a ) where
    generator tag conf key = do
        counter <- newIORef 10000
        ( t, Just x, c )  <- do
	      t <- roll   ( C.operators_in_instance conf ) 
		          ( C.max_formula_size_for_instance conf )
	      c <- readIORef counter
	      writeIORef counter $ pred c
	      debug $ unlines 
		    [ show c, show t ]			   
	      return ( t, result $ evaluate tag t, c )
	  `repeat_until` \ ( t, mx, c ) -> case mx of
	      _  | c < 0 -> error $ unlines
		     [ "generator could not produce instance,"
		     , "Tutor: perhaps remove some restrictions."
		     , "Student: report error to Tutor."
		     ] 
	      Nothing -> False
	      Just x  -> isJust 
                  $ result $ investigate ( C.restrictions conf ) x
	return ( I.Make
		         { I.target = x
		         , I.description = case C.information conf of
			     C.Value -> Nothing
			     C.Formula -> Just $ show t
		         , I.operators = C.operators_in_solution conf
		         , I.max_size = max ( size t )
				  ( C.max_formula_size_for_solution conf )
		         }
		       , t
		       )


instance Algebraic tag a =>  Project tag ( I.Type a, Exp a ) ( I.Type a ) where
    project tag (i, f) = i

make :: ( Condition c a, Algebraic tag a )
     => tag -> Make
make tag = quiz tag $ C.Make
     { C.max_formula_size_for_instance = 6
	 , C.operators_in_instance = default_operators tag
	 , C.operators_in_solution = default_operators tag
	 , C.restrictions = suggest
         , C.information = C.Value
	 , C.max_formula_size_for_solution = 10
	 }

