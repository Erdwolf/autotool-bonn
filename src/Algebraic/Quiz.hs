{-# OPTIONS -fglasgow-exts #-}

module Algebraic.Quiz where

import Algebraic.Central
import Algebraic.Class
import qualified Algebraic.Config as C
import qualified Algebraic.Instance as I

import Inter.Types
import Inter.Quiz

import Condition

import qualified Autolib.TES.Binu as B
import Autolib.TES.Type
import Autolib.Choose
import Autolib.Size
import Autolib.Util.Zufall
import Data.Maybe (isJust )


instance ( Condition c a,  Algebraic tag a )
    => Generator tag ( C.Type c a ) ( I.Type a , Exp a ) where
    generator tag conf key = do
        ( t, Just x )  <- do
	      -- FIXME this may loop indefinitely
	      t <- choose ( C.operators_in_instance conf ) 
		          ( C.max_formula_size_for_instance conf )
	      return ( t, result $ evaluate tag t )
	  `repeat_until` \ ( t, mx ) -> case mx of
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

