{-# OPTIONS -fglasgow-exts #-}

-- | generate randomly some type expressions

module FP.Roll where

import FP.Conf

import FP.Arrow
import FP.Expression
import FP.Type
import FP.Env
import FP.Reconstruct
import FP.Instance
import FP.Check

import Autolib.TES.Term
import Autolib.TES.Identifier
import Autolib.TES.Position

import Autolib.Util.Zufall

import Autolib.FiniteMap
import Autolib.Reporter hiding ( wrap )
import Autolib.Size
import Autolib.Set

import Autolib.Util.Sort

import Data.List ( nub )
import Data.Maybe
import Data.Ix
import Control.Monad ( mzero )

nice_problem conf = do
    env <- sig conf
    let tts = typed_terms conf 200 env
    let ( small, large ) = splitAt (length tts `div` 2) tts
    let diff = setToList $ mkSet large `minusSet` mkSet small
    if null diff 
       then nice_problem conf
       else do
          let nontrivial = take 1 $ do
	          ( x,t ) <- diff
		  guard $ null $ do
		      (y, s) <- small 
		      guard $ isJust $ result $ isinstanceof (core s) (core t)
		      return ()
		  return $ (x, TI { target = t , signature = env } )
          if null nontrivial 
	      then nice_problem conf    
	      else return $ head $ sortBy ( size . target . snd ) nontrivial



typed_terms :: Conf -> Int -> Env -> [ ( Expression Identifier, Type ) ]
typed_terms conf num ( Env env ) = 
    let start = do ( v, t ) <- fmToList env ; return ( Atomic v, t )
    in  
	     filter ( \ (x,t) -> inRange (target_size_bounds conf) (size t))
	   $ filter ( \ (x,t) -> inRange (solution_size_bounds conf) (size x))
	   $ filter ( \ (x,t) -> 1 < cardinality ( symbols x ) )
	   $ take num 
	   $ takeWhile ( (<100) . size ) 
	   $ complete start start 

complete done xts = 
     let next = Data.List.nub $ do
            (x,t) <- done
            (y,s) <- done ++ xts
	    (z ,mu) <- [ (Apply x y, combine' (x,t) (y,s) )
		       , (Apply y x, combine' (y,s) (x,t) )
		       ]
	    case Autolib.Reporter.result mu of
	        Just u  -> return ( z, wrap u )
	        Nothing -> mzero
     in  xts ++ if null next then [] else complete (done ++ xts) next

combine' (x,t) (y,s) = combine (x, core t) ( y,  core s )

sig :: Conf -> IO Env
sig conf = do
    s <- randomRIO $ signature_size_bounds conf
    let ids = do c <- [ 'f' .. ] ; return $ mkunary [ c ] 
    types <- sequence $ replicate s $ typ conf
    return $ Env $ listToFM $ zip ids types

typ :: Conf -> IO Type
typ conf = do
    s <- randomRIO $ type_size_bounds conf
    t <- typ_term conf s
    return $ wrap $ Arrow t

typ_term conf s = 
    if  s <= 1 
    then do
        v <- eins $ typevars conf
	return $ Var v
    else do
        conar <- eins $ typecons conf  
	( con, ar ) <- eins [ conar, ( read "Arrow", 2 ) ]
	if 0 == ar 
	   then return $ Node con []
	   else do
	       xs <- distribution ar ( s - 1 )
	       ts <- mapM ( typ_term conf ) xs
	       return $ Node con ts

-- | besser verteilt, aber zeitraubend (linear in total)
distribution :: Int -> Int -> IO [ Int ]
distribution width total = do
    xs <- sequence $ replicate total $ randomRIO ( 0, width - 1 )
    let fm = addListToFM_C (+) emptyFM $ do zip xs $ repeat 1
    return $ do 
        k <- [ 0 .. width - 1 ] 
	return $ lookupWithDefaultFM fm 0 k
