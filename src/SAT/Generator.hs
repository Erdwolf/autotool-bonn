module SAT.Generator where

-- -- $Id$

import SAT.Types
import SAT.Param
import SAT.Wert
import SAT.State

import Random
import Autolib.Util.Zufall
import Autolib.Util.Sort
import qualified Autolib.Relation as Relation
import Autolib.Set
import Autolib.FiniteMap
import Control.Monad ( guard )
import Data.Maybe

-- | algorithm "hgen2" (c) Edward A Hirsch
-- as sketched in Simon/Berre/Hirsch: SAT2002 Competition

hgen2 :: Param -> IO ( Formel, Belegung )
hgen2 p = do
    Just s <- generate p `repeat_until` isJust
    let strat f = And $ sort 
		$ map ( \ k -> Or $ sort $ literale k ) 
		$ klauseln f
	f = strat $ formula s
	b = assignment s
    case wert f b of -- sanity check
         Just True -> return ( f, b )
	 sonst     -> error $ "SAT.Generator: " ++ show (f, b)

mkBel :: Param -> IO Belegung
mkBel p = do
    pairs <- sequence $ do
        var <- setToList $ vars p
	return $ do
	    val <- randomRIO (False, True)
	    return (var, val)
    return $ listToFM pairs

mkLits :: Param -> Set Literal 
mkLits p = mkSet $ do
    v <- setToList $ vars p
    [ Pos v, Neg v ]

generate :: Param -> IO ( Maybe State )
generate p = do
    s0 <- start p
    let helper s = do
	    ms' <- step s
	    case ms' of
		 Nothing -> return Nothing
		 Just s' -> if 0 == todo s'
			    then return $ return s'
			    else helper s'
    helper s0

-------------------------------------------------------

step :: State -> IO (Maybe State)
step s = do
--     putStr $ show ( todo s, length $ literale ( clause s) )
     if 0 == todo s  
	 then return $ Just s
         else if length ( literale $ clause s ) == width s 
	 then advance s
	 else do
              let cand = do 
		  -- condition 1
                  lit <- setToList $ unfrequent s
		  let var = unLiteral lit -- neu
		      vars = map unLiteral (literale $ clause s ) -- schon da
		  -- muß erfüllen
                  let forced = not ( csat s ) 
			   && length vars == pred ( width s ) 
		  guard $ forced <= ( lit `elementOf` satisfying s )

		  -- condition 2
                  guard $ not $ var `elem` vars

		  -- condition 3
                  let deps = Relation.simages ( dependencies s ) ( mkSet vars )
		  -- guard $ not $ var `elementOf` deps

		  -- todo: dependencies, condition 4 (too hard for few vars)

		  -- condition 5
                  -- guard $  lit `elementOf` satisfying s
		  --	|| opposite lit `elementOf` morefrequent s
		  return lit
	      if null cand 
		 then return Nothing
		 else do
		      lit <- eins cand
		      return $ return $ pick s lit

start :: Param -> IO State
-- initialize everything
start p = do
    bel <- mkBel p
    return $ State
	   { assignment = bel
           , satisfying = mkSet $ do
	         ( var, val ) <- fmToList bel
	         return $ if val then Pos var else Neg var
	   , width = 3
	   , todo = clauses p
	   , formula  = And []
	   , clause = Or []
	   , csat = False
	   , unfrequent = mkLits p
	   , morefrequent = emptySet
	   , dependencies = Relation.make []
	   }

pick :: State -> Literal -> State
-- add one literal to current claus
pick s lit = 
    let  unf = unfrequent   s `minusSet` unitSet lit
         mof = morefrequent s `union` unitSet lit
	 update s = if isEmptySet unf
		    then s { unfrequent = mof
			   , morefrequent = emptySet 
			   }
		    else s { unfrequent = unf
			   , morefrequent = mof
			   }
         deps = Relation.make $ do 
                  let v1 = unLiteral lit
                  v2 <- map unLiteral $ literale ( clause s )
                  [ (v1, v2), (v2, v1) ]
    in   update 
	 $ s { clause = Or $ lit : literale ( clause s )
	     , csat = ( lit `elementOf` satisfying s ) || csat s
	     , dependencies = Relation.plus ( dependencies s ) deps
	     }

advance :: State -> IO ( Maybe State )
-- collect recently built clause
-- add it to the formula
advance s = do
    if clause s `elem` klauseln (formula s)
        then return Nothing
        else return $ return $ s { todo = pred $ todo s
	   , formula = And $ clause s : klauseln ( formula s )
	   , csat = False
	   , clause = Or []
	   }


    



