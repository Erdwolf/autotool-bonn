module SAT.Generator where

-- $Id$

import SAT.Types
import SAT.Param
import SAT.State

import Random
import Util.Zufall
import Util.Sort
import Set
import FiniteMap
import Monad ( guard )
import Maybe

-- algorithm "hgen2" (c) Edward A Hirsch
-- as sketched in Simon/Berre/Hirsch: SAT2002 Competition

hgen2 :: Param -> IO ( Formel, Belegung )
hgen2 p = do
    Just s <- generate p `repeat_until` isJust
    let mkf cls = do 
	    xyz <- setToList cls
	    let [x,y,z] = sort xyz
	    return ( x, y, z )
    return ( mkf $ formula s, assignment s )


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
     -- print s
     putStr "*"
     if 0 == todo s  
	 then return $ Just s
         else if length ( clause s ) == width s 
	 then step $ advance s
	 else do
              let cand = do 
		  -- condition 1
                  lit <- setToList $ unfrequent s
		  -- muß erfüllen
                  let forced = not ( csat s ) 
			   && length ( clause s ) == pred ( width s ) 
		  -- condition 2
                  guard $ not
			$ unLiteral lit `elem` map unLiteral ( clause s ) 
		  -- todo: dependencies
		  -- condition 5
                  guard $  lit `elementOf` satisfying s
			|| opposite lit `elementOf` morefrequent s
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
	   , formula  = emptySet
	   , clause = []
	   , csat = False
	   , unfrequent = mkLits p
	   , morefrequent = emptySet
	   , dependencies = emptyFM
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
    in   update 
	 $ s { clause = lit : clause s
	     , csat = ( lit `elementOf` satisfying s ) || csat s
	     -- , dependencies 
	     }

advance :: State -> State
-- collect recently built clause
-- add it to the formula
advance s = 
    if clause s `elementOf` formula s
    then s -- ignore
    else s { todo = pred $ todo s
	   , formula = formula s `union` unitSet (clause s)
	   , csat = False
	   , clause = []
	   }


    



