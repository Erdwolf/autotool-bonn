module SAT.Generator where

-- $Id$

import SAT.Types
import SAT.Param
import SAT.State

import Random
import Set
import FiniteMap

-- algorithm "hgen2" (c) Edward A Hirsch
-- as sketched in Simon/Berre/Hirsch: SAT2002 Competition

mkBel :: Param -> IO Belegung
mkBel p = do
    pairs <- sequence $ do
        var <- setToList $ vars p
	return $ do
	    val <- randomRIO (False, True)
	    return (var, val)
    return $ listToFM pairs

mkLits :: Param -> [ Literal ]
mkLits p = do
    v <- setToList $ vars p
    [ Pos v, Neg v ]

start :: Param -> IO State
start p = do
    bel <- mkBel p
    return $ State
	   { assignment = bel
	   , formula  = []
	   , unfrequent = mkLits p
	   , morefrequent = []
	   , dependencies = emptyFM
	   }



