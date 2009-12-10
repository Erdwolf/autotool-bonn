{-# LANGUAGE TemplateHaskell #-}

module NFA.Nerode.Incongruent.Solution where

import Autolib.Reader
import Autolib.ToDoc

import Convert.Input

import Autolib.FiniteMap
import Autolib.NFA ( NFAC )

import Data.Typeable

import Prelude hiding ( words )

data NFAC c Int => Solution c =
     Solution { words :: [ [c] ]
              , proofs :: FiniteMap (Int,Int) [c]
              }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Solution])

example :: NFAC c Int => [c] -> Solution c
example w = Solution
        { words = [ w , w ++ w ]
	, proofs = listToFM 
	        [ ((0,1), reverse $ take 3 $ concat $ replicate 4 w ) ]
        }

-- local variables:
-- mode: haskell
-- end;
