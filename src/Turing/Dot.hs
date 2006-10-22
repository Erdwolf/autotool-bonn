{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

-- convert to NFA for displaying
--   $Id$

module Turing.Dot where

import Turing.Type
import Autolib.Dot.Dot

import qualified Autolib.NFA as N
import qualified Autolib.NFA.Compact as C
import Autolib.ToDoc

-- type Triple y = ( y,  y, Bewegung )

data Triple y = Triple ( y,  y, Bewegung ) 
                deriving ( Eq, Ord )
instance ToDoc y => ToDoc ( Triple y ) where
    toDoc ( Triple ( p, q, m ) ) = 
        toDoc ( p, q, m ) 



convert :: ( N.NFAC (Triple Char) z 
           , UM z ) 
	=> Turing Char z -> N.NFA (Triple Char ) z
convert tm = 
    let transitions = do
	      ( (input, p), next ) <- fmToList $ tafel tm
	      ( output, q, move )  <- setToList next
	      return ( p, Triple (input, output, move) , q )
    in N.NFA 
        { N.alphabet = mkSet $ do ( p, t, q ) <- transitions; return t
        , N.nfa_info = text "converted from Turing"
	, N.states   = zustandsmenge tm
	, N.starts   = unitSet $ startzustand tm
	, N.finals   = endzustandsmenge tm
	, N.trans    = N.collect transitions
	}

instance ToDoc [ Triple Char ] where
    toDoc ts = brackets $ vcat $ punctuate comma $ map toDoc ts


instance ( N.NFAC (Triple Char) z , N.NFAC [Triple Char] z , TuringC Char z ) 
    => ToDot ( Turing Char z ) where
    toDot tm = toDot 
	     --   $ parallel_compact 
	     $ convert tm
    toDotOptions tm = "-Grankdir=LR"

