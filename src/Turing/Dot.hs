-- convert to NFA for displaying
-- $Id$

module Turing.Dot where

import Turing.Type
import Dot.Dot

import qualified NFA as N
import Compact
import ToDoc

type Triple y = ( y,  y, Bewegung )

convert :: ( N.NFAC (Triple Char) z , UM z ) 
	=> Turing Char z -> N.NFA (Triple Char ) z
convert tm = 
    N.NFA { N.nfa_info = text "converted from Turing"
	, N.states   = zustandsmenge tm
	, N.starts   = unitSet $ startzustand tm
	, N.finals   = endzustandsmenge tm
	, N.trans    = N.collect $ do
	      ( (input, p), next ) <- fmToList $ tafel tm
	      ( output, q, move )  <- setToList next
	      return ( p, (input, output, move) , q )
	}

instance ToDoc [ Triple Char ] where
    toDoc ts = brackets $ vcat $ punctuate comma $ map toDoc ts
instance Show  [ Triple Char ] where
    show = render . toDoc


instance ( N.NFAC (Triple Char) z , N.NFAC [Triple Char] z , TUM Char z ) 
    => ToDot ( Turing Char z ) where
    toDot tm = toDot 
	     -- $ parallel_compact 
	     $ convert tm
    toDotOptions tm = "-Grankdir=LR"


instance N.NFAC (Turing.Dot.Triple Char) Char
instance N.NFAC [Turing.Dot.Triple Char] Char
