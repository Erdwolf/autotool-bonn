-- convert to NFA for displaying
-- $Id$

module Turing.Dot where

import Turing.Type
import Dot.Dot

import NFA
import Compact
import ToDoc

type Triple y = ( y,  y, Bewegung )

convert :: ( NFAC (Triple Char) z , UM z ) 
	=> Turing Char z -> NFA (Triple Char ) z
convert tm = 
    NFA { nfa_info = text "converted from Turing"
	, states   = zustandsmenge tm
	, starts   = unitSet $ startzustand tm
	, finals   = endzustandsmenge tm
	, trans    = collect $ do
	      ( (input, p), next ) <- fmToList $ tafel tm
	      ( output, q, move )  <- setToList next
	      return ( p, (input, output, move) , q )
	}

instance ToDoc [ Triple Char ] where
    toDoc ts = brackets $ vcat $ punctuate comma $ map toDoc ts
instance Show  [ Triple Char ] where
    show = render . toDoc


instance ( NFAC (Triple Char) z , NFAC [Triple Char] z , TUM Char z ) 
    => ToDot ( Turing Char z ) where
    toDot tm = toDot $ parallel_compact $ convert tm
    toDotOptions tm = ""
