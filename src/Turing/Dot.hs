-- convert to NFA for displaying
-- $Id$

module Turing.Dot where

import Turing.Type
import NFA
import ToDoc

data Triple y = Triple y y Bewegung
    deriving ( Eq, Ord, Show, Read )

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
	      return ( p, Triple input output move , q )
	}
