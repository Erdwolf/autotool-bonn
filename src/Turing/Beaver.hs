-- | parser for busy beaver TMs, as in
-- | <http://www.drb.insel.de/~heiner/BB/bb-list>

module Turing.Beaver where

--   $Id$

import Turing
import qualified NFA as N
import qualified Turing.Dot

import Control.Monad ( guard )


-- | examples (Marxen/Buntrock), see above web site
five = " 1 B1L C1R C1L B1L D1L E0R A1R D1R H1L A0R  4098 47176870 "
six  = " 3 B1R C0R A0L D0R D1R H1R E1L D0L F1R B1L A1R E1R "
--    ones  = 2,537,699,363,594,175,843,063
--    steps = 5,366,598,383,321,904,238,506,234,609,927,865,294,538,105

bb :: String -> Turing Char Int
bb line = 
    let code :: Char -> Int
	code c = fromEnum c - fromEnum 'A'
        header :: [ ( Char, Int ) ]
        header = do 
	    p <- [ 'A' .. ]
	    inp <- "01"
	    return ( inp, code p )
	trips :: [ ( Char, Int, Bewegung ) ]
        trips = do
	    [ q, outp, move ] <- words line
	    guard $ move `elem` "LR"
	    guard $ outp `elem` "01"
	    return ( outp, code q, read [move] :: Bewegung )
	tab = collect $ zip header trips
	states :: Set Int
	states = mkSet $ do
	     ( (inp, p), (outp, q, move) ) <- unCollect tab
	     [ p, q ]
    in  Turing {  eingabealphabet = mkSet "01"
	    , arbeitsalphabet  = mkSet "01"
	    , leerzeichen      = '0'
	    , zustandsmenge    = states
	    , tafel	       = tab
	    , startzustand     = code 'A'
	    , endzustandsmenge = unitSet $ code 'H'
	    }


	    
