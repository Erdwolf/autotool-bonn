-- $Header$

module Language.Pali 

( pali
, nopali
)

-- $Log$
-- Revision 1.2  2002-12-17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1  2002/11/08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.1  2001/12/05 21:11:31  autotool
-- lang/*
--


where

import Language.Type
import Util.Wort

import Set


pali :: Language
pali = Language
	{ abbreviation = "{ w : w in {0,1}^*, w = reverse w }"
	, alphabet     = mkSet "01"
	, contains     = is_pali 
	, sample       = sam
	}

nopali :: Language
nopali = Language
	{ abbreviation = "{ w : w in {0,1}^*, w /= reverse w }"
	, alphabet     = mkSet "01"
	, contains     = not . is_pali
	, sample       = random_sample nopali
	}

-------------------------------------------------------------------------

sam :: Int -> Int -> IO [ String ]
sam 0 n = return []
sam c 0 = return [ "" ]
sam c n = do
    let (q, r) = divMod n 2
    sequence $ replicate c $ do
        w <- someIO (setToList $ alphabet pali) (q + r)
	return $ w ++ drop r (reverse w)

is_pali w = w == reverse w











