module Language.Cee where

-- -- $Id$

import Language.Type

import Util.Wort
import Util.Zufall
import Monad (guard)

import Set
import Random

cee :: Char -> Language -> Language
cee c l = Language
    { abbreviation = "{ u " ++ [c] ++ " v | u v  in " 
		   ++ abbreviation l ++ " und |u| = |v| }"
    , alphabet = union ( alphabet l ) ( unitSet c )

    , contains = \ w ->
        let n = length w
	    ( u, m : v ) = splitAt ( n `div` 2 ) w
	in odd n && c == m && contains l ( u ++ v )

    , sample = \ count n -> do
        wss <- sequence $ do
            nn <- [ n, n + 1 ]
	    return $ do
	        ws <- sample l count nn
	        return $ do 
	            w <- ws
		    let n = length w
		    guard $ even n
		    let (u, v) = splitAt (n `div` 2) w
		    return $ u ++ [c] ++ v
	return $ concat wss
        
    , anti_sample = \ count n -> do
	  -- die falschen wörter, aber mit richtigem trennzeichen
          this <- sample ( cee c $ komplement l ) count n

	  -- die richtigen wörter (aus l), aber mit falschen trennzeichen
	  ws <- sample l count n
	  that <- sequence $ do
	      w <- ws
	      return $ do
	          k <- randomRIO (0, 5)
		  eins $ shuffle w $ replicate k c
	  -- zur sicherheit nochmal filtern
	  -- denn das trennzeichen könnte doch genau in der mitte sein
	  return $ filter ( not . contains ( cee c l ) ) $ this ++ that

    }

