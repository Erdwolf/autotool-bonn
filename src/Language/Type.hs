module Language.Type

-- -- $Id$


where

import Util.Uniq
import Util.Wort

import ToDoc
import Data.Set
import Control.Monad (guard)
import Random


data Language = Language
	      { abbreviation :: String
	      , nametag      :: String
	      , alphabet     :: Set Char

	      -- testet Mitgliedschaft in Sprache
	      , contains     :: String -> Bool

	      -- sample  c n
	      -- würfelt maximal c Wörter der Sprache
	      -- mit Länge == n
	      , sample       :: Int -> Int -> IO [ String ]

              -- das gleiche für wörter im komplement
              , anti_sample  :: Int -> Int -> IO [ String ]

	      }

komplement :: Language -> Language
komplement l = l { nametag = "Com" ++ nametag l
		 , abbreviation = "Komplement von " ++ abbreviation l
		 , contains = not . contains l
		 , sample = anti_sample l
		 , anti_sample = sample l
		 }

instance ToDoc Language where
    toDoc l = text ( abbreviation l )
instance Show Language where
    show = render . toDoc

komplett :: Language -> [ String ]
-- wirklich *alle* wörter des alphabets werden erzeugt und getestet,
-- es entsteht unendliche liste
komplett l = do
    n <- [ 0 .. ]
    w <- alle ( setToList $ alphabet l ) n  
    guard $ contains l w
    return w


random_sample :: Language -> Int -> Int -> IO [ String ]
-- würfeln und testen
-- nur sinnvoll, wenn sprache genügend dicht ist
random_sample l c n = do
    ws <- sequence $ replicate c $ someIO (setToList $ alphabet l) n
    return $ filter (contains l) ws

samples :: Language -> Int -> Int -> IO [ String ]
-- würfelt genau (!) c Wörter der Sprache l, mit Länge >= n,
-- dabei von jeder festen länge höchstens sqrt c viele
-- das klappt nur, wenn die sprache unendlich ist
samples l c n | c > 0 = do
    let m = truncate $ sqrt $ fromIntegral c    
    here <- sample l m n 
    let d = 1 -- d <- randomRIO (1, 3)
    there <- samples l (c - length here) (n + d)
    return $ uniq $ here ++ there
samples l c n = return []

anti_samples :: Language -> Int -> Int -> IO [ String ]
anti_samples l = samples ( komplement l )



present :: Language -> IO ()
present l = do
    ws <- samples l 20 0
    print $ vcat [ text "Zur Sprache" <+> text (abbreviation l)
		 , text "gehören zum Beispiel diese Wörter:"
		 , toDoc ws
		 ]

