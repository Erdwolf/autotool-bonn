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

	      -- | testet Mitgliedschaft in Sprache
	      , contains     :: String -> Bool

	      -- | sample  c n
	      -- w�rfelt maximal c W�rter der Sprache
	      -- mit L�nge == n
	      , sample       :: Int -> Int -> IO [ String ]

              -- | das gleiche f�r w�rter im komplement
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

-- | wirklich *alle* w�rter des alphabets werden erzeugt und getestet,
-- es entsteht unendliche liste
komplett :: Language -> [ String ]
komplett l = do
    n <- [ 0 .. ]
    w <- alle ( setToList $ alphabet l ) n  
    guard $ contains l w
    return w


-- | w�rfeln und testen
-- nur sinnvoll, wenn sprache gen�gend dicht ist
random_sample :: Language 
	      -> Int -- ^ so oft w�rfeln 
	      -> Int -- ^ f�r w�rter genau dieser l�nge
	      -> IO [ String ]
random_sample l c n = do
    ws <- sequence $ replicate c $ someIO (setToList $ alphabet l) n
    return $ filter (contains l) ws

-- | w�rfelt genau (!) c W�rter der Sprache l, mit L�nge >= n,
-- dabei von jeder festen l�nge h�chstens sqrt c viele
-- das klappt nur, wenn die sprache unendlich ist
samples :: Language 
	-> Int -- ^ so viele w�rter (c)
	-> Int -- ^ mindestens so lang (n)
	-> IO [ String ]
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
		 , text "geh�ren zum Beispiel diese W�rter:"
		 , toDoc ws
		 ]

