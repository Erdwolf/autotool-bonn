-- |  Sprachen 
--  $Id$

module Language.Type where

import Autolib.Util.Uniq
import Autolib.Util.Wort ( alle )

import Autolib.ToDoc
import Autolib.Set
import Autolib.Util.Zufall
import Control.Monad (guard)

data Language = Language
	      { abbreviation :: String
	      , nametag      :: String
	      , alphabet     :: Set Char

	      -- | testet Mitgliedschaft in Sprache
	      , contains     :: String -> Bool

	      -- | sample  c n
	      -- würfelt maximal c Wörter der Sprache
	      -- mit Länge == n
	      , sample       :: Int -> Int -> RandoM [ String ]

              -- | das gleiche für wörter im komplement
              , anti_sample  :: Int -> Int -> RandoM [ String ]

	      }



uneps :: Language -> Language
uneps l = l  { nametag = "Uneps" ++ nametag l
             , abbreviation = abbreviation l ++ " ohne Epsilon"
             , contains = \ w -> not (null w) && contains l w
             , sample = \ c n -> do ws <- sample l c n ;  return $ filter ( not . null ) ws
             , anti_sample = \ c n -> do ws <- anti_sample l c n ; return $ "" : ws
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

-- | wirklich *alle* wörter des alphabets werden erzeugt und getestet,
-- es entsteht unendliche liste
komplett :: Language -> [ String ]
komplett l = do
    n <- [ 0 .. ]
    w <- alle ( setToList $ alphabet l ) n  
    guard $ contains l w
    return w


-- | würfeln und testen
-- nur sinnvoll, wenn sprache genügend dicht ist
random_sample :: Language 
	      -> Int -- ^ so oft würfeln 
	      -> Int -- ^ für wörter genau dieser länge
	      -> RandoM [ String ]
random_sample l c n = do
    ws <- sequence $ replicate c $ someIO (setToList $ alphabet l) n
    return $ filter (contains l) ws

-- | würfelt genau (!) c Wörter der Sprache l, mit Länge >= n,
-- dabei von jeder festen länge höchstens sqrt c viele
-- das klappt nur, wenn die sprache unendlich ist
samples :: Language 
	-> Int -- ^ so viele wörter (c)
	-> Int -- ^ mindestens so lang (n)
	-> RandoM [ String ]
samples l c n | c > 0 = do

    let m = truncate $ sqrt $ fromIntegral c
    here <- sample l m n 

    let d = 1 -- d <- randomRIO (1, 3)
    there <- samples l (c - length here - if null here then 1 else 0) 
		       (n + if null here then d  else m )
    return $ uniq $ here ++ there
samples l c n = return []

anti_samples :: Language -> Int -> Int 
             -> RandoM [ String ]
anti_samples l = samples ( komplement l )


-- | TODO: das ist nicht OK, weil es IO benutzt.
-- es sollte ein Random-State genügen.
-- das würde auch andere Würfel-probleme lösen.

present :: Language -> RandoM Doc
present l = do
    ws <- samples l 20 0
    return $ vcat [ text "Zur Sprache" <+> text (abbreviation l)
		 , text "gehören zum Beispiel diese Wörter:"
		 , toDoc ws
		 ]

