-- | formeln des aussagenkalküls
--  $Id$

module Language.AK

( form
-- , allg
)

where

import Language.Type
import Language.Mutate
import Autolib.Util.Wort
import Autolib.Util.Zufall

import Autolib.Set
import System.Random
import Data.List (intersperse)
import Control.Monad ( guard )
import Data.Maybe (isJust)

-- | syntaktisch korrekte formeln mit diesen variablen
form :: [ Char ] -> Language
form vs = mutate $ Language
	{ abbreviation = "aussagenlogische Formeln mit A(ND), O(R), N(OT) und Variablen " ++ intersperse ',' vs
	, alphabet     = mkSet $ "AON()," ++ vs
	, contains     = is_form vs
	, sample       = sam vs
	, anti_sample  = anti_sam vs
	}

is_form vs w = isJust $ do
    rest <- formel vs w
    guard $ null rest

formel vs "" = Nothing
formel vs ( x : xs ) = case x of
    'A' -> paar vs xs
    'O' -> paar vs xs
    'N' -> einzel vs xs
    v   -> do guard $ v `elem` vs ; return xs

einzel vs xs = geklammert ( formel vs ) xs
paar   vs xs = geklammert ( zwei vs   ) xs

zwei vs xs = do 
    ',' : xs <- formel vs xs
    xs <- formel vs xs
    return xs

geklammert p ( '(' : rest ) = do
    ')' : rest <- p rest
    return rest
geklammert p _ = Nothing
    

-------------------------------------------------------------------------

sam :: String -> Int -> Int -> IO [ String ]
sam vs c n = sequence $ replicate c 
	   $ samf vs n

-- sample formel
samf vs n | n <= 4 = do
    v <- eins vs
    return [v]
samf vs n = entweder (einst ['N'] vs n) (zweist ['A','O'] vs n)

-- sample formel mit einstelligem top-symbol
einst ts vs n = do
    t <- eins ts
    w <- samf vs (n-3)
    return $ t : "(" ++ w ++ ")"

-- sample formel mit zweistelligem top-symbol
zweist ts vs n = do
    t <- eins ts
    sl <- randomRIO (1, n-4)
    let sr = n-3-sl
    l <- samf vs sl
    r <- samf vs sr    
    return $ t : "(" ++ l ++ "," ++ r ++ ")"


anti_sam :: String -> Int -> Int -> IO [ String ]
-- wir würfeln hier einfach irgendeinen string.
-- das reicht, da weiter oben noch mutiert wird
anti_sam vs c n = do
    let xs = setToList $ alphabet $ form vs
    sequence $ replicate c $ someIO xs n

    
    










