module Language.Lukas

-- -- $Id$


( lukas
, nolukas
, dyck
, nodyck
)

where

-- Sprachen von Lukasiewicz, Dyck

import Autolib.Set
import Data.List ( mapAccumL, nub )
import System.Random

import Autolib.Util.Wort
import Language.Type

lukas :: Language
lukas = Language
	      { nametag      = "Lukas" 
	      , abbreviation = "Lukasiewicz-Sprache über {a,b}"
	      , alphabet     = mkSet "ab"
	      , contains     = lukas_ok
	      , sample       = lukas_sam
	      , anti_sample  = sample nolukas
	      }

nolukas :: Language
nolukas = Language
	      { nametag      = "ComLukas"
	      , abbreviation = "Komplement der Lukasiewicz-Sprache über {a,b}"
	      , alphabet     = mkSet "ab"
	      , contains     = not . lukas_ok
	      , sample       = random_sample nolukas
	      , anti_sample  = sample lukas
	      }

dyck :: Language
dyck = Language
	      { nametag      = "Dyck"
	      , abbreviation = "Dyck-Sprache (korrekt geklammerte Wörter über {a,b})"
	      , alphabet     = mkSet "ab"
	      , contains     = dyck_ok
	      , sample       = dyck_sam
	      , anti_sample  = sample nodyck
	      }


nodyck :: Language
nodyck = Language
	      { nametag      = "ComDyck"
	      , abbreviation = "Komplement der Dyck-Sprache (nicht korrekt geklammerte Wörter über {a,b})"
	      , alphabet     = mkSet "ab"
	      , contains     = not . dyck_ok
	      , sample       = random_sample nodyck
	      , anti_sample  = sample dyck
	      }

-------------------------------------------------------------------------

diffs :: String -> [ Int ]
diffs w = snd $ mapAccumL (\ x c -> 
      let y = case c of 'a' -> 1; 'b' -> -1 
      in  (x+y, x+y) ) 0 w

lukas_ok :: String -> Bool
lukas_ok [] = False
lukas_ok w = 
    let ds = diffs w
    in	and [ d >= 0 | d <- init ds ]
	&& last ds < 0

dyck_ok :: String -> Bool
dyck_ok [] = True
dyck_ok w = 
    let ds = diffs w
    in	and [ d >= 0 | d <- ds ]
	&& last ds == 0

-------------------------------------------------------------------------

balanced :: Int -> IO String
balanced n | n > 0 = do
    a <- randomRIO (0,n-1)
    let b = n-1-a
    u <- balanced a
    v <- balanced b
    return $ "a" ++ u ++ "b" ++ v
balanced _ = return ""

-----------------------------------------------------------------

good :: Int -> IO String
good n = do
    w <- balanced n
    return $ w ++ "b"

lukas_sam :: Int -> Int -> IO [ String ]
lukas_sam c n | even n = return []
lukas_sam c n = do
    ws <- sequence $ replicate c $ good (n `div` 2)
    return $ nub ws

dyck_sam c n | odd n = return []
dyck_sam c n = do
    ws <- sequence $ replicate c $ balanced (n `div` 2)
    return $ nub ws






