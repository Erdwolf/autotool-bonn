{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

--  $Id$

module PCP.Symbolic where

import PCP.Compressed (tops, bots)
import Autolib.Tex
import Autolib.ToTex

import Data.Char
import Control.Monad

data Item = Num Int 
	  | Yps Int
          | Fmt String
          | Box Item
     deriving (Eq, Show)

shift :: Int -> Item -> Item
shift d (Num i) = Num ((i + d) `mod` 3)
shift d (Yps i) = Yps ((i + d) `mod` 3)

instance Read Item where
    readsPrec p (d : rest) 
        | d == 'y'  = return (Yps $ 0       , rest)
        | isDigit d = return (Num $ read [d], rest)
        | isSpace d = readsPrec p rest
    readsPrec p _ = mzero

instance ToTex Item where
    toTex (Num i) = Direct $ show i
    toTex (Yps i) = Direct $ case i of
        0 -> "y" ; 1 -> "y_+" ; 2 -> "y_-"
    toTex (Fmt cs) = Direct cs
    toTex (Box it) = Macro { name = "fbox", args = [ Req $ toTex it ] }

-- | in fact: exactly four items per block
type Block = [ Item ] 

instance ToTex Block where
    toTex its = Sequence { items = map toTex its }

instance Read Block where
    readsPrec p cs = 
        case readsPrec p cs of
             [(i, rest)] -> do
	         (is, rest') <- readsPrec p rest
                 return (i : is, rest')
             _ -> return ([], cs)

type Line = [ Block ]

instance ToTex [ Line ] where
    toTex lines = Align { rows = map (map toTex) lines }

next :: Block -> Line -> (Int, Int) -> Line
next w line (b, p')  = 
    let (bre, block : bost) = splitAt b line
        m = length block
        p = if p' < 0 then p' + m else p'
        (pre, item  : post) = splitAt p block
        items' = case item of
            Num d -> map (shift $ succ d) w
            Yps d -> error "PCP.Describe.next.exp"
        (here, there) = splitAt m $ pre ++ items' ++ post
        block' = [here, there]
    in  bre ++ block' ++ bost

topform :: Block -> [(Int, Int)] -> [ Line ]
topform w bps = zipWith (:)
    ( [Fmt " "] : repeat [Fmt "\\mto"] )
    ( fbox bps $ scanl (next w) [ init w ] bps )

botform :: Block -> [(Int, Int)] -> [ Line ]
botform w bps = zipWith (:)
    ( [Fmt "\\msim"] : repeat [Fmt "\\mfrom"])
    ( reverse $ fbox (reverse bps) $ scanl (next w) [ tail w ] $ reverse bps )

fbox bps lines = 
    let (here, rest) = splitAt (length bps) lines
    in  do ((b, p'), line) <- zip bps here
	   let (bre, block : bost) = splitAt b line
               m = length block
	       p = if p' < 0 then p' + m else p'
	       (pre, item  : post) = splitAt p block
           return $ bre ++ [ pre ++ Box item : post ] ++ bost
        ++ rest        

form :: String -> (Int, Int) -> Tex
form cs (t, b) =
    let w = read cs :: Block
        lines =  topform w (tops !! t)
              ++ botform w (bots !! b)
        width = maximum $ map length lines
        fmt = Direct $ replicate width 'l'
    in  Env { name = "array"
	    , args = [ Req fmt ]
	    , contents = toTex lines
	    }

runit :: IO ()
runit = do
    let handle w (t, b) = do
            let fname = "PCP/" ++ w ++ ".tex"
            putStrLn $ unwords [ "writing", "file", fname ]
            writeFile fname $ show $ form w (t, b)

    handle "001"   (3,0)
    handle "011"   (6,7)
    handle "00y01" (0,1)
    handle "00y11" (2,2)
    handle "01y01" (2,2)
    handle "01y11" (5,6)
