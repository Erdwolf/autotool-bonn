module PCP.Patch where

import PCP.Type
import PCP.Form
import PCP.Examples

import Autolib.Util.Wort

import Control.Monad

-- | rectangular area (no wrap-around)
type Patch c = ([c], [Int] ,[c])

patches :: PCP Char
        -> Int -- ^ depth (max)
	-> Int -- ^ width (max, at start and end)
        -> [ Patch Char ]
patches pcp d w = do
    start <- alles "01" w
    p @ (u, xs, v) <- patches_from pcp d start
    guard $ length v <= w
    return p

patches_from :: Eq c 
	     => PCP c
	     -> Int
	-> [c]
	-> [ Patch c ]
patches_from pcp depth w = do
    (stack, top) <- pf pcp depth [] w [] w
    return (w, stack, top)

pf pcp d stack w done [] =
       do
	  guard $ done /= w
          return (stack, done)
    ++ do
          guard $ d > 0
	  pf pcp (pred d) stack w [] done

pf pcp d stack w done ahead = do
    (i, (l, r)) <- zip [0..] pcp
    let (pre, post) = splitAt (length r) ahead
    guard $ pre == r
    pf pcp d (i : stack) w (done ++ l) post

    
    
    