--  $Id$

module PCP.Top where

import PCP.Type
import PCP.Form
import PCP.Solve

import Autolib.Util.Wort

import Control.Monad

find f = do
    n' <- [ 0 .. ]
    w' <- alle "01" n'
    let w = "0" ++ w' ++ "1"
    let n = length w
	p = form w
    guard $ p <= spiegel p
    let ss = take 1 $ solve p (f * n)
    return (p, map length ss, ss)

