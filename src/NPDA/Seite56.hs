module Seite56 where

-- $Log$
-- Revision 1.1  2003-01-13 23:20:05  joe
-- NPDA revamped
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2002/01/13 23:03:33  autotool
-- kellerautomaten: nutze toDoc, akzeptanzmodus sichtbar
--

import NPDA
import NPDATest

import Wort
import Random
import ToDoc

pos :: [String]
pos = do
    l <- [0, 1, 2, 4, 8]
    u <- take 4 $ alle "01" l
    let v = u ++ reverse u
    return v

non :: IO String
non = do
    l <- randomRIO (0, 10)
    w <- someIO "01" l
    if even (length w) && w == reverse w 
       then non -- nochmal
       else return w 

negIO :: IO [String]
negIO = sequence . take 10 . repeat $ non

bewerte student = do
    neg <- negIO
    testNPDA  100 pos neg student
