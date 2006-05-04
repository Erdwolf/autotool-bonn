--   $Id$

module Scorer.Util 

( module Scorer.Util
, module Control.Types
)

where

import Control.Types

import Time

zeit :: IO String
zeit = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return $ calendarTimeToString cal

-------------------------------------------------------------------------------

fill :: Int -> String -> String
fill n s = take n $ s ++ take n (repeat ' ')

sep :: Int -> String -> String
sep n s = fill n s ++ " | "

stretch :: Int -> String -> String
stretch = stretchWith ' '

stretchWith :: Char -> Int -> String -> String
stretchWith c n s = 
    let missing = max 0 ( n - length s )
    in  replicate missing c ++ s 

-------------------------------------------------------------------------------

heading :: String -> String
heading s = unlines [ s, take (length s) (repeat '-') ]

-------------------------------------------------------------------------------

pairAdd :: (Num a) => (a,a) -> (a,a) -> (a,a)
pairAdd (a,b) (c,d) = (a+c,b+d)
