module Shift.Boiler where

import Shift.Linear
import Shift.Computer
import Shift.Break

import Bits
import List ( group )
import ToDoc


ak k = [2*k,3*k,5*k+1,7*k+1]

ch f = if f then '+' else '-'

itemize = map ( \ g -> Repeat { start = [ Item $ ch $ head g ]
			   , diff  = [ DZero ]
			   , count = length g
			   }
	   ) . group 

sub ps = itemize $ folge ps


brk :: Break -> [ Linear () ]
brk ms = map ( \ n -> Repeat { start = [ Item () ], diff = [ DZero ]
			     , count = fromIntegral n
			     } )
       $ grundy ms

-- mainf a b c = smp $ worker False a $ take b $ brk c
mainf a b c d = do
    putStrLn $ "find patterns"
    let xss = worker a b $ take c $ itemize $ folge d
    smp $ xss
    putStrLn $ "find junk"
    smp $ junks $ last xss
    putStrLn $ "render program"
    smp $ map mkFor $ last xss


