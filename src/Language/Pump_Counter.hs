module Language.Pump_Counter 

-- -- $Id$

where

import Language.Type

import Set
import Random

alpha = "abc"

pump_counter :: Language
pump_counter = Language
	    { abbreviation = "{ a^x b^y c^z : x = 0 oder y = z }"
	    , alphabet	   = mkSet alpha
	    , contains	   = \ w -> 
	          let ( aa, bcs ) = span ( == 'a' ) w
		      ( bb, cs  ) = span ( == 'b' ) bcs
		      ( cc, rest) = span ( == 'c' ) cs
		  in     null rest
		      && ( length aa == 0 || length bb == length cc )
	    , sample       = \ c n -> sequence $ replicate c $ sam n
	    }

sam :: Int -> IO String
-- würfelt ein Wort von ungefähr passender Länge
sam l = do
    f <- randomRIO (False, True)
    let form x y z =  replicate x 'a' ++ replicate y 'b' ++ replicate z 'c'
    if f -- set x = 0
       then do y <- randomRIO (0, l)
	       return $ form 0 y (l - y)
       else do y <- randomRIO (0, l `div` 2)
	       let x = l - 2 * y
	       return $ form x y y


