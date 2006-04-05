module Turing.Example where

--   $Id$

import Turing

student :: Turing Char Int
student = Turing 
	             {eingabealphabet = mkSet "1.", 
        arbeitsalphabet = mkSet "01.#",
        leerzeichen = '#', 
        zustandsmenge = mkSet [0, 1, 2, 3, 4, 7],
        tafel = listToFM [(('0', 0), mkSet [('1', 1, L)]),
                          (('0', 1), mkSet [('1', 2, L)]), 
                          (('0', 2), mkSet [('1', 3, L)]),
                          (('0', 3), mkSet [('1', 0, R)]), 
                          (('0', 4), mkSet [('1', 7, L)]),
                          (('1', 0), mkSet [('1', 2, R)]), 
                          (('1', 1), mkSet [('1', 1, L)]),
                          (('1', 2), mkSet [('0', 4, R)]), 
                          (('1', 3), mkSet [('1', 3, R)]),
                          (('1', 4), mkSet [('0', 0, R)])],
        startzustand = 0, 
        endzustandsmenge = mkSet [7]}
	   