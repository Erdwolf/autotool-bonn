module RM.Basic where

import RM.Type ( Program )
import RM.Ceh ( conc )

-------------------------------------------------------------------------------

add, sub, mult, quad, exp, div, divMod, fib, sqrt, fac, min :: Program
add    = read $ conc [ clear 3, clear 4, gen_add 1 2 0 4 ]
sub    = read $ conc [ clear 3, clear 4, gen_sub 1 2 0 4 ]
mult   = read $ conc [ clear 3, clear 4, clear 5 , gen_mult 1 2 0 4 5 ]
quad   = read $ conc [ clear 2, clear 3, clear 4 , gen_mult 1 1 0 2 4 ]
exp    = read $ conc [ clear 3, clear 4, clear 5, clear 6, clear 7 
		     , gen_exp 1 2 0 4 5 6 7
		     ]
div    = read $ conc [ clear 3, clear 4, clear 5, clear 6
		     , gen_div 1 2 0 4 5 6 
		     ]
divMod = read $ conc [ clear 3, clear 4, clear 5, clear 6, clear 7, clear 8
		     , clear 9, gen_divMod 1 2 0 4 5 6 7 8 9
		     ]
fib  = read "a2(s1a3(s0a4)0(s2a0a4)2(s4a2)4)1(s3a1)3(s2)2"
sqrt = read "(s1a0a2)1(s2a1)2a4(s4(s0a3a4)0(s4a0)4(s3(s0a2a4)0(s4a0)4)3(s1s2a3)1(s3a1)3(s2(s4)4a4)2(s4s0a3)4(s3a4)3)4"
fac  = read "a0((s0a2)0(s2(s1a0a3)1(s3a1)3)2a4s1)1(s4a1)4"
min  = read "(s2a3a4)2(s4a2)4(s1s3a4)1(s4a1)4(s3(s4)4a4)3a5(s4s5(s1a0a3)1(s3a1)3)4(s5(s2a0a3)2(s3a2)3)5"

-------------------------------------------------------------------------------

-- | [x,y,z,_] -> [x,y,x+y,_]

gen_add :: Int -> Int -> Int -> Int -> String
gen_add x y z h = conc [ copy "a" x z h, copy "a" y z h ]

-- | [x,y,z,_] -> [x,y,max 0 (x-y),_]

gen_sub :: Int -> Int -> Int -> Int -> String
gen_sub x y z h = conc [ copy "a" x z h, copy "s" y z h ]

-- | [x,y,z,_,_] -> [x,y,x*y,_,_]

gen_mult :: Int -> Int -> Int -> Int -> Int -> String
gen_mult x y z h1 h2 = conc [ copy "a" x h1 h2 
			    , while h1 $ conc [ copy "a" y z h2 ]
			    ]

-- | [x,y,0,_,_,_] -> undefined
-- | [x,y,z,_,_,_] -> [x,y, min { z | yz \geq x},_,_]

gen_div :: Int -> Int -> Int -> Int -> Int -> Int -> String
gen_div x y z h4 h5 h6
    = conc [ "a" , show h5
	   , while y $ conc [ "s" , show h5
			    , "a" , show h6 
			    ] 
--	   , while' h5 $ conc [ "a" , show h5 ]
	   , while h6 $ conc [ "a" , show y  ]
	   , copy "a" x h4 h5
	   , while' h4 $ conc [ "a" , show z
			      , while y $ conc [ "s" , show h4
					       , "a" , show h5 
					       ]
			      , while h5 $ conc [ "a" , show y ]
			      ]
	   ]

-- | [x,0,zq,zr,_,...] -> undefined
-- | [x,y,zq,zr,_,...] -> [x,y,div x y,mod x y,_,...]

gen_divMod :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int 
	   -> String
gen_divMod x y zq zr h1 h2 h3 h4 h5
    = conc [ gen_div x y h1 h2 h3 h4
	   , gen_mult y h1 h2 h3 h4
	   , gen_sub h2 x h3 h4
	   , copy "a" h3 h5 h4
	   , gt0 h3 h4
	   , move h5 h3
	   , move h1 zq
	   , while h4 $ conc [ "s" , show zq , gen_sub y h3 zr h4 ]
	   ]

-- | [x,y,z,_,_,_,_] -> [x,y,x^y,_,_,_,_]

gen_exp :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
gen_exp x y z h1 h2 h3 h4
    = conc [ "a" , show z
	   , copy "a" y h1 h2
	   , while h1 $ conc [ gen_mult x z h2 h3 h4
			     , clear z , move h2 z
			     ]
	   ]

-------------------------------------------------------------------------------

-- | [x,z] -> [0,if x > 0 then 1 else 0]

gt0 :: Int -> Int -> String
gt0 x z = while x $ conc [ clear z , "a" , show z ]

-- | [x,y,z,_] -> [x,y,if x < y then 1 else 0]

lt :: Int -> Int -> Int -> Int -> String
lt x y z h = conc [ gen_sub y x z h
		  , clear h
		  , gt0 z h
		  , move h z
		  ]

-- | [x,y,z,_,_,_,_,_,_] -> [x,y,if x==y then 1 else 0,_,_,_,_,_,_]

eq :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
eq x y z h1 h2 h3 h4 h5 h6
    = conc [ clear h6 , lt x y h1 h6
	   , clear h6 , lt y x h2 h6
	   , RM.Basic.not h1 h3 h5 h6
	   , RM.Basic.not h2 h4 h5 h6
	   , clear h5
	   , clear h6
	   , RM.Basic.and h3 h4 z h5 h6
	   ]

-- | [x,y,z,_,_] -> [x,y,if x > 0 and y > 0 then 1 else 0,_,_]

and :: Int -> Int -> Int -> Int -> Int -> String
and x y z h1 h2 = conc [ gt0 x h1
		       , gt0 y h2
		       , clear z
		       , while h1 $ conc [ "a" , show z ]
		       , while h2 $ conc [ "a" , show z ]
		       , "s" , show z
		       , clear h1
		       , move z h1
		       , gt0 h1 z
		       ]

-- | [x,z,_,_] -> [x,1-x,_,_]

not :: Int -> Int -> Int -> Int -> String
not x z h1 h2 = conc [ clear h1 , "a" , show h1      -- h1 = 1
		     , clear h2 , gen_sub h1 x z h2
		     ]

-------------------------------------------------------------------------------

-- | R_post[n] = 0

clear :: Int -> String
clear n = while n []

-- | while (R[n]>0) { R[n]=R[n]-1; s; }

while :: Int -> String -> String
while n s = while' n $ conc [ "s" , show n , s ]

-- | while (R[n]>0) { s; }

while' :: Int -> String -> String
while' n s = conc [ "(" , s , ")" , show n ]

-- | R_post[to] = R_pre[to] + R_pre[from]
-- | R_post[from] = 0

move :: Int -> Int -> String
move from to = while from $ conc [ "a", show to ]

-- | R_post[from] = R_pre[from]
-- | R_post[to]   = R_pre[to] `f` R_pre[from]
-- | R_post[tmp]  = 0

copy :: String -> Int -> Int -> Int -> String
copy f from to tmp 
    = conc [ while from $ conc [ f , show to , "a" , show tmp ]
	   , move tmp from
	   ]

