module Fun.Examples where

--   $Id$

import Fun.Type

plus = PR 2 [ Proj 1 1 
	     , Sub 3 [ Succ 1, Proj 3 3 ] ]

mal  = PR 2 [ Zero 1   
	     , Sub 3 [ Builtin 2 Plus
		     , Proj 3 1
		     , Proj 3 3 ] ]

hoch = PR 2 [ Sub 1 [ Succ 1, Zero 1 ]
	     , Sub 3 [ Builtin 2 Times
		     , Proj 3 1
		     , Proj 3 3 ] ]

vor   = PR 1 [ Zero 0
	      , Proj 2 1 ]

minus = PR 2 [ Proj 1 1
	      , Sub 3 [ vor , Proj 3 3 ] ]

-- ite x y z == if (0 == x) then y else z
ite = Sub 3 [ PR 3 [ Proj 2 1, Proj 4 2 ]
	    , Proj 3 2, Proj 3 3, Proj 3 1
	    ]

-- du x y z = größtes z' < z mit y * z <= x  (sonst 0)
du = PR 3 [ Zero 2
	   , Sub 4 [ ite
		   , Sub 4 [ Builtin 2 Minus
			   , Sub 4 [ Builtin 2 Times
				   , Proj 4 2, Proj 4 3 ]
			   , Proj 4 1 ]
		   , Proj 4 3
		   , Proj 4 4
		   ]
	   ]

durch = Sub 2 [ du
	      , Proj 2 1
	      , Proj 2 2
	      , Sub 2 [ Succ 1, Proj 2 1 ]
	      ]

rest = Sub 2 [ Builtin 2 Minus
	     , Proj 2 1
	     , Sub 2 [ Builtin 2 Times, Proj 2 1, Proj 2 2 ]
	     ]

