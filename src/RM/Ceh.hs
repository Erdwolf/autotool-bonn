-- $Id$

module RM.Ceh 

( ceh
, conc
)

where

import RM.Type

import Data.List ( intersperse )

-------------------------------------------------------------------------------

-- | erzeugt (formatiertes) C-Program aus Registermaschinenprogram
ceh :: Program -> String
ceh = unlines . cehs

cehs :: Program -> [String]
cehs p = let m = succ $ max_reg p
	 in [ "#include <stdio.h>"
	    , "#include <stdlib.h>"
	    , ""
	    , "int main (int argc, char ** argv)"
	    ] ++ block 0 
	         ([ indent 1 $ command [ "int", reg m ]
		  , indent 1 $ command [ "int" , "i", "=" , "0" ]
		  , indent 1 $ command [ "int" , "v", "=" , "0" ]
		  , indent 1 $ command [ "int" , "a", "=" , "0" ]
		  , indent 1 $ command [ "int" , "s", "=" , "0" ]
		  , ""
		  , for 1 ("0",show m) [ command [ regS "i", "=", "0" ] ] 
		  , if_ 1 [ "argc" , ">" , show m ]
		    [ command [ "fprintf" , "(" , "stderr" , ","
			      , "\"zu viele argumente\\n\""
			      , ")"
			      ]
		    , command [ "exit" , "(" , "-1" , ")" ]
		    ]
		  , for 1 ("1","argc") 
		    [ command [ regS "i", "="
			      , "abs" , "(" 
			      , "atoi" , "(" , "argv" , "[" , "i" , "]"
			      , ")" , ")"
			      ]
		    ]
		  , indent 1 $ command
		    [ "printf" , "("
		    , "\"Program:" , show p, "\\n\"", ")"
		    ]
		  , indent 1 $ command 
		    [ "printf" , "("
		    , "\"Registerinhalte vor Programstart:\\n\""
		    , ")"
		    ]
		  , for 1 ("0",show m) 
		    [ command [ "printf" , "(" 
			      , "\"" ++ regS "%d" , "=" , "%d\\n\""
			      , "," , "i" , "," , regS "i"
			      , ")" 
			      ] 
		    ] 
		  ] ++ ceh_prec 1 p ++
		  [ indent 1 $ command 
		    [ "printf" , "("
		    , "\"Registerinhalte nach Programmende:\\n\""
		    , ")"
		    ]
		  , for 1 ("0",show m) 
		    [ command [ "printf" , "(" 
			      , "\"" ++ regS "%d" , "=" , "%d\\n\""
			      , "," , "i" , "," , regS "i"
			      , ")" 
			      ] 
		    ]
		  , indent 1 $ command
		    [ "printf" , "("
		    , "\"Vergleiche:", "%d"
		    , "," , "Additionen:", "%d"
		    , "," , "Subtraktionen:", "%d"
		    , "\\n\"" , "," , "v" , "," , "a" , "," , "s" , ")"
		    ]
		  , indent 1 $ command
		    [ "printf" , "("
		    , "\"Operationen insgesamt:", "%d"
		    , "\\n\"" , "," , "v" , "+" , "a" , "+" , "s" , ")"
		    ]
		  , indent 1 $ command [ "return" , "0" ] 
		  ]
		 )

with_line :: Int -> [String] -> [String] -> String
with_line d l ls = indent d $ unlines $
		   (conc l) : block d (map (indent (d+1)) ls)

for :: Int -> (String,String) -> [String] -> String
for d (lo,hi)
    = with_line d [ "for" , "(" , "i" , "=" , lo 
		  , ";" , "i" , "<" , hi
		  , ";" , "i" , "++" , ")"
		  ]

if_ :: Int -> [String] -> [String] -> String
if_ d l = with_line d ([ "if" , "(" ] ++ l ++ [ ")" ])

ceh_prec :: Int -> Program -> [String]
ceh_prec d (Add n) 
    = [ indent d $ regreg n "+" 
      , indent d $ command [ "a", "++" ]
      ]

ceh_prec d (Sub n) 
    = [ indent d $ command [ "v" , "++" ]
      , indent d $ if_zero n 
      ] ++ block d [ indent (d+1) $ regreg n "-" 
		   , indent (d+1) $ command [ "s", "++" ]
		   ]

ceh_prec d (Conc ps) 
    = concatMap (ceh_prec d) ps

ceh_prec d (While n p) 
    = [ indent d $ command [ "v" , "++" ]
      , indent d $ while_positiv n 
      ] ++ block d ((indent (d+1) $ command [ "v" , "++" ]) : ceh_prec (d+1) p)

regreg :: Register -> String -> String
regreg n fun = command [ reg n , "=" , reg n , fun , "1" ]

command :: [String] -> String
command xs = conc $ xs ++ [ ";" ]

indent :: Int -> String -> String
indent d = (++) (take (4*d) $ repeat ' ')

block :: Int -> [String] -> [String]
block d xs = [ indent d "{" ] ++ xs ++ [ indent d "}" ]

reg :: Register -> String
reg = regS . show

regS :: String -> String
regS s = conc [ "R[" , s , "]" ]

if_zero :: Register -> String
if_zero n = conc [ "if", is_positiv n ]

is_positiv :: Register -> String
is_positiv n = conc [ "(" , reg n , " > 0", ")" ] 

while_positiv :: Register -> String
while_positiv n = conc [ "while", is_positiv n ]
 
conc :: [String] -> String
conc = concat . intersperse " "
