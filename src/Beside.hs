module Beside where

import Autolib.ToDoc

beside :: Doc -> Doc -> Doc
beside l r = 
    let ls = lines $ render l
        wl = maximum $ 0 : map length ls
        rs = lines $ render r
        comb x y = x ++ replicate (wl - length x) ' ' ++ " | " ++ y
        d  = max ( length ls ) ( length rs )
    in  vcat $ map text 
	     $ take d
	     $ zipWith comb ( ls ++ repeat "" ) ( rs ++ repeat "" )

besides :: [ Doc ] -> Doc
besides [] = empty
besides xs = foldr1 beside xs
