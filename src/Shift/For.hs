module Shift.For where

-- -- $Id$

import ToDoc
import Control.Monad ( guard )

-- this is only used for printing
data Prog a = For { var :: String
		, bound :: Ex
		, body :: [ Prog a ]
		}
	  | It { unIt :: a }
	  | FJunk

instance ToDoc a => ToDoc ( Prog a ) where
    toDoc ( x @ It {} ) = toDoc $ unIt x
    toDoc ( x @ For {} ) = fsep
          [ hsep [ text "for", text "0", text "<= ", text (var x)
		 , text "<", toDoc (bound x) , text "loop"
		 ]
	  , nest 4 $ fsep $ punctuate ( text ";") $ map toDoc $ body x
	  , hsep [ text "end", text "loop" ]
	  ]
    toDoc FJunk = text "?"

instance ToDoc a => Show ( Prog a ) where show = render . toDoc


data Ex = Ex { off :: Int
	     , vars :: [ (Int, String) ]
	     }

neu :: Ex -> String
neu ex = let l = length ( vars ex ) 
	 in  "x" ++ show l

signed :: ( Num a, Ord a, ToDoc a ) => a -> Doc
signed x = ( if x >= 0 then text "+" else empty ) 
       <+> toDoc x

factored i = 
    if -1 == i then text "-"
       else if 1 == i then text "+"
	    else signed i

instance ToDoc Ex where
    toDoc ex = hsep $ do ( i, v ) <- vars ex 
			 guard $ i /= 0
			 return $ factored i <+> text v
		    ++ do
		         guard $ off ex /= 0
                         return $ signed ( off ex ) 

instance Show Ex where show = render . toDoc
