module Boolean.Eval where

--  $Id$

-- import Boolean.Data
import Boolean.Op 
import Expression.Op

import Autolib.FiniteMap
import Autolib.Util.Wort
import Autolib.Set
import Autolib.Reporter
import Autolib.ToDoc

type Belegung v = FiniteMap v Bool

belegungen :: Ord v 
	  => Set v
	  -> [ Belegung v ]
belegungen vs = do
    xs <- alle [ False, True ] $ cardinality vs
    return $ listToFM $ zip ( setToList vs ) xs

eval :: Belegung Identifier -> Exp Bool -> Reporter Bool
eval b = 
    let look x = case lookupFM b x of
	    Just y -> return y
	    Nothing -> reject $ text "Boolean.Eval:" <+> toDoc x
    in tfoldR look inter
