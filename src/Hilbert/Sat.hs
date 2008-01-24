-- | teste, ob formel gültig/erfüllbar

module Hilbert.Sat

( satisfiable
)

where


import Autolib.FiniteMap
import Autolib.Set

import Boolean.Op
import Expression.Op

import Autolib.Util.Uniq
import Autolib.TES.Term 
    hiding ( unvar, assoc, precedence, arity )
import Autolib.Size
import Autolib.TES.Position

import Autolib.Reporter.Type

satisfiable targets = 
    let form_variables = uniq $ do
	    t <- targets
	    Node f [] <- subterms t	
	    return f
	schema_variables = uniq $ do
	    t <- targets
	    setToList $ vars t
    in  and $ do
	    f <- assignments form_variables
	    return $ or $ do
	        g <- assignments schema_variables
		return $ and $ do
		    t <- targets
		    return $ evaluate f g t

assignments [] = return emptyFM
assignments ( v : vs ) = do
    rest <- assignments vs
    this <- [ False, True ]
    return $ addToFM rest v this

evaluate f g t = case t of
    Var  v    -> lookupWithDefaultFM g ( error "g" ) v
    Node v [] -> lookupWithDefaultFM f ( error "f" ) v
    Node op args -> 
        let Just r = result
                   $ inter op $ map ( evaluate f g ) args
        in  r
