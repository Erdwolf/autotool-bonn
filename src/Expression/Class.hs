module Expression.Class where

import Expression.Type
import Expression.Reader
import ToDoc

class Expressive a where 
      -- default impl. gibt runtime-warning (ist besser als compile-time ??)
      toExp :: a -> Expression
      toExp = error "missing method implementation Expressive.toExp"
      fromExp :: Expression -> a
      fromExp = error "missing method implementation Expressive.fromExp"

showExp :: Expressive a => (a -> String)
showExp = showDoc . toDoc . toExp

readExp :: Expressive a => Int -> ReadS a
readExp p cs = do
        ( x, rest ) <- readsPrec p cs
	return ( fromExp x, rest )

