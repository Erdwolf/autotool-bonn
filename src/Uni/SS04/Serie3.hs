module Uni.SS04.Serie3 where

--  $Id$

import Fun.Type
import Fun.Machine
import Fun.Check
import qualified Fun.Examples
import qualified RAM.Builtin

import qualified Random
import Array

import Machine.Class
import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Inter as I

import qualified Inter.Types as T
import Inter.Wrapper

import ToDoc ( text )


top :: Integer
top = 15

fibs :: Array Integer Integer
fibs = array ( 0, top ) $ ( 0, 0) : ( 1, 1 ) : do
    k <- [ 2 .. top ]
    return ( k, fibs ! (k-1) + fibs ! (k-2) )
    			  
example :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
example = do
    let it = N.Make
	   { N.fun = \ [ x ] -> fibs ! x
	   , N.fun_info = text "\\ x -> fib x"
	   , N.args = do x <- [ 0 .. top ] ; return [x]
	   , N.cut = 2000
	   , N.check = \ f -> do
	         check_builtins RAM.Builtin.every f
	         check_arity 1 f
	   , N.start = Fun.Examples.plus
	   }
    return $ I.computer "FUN" "FIB" it

generate :: [ IO T.Variant ]
generate = 
         [ do e <- example ; return $ T.Variant e 
	 ]
