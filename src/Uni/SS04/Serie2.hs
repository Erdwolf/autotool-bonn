module Uni.SS04.Serie2 where

--  $Id$

import RAM
import RAM.Example

-- import Turing_Fun

import Wort
import ToDoc
import Random
import Data.List (inits)

import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Inter as NI

import qualified Machine.Clock.Type as C
import qualified Machine.Clock.Inter as CI

import qualified Inter.Types as T
import Inter.Wrapper

-- --------------------
tmstep2 :: IO ( T.Var C.Clock
		( C.Type ( Program ) )
		( Program )
	  )

-- Forderung an alle Maschinen
checkforall m = do
		return ()

-- ---------------------------------------
-- Maschinen mit Schrittbeschränkungen
-- ---------------------------------------

tmstep2 = do
    let it = C.Make
	   { C.fun = \ n -> (2*n) + 3
	   , C.fun_info = text "\\ n -> 2n + 3"
	   , C.args = [ 0 .. 7 ] ++ [ 13 ]
	   , C.cut = 1000
	   , C.check = checkforall
       , C.start = stepexample
       }
    return $ CI.clock "RAM" "Linear" it

-- Beispiel für diese Maschinen
stepexample = 
    [ Inc "x0"
     , Loop "x1" [ Loop "x0" [ Inc "x0" ] ]
     ]

-- ----------------------------------------
-- Standard TMs
-- ---------------------------------------
tmpred  :: IO ( T.Var N.Computer 
		  ( N.Type Program )
		  ( Program )
		)

tmpred = do
    let it = N.Make
           { N.fun_info = text "\\ [ x, y ] -> x * y"
           , N.fun = \ [x, y] -> x * y
	   , N.args = do 
	         x <- [ 0 .. 10 ]
		 y <- [ 0 .. 10 ]
		 return [x, y]
           , N.cut = 10000
           , N.check = checkforall
           , N.start = stepexample
           }
    return $ NI.computer "RAM" "Mult" it

-------------------------------------------------------------

generate :: [ IO T.Variant ]
generate = 
    [ do i <- tmpred   ; return $ T.Variant i
    , do i <- tmstep2  ; return $ T.Variant i
    ]