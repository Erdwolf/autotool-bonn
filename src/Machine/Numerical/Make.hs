module Machine.Numerical.Make where

--  $Id$

import Inter.Types

import Machine.Class
import qualified Machine.Var as M
import qualified Arithmetic.Op as A
import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Config as Con
import qualified Autolib.Reporter.Checker as Checker

import Autolib.ToDoc
import Autolib.Reporter
import Data.List ( intersperse )
import Random

testliste :: Int -> Int -> Integer -> IO [[Integer]]
testliste len ari hei = sequence $ replicate len $ do
    xs <- sequence $ replicate ari $ randomRIO (0, hei)
    return xs

{-
make :: ( Con.Check c m , Con.ConfigC c m , Machine m dat conf )
     => Con.Config c m
      ->  Make
-}
make ( defcon :: Con.Config c m ) = 
    let t = "Machine.Numerical" ++ "." ++ Con.name defcon
    in Make t
            ( \ ( conf :: Con.Config c m ) ->  Var 
	          { problem = N.Computer
		  , tag = t
		  , key = \ matrikel -> return matrikel
		  , gen = \ vnr manr key -> fnum conf key 
		  }
	    ) defcon


fnum ::  ( Con.Check c m , Con.ConfigC c m , Machine m dat conf )
     => Con.Config c m 
     -> String
     -> IO ( Reporter ( N.Type m ))
fnum conf key = do
    xss <- testliste 
        ( Con.num_args conf ) ( Con.arity conf ) ( Con.max_arg conf )
    let xs = map M.Var [ 1 .. fromIntegral $ Con.arity conf ]
    return $ return $ N.Make { N.fun = \ xs -> 
                      A.eval ( mkargs xs key ) ( Con.op conf ) 
	      , N.fun_info = fsep 
		     [ text "\\" , toDoc xs , text "->", toDoc $ Con.op conf ]
	      , N.args = xss
	    , N.cut = Con.cut conf
	    , N.check = check_all $ Con.checks conf
	    , N.start = Con.start conf
	      }

mkargs xs key = A.bind $ ( "mat", read key ) : do
         (i, x) <- zip [ 1 .. ]  xs
         return ( "x" ++ show i , x )

check_all :: Con.Check c m 
	  => [c] -> m -> Reporter ()
check_all cs = \ m -> sequence_ $ do c <- cs ; return $ Con.check c m






