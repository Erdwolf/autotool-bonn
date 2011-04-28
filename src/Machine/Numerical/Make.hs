{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
module Machine.Numerical.Make where

import Inter.Types

import Machine.Class
import qualified Machine.Var as M
import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Config as Con

import Challenger.Partial

import Autolib.ToDoc
import Autolib.Reporter
import System.Random
import Data.Typeable

instance OrderScore N.Computer where
    scoringOrder _ = Increasing

testliste :: Int -> Int -> Integer -> IO [[Integer]]
testliste len ari hei = sequence $ replicate len $ do
    xs <- sequence $ replicate ari $ randomRIO (0, hei)
    return xs

make :: forall c m dat conf b
     . ( Show c, Con.Check c m , Con.ConfigC c m , Machine m dat conf 
       , Partial N.Computer ( N.Type c m ) b, Typeable b
       )
     => Con.Config c m
      ->  Make
make ( defcon :: Con.Config c m ) = 
    let t = "Machine.Numerical" ++ "." ++ Con.name defcon
    in Make N.Computer t
            ( \ ( conf :: Con.Config c m ) ->  Var 
	          { problem = N.Computer
		  , tag = t
		  , key = \ matrikel -> return matrikel
		  , gen = \ _vnr _manr key _cache -> fnum conf key 
		  }
	    )
	    ( \ _con -> return () ) -- verify
	    defcon


fnum ::  ( Show c , Con.Check c m , Con.ConfigC c m , Machine m dat conf )
     => Con.Config c m 
     -> String
     -> IO ( Reporter ( N.Type c m ))
fnum conf key = do
    xss <- testliste 
        ( Con.num_args conf ) ( Con.arity conf ) ( Con.max_arg conf )
    let xs = map M.Var [ 1 .. fromIntegral $ Con.arity conf ]
    return $ return $ N.Make { N.op = Con.op conf
              , N.key = read key
	      , N.fun_info = fsep 
		     [ text "\\" , toDoc xs , text "->", toDoc $ Con.op conf ]
	      , N.extra_info = vcat $
		(text "Die Maschine soll die folgenden Bedingungen erfüllen:") 
		 : 
		(do c <- map show ( Con.checks conf) ++ Con.conditions conf
		    return $ nest 4 $ text $ "* " ++ c
		)
	      , N.args = xss
	    , N.cut = Con.cut conf
	    , N.checks = Con.checks conf
	    , N.start = Con.start conf
	      }

