module Uni.SS04.Serie1 where

--  $Id$

import Turing
import Turing.Example
import Turing.Bin

-- import Turing_Fun

import Wort
import ToDoc
import Random
import Data.List (inits)

import qualified Machine.Fun.Type as F
import qualified Machine.Fun.Inter as FI

import qualified Machine.Clock.Type as C
import qualified Machine.Clock.Inter as CI

import qualified Inter.Types as T
import Inter.Wrapper


eingaben = do n <- [1 .. maxversuche]
	      return $ unstr (n)
ausgaben = do n <- [1 .. maxversuche]
	      return $ unstr (n) ++ "#" ++ unstr (2*n) 

-- testliste erzeugen (z.B. [("1","1#11"),...]
testliste :: IO [(String,String)]
testliste = do
    -- hier könnte auch gewürfelt werden (deswegen IO)
    let xys = zip eingaben ausgaben
    return xys

-- n = 1 ..
maxversuche = 10 :: Int
-- max. schrittanzahl der tm
maxschritte = 1000 :: Int

tm2 :: IO ( T.Var F.Computer 
		      ( F.Type ( Turing Char Int ) String )
		      ( Turing Char Int )
	       )
tm2 = do
    testing <- testliste
    let it = F.Make
           { F.fun_info = text "verdoppeln??"
           , F.pairs = testing
           , F.cut = 10000
           , F.check = \ m -> return () -- alles OK ?? TODO: det
           , F.start = Turing.Example.student -- ??
           }
    return $ FI.computer "TM" "2" it

clock3 :: IO ( T.Var C.Clock
		      ( C.Type ( Turing Char Int ) )
		      ( Turing Char Int )
	       )
clock3 = do
    let it = C.Make
	   { C.fun = \ n -> n + 1
	   , C.fun_info = text "\\ n -> n + 1"
	   , C.args = [ 2 .. 10 ] ++ [ 12 , 14 .. 20 ]
	   , C.cut = 30
	   , C.check = \ m -> return () -- alles OK ?? TODO: det
	   , C.start = Turing.Example.student
	   }
    return $ CI.clock "TM" "3" it

generate :: [ IO T.Variant ]
generate = 
    [ do i <- tm2   ; return $ T.Variant i
    , do i <- clock3 ; return $ T.Variant i
    ]



