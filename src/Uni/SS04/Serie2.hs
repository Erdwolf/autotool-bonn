module Uni.SS04.Serie2 where

--  $Id$

import RAM
import RAM.Example
import RAM.Check
import RAM.Builtin

import Prime (prime)
import Sets (mkSet)
-- import Turing_Fun

import Wort
import ToDoc
import Random
import Array
import Data.List (inits)

import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Inter as NI

import qualified Machine.Clock.Type as C
import qualified Machine.Clock.Inter as CI

import qualified Inter.Types as T
import Inter.Wrapper


-- Helper 
-- Forderung an alle Maschinen
checkforall m = do
        return ()

conformloop bs prog = 
    do
    loopy prog
    builtins bs prog
    

conformwhile bs prog = 
    do
    builtins bs prog

loopexample = [ Loop "x1" [ Dec "x4" ] ]

-- ----------------------------------------
-- Standard Machine
-- ---------------------------------------
loopfib  :: IO ( T.Var N.Computer 
          ( N.Type Program )
          ( Program )
        )

-- tmpred  :: IO ( T.Var N.Computer 
--           ( N.Type Program )
--           ( Program )
--         )
-- tmpred = do
--     let it = N.Make
--            { N.fun_info = text "\\ [ x, y ] -> x * y"
--            , N.fun = \ [x, y] -> x * y
--            , N.args = do 
--              x <- [ 0 .. 10 ]
--              y <- [ 0 .. 10 ]
--              return [x, y]
--            , N.cut = 10000
--            , N.check = checkforall
--            , N.start = stepexample
--            }
--     return $ NI.computer "RAM" "Mult" it

-- -----------------------------------------------------------
-- LOOP-FIB 
-- Builtin = None
-- -----------------------------------------------------------

-- Helper für loopfib
top :: Integer
top = 15

-- fibs = Cache-Array mit Fibonacci Zahlen bis top
fibs :: Array Integer Integer
fibs = array ( 0, top ) $ ( 0, 0) : ( 1, 1 ) : do
    k <- [ 2 .. top ]
    return ( k, fibs ! (k-1) + fibs ! (k-2) )
                  
fibtestliste :: IO [[Integer]]
fibtestliste = sequence $ replicate 10 $ do
    xy <- sequence $ replicate 1 $ randomRIO (0, top)    
    return xy


loopfib = do
    tests <- fibtestliste
    let it = N.Make
             { N.fun_info = text "x0 := fib( x1 )"
             , N.fun      = \ [ x ] -> fibs ! x
             , N.args     = tests
             , N.cut      = 10000
             , N.check    = conformloop RAM.Builtin.every --RAM.Builtin.none
             , N.start    = loopexample
             }
    return $ NI.computer "LOOP" "FIB" it
-- -----------------------------------------------------------
-- LOOP-PRIM
-- Builtin = Mod
-- -----------------------------------------------------------
nums :: IO [Integer]
nums = do 
    let fixed = [ 0 .. 10] ++ [ 23 , 101 ]
    rnds <- sequence $ replicate 10 $ randomRIO ( 10, 100 )
    return $ fixed ++ rnds


iolooptests :: IO [[Integer]]
iolooptests = do
    xs <- nums 
    return $ map return xs

loopprim = do
    tests <- iolooptests  
    let it = N.Make
             { N.fun_info = text "x0 :=  if istPrimzahl( x1 ) then 1 else 0"
             , N.fun      = \ [x]  -> if prime x then 1 else 0
             , N.args     = tests
             , N.cut      = 10000
             , N.check    = conformloop RAM.Builtin.every -- $ mkSet [ RAM.Builtin.Mod ]
             , N.start    = loopexample
             }
    return $ NI.computer "LOOP" "PRIM" it

-- -----------------------------------------------------------
-- LOOP-SQRT
-- Builtin = Times
-- -----------------------------------------------------------
loopsqrt = do 
    tests <- iolooptests  
    let it = N.Make
             { N.fun_info = text "x0 :=  abgerundete Quadratwurzel von x1"
             , N.fun      = \ [ x ] -> truncate $ sqrt $ fromIntegral x
             , N.args     = tests
             , N.cut      = 10000
             , N.check    = conformloop RAM.Builtin.every -- $ mkSet [ RAM.Builtin.Times ]
             , N.start    = loopexample
             }
    return $ NI.computer "LOOP" "SQRT" it

-- -----------------------------------------------------------
-- WHILE-DIV
-- Builtin = ?
-- -----------------------------------------------------------
whilediv = do 
    tests <- iowhiletests  
    let it = N.Make
             { N.fun_info = text "x0 :=  x1 div x2"
             , N.fun      = \ [ x1 , x2 ] -> x1 `div` x2
             , N.args     = tests --foldr1 (++) [ [x1 , x2]  | x1 <- [1 .. 10] ++ [ 21 .. 23] , x2 <- [1 .. 7] ]
             , N.cut      = 10000
             , N.check    = conformwhile RAM.Builtin.none
             , N.start    = [ Loop "x1" [ Dec "x4" ] ]
             }
    return $ NI.computer "WHILE" "DIV" it
-- -----------------------------------------------------------
-- WHILE-MOD
-- Builtin = ?
-- -----------------------------------------------------------

iowhiletests :: IO [[Integer]]
iowhiletests = do
	let xs = foldr1 (++) [ [ x1 , x2 ] | x1 <- [1 .. 10] ++ [ 21 .. 23] , x2 <- [1 .. 7] ]
 	return $ map return xs 

whilemod = do 
    tests <- iowhiletests
    let it = N.Make
             { N.fun_info = text "x0 :=  abgerundete Quadratwurzel von x1"
             , N.fun      = \ [ x1 , x2 ] -> x1 `mod` x2
             , N.args     = tests -- foldr1 (++) [ [ x1 , x2 ] | x1 <- [1 .. 10] ++ [ 21 .. 23] , x2 <- [1 .. 7] ]
             , N.cut      = 10000
             , N.check    = conformwhile RAM.Builtin.none
             , N.start    = [ Loop "x1" [ Dec "x4" , Dec "x3" ] ]
             }
    return $ NI.computer "WHILE" "MOD" it


-- -----------------------------------------------------------
generate :: [ IO T.Variant ]
generate = 
        [ do i <- loopfib  ; return $ T.Variant i
        , do i <- loopprim  ; return $ T.Variant i
		, do i <- loopsqrt  ; return $ T.Variant i
		, do i <- whilemod   ; return $ T.Variant i
		, do i <- whilediv   ; return $ T.Variant i
        ]


-- -- --------------------
-- tmstep2 :: IO ( T.Var C.Clock
--         ( C.Type ( Program ) )
--         ( Program )
--       )


-- -- ---------------------------------------
-- -- Maschinen mit Schrittbeschränkungen
-- -- ---------------------------------------

-- tmstep2 = do
--     let it = C.Make
--               { C.fun = \ n -> (2*n) + 3
--               , C.fun_info = text "\\ n -> 2n + 3"
--               , C.args = [ 0 .. 7 ] ++ [ 13 ]
--               , C.cut = 1000
--               , C.check = checkforall
--               , C.start = stepexample
--               }
--     return $ CI.clock "RAM" "Linear" it

--  -- Beispiel für diese Maschinen
-- stepexample = 
--      [ Inc "x0"
--       , Loop "x1" [ Loop "x0" [ Inc "x0" ] ]
--       ]

