module Uni.SS04.Serie3 where

--   $Id$ 


import Fun.Type
import Fun.Machine
import Fun.Check
import qualified Fun.Examples
import qualified RAM.Builtin

import Fun.Quiz
import Fun.Quiz.Type
import Fun.Create
import Fun.Table


-- Helper
import qualified Uni.SS04.Testliste as TL

import Array
import Prime
import Sets (mkSet,union)


-- Machine Parts
import Machine.Class
import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Inter as I

-- Challenger

import Challenger.Partial

-- Inter.Face
import qualified Inter.Types as T
import Inter.Wrapper

import ToDoc ( text )


aufgaben = [ funFib , funFak 
           , funTeilbar 
           , funTeilbar2 
           , funPrim
           ]

generate :: [ IO T.Variant ]
generate = map toVariant aufgaben 
           ++ [ do i <- funQuiz ; return $ T.Variant i ]
   where
   toVariant var = do i<- var ; return $ T.Variant i
-- --------------------------------------------------
-- FUN FIB
-- --------------------------------------------------
top :: Integer
top = 15

fibs :: Array Integer Integer
fibs = array ( 0, top ) $ ( 0, 0) : ( 1, 1 ) : do
    k <- [ 2 .. top ]
    return ( k, fibs ! (k-1) + fibs ! (k-2) )
                          
funFib :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funFib = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 1
                  , TL.len = 5
                  , TL.fixs = [[0]]
                  , TL.randVonBis = (1,10) 
                  }

    let it = N.Make
           { N.fun = \ [ x ] -> fibs ! x
           , N.fun_info = text "\\ x -> fib x"
           , N.args = args
           , N.cut = 2000
           , N.check = \ f -> do
                 check_builtins RAM.Builtin.every f
                 check_arity 1 f
                 anzeig $ tabelle1 f 9
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "FIB" it


-- ich wuerde gerne fuer die naechste Aufgabenserie primitiv rekursive
-- Funktionen fuer fib, teilbar, prim, Fakultaet schreiben lassen, sowie die
-- quiz-Aufgabe geben.

-- --------------------------------------------------
-- FUN TEILBAR 
-- --------------------------------------------------


funTeilbar :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funTeilbar = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 2 
                  , TL.len = 7
                  , TL.fixs = []
                  , TL.randVonBis = (1,20) 
                  }
    let it = N.Make
           { N.fun = \ [ x , y ] -> if x `mod` y == 0 then 1 else 0
           , N.fun_info = text "if x `mod` y == 0 then 1 else 0"
           , N.args =  args -- [[x,y] | x <- [ 13 .. 25 ] , y <- [ 2 .. 7 ]]-- testlisteTeilbar
           , N.cut = 20000
           , N.check = \ f -> do
                 check_builtins RAM.Builtin.every f
                 check_arity 2 f
                 anzeig $ tabelle2 f (5, 5)
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "TEILBAR" it

-- --------------------------------------------------
-- FUN TEILBAR2
-- --------------------------------------------------

funTeilbar2 :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funTeilbar2 = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 2 
                  , TL.len = 7
                  , TL.fixs = []
                  , TL.randVonBis = (1,20) 
                  }
    let it = N.Make
           { N.fun = \ [ x , y ] -> if x `mod` y == 0 then 1 else 0
           , N.fun_info = text "if x `mod` y == 0 then 1 else 0"
           , N.args =  args -- [[x,y] | x <- [ 13 .. 20 ] , y <- [ 2 .. 7 ]]-- testlisteTeilbar
           , N.cut = 200000
           , N.check = \ f -> do
                 check_builtins ( mkSet[ RAM.Builtin.Plus 
                                     , RAM.Builtin.Minus
                                     , RAM.Builtin.Times  
                                     ] ) f
                 check_arity 2 f
                 anzeig $ tabelle2 f (5,5)
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "TEILBAR2" it

-- --------------------------------------------------
-- FUN FAK
-- --------------------------------------------------

fak :: Integer -> Integer
fak x | x < 2 = 1
fak x = x * fak ( x - 1 )

funFak :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funFak = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 1 
                  , TL.len = 5
                  , TL.fixs = []
                  , TL.randVonBis = (0,10) 
                  }
    let it = N.Make
           { N.fun = \ [ x ] -> fak x
           , N.fun_info = text "\\ x -> x!"
           , N.args = [[x] | x <- [0 .. 5] ]-- testlisteFak
           , N.cut = 20000
           , N.check = \ f -> do
                 check_builtins RAM.Builtin.every f
                 check_arity 1 f
                 anzeig $ tabelle1 f 9
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "FAK" it

-- --------------------------------------------------
-- FUN PRIM
-- --------------------------------------------------

funPrim :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funPrim = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 1 
                  , TL.len = 10 
                  , TL.fixs = (map return [0,1,2,7,23,51])
                  , TL.randVonBis = (10,100) 
                  }
    let it = N.Make
           { N.fun = \ [ x ] -> if prime x then 1 else 0
           , N.fun_info = text "\\ x -> if istPrimzahl( x ) then 1 else 0"
           , N.args = args
           , N.cut = 20000
           , N.check = \ f -> do
                 check_builtins RAM.Builtin.every f
                 check_arity 1 f
                 anzeig $ tabelle1 f 9
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "PRIM" it

-- ---------------------------------------------------
-- FUN QUIZ
-- ---------------------------------------------------
funQuiz :: IO (T.Var Quiz Tafel Fun )
funQuiz = return $ quiz "FUN" "QUIZ" 12 10
