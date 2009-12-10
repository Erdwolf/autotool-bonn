{-# LANGUAGE NoMonomorphismRestriction #-}

module TAC.Emit where

import qualified TAC.Data as T

import qualified JVM.Type as J

program :: T.Program -> [ J.Statement ]
program stmts = ( do s <- stmts ; statement s ) ++ [ J.Halt ]

statement :: T.Statement -> [ J.Statement ]
statement s = case s of
    T.Constant i c -> number c ++ number i ++ [ J.Store ]
    T.Add i j k -> operation J.Add i j k
    T.Mul i j k -> operation J.Mul i j k

number 0 = [ J.Push 0 ]
number k = replicate k ( J.Push 1 )
       ++ replicate (k-1) ( J.Add )

operation op i j k =
       number j ++ [ J.Load ] 
    ++ number k ++ [ J.Load ]
    ++ [ op ]
    ++ number i ++ [ J.Store ]
