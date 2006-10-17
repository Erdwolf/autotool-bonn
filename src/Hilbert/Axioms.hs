module Hilbert.Axioms

( axioms
)

where

import Hilbert.Syntax
import Hilbert.Env

import Boolean.Op
import Expression.Op

axioms :: Env ( Exp Bool )
axioms = Hilbert.Env.make
     [ read "(H1, A -> (B -> A))" 
     , read "(H2, ((A -> B) -> A) -> A)"
     , read "(H3, (A -> B) -> ((B -> C) -> (A -> C)))"

     , read "(H4, A && B -> A )"
     , read "(H5, A && B -> B )"
     , read "(H6, (A -> B) -> ((A -> C) -> (A -> B && C)) )"

     , read "(H7, A -> A || B )"
     , read "(H8, B -> A || B )"
     , read "(H9, (A -> C) -> ((B -> C) -> (A || B -> C)) )"

     , read "(H10, (A <-> B) -> (A -> B) )" 
     , read "(H11, (A <-> B) -> (B -> A) )"
     , read "(H12, (A -> B) -> ((B -> A) -> (A <-> B)) )"

     , read "(H13, (A -> B) -> (not B -> not A) )"
     , read "(H14, A -> not (not A) )"
     , read "(H15, not (not A) -> A )"
     ]                                                





