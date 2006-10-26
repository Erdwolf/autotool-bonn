{-# OPTIONS -fglasgow-exts #-}

-- | Registermaschine:
-- | * Beliebig viele Register 
-- | * Beliebig große Zahlen
-- | * Operationen: Add, Sub, While, Conc
-- |
-- | Semantik:
-- | * Add n       => R[n] = R[n] + 1 ;
-- | * Sub n       => if (R[n]>0) then R[n] = R[n]-1 ;
-- | * While n p   => while (R[n]>0) { p; }
-- | * Conc []     => nop
-- | * Conc (p:ps) => p; Conc ps

module RM.Type 

( Register
, Program (..)
, regs
, max_reg
)

where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import Text.ParserCombinators.Parsec

-------------------------------------------------------------------------------

type Register = Integer

data Program = Add Register
	     | Sub Register
	     | While Register Program
	     | Conc [ Program ]
	       deriving ( Eq , Ord , Typeable )

{-! for Program derive: Haskell2Xml !-}

measure :: Program -> Int
measure (While _ p) = succ $ measure p
measure (Conc ps  ) = sum $ map measure ps
measure _           = 1

instance Size Program where
    size p = cardinality (regs p) * measure p

instance ToDoc Program where
    toDoc (Add n)     = text "a" <> toDoc n
    toDoc (Sub n)     = text "s" <> toDoc n
    toDoc (While n p) = parens ( toDoc p ) <+> toDoc n 
    toDoc (Conc ps)   = fsep $ map toDoc ps

-- | liste aller benutzten register
regs :: Program -> Set Register
regs (Add n)     = unitSet n
regs (Sub n)     = unitSet n
regs (While n p) = addToSet (regs p) n
regs (Conc ps)   = unionManySets $ map regs ps

-- | maximal benutztes register
max_reg :: Program -> Register
max_reg = maximum . setToList . regs

-------------------------------------------------------------------------------

instance Reader Program where atomic_reader = program

atom :: Parser Program
atom =     (satisfy (=='a') >> reader >>= return . Add)
       <|> (satisfy (=='s') >> reader >>= return . Sub)

while :: Parser Program
while = do p <- my_parens program ; n <- reader ; return $ While n p

program :: Parser Program
program = many1 (atom <|> while) >>= return . Conc

-- local variables:
-- mode: haskell
-- end;
