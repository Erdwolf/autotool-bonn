{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts #-} 

module NFA.Nerode.Separation.Solution where

import Autolib.Reader
import Autolib.ToDoc

import Convert.Input

import Autolib.FiniteMap
import Autolib.Exp
import Autolib.Exp.Example
import Autolib.NFA

import Data.Typeable

import Prelude hiding ( words )

data NFAC c Int => Solution c =
     Solution { language :: Autolib.Exp.RX c
              , proofs :: FiniteMap (Int,Int) [c]
              }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Solution])

example :: [ Char ] -> Solution Char
example w = Solution
        { language = Autolib.Exp.Example.example ( mkSet w )
	, proofs = listToFM 
	        [ ((0,1), reverse $ take 3 $ concat $ replicate 4 w ) ]
        }

-- local variables:
-- mode: haskell
-- end;
