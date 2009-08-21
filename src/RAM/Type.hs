{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module RAM.Type where

--   $Id$

import RAM.Builtin

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES.Identifier

import Data.Typeable
import Autolib.Xml

type Var = Identifier

instance Container Identifier String where
    label _ = "Autolib.TES.Identifier"
    pack = show
    unpack = read

data Statement = Inc Var
	 | Dec Var
	 | Loop Var Program
	 | While Var Program
	 | Builtin { name :: Builtin, res :: Var, args :: [ Var ] }
    deriving ( Eq, Ord, Typeable )

type Program = [ Statement ]

instance Size Program where 
    size = sum . map size
instance Size Statement where
    size ( Loop v p ) = succ $ size p
    size ( While v p ) = succ $ size p
    size _ = 1

flatten :: Program -> Program
flatten ps = do
    p <- ps
    p : case p of Loop v q -> flatten q
		  While v q -> flatten q
		  _ -> []

$(derives [makeReader, makeToDoc] [''Statement])

-- Local variables:
-- mode: haskell;
-- end:


