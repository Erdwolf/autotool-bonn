-- | Boolean expressions, in Java\/C syntax

module Boolean.Data 

( Op, Exp, Identifier
, module Autolib.TES.Term
, module Autolib.TES.Position
)


where

--  $Id$

import Boolean.Op

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Identifier
import Autolib.TES.In

import Autolib.Reader

type Exp = Term Identifier ( Op Bool )

instance Reader Exp where
    readerPrec p = treader $ Config { reserved_symbols = ops
				    , allow_new_symbols = False
				    }

instance Read Exp where
    readsPrec = parsec_readsPrec

