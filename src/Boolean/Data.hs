-- | Boolean expressions, in Java/C syntax

module Boolean.Data 

( Op, Exp, Identifier
, module TES.Term
, module TES.Position
)


where

--  $Id$

import Boolean.Op

import TES.Term
import TES.Position
import TES.Identifier
import TES.In

import Reader

type Exp = Term Identifier Op

instance Reader Exp where
    readerPrec p = treader ops False

instance Read Exp where
    readsPrec = parsec_readsPrec

