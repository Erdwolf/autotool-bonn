-- | Boolean expressions, in Java/C syntax

module Boolean.Data where

--  $Id$

import Boolean.Op

import TES.Term
import TES.Symbol
import TES.Identifier

import Reader

type Exp = Term Identifier Op

instance Reader Exp where
    readerPrec p = treader ops False

instance Read Exp where
    readsPrec = parsec_readsPrec
