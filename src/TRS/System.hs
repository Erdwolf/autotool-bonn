module TRS.System 

( module TRS.System
, module TRS.Term
)

where

-- $Id$

import TRS.Term

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec

import Sets

type System a = [ Rule a ]

data Rule a = Rule { variables :: Set a 
		   , lhs :: Term a
		   , rhs :: Term a
		   }

instance Reader a => Reader ( Rule a ) where
    readerPrec p = do
        l <- readerPrec 0
	reservedOp tes "->"
	r <- readerPrec 0
	return $ Rule { variables = emptySet 
		      , lhs = l
		      , rhs = r
		      }

instance Reader a => Read ( Rule a ) where
    readsPrec = parsec_readsPrec

instance ToDoc a => ToDoc ( Rule a ) where
    toDoc rule = hsep [ toDoc $ lhs rule, text "->", toDoc $ rhs rule ]

instance ToDoc a => Show ( Rule a ) where
    show = render . toDoc



