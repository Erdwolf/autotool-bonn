module TRS.Symbol where

-- -- $Id$

import Sets

import ToDoc
import Reader

import Parsec
import ParsecToken
import ParsecLanguage

data Symbol = Symbol { name :: String
		     }
     deriving ( Eq, Ord )

seems_variable :: Symbol -> Bool
seems_variable s = case name s of
    [ c ] | c >= 'r' -> True
    _                -> False
   

type Signature = Set Symbol

instance ToDoc Symbol where 
    toDoc = text . name

instance Reader Symbol where 
    readerPrec p = do
        i <- identifier haskell
	return $ Symbol { name = i }

instance Show Symbol where show = render . toDoc
instance Read Symbol where readsPrec = parsec_readsPrec
