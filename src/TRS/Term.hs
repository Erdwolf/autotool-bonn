module TRS.Term 

( module TRS.Term
, module Data.Tree
)

where

--   $Id$

import Data.Tree

import ToDoc
import Reader

import Parsec
import ParsecToken
import ParsecLanguage

type Term a = Tree a

instance ToDoc a => ToDoc ( Term a ) where
    toDoc ( Node t xs ) = toDoc t <> 
	if null xs 
	then ToDoc.empty 
	else ToDoc.parens $ hcat $ punctuate ToDoc.comma $ map toDoc xs

-- instance ToDoc a => Show ( Term a ) where 
--     show = render . toDoc

instance Reader a => Reader ( Term a ) where
    readerPrec p = do
        t <- readerPrec 0 
	xs <- option [] $ ParsecToken.parens haskell
		        $ commaSep haskell
	                $ readerPrec 0 
	return $ Node t xs

instance Reader a => Read ( Term a ) where
    readsPrec = parsec_readsPrec

