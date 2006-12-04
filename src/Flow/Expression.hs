{-# OPTIONS -fglasgow-exts #-}

module Flow.Expression where

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

-----------------------------------------------------------------------

data Expression = Expression Bool Identifier
    deriving ( Eq, Ord, Typeable )  

instance ToDoc Expression where
    toDoc ( Expression True  this ) = toDoc this
    toDoc ( Expression False this ) = text "!" <+> toDoc this

instance Reader Expression where
    reader = do
        flag <- ( do my_reserved "!" ; return False ) <|> return True
	c <- reader
	return $ Expression flag c
