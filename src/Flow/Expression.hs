{-# OPTIONS -fglasgow-exts #-}

module Flow.Expression where

import Flow.Conditions
import Flow.State

import Autolib.TES.Identifier
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Map ( Map )
import qualified Data.Map as M

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

-----------------------------------------------------

data Expression = Expression Bool Identifier
    deriving ( Eq, Ord, Typeable )  

instance ToDoc Expression where
    toDoc ( Expression True  this ) = 
        toDoc this
    toDoc ( Expression False this ) = 
        text "!" <+> toDoc this

instance Reader Expression where
    reader = do
        flag <-  ( do my_reserved "!" ; return False ) 
             <|> return True
	c <- reader
	return $ Expression flag c

instance Conditions Expression where
    conditions ( Expression flag id ) = 
        S.fromList [ id ]

evaluate :: State -> Expression -> Bool
evaluate s x = case x of
    Expression flag id -> 
         case M.lookup id s of
             Just v -> flag == v
