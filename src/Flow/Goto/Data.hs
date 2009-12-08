{-# OPTIONS -fglasgow-exts #-}

module Flow.Goto.Data where

import Flow.Expression
import Flow.Conditions
import Flow.Program

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Size

import Data.Set ( Set )
import qualified Data.Set as S

import Text.ParserCombinators.Parsec.Char

import Data.Typeable

example :: Program Statement
example = read "foo : if (a) goto foo;"

--------------------------------------------------

data Statement 
    = Statement ( Maybe Identifier ) Atomic
    deriving ( Eq, Ord, Typeable )

instance Conditions Statement where
    conditions ( Statement _ a ) = conditions a

instance Size Statement where size _ = 1

instance ToDoc Statement where
    toDoc ( Statement mlabel action ) = 
        ( case mlabel of
	    Nothing -> empty
	    Just l -> toDoc l <> text ":"
	) <+> toDoc action

instance Reader Statement where
    reader = do
        mlabel <- 
                 try ( do l <- reader ; my_reserved ":" ; return $ Just l )
	     <|> return Nothing
        act <- reader
	return $ Statement mlabel act

----------------------------------------------------

data Atomic 
    = Skip
    | Action Identifier
    | Goto Identifier
    | If_Goto Expression Identifier
    deriving ( Eq, Ord, Typeable )

instance Conditions Atomic where
    conditions a = case a of
        If_Goto test goal -> conditions test
        _ -> S.empty

instance ToDoc Atomic where
    toDoc a = case a of
        Skip -> text "skip" <> semi
        Action act  -> toDoc act <> semi
    	Goto label -> text "goto" <+> toDoc label <> semi
	If_Goto c label -> 
	    text "if" <+> parens ( toDoc c ) <+> toDoc ( Goto label )

instance Reader Atomic where
    reader 
        =   do my_reserved "skip" ; my_semi ; return Skip

        <|> do my_reserved "goto" ; t <- reader ; my_semi 
	       return $ Goto t
	<|> do my_reserved "if" ; c <- my_parens reader 
	       my_reserved "goto" ; t <- reader ; my_semi 
	       return $ If_Goto c t
	<|> do act <- reader ; my_semi
	       return $ Action act



