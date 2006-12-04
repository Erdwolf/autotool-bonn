{-# OPTIONS -fglasgow-exts #-}

module Flow.Goto.Data where

import Flow.Expression
import Flow.Program

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Size

import Text.ParserCombinators.Parsec.Char

import Data.Typeable

example :: Program Statement
example = read "foo : if (a) goto foo;"

---------------------------------------------------------------------------

data Statement 
    = Statement ( Maybe Identifier ) Atomic
    deriving ( Eq, Ord, Typeable )

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

---------------------------------------------------------------------------

data Atomic 
    = Action Identifier
    | Goto Identifier
    | If_Goto Expression Identifier
    deriving ( Eq, Ord, Typeable )

instance ToDoc Atomic where
    toDoc a = case a of
        Action act  -> toDoc act <> semi
    	Goto label -> text "goto" <+> toDoc label <> semi
	If_Goto c label -> 
	    text "if" <+> parens ( toDoc c ) <+> toDoc ( Goto label )

instance Reader Atomic where
    reader 
        =   do my_reserved "goto" ; t <- reader ; my_semi 
	       return $ Goto t
	<|> do my_reserved "if" ; c <- my_parens reader 
	       my_reserved "goto" ; t <- reader ; my_semi 
	       return $ If_Goto c t
	<|> do act <- reader ; my_semi
	       return $ Action act



