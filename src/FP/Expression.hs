module FP.Expression 

( Expression (..)
)

where

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

data Expression at = Atomic at
		| Apply { fun :: Expression at
			, arg :: Expression at 
			}
     deriving ( Eq, Ord )

spine :: Expression at -> ( at, [ Expression at ] )
spine x = case x of
    Atomic at -> ( at, [] )
    Apply {}  -> 
        let ( f, args ) = spine ( fun x )
	in  ( f, args ++ [ arg x ] )

unspine ::  ( at, [ Expression at ] ) -> Expression at
unspine ( f, args ) = foldl Apply ( Atomic f ) args

instance ToDoc at => ToDoc ( Expression at ) where
    toDocPrec p ( Atomic at ) = toDocPrec p at
    toDocPrec p ( x @ Apply {} ) = docParen ( p > 1 ) 
        $ fsep [ toDoc ( fun x ) , toDocPrec 2 ( arg x ) ]

instance Reader at => Reader ( Expression at ) where
    reader = do 
        f <- reader
	args <- many atomic
	return $ unspine ( f, args )

atomic :: Reader at => Parser ( Expression at )
atomic = my_parens reader 
     <|> do at <- reader ; return $ Atomic at

