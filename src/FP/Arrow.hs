{-# OPTIONS -fno-monomorphism-restriction -fglasgow-exts #-}

module FP.Arrow 

( Arrow (..)
, parse_arrow
, name
)

where


import Autolib.TES.Identifier
import Autolib.TES.Term

import Autolib.ToDoc
import Autolib.Reader

data Arrow at = Arrow { unArrow :: Term Identifier at }
     deriving ( Eq, Ord )

instance ( Symbol at ) => ToDoc ( Arrow at ) where
    toDocPrec p ( Arrow ( Var v ) ) = toDoc v
    toDocPrec p ( Arrow ( Node fun args ) ) = case ( show fun, args ) of
        ( "[]", [arg] ) -> brackets $ toDoc $ Arrow arg
	( "->", [ from, to ] ) -> docParen ( p > 0 ) $
            hsep [ toDocPrec 1 $ Arrow from
		 , text "->" 
		 , toDocPrec 0 $ Arrow to 
		 ]
	( "()" , args ) -> parens 
	         $ hsep $ punctuate comma $ map ( toDoc . Arrow ) args
	_ -> docParen ( p > 1 && not ( null args ) ) 
	     $ toDoc fun <+> hsep ( map (toDocPrec 2 . Arrow) args )

instance Reader ( Arrow Identifier ) where
    reader = parse_arrow  []

parse_arrow vars = do 
    t <- arrow vars
    return $ Arrow t

arrow vars = do 
        let parrow = do string "->" ; my_whiteSpace
        xs <- Autolib.Reader.sepBy1 ( atomic vars ) parrow
        let barrow from to = Node ( mkunary "->" ) [ from, to ]
	return $ foldr barrow ( last xs ) ( init xs )

atomic vars = tuple vars <|> list vars <|> application vars

tuple vars = my_parens $ do
    args <- Autolib.Reader.sepBy ( arrow vars ) my_comma
    return $ case args of
        [ arg ] -> arg
	_ -> Node ( mkunary "()" ) args
	
list vars = my_brackets $ do
    arg <- arrow vars
    return $ Node ( mkunary "[]" ) [ arg ]

application vars = do
    fun <- name
    args <- many ( basic vars )
    mkapp vars fun args

mkapp vars fun args = 
    case ( fun `elem` vars, args ) of
        ( False, _ ) -> return $ Node fun args
	( True, [] ) -> return $ Var fun
	( True, _  ) -> fail "Typvariable mit Argument(en)"

basic vars = tuple vars 
      <|> do n <- name; mkapp vars n [] 

name = do
    c <- letter
    cs <- many alphaNum
    my_whiteSpace
    return $ mkunary $ c : cs
