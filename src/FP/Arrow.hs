{-# LANGUAGE NoMonomorphismRestriction #-}

module FP.Arrow 

( Arrow (..)
, parse_arrow
, name
)

where


import Autolib.TES.Identifier
import Autolib.TES.Term
import Autolib.TES.Position

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Size

import Data.Char ( isLower )

data Arrow at = Arrow { unArrow :: Term Identifier at }
     deriving ( Eq, Ord )

instance Size ( Arrow at ) where size = size . unArrow

instance ( Symbol at ) => ToDoc ( Arrow at ) where
    toDocPrec p ( Arrow ( Var v ) ) = toDoc v
    toDocPrec p ( Arrow ( Node fun args ) ) = case ( show fun, args ) of
        ( "List", [arg] ) -> brackets $ toDoc $ Arrow arg
	( "Arrow", [ from, to ] ) -> docParen ( p > 0 ) $
            hsep [ toDocPrec 1 $ Arrow from
		 , text "->" 
		 , toDocPrec 0 $ Arrow to 
		 ]
	( "Tuple" , args ) -> parens 
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
        let barrow from to = Node ( mkunary "Arrow" ) [ from, to ]
	return $ foldr barrow ( last xs ) ( init xs )

atomic vars = tuple vars <|> list vars <|> application vars

tuple vars = my_parens $ do
    args <- Autolib.Reader.sepBy ( arrow vars ) my_comma
    return $ case args of
        [ arg ] -> arg
	_ -> Node ( mkunary "Tuple" ) args
	
list vars = my_brackets $ do
    arg <- arrow vars
    return $ Node ( mkunary "List" ) [ arg ]

application vars = do
    fun <- bounded_name vars
    args <- many ( basic vars )
    mkapp vars fun args

mkapp vars fun args = 
    case ( fun `elem` vars, args ) of
        ( False, _ ) -> return $ Node fun args
	( True, [] ) -> return $ Var fun
	( True, _  ) -> fail "Typvariable mit Argument(en)"

basic vars = tuple vars 
      <|> do n <- bounded_name vars; mkapp vars n [] 

name = checked_name Nothing
bounded_name vars = checked_name ( Just vars )

checked_name mvars = do
    c <- letter
    cs <- many alphaNum
    let n = mkunary $ c : cs
    case ( isLower c, mvars ) of
      ( True , Just vars ) -> if elem n vars
              then return ()
              else fail $ show $ vcat
                     [ text "nicht gebundene Typvariable:"
                     , nest 4 $ toDoc n
                     , text "gebunden sind hier:"
                     , nest 4 $ toDoc vars
                     ]
      _ -> return ()
    my_whiteSpace
    return n
