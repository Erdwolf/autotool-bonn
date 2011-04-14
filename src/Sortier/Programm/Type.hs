{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Sortier.Programm.Type where

import Autolib.ToDoc
import qualified Autolib.Reader as R

import Autolib.TES.Identifier
import Autolib.Size

import Data.Typeable

-- | represents simply branching programs
-- to build sorting algorithms.
-- the measure (size) of a program is the maximum depth
-- (number of comparisons on any execution path)
data Statement = Swap Identifier Identifier
	  | If_Greater_Else Identifier Identifier Program Program
    deriving Typeable

data Program = Sequence [ Statement ]
    deriving Typeable

instance Size Program where
    size ( Sequence xs ) = sum $ map size xs

instance Size Statement where
    size s = case s of
        If_Greater_Else x y s t -> 1 + max ( size s ) ( size t )
	_ -> 0

-------------------------------------------------------------------------

instance R.Reader Program where
    reader = do
        ps <- R.many R.reader
        return $ Sequence ps

instance R.Reader Statement where
    reader = swapped R.<|> iffed

swapped = do
    R.my_reserved "swap"
    (x,y) <- R.reader
    R.my_semi
    return $ Swap x y

iffed = do
    R.my_reserved "if"
    (x,y) <- comp
    s <- block
    t <- R.option ( Sequence [] ) $ do
        R.my_reserved "else"
	block
    return $ If_Greater_Else x y s t
    
comp = R.my_parens $ do
    x <- R.reader
    R.my_symbol ">"
    y <- R.reader
    return ( x, y )

block = R.my_braces $ R.reader

-------------------------------------------------------------------------

instance ToDoc Statement where
    toDoc p = case p of
        Swap x y -> 
	    text "swap" <+> toDoc ( x, y ) <> semi
	If_Greater_Else x y s t -> vcat 
	    [ ifgt x y <+> text "{"
	    , nest 4 $ toDoc s
            , case t of
	          Sequence [] -> empty
		  Sequence ps -> vcat
		      [ text "} else {"
		      , nest 4 $ toDoc t
		      ]
	    , text "}"
	    ]

ifgt x y = hsep
     [ text "if" 
     , parens $ hsep [ toDoc x, text ">", toDoc y ]
     ]

instance ToDoc Program where
    toDoc ( Sequence ps ) = vcat $ map toDoc ps
