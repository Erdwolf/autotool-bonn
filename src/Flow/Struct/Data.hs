{-# OPTIONS -fglasgow-exts #-}

module Flow.Struct.Data where

import Flow.Program
import Flow.Expression
import Flow.Conditions

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set as S

data Statement
    = Atomic Identifier
    | Block [ Statement ]
    | Branch Expression Statement ( Maybe Statement )
    | While Expression Statement 
    deriving ( Eq, Ord, Typeable )

example :: Program Statement
example = read "while (f) { o; o; }"

instance Conditions Statement where
    conditions s = case s of
        Atomic {} -> S.empty
        Block ss -> conditions ss
        Branch t s1 ms2 -> 
            S.unions [ conditions t
                     , conditions s1
                     , case ms2 of 
                         Nothing -> S.empty
                         Just s2 -> conditions s2
                     ]
        While t s -> S.union ( conditions t ) 
                           ( conditions s )


instance Size Statement where
    size st = case st of
        Atomic _ -> 1
	Block sts -> sum $ map size sts
	Branch _ yes Nothing -> 1 + size yes
	Branch _ yes ( Just no) -> 1 + size yes + size no
	While _ st -> 1 + size st

instance ToDoc Statement where
    toDoc s = case s of
        Atomic action -> toDoc action <> semi
	Block  stmts  -> braces $ vcat $ map toDoc stmts
	Branch c yes mno -> 
            vcat [ text "if" <+> parens ( toDoc c )
	         , nest 4 $ toDoc yes
		 , case mno of
			Nothing -> empty
			Just no -> vcat
		            [ text "else"
			    , nest 4 $ toDoc no
			    ]
		 ]

	While c body -> 
	    vcat [ text "while" <+> parens ( toDoc c )
		 , nest 4 $ toDoc body
		 ]

instance Reader Statement where
    reader = block
 	<|> branch
	<|> while
	<|> atomic

block = my_braces $ do 
    xs <- many reader
    return $ Block xs 

atomic = do
    at <- reader
    my_semi
    return $ Atomic at

branch = do
    my_reserved "if"
    c <- my_parens reader
    yes <- reader
    mno <- ( do my_reserved "else" ; no <- reader ; return $ Just no )
       <|> return Nothing
    return $ Branch c yes mno
    
while = do
    my_reserved "while"
    c <- my_parens reader
    body <- reader
    return $ While c body


