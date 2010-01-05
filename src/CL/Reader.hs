module CL.Reader where

import CL.Data

import Autolib.Reader

instance Reader Identifier where
    reader = do s <- my_identifier ; return $ Identifier s

instance Reader Term where
    reader = do n <- my_integer ; return $ Var n
        <|>  do s <- reader ; return $ Sym s
        <|>  my_parens ( do
                xs <- many reader
                return $ unspine xs )
