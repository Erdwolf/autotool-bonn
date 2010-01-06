module CL.Reader where

import CL.Data

import Autolib.Reader

instance Reader Identifier where
    reader = do s <- my_identifier ; return $ Identifier s

instance Reader Term where
    reader = do xs <- many1 atomic ; return $ unspine xs

atomic = fmap Sym reader 
    <|>  my_parens reader

