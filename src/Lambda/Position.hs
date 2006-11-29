module Lambda.Position where

import Lambda.Data
import Lambda.IO

import Autolib.ToDoc

type Position = [Int]

positions :: Lambda -> [ Position ]
positions t = [] : case t of
    Apply fun arg -> do
        ( x, s ) <- [(0,fun), (1,arg)]
        p <- positions s
        return $ x : p
    Abstract v b -> do
        p <- positions b
        return $ 0 : p
    _ -> []

peek :: Monad m => Lambda -> Position -> m Lambda
peek t [] = return t
peek ( Apply fun arg ) ( 0 : p ) = peek fun p
peek ( Apply fun arg ) ( 1 : p ) = peek arg p
peek ( Abstract v b  ) ( 0 : p ) = peek b   p
peek t p = fail $ show $ vcat
                [ text "peek: im (Teil-)Term" <+> toDoc t
                , text "gibt es keine Position" <+> toDoc p
                ]

poke :: Monad m => Lambda -> ( Position, Lambda ) -> m Lambda
poke t ( [], a ) = return a
poke ( Apply fun arg ) ( 0 : p , a ) = do
    fun' <- poke fun ( p, a ) 
    return $ Apply fun' arg
poke ( Apply fun arg ) ( 1 : p , a ) = do
    arg' <- poke arg ( p, a ) 
    return $ Apply fun arg'
poke ( Abstract v b  ) ( 0 : p , a ) = do
    b' <- poke b ( p, a ) 
    return $ Abstract v b'
poke t p = fail $ show $ vcat
                [ text "poke: im (Teil-)Term" <+> toDoc t
                , text "gibt es keine Position" <+> toDoc p
                ]
