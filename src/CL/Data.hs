{-# language DeriveDataTypeable #-}

module CL.Data where

import Data.Typeable
import Autolib.Util.Size


data Identifier = Identifier String
    deriving ( Eq, Ord, Typeable )


data Term = App Term Term
          | Sym { unSym :: Identifier }
    deriving ( Eq, Ord, Typeable )

isSym ( Sym {} ) = True
isSym _ = False

instance Size Term where
    size t = case t of
        App l r -> sum $ map size [l,r]
        _ -> 1

positions :: Term -> [ (Term -> Term, Term) ]
positions t = ( id, t ) : case t of
    App l r -> 
           do ( c, s ) <- positions l ; return ( \ s -> App (c s) r , s )
        ++ do ( c, s ) <- positions r ; return ( \ s -> App l (c s) , s )
    _ -> []

subterms :: Term -> [ Term ]
subterms = map snd . positions

spine :: Term -> [ Term ]
spine t = 
    let f t xs = case t of
            App l r ->  f l (r : xs)
            _       ->  t : xs
    in  f t []

unspine :: [ Term ] -> Term
unspine (t : ts) = foldl App t ts

