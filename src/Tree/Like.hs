{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Tree.Like where

type Position = [ Int ]

class Class t a | t -> a  where
   children :: t -> [t]
   label :: t -> a
   build :: a -> [t] -> t

-------------------------------------------------------------------

arity :: Class t a => t -> Int
arity = length . children

positions :: Class t a => t -> [ Position ]
positions t = [] : do
    (p, s) <- zip [0..] $ children t
    ps <- positions s
    return $ p : ps

peek :: Class t a => t -> Position -> t
peek t [] = t
peek t (p : ps) = peek ( children t !! p ) ps

poke :: Class t a => t -> Position -> t -> t
poke t [] s = s
poke t (p : ps) s = 
    let ( pre, this : post ) = splitAt p $ children t
    in  build ( label t ) $ pre ++ poke this ps s : post

-------------------------------------------------------------

smaller :: Class t a => t -> [t]
smaller t = do
    p <- positions t
    s <- children $ peek t p 
    return $ poke t p s

