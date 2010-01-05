module CL.Reduce where

import CL.Term
import CL.Combinator

import Data.Maybe
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad ( guard )

step :: Map Identifier Combinator
     -> Term -> [ Term ]
step coms t = here coms t ++ do
    ( con, s ) <- drop 1 $ positions t
    s' <- step coms s
    return $ con s'
   
here coms t = do
    Sym x : xs <- return $ spine t
    c <- maybeToList $ M.lookup x coms
    guard $ arity c <= length xs
    let ( pre, post ) = splitAt ( arity c ) xs
    let s' = expand pre $ result c
    return $ unspine $ s' : post

expand pre t = case t of
    App l r -> App ( expand pre l ) ( expand pre r )
    Var i   -> pre !! ( fromIntegral i - 1 )
    _       -> t
