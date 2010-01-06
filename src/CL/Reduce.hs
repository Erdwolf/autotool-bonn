module CL.Reduce where

import CL.Term
import CL.Combinator
import CL.Substitution

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
        sub = M.fromList $ zip ( arguments c ) pre
    let s' = apply sub $ result c
    return $ unspine $ s' : post

