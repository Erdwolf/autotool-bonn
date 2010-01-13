module Prolog.Unify where

import Prolog.Data
import Prolog.Substitution

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad ( guard )

mgu :: Term -> Term -> Maybe Substitution
mgu s t | s == t = 
    return $ M.empty
mgu ( Variable v ) t = do
    guard $ not $ S.member v $ variables t 
    return $ M.fromList [ (v, t) ]
mgu t ( Variable v ) = mgu ( Variable v ) t
mgu ( Apply f xs ) ( Apply g ys ) = do
    guard $ f == g
    mgu_for_list xs ys

mgu_for_list [] [] = return M.empty
mgu_for_list (x:xs) (y:ys) = do
    sub1 <- mgu_for_list xs ys
    sub2 <- mgu ( apply sub1 x ) ( apply sub1 y )
    return $ times sub1 sub2


test1 = mgu ( read "f(a,X)" ) (read "f(Y,b)" )
test2 = mgu ( read "f(a,X)" ) (read "f(Y,a)" )
test3 = mgu ( read "f(a,X)" ) (read "f(X,b)" )
    
