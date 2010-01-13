module Prolog.Substitution where

import Prolog.Data

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad ( guard )


type Substitution = Map Identifier Term

apply :: Substitution -> Term -> Term
apply sub t = case t of
    Variable v -> case M.lookup v sub of
        Just t' -> t'
        Nothing -> t
    Apply f xs -> Apply f $ map ( apply sub ) xs

times :: Substitution 
      -> Substitution 
      -> Substitution
times s t = M.unions  
    [ chained s t
    , s `without` M.keysSet t
    , t `without` M.keysSet s
    ]

f `without` ks = 
    M.filterWithKey ( \ k v -> S.notMember k ks ) f

chained s t = M.fromList $ do
    ( k, v ) <- M.toList s
    let w = apply t v
    guard $ v /= Variable k
    return ( k, w )
