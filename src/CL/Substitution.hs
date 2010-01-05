module CL.Substitution where

import CL.Term

import Autolib.FiniteMap
import Autolib.Set

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

type Substitution = FiniteMap Identifier Term

dom :: Substitution -> Set Identifier
dom = M.keysSet

apply :: Substitution -> Term -> Term
apply s t = case t of
    App l r -> App ( apply s l ) ( apply s r )
    Sym y -> case M.lookup y s of
        Nothing -> t
        Just t' -> t'
    Var {} -> t
