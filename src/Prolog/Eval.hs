module Prolog.Eval ( eval ) where

import Prolog.Data
import qualified Prolog.Program  as P
import Prolog.Unify
import Prolog.Substitution

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Maybe

eval :: P.Program -> [ Substitution ]
eval p = do
    ( sub, _) <- 
        query ( P.clauses p ) 0 ( P.query p )
    return sub

query_apply cs subs ts = do
    ( sub1, u1 ) <- subs
    ( sub2, u2 ) <- 
        query cs u1 $ map ( apply sub1 ) ts 
    return ( times sub1 sub2 , u2 )

query :: [ P.Clause ] 
         -> Int -- ^ for unique naming
         -> Terms
         -> [ (Substitution, Int) ]
query cs u0 [] = do
    return ( M.empty, u0 )
query cs u0 (t : ts) = 
    query_apply cs ( single cs u0 t ) ts

single :: [ P.Clause ] 
         -> Int -- ^ for unique naming
         -> Term
         -> [ (Substitution, Int) ]
single cs u0 t = do
    c0 <- cs
    let c = rename u0 c0
        ufs = do 
             sub <- maybeToList $ mgu t $ P.head c
             return ( sub, u0 + 1 )
    ( sub, u1) <- query_apply cs ufs $ P.body c
    let vs = S.unions $ map variables 
                      $ P.head c : P.body c
    return ( cleanup vs sub, u1 )

rename :: Int -> P.Clause -> P.Clause
rename u c = 
    let f = varmap ( \ n -> "_" ++ n ++ show u ) 
    in  P.Clause { P.head = f $ P.head c
                 , P.body = map f $ P.body c 
                 }

cleanup vs sub = 
    M.filterWithKey ( \ v _ -> S.notMember v vs ) sub


test4 =  take 3
      $ eval 
      $ read "p(a,b). p(b,c). t(X,Z) :- p(X,Z). t(X,Z) :- t(X,Y), t(Y,Z). ?- t(X,Y)."
