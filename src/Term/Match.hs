module Term.Match where

--   $Id$

import Term.Type

import Data.FiniteMap
import Control.Monad ( guard, mzero )


apply :: Substitution (Term a) -> VTerm a -> Term a
apply sub x = case symbol x of
      Left v -> case lookupFM sub v of
		     Nothing -> error "variable does not occur"
		     Just t  -> t
      Right f -> Term { symbol = f
		      , children = map (apply sub) $ children x 
		      }

applyV :: Substitution (Term a) -> VTerm a -> VTerm a
applyV sub x = case symbol x of
      Left v -> case lookupFM sub v of
		     Nothing -> x
		     Just t  -> fmap Right t
      Right f -> x { children = map (applyV sub) $ children x }

match :: VTerm a -> Term a -> Maybe (Substitution (Term a))
match pat t = case symbol pat of
      Left v -> return $ listToFM [ (v, t) ]
      Right f -> matches  (children pat) (children t)

matches :: [VTerm a] -> [Term a] -> Maybe (Substitution (Term a))
matches [] [] = return emptyFM
matches [] _  = mzero
matches _ []  = mzero
matches (v : vs) (t : ts) = do
    sub  <- match v t
    subs <- matches ( map (applyV sub) vs ) ts
    return $ plusFM_C (error "Term.Rewrite.matches") sub subs

