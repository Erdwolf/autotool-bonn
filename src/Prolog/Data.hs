{-# LANGUAGE DeriveDataTypeable #-}

module Prolog.Data where

import qualified Autolib.Reader as R
import qualified Autolib.ToDoc as T

import Autolib.Util.Size

import Data.Char

import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable

data Identifier = Identifier { name :: String }
    deriving ( Eq, Ord, Typeable )

instance T.ToDoc Identifier where
    toDoc ( Identifier i ) = T.text i
instance R.Reader Identifier where
    reader = do
        i <- R.my_identifier
        return $ Identifier i

data Term = Variable Identifier
          | Apply Identifier [ Term ]
    deriving ( Eq, Typeable )

instance Size Term where 
    size t = case t of
        Apply f xs -> succ $ sum $ map size xs
        _ -> 1

type Position = [Int]

leaf_positions :: Term -> [ Position ]
leaf_positions t = case t of
    Apply f xs | not ( null xs ) -> do
        ( k, x ) <- zip [ 0.. ] xs
        p <- leaf_positions x
        return $ k : p
    _ -> [ [] ]


positions :: Term -> [ Position ]
positions t = [] : case t of
    Apply f xs -> do
        ( k, x ) <- zip [ 0.. ] xs
        p <- positions x
        return $ k : p
    _ -> []

subterms :: Term -> [Term]
subterms t = do p <- positions  t ; return $ peek t p

function_symbols :: Term -> [ Identifier ]
function_symbols t = do
    Apply f _ <- subterms t
    return f

peek :: Term -> Position -> Term
peek t [] = t
peek (Apply f xs) (p:ps) = peek ( xs !! p) ps

poke  :: Term -> Position -> Term -> Term
poke t [] s = s
poke (Apply f xs) (p:ps) s = 
    let ( pre, x: post) = splitAt p xs
    in  Apply f $ pre ++ poke ( xs !! p ) ps s : post


type Terms = [ Term ]

variables :: Term -> Set Identifier
variables t = case t of
    Variable v -> S.singleton v
    Apply f xs -> S.unions $ map variables xs

varmap :: ( Identifier -> Identifier )
       -> ( Term -> Term )
varmap sub t = case t of
    Variable v -> Variable $ sub v
    Apply f xs -> Apply f ( map ( varmap sub ) xs )


instance T.ToDoc Term where
    toDoc t = case t of
        Variable v -> T.toDoc v
        Apply f [] -> T.toDoc f
        Apply f xs -> T.toDoc f 
            T.<+> T.parens ( T.fsep $ T.punctuate T.comma $ map T.toDoc xs )

instance R.Reader Term where
    reader = do
        f <- R.reader
        if isUpper $ head $ name f
           then return $ Variable f
           else do
               xs <- R.option [] $ R.my_parens $ R.my_commaSep $ R.reader
               return $ Apply f xs

