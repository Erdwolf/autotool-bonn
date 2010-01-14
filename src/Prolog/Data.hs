{-# language DeriveDataTypeable #-}

module Prolog.Data where

import qualified Autolib.Reader as R
import qualified Autolib.ToDoc as T

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
    deriving ( Eq )

positions :: Term -> [[Int]]
positions t = [] : case t of
    Apply f xs -> do
        ( k, x ) <- zip [ 0.. ] xs
        p <- positions x
        return $ k : p
    _ -> []

poke  :: Term -> [Int] -> Term -> Term
poke t [] s = s
poke (Apply f xs) (p:ps) s = 
    poke ( xs !! p ) ps s


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

