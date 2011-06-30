{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Prolog.Unifier.Central where

import Prolog.Unifier.Data

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.List ((\\), nub, sort, intercalate)
import Data.Generics (everything, mkQ)
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*))
import Control.Arrow (first)
import Control.Monad (guard)

import Prolog.Programming.Prolog (unify_with_occurs_check, Term(Var), VariableName(..), simplify)

data Prolog_Unifier = Prolog_Unifier deriving Typeable

$(derives [makeReader, makeToDoc] [''Prolog_Unifier])

make_fixed = direct Prolog_Unifier $ Config "a(X,Y)" "a(b,c)"


instance OrderScore Prolog_Unifier where
    scoringOrder h = Increasing

instance Verify Prolog_Unifier Config where
    verify _ (Config _ _) = do
        -- TODO Do some verification.
        return ()

instance Partial Prolog_Unifier Config Unifier where
    describe p (Config t1 t2) = text $ intercalate "\n"
        [ "Geben Sie den allgemeinsten Unifikator von"
        , ""
        , "    " ++ show t1
        , ""
        , " und "
        , "    " ++ show t2
        , ""
        , " in der Form"
        , ""
        , "    X = ..."
        , "    Y = ..."
        , "    usw."
        , ""
        , "an."
        ]

    initial p _ = Unifier [] -- [(VariableName 0 "X", "_")]

    total p (Config t1 t2) (Unifier u) = do
        case (unify_with_occurs_check t1 t2 >>= guard . equivalent u) of
           Just () -> inform $ text "Ja."
           Nothing -> reject $ text "Nein."

equivalent u1 u2 =
   length u1 == length u2 &&
   all id (zipWith (==) (canonical u1)
                        (canonical u2))
     where canonical = sort . map sortPair . map (first Var) . simplify

sortPair (x,y) | x <= y = (x,y)
sortPair (x,y)          = (y,x)
