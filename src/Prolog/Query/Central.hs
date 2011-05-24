{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances #-}

module Prolog.Query.Central where

import Prolog.Query.Prolog
import Prolog.Query.Data

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)


data Prolog_Query = Prolog_Query deriving Typeable

$(derives [makeReader, makeToDoc] [''Prolog_Query])


instance OrderScore Prolog_Query where
    scoringOrder h = Increasing

instance Verify Prolog_Query Config where
    verify _ (Config _ _) = do
        -- TODO Do some verification.
        return ()

instance Partial Prolog_Query Config Query where
    describe p (Config _ facts) = vcat
        [ text "Gegeben ist folgende Faktenbasis:"
        , nest 4 $ text facts
        , text "Formulieren Sie ein Query, das die auf dem Aufgabenzettel gewünschte Ausgabe liefert. (TODO Text sollte auch noch konfigurierbar sein)"
        ]
    initial p _ = Query "?- parent(bob,D), female(D)."

    partial p _ _ = do
        inform $ text "Keine Überprüfung."

    total p (Config answer facts) (Query query) = do
        let solution = map (\s -> Struct s []) answer :: [Term]
        if solution  == concatMap (map snd) (solveString facts query)
           then inform $ text $ "Ja."
           else reject $ text $ "Nein."


make_fixed = direct Prolog_Query $ Config ["mary","lisa"] $ concat
   [ "female(mary)."
   , "female(sandra)."
   , "female(juliet)."
   , "female(lisa)."
   , "male(peter)."
   , "male(paul)."
   , "male(dick)."
   , "male(bob)."
   , "male(harry)."
   , "male(luke)."
   , "parent(lisa, harry)."
   , "parent(peter, harry)."
   , "parent(bob,mary)."
   , "parent(juliet, paul)."
   , "parent(bob, lisa)."
   , "parent(bob, paul)."
   , "parent(juliet, mary)."
   , "parent(mary, dick)."
   , "parent(mary, sandra)."
   , "parent(juliet, lisa)."
   , "parent(harry, luke)."
   ]
