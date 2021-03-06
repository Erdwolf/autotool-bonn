{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections #-}
module Syntax.Central where

import qualified TextConfig

import Syntax.Checker (check)
import Syntax.Data
import Syntax.Syntax
import Syntax.Generator (terminals)
import Syntax.LaTeX (asImage)

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), (<+>), hcat, hsep, toDoc, nest, ToDoc(..), punctuate)
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Data.List (sort,nub)

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data Syntax = Syntax deriving Typeable

$(derives [makeReader, makeToDoc] [''Syntax])

make_fixed = direct Syntax $ Config True 3
   [ ("S", Fork (Terminal "a")
                (Fork (Symbol "B")
                      Empty))
   , ("B", Fork (Terminal "b" `Chain` Terminal "b")
                Empty)
   ]

instance OrderScore Syntax where
    scoringOrder h = Increasing

instance Verify Syntax Config where
    verify _ cfg = do
        return ()


instance Partial Syntax Config Solution where
    describe p (Config giveFeedback n lang) =
      vcat$[ hsep [ text "Geben Sie genau", text (show n), text "Worte der mit dem folgenden Syntaxdiagramm-System (Startdiagramm ist \"" <> text (fst (head lang)) <> text "\") erzeugbaren Sprache an."]
           , text "Das zu Grunde liegende Alphabet besteht aus den Symbolen " <+> hcat (punctuate (text ", ") $ map text (sort $ nub $ terminals lang))  <+> text "."
           , text ""
           --, vcat [ vcat [ text symbol <> text ":", nest 4 $ vcat $ map text $ ascii graph ] | (symbol,graph) <- lang ]
           , text (asImage lang)
           , text ""
           , text "Ihre Lösung sollte wie folgt aussehen:"
           , text ""
           , text "Solution"
           , nest 3 $ toDoc [ "Wort" ++ show i | i <- [1..n] ]
           , text ""
           ] ++ if giveFeedback then [] else
           [ text ""
           , TextConfig.noFeedbackDisclaimer
           ]

    initial p _ = Solution []

    total p (Config giveFeedback n lang) (Solution words) = do
       when (length words /= n) $ do
         reject $ hsep [ text "Nein, es ist nach genau", text (show n), text "Worten gefragt." ]
       when (length (nub words) /= n) $ do
         reject $ hsep [ text "Nein, die", text (show n), text "Worte müssen unterschiedlich sein." ]
       when giveFeedback $ do
          unless (all (check lang) words) $ do
            reject $ text $ "Nein, eines oder mehrere der angegebenen Worte sind nicht in der Sprache enthalten."
       if giveFeedback
           then inform TextConfig.ok
           else inform TextConfig.noFeedbackResult
