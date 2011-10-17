{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections #-}

module Syntax.Central where

import Syntax.Checker (check)
import Syntax.Printer (ascii)
import Syntax.Data
import Syntax.Syntax

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Data.List (nub)

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data Syntax = Syntax deriving Typeable

$(derives [makeReader, makeToDoc] [''Syntax])

make_fixed = direct Syntax $ Config 3
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
    describe p (Config n lang) =
      vcat [ hsep [ text "Geben Sie", text (show n), text "Wörter dieser Sprache (mit Startsymbol \"" <> text (fst (head lang)) <> text "\") an:"]
           , text ""
           , vcat [ vcat [ text symbol <> text ":", nest 4 $ vcat $ map text $ ascii graph ] | (symbol,graph) <- lang ]
           ]

    initial p _ = Solution []

    total p (Config n lang) (Solution words) = do
       when (length words /= n) $ do
         reject $ hsep [ text "Nein, es ist nach genau", text (show n), text "Wörtern gefragt." ]
       when (length (nub words) /= n) $ do
         reject $ hsep [ text "Nein, die", text (show n), text "Wörter müssen unterschiedlich sein." ]
       unless (all (check lang) words) $ do
         reject $ text $ "Nein, eines oder mehrere der angegebenen Wörter sind nicht in der Sprache enthalten."
       inform $ text $ "Ja."
