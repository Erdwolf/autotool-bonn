{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module EditDistance.Central where

import EditDistance.Data
import EditDistance.CalculateTable (table)

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data EditDistance = EditDistance deriving Typeable

$(derives [makeReader, makeToDoc] [''EditDistance])

make_fixed = direct EditDistance $ Config NumberOfErrors "acbda" "abcbcba"

instance OrderScore EditDistance where
    scoringOrder h = Increasing

instance Verify EditDistance Config where
    verify _ cfg = do
        return ()




instance Partial EditDistance Config Solution where
    report p (Config feedback s t) = do
      inform $ vcat [ text "Berechnen Sie die Tabelle der Edit-Distanzen d_ij für die Zeichenfolgen"
                    , nest 3 $ text (show s)
                    , text "und"
                    , nest 3 $ text (show t) <> "."
                    ]

      when (feedback == None) $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt sein muss."
                      ]

    initial p (Config _ _ s t) =
        let n = length s
            m = length t
        in Solution [ [ if i == n then j else if j == m then i else 0 | j <- [0..m] ] |  i <- [0..n] ]

    total p (Config feedback e s t) (Solution dt1) = do
       when (feedback /= None) $ do
         let dt2 = table s t
             wrongEntries = [ (i,j) | (i, row1, row2) <- zip3 [0..] dt1 dt2, (j, x1, x1) <- zip3 [0..] row1 row2, x1 /= x2 ]
         unless (null wrongEntries) $ do
            case feedback of
                WrongEntries -> do
                    reject $ vcat $ [ text "Nein."
                                    , text ""
                                    , text "Die folgenden Einträge sind falsch:"
                                    , nest 3 $ vcat $ map (text . show) wrongEntries
                                    ]
                NumberOfErrors -> do
                    reject $ text $ "Nein, es sind " ++ show (length wrongEntries) ++ " Einträge falsch."

       inform $ text "Ja."
