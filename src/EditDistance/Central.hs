{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module EditDistance.Central where

import EditDistance.Data
import EditDistance.CalculateTable (table)

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
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

make_fixed = direct EditDistance $ Config NumberOfErrors 0 "acbda" "abcbcba"

instance OrderScore EditDistance where
    scoringOrder h = Increasing

instance Verify EditDistance Config where
    verify _ cfg = do
        return ()




instance Partial EditDistance Config Solution where
    report p (Config feedback e s t) = do
      inform $ vcat [ text "Berechnen Sie die Tabelle der Edit-Distanzen d_ij für die Zeichenfolgen"
                    , nest 3 $ text (show s)
                    , text "und"
                    , nest 3 $ text (show t) <> text "."
                    ]

      when (e > 0) $ do
        inform $ vcat [ text ""
                      , text "Sie dürfen hierbei maximal" <+> text (show e) <+> text "Fehler machen, damit die Lösung noch als korrekt gewertet wird."
                      ]

      when (feedback == None) $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt sein muss."
                      ]

    initial p (Config _ _ s t) =
        let n = length s
            m = length t
        in Solution [ [ if i == n then m-j else if j == m then n-i else 0 | j <- [0..m] ] |  i <- [0..n] ]

    total p (Config feedback e s t) (Solution dt1) = do
       let dt2 = table s t
           wrongEntries = [ (i,j) | (i, row1, row2) <- zip3 [0..] dt1 dt2, (j, x1, x2) <- zip3 [0..] row1 row2, x1 /= x2 ]
           numberOfWrongEntries = length wrongEntries

       when (dimensions dt1 /= dimensions dt2) $ do
          reject $ text "Nein. Die eingegebene Matrix hat die falschen Dimensionen. Füllen sie  Sie nur die Einträge in die vorgegebenen Matrix ein und ändern Sie nicht die Anzahl der Reihen und Spalten."

       if feedback == None
          then inform $ text "Ja."
          else if (numberOfWrongEntries > e)
                  then reject $ case e of
                                    0 -> text "Nein."
                                    1 -> text "Nein. Es ist mehr als ein Eintrag falsch." 
                                    _ -> text "Nein. Es sind mehr als" <+> text (show e) <+> text "Einträge falsch."
                  else do inform $ text "Ja."
                          unless (numberOfWrongEntries == 0) $ do
                            inform $ text ""
                            inform $ case feedback of
                                       WrongEntries ->
                                          vcat [ text $ "Die Einträge mit folgenden Indizes sind aber noch falsch (insgesamt " ++ show numberOfWrongEntries ++ " von " ++ show e ++ " erlaubten)"
                                               , nest 3 $ vcat $ map (text . show) wrongEntries
                                               ]
                                       NumberOfErrors ->
                                          case numberOfWrongEntries of
                                            1 -> text $ "Es ist aber noch ein Eintrag falsch. Bis zu " ++ show e ++ " sind erlaubt."
                                            _ -> text $ "Es sind aber noch " ++ show numberOfWrongEntries ++ " Einträge falsch. Bis zu " ++ show e ++ " sind erlaubt."


dimensions :: [[a]] -> [Int]
dimensions = map length
