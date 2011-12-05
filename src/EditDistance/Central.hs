{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module EditDistance.Central where

import EditDistance.Data
import EditDistance.CalculateTable (table, miscalculations)

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Data.List (zip5)

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data EditDistance = EditDistance deriving Typeable

$(derives [makeReader, makeToDoc] [''EditDistance])

make_fixed = direct EditDistance $ Config (NumberOfErrorsWithCutoffAt 0) WrongNumbers "acbda" "abcbcba"

instance OrderScore EditDistance where
    scoringOrder h = Increasing

instance Verify EditDistance Config where
    verify _ cfg = do
        return ()




instance Partial EditDistance Config Solution where
    report p (Config feedback et s t) = do
      inform $ vcat [ text "Berechnen Sie die Tabelle der Edit-Distanzen d_ij für die Zeichenfolgen"
                    , nest 3 $ text (show s)
                    , text "und"
                    , nest 3 $ text (show t) <> text "."
                    , text ""
                    , text "Als Fehler zählen hierbei" <+> case et of
                                                              WrongNumbers    -> text "falsche Einträge in der Matrix."
                                                              Miscalculations -> text "Einträge, die falsch berechnet wurden (unter Berücksichtung von Folgefehlern)."
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

    total p (Config feedback et s t) (Solution dt1) = do
       let dt2 = table s t
           errors =
             case et of
               WrongNumbers    -> [ (i,j) | (i, row1, row2) <- zip3 [0..] dt1 dt2, (j, x1, x2) <- zip3 [0..] row1 row2, x1 /= x2 ]

               Miscalculations -> miscalculations dt1 s t

           numberOfErrors = length errors

       when (dimensions dt1 /= dimensions dt2) $ do
          reject $ vcat [ text "Nein. Die eingegebene Matrix hat die falschen Dimensionen."
                        , text ""
                        , text "Füllen Sie nur die Einträge in die vorgegebenen Matrix ein und ändern Sie nicht die Anzahl der Reihen und Spalten."
                        ]

       if feedback == None || numberOfErrors == 0
          then inform $ text "Ja."
          else reject $ case feedback of
                          WrongEntries ->
                             vcat [ text $ "Nein. Die Einträge mit folgenden Indizes sind falsch (insgesamt " ++ show numberOfErrors ++ ")"
                                  , nest 3 $ vcat $ map (text . show) errors
                                  ]
                          NumberOfErrorsWithCutoffAt e | numberOfErrors > e ->
                             case et of
                               WrongNumbers ->
                                 case e of
                                   0 -> text $ "Nein. Es ist noch Einträge falsch."
                                   1 -> text $ "Nein. Es ist noch mehr als ein Eintrag falsch."
                                   _ -> text $ "Nein. Es sind noch mehr als " ++ show e ++ " Einträge falsch."
                               Miscalculations ->
                                 case e of
                                   0 -> text $ "Nein. Es ist noch Einträge falsch berechnet worden (unter Berücksichtung von Folgefehlern)."
                                   1 -> text $ "Nein. Es ist noch mehr als ein Eintrag falsch berechnet worden (unter Berücksichtung von Folgefehlern)."
                                   _ -> text $ "Nein. Es sind noch mehr als " ++ show e ++ " Einträge falsch berechnet worden (unter Berücksichtung von Folgefehlern)."

                          NumberOfErrorsWithCutoffAt _ ->
                             case et of
                               WrongNumbers ->
                                 case numberOfErrors of
                                   1 -> text $ "Nein. Es ist noch ein Eintrag falsch."
                                   _ -> text $ "Nein. Es sind noch " ++ show numberOfErrors ++ " Einträge falsch."
                               Miscalculations ->
                                 case numberOfErrors of
                                   1 -> text $ "Nein. Es ist noch ein Eintrag falsch berechnet worden (unter Berücksichtung von Folgefehlern)."
                                   _ -> text $ "Nein. Es sind noch " ++ show numberOfErrors ++ " Einträge falsch berechnet worden (unter Berücksichtung von Folgefehlern)."

dimensions :: [[a]] -> [Int]
dimensions = map length
