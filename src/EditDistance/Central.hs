{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module EditDistance.Central where

import EditDistance.Data
import EditDistance.CalculateTable (table, miscalculations)

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, Doc(..), text, vcat, hcat, ($$),  (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>), empty)
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Data.List (zip5, transpose, intersperse)

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
                    , text ""
                    , text ""
                    , nest 3 $ text (show s)
                    , text ""
                    , text "und"
                    , text ""
                    , text ""
                    , nest 3 $ text (show t) <> text "."
                    , text ""
                    , text "Als Fehler zählen hierbei" <+> case et of
                                                              WrongNumbers    -> text "falsche Einträge in der Tabelle. Folgefehler werden nicht begünstigend berücksichtigt."
                                                              Miscalculations -> text "Einträge, die falsch berechnet wurden (unter Berücksichtigung von Folgefehlern)."
                    ]

      when (feedback == None) $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
                      ]

    initial p (Config _ _ s t) =
        let n = length s
            m = length t
        in Solution (s,t) [ [ if i == n then m-j else if j == m then n-i else 0 | j <- [0..m] ] |  i <- [0..n] ]

    total p (Config feedback et s t) (Solution _ dt1) = do
       let dt2 = table s t
           errors =
             case et of
               WrongNumbers    -> [ (i,j) | (i, row1, row2) <- zip3 [0..] dt1 dt2, (j, x1, x2) <- zip3 [0..] row1 row2, x1 /= x2 ]

               Miscalculations -> miscalculations dt1 s t

           numberOfErrors = length errors

       when (dimensions dt1 /= dimensions dt2) $ do
          reject $ vcat [ text "Nein. Die eingegebene Tabelle hat die falschen Dimensionen."
                        , text ""
                        , text "Füllen Sie nur die Einträge in die vorgegebene Tabelle ein und ändern Sie nicht die Anzahl der Zeilen und Spalten."
                        ]

       if feedback == None
        then do
           inform $ vcat [ text "Nicht geprüft."
                         , text ""
                         , text "Die Einsendung wird von Ihrem Tutor bewertet."
                         , text ""
                         , text "Ignorieren Sie die unten angezeigte Bewertung. "
                         ]
        else if numberOfErrors == 0
          then inform $ text "Ja, Ihre Einsendung ist richtig."
          else reject $ case feedback of
                          WrongEntries ->
                             let wrong doc = text "<strike style='color: red;'>" <> doc <> text "</strike>"
                             in
                                vcat [ text $ case et of
                                                WrongNumbers ->
                                                  case numberOfErrors of
                                                    1 -> "Nein. Der folgend markierte Eintrag ist falsch:"
                                                    _ -> "Nein. Die folgend markierten " ++ show numberOfErrors ++ " Einträge (inklusive Folgefehlern) sind falsch:"
                                                Miscalculations ->
                                                  case numberOfErrors of
                                                    1 -> "Nein. Der folgend markierte Eintrag ist falsch berechnet worden (unter Berücksichtigung von Folgefehlern):"
                                                    _ -> "Nein. Die folgend markierten " ++ show numberOfErrors ++ " Einträge sind falsch berechnet worden (unter Berücksichtigung von Folgefehlern):"
                                     , text ""
                                     , text ""
                                     --, vcat (zipWith (<+>) (text "[": repeat (text ",")) $ map toDoc (transpose xss)) $$ text "]"
                                     , nest 4 $ vcat (zipWith (<+>) (text "[": repeat (text ","))
                                                                    (zipWith (<+>) [ list [ if (i,j) `elem` errors then wrong (toDoc x) else toDoc x  | (i,x) <- zip [0..] row ] | (j,row) <- zip [0..] $ transpose dt1 ]
                                                                                   ([ text "--" <+> text [chr] | chr <- t ] ++ repeat empty)))
                                                $$ (text "]-- " <> hcat (intersperse (text "   ") [ text [chr] | chr <- s ]))
                                     ]
                          NumberOfErrorsWithCutoffAt e | numberOfErrors > e ->
                             case et of
                               WrongNumbers ->
                                 case e of
                                   0 -> text $ "Nein. Es sind noch Einträge falsch."
                                   1 -> text $ "Nein. Es ist noch mehr als ein Eintrag falsch. Folgefehler werden nicht begünstigend berücksichtigt."
                                   _ -> text $ "Nein. Es sind noch mehr als " ++ show e ++ " Einträge falsch. Folgefehler werden nicht begünstigend berücksichtigt."
                               Miscalculations ->
                                 case e of
                                   0 -> text $ "Nein. Es sind noch Einträge falsch berechnet worden."
                                   1 -> text $ "Nein. Es ist noch mehr als ein Eintrag falsch berechnet worden."
                                   _ -> text $ "Nein. Es sind noch mehr als " ++ show e ++ " Einträge falsch berechnet worden."

                          NumberOfErrorsWithCutoffAt _ ->
                             case et of
                               WrongNumbers ->
                                 case numberOfErrors of
                                   1 -> text $ "Nein. Es ist noch ein Eintrag falsch."
                                   _ -> text $ "Nein. Es sind noch " ++ show numberOfErrors ++ " Einträge falsch. Folgefehler werden nicht begünstigend berücksichtigt."
                               Miscalculations ->
                                 case numberOfErrors of
                                   1 -> text $ "Nein. Es ist noch ein Eintrag falsch berechnet worden (unter Abzug von eventuellen Folgefehlern)."
                                   _ -> text $ "Nein. Es sind noch " ++ show numberOfErrors ++ " Einträge falsch berechnet worden (unter Abzug von eventuellen Folgefehlern)."

list :: [Doc] -> Doc
list docs = text "[" <+> joinWith (text " , ") docs <+> text "]"

joinWith :: Doc -> [Doc] -> Doc
joinWith sep = hcat . intersperse sep

dimensions :: [[a]] -> [Int]
dimensions = map length
