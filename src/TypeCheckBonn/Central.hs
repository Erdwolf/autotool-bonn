{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckBonn.Central where

import Type.Data
import Type.Tree
import TypeCheckBonn.Infer
import TypeCheckBonn.Data (Config(..), Feedback(..))

import Autolib.Reporter.Type
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Identifier
import qualified Autolib.TES.In as T

import Data.Typeable
import Inter.Types (OrderScore(scoringOrder),Increasing, Make, direct)
import qualified Challenger as C

import Control.Monad.Writer
import Control.Monad.Error

data TypeCheckBonn = TypeCheckBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheckBonn where
    scoringOrder _ = Increasing



instance C.Partial TypeCheckBonn Config Exp where

    describe p (Config fb goal decls) = vcat $
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc goal
        , text "unter Verwendung von:"
        , text ""
        , text ""
        , nest 4 $ toDoc decls
        , text ""
        ] ++ if fb /= None then [] else
        [ text ""
        , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
        , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
        ]

    initial p i = read "f(a,g(b))"

    total p (Config fb goal decls) b = do
        let (e_t,output) = runWriter $ runErrorT $ infer decls b
        case (fb, e_t) of
            (None, _) -> do
                inform $ vcat [ text "Nicht geprüft."
                              , text ""
                              , text "Die Einsendung wird von Ihrem Tutor bewertet."
                              , text ""
                              , text "Ignorieren Sie die unten angezeigte Bewertung."
                              ]
            (_, Right t) | t == goal -> do
                inform $ vcat [ text "Ja, Ihre Einsendung ist richtig."
                              , text ""
                              , text "Ignorieren Sie die unten angezeigte Bewertung."
                              ]
            (YesNo, _) -> do
                inform $ text "Nein, die Lösung ist nicht korrekt."
                inform $ text ""
                inform $ text "Der eingegebene Ausdruck hat die Struktur:"
                peng b
                reject $ text "Die Typen und/oder Parameteranzahlen passen jedoch nicht."
            (Detailed, Right t) -> do
                inform $ text "Nein. Der eingebene Ausdruck hat Typ" <+> toDoc t <> text "gefordert war aber" <+> toDoc goal <> text "."
                inform $ text ""
                inform $ text "Der eingegebene Ausdruck hat die Struktur"
                peng b
                inform $ text "und der Typcheck liefert folgende Aussage:"
                inform $ vcat output
                reject $ text ""
            (Detailed, Left ()) -> do
                inform $ text "Nein."
                inform $ text ""
                inform $ text "Der eingegebene Ausdruck hat die Struktur"
                peng b
                inform $ text "und der Typcheck liefert folgende Aussage:"
                mapM_ inform output
                reject $ text ""

{-
instance C.Measure TypeCheckBonn Config Exp where
    measure p i b = fromIntegral $ size b
-}

make :: Make
make = direct TypeCheckBonn $
    Config { feedback = Detailed
           , targetType = read "boolean"
           , declarations = read "int a; boolean eq (int a, int b);"
           }

