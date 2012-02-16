{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances #-}

module ArrayBonn.Central where

import ArrayBonn.Environment
import ArrayBonn.Program
import ArrayBonn.Statement
import ArrayBonn.Semantics as S

import qualified ArrayBonn.Roll as R
import ArrayBonn.Config as Cfg
import ArrayBonn.Value as V

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import qualified Challenger as C
import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Util.Zufall ( repeat_until )

import Data.Typeable
import Data.Maybe ( isNothing, isJust )

data ArrayBonn = ArrayBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore ArrayBonn where
    scoringOrder _ = None -- ?

make_quiz :: Make
make_quiz = quiz ArrayBonn Cfg.example

data InstanceConfig = InstanceConfig
    { feedback :: Bool
    , initial :: Environment Value
    , program :: Program Statement
    , final   :: Environment Value
    }
  deriving Typeable

$(derives [makeReader, makeToDoc] [''InstanceConfig])


instance Generator ArrayBonn Config InstanceConfig where
    generator p conf@(Config fb _ _ mds _ _ _) _ = do
        (i,prog,final) <- R.roll conf `repeat_until` nontrivial
        return $ InstanceConfig fb i prog final
     where
        nontrivial (i,prog,final) = and $ do
            let bnd = ( 0 , fromIntegral mds )
                Program sts = prog
            ps <- [] : map return ( patches final bnd )
            return $ isNothing $ result $ C.total ArrayBonn (InstanceConfig True undefined (Program $ ps ++ sts) final) final


instance Project ArrayBonn InstanceConfig InstanceConfig where
    project _ = id

type Solution = Environment Value

instance C.Partial ArrayBonn InstanceConfig Solution where

    describe _ (InstanceConfig giveFeedback _ p e) = vcat $
        [ text "Deklarieren und initialisieren Sie die Variablen,"
        , text "so dass sich nach Ausführung des Programmes"
        , text ""
        , text ""
        , nest 4 $ toDoc p
        , text ""
        , text "die folgende Belegung ergibt:"
        , text ""
        , nest 4 $ toDoc e
        ] ++ if giveFeedback then [] else
        [ text ""
        , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
        , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
        ]

    initial _ (InstanceConfig _ _ p e) = e

    total _ (InstanceConfig True _ p target) start = do
        inform $ text "Ich führe das Programm aus:"
        actual <- nested 4 $ S.execute start p
        inform $ vcat
            [ text "Die resultierende Belegung ist:"
            , text ""
            , nest 4 $ toDoc actual
            ]
        inform $ text "Ich vergleiche mit der Aufgabenstellung:"
        nested 4 $ must_be_equal target actual
        inform $ vcat [ text "Ja, Ihre Einsendung ist richtig."
                      , text ""
                      , text "Ignorieren Sie die unten angezeigte Bewertung."
                      ]

    total _ (InstanceConfig False _ p target) start = do
        inform $ vcat [ text "Nicht geprüft."
                      , text ""
                      , text "Die Einsendung wird von Ihrem Tutor bewertet."
                      , text ""
                      , text "Ignorieren Sie die unten angezeigte Bewertung."
                      ]



make_fixed :: Make
make_fixed = direct ArrayBonn $ InstanceConfig True env (Program []) env where env = read "int a[3] = { 1, 2, 3 };"
