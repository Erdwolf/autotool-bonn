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
        (i,prog,final) <- R.roll conf -- `repeat_until` nontrivial
        return $ InstanceConfig fb i prog final
     where
        nontrivial (i,prog,final) = not $ or $ do
            let bnd = ( 0 , fromIntegral mds )
                Program sts = prog
            ps <- [] : map return ( patches final bnd )
            return $ isJust $ result $ C.total ArrayBonn (InstanceConfig True undefined (Program $ ps ++ sts) final) final


instance Project ArrayBonn InstanceConfig InstanceConfig where
    project _ = id

type Solution = Environment Value

instance C.Partial ArrayBonn InstanceConfig Solution where

    describe _ (InstanceConfig fb _ p e) = vcat
        [ text "Deklarieren und initialisieren Sie die Variablen,"
	, text "so dass sich nach Ausführung des Programmes"
	, nest 4 $ toDoc p
	, text "die folgende Belegung ergibt:"
	, nest 4 $ toDoc e
	]

    initial _ (InstanceConfig _ _ p e) = e -- Program.Array.Environment.example

    total _ (InstanceConfig fb _ p target) start = do
        inform $ text "Ich führe das Programm aus:"
        actual <- nested 4 $ S.execute start p
	inform $ vcat
	    [ text "Die resultierende Belegung ist:"
	    , nest 4 $ toDoc actual
	    ]
	inform $ text "Ich vergleiche mit der Aufgabenstellung:"
	nested 4 $ must_be_equal target actual
	inform $ text "Ok."



make_fixed :: Make
make_fixed = direct ArrayBonn $ InstanceConfig True env (Program []) env where env = read "int a[3] = { 1, 2, 3 };"
