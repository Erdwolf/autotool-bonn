{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckBonn.Central where

import Type.Data
import Type.Tree
import TypeCheckBonn.Infer

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
import Inter.Types
import qualified Challenger as C

import Control.Monad.Writer
import Control.Monad.Error

data TypeCheckBonn = TypeCheckBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheckBonn where
    scoringOrder _ = Increasing


newtype ExpBonn = ExpBonn Exp deriving ( Typeable )

instance ToDoc ExpBonn where
    toDoc (ExpBonn e) = toDoc e

instance Reader ExpBonn where
    reader = reader >>= return . ExpBonn


instance C.Partial TypeCheckBonn TI ExpBonn where

    describe p i = vcat
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc (target i)
        , text "unter Verwendung von:"
        , text ""
        , text ""
        , nest 4 $ toDoc (signature i)
        ]

    initial p i = read "f(a,g(b))"

    total p i (ExpBonn b) = do
        inform $ text "Der Ausdruck hat die Struktur"
        peng b
        inform $ text "und der Typcheck liefert folgende Aussage:"
        let (e_t,output) = runWriter $ runErrorT $ infer (signature i) b
        case e_t of
            Right t | t == target i -> do
                inform $ text "Ja."
            Right t -> do
                reject $ text "Nein."
            Left () -> do
                inform $ vcat output
                reject $ text "Nein."

instance C.Measure TypeCheckBonn TI ExpBonn where
    measure p i (ExpBonn b) = fromIntegral $ size b

make :: Make
make = direct TypeCheckBonn $
    TI { target = read "boolean"
       , signature = read "int a; boolean eq (int a, int b);"
       }

