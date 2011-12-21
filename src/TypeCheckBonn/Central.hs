{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckBonn.Central where

import Type.Data
import Type.Tree
import TypeCheckBonn.Infer
-- import TypeCheckBonn.Data (Config(..))

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


type Config = TI


instance C.Partial TypeCheckBonn Config Exp where

    describe p i = vcat
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc (target i)
        , text "unter Verwendung von:"
        , text ""
        , text ""
        , nest 4 $ toDoc (signature i)
        ]

    initial p i = read "f(a,g(b))"

    total p i b = do
        let (e_t,output) = runWriter $ runErrorT $ infer (signature i) b
        case e_t of
            Right t | t == target i -> do
                inform $ text "Ja."
            Right t -> do
                inform $ text "Nein. Der eingebene Ausdruck hat Typ" <+> toDoc t <> text "gefordert war aber" <+> toDoc (target i) <> text "."
                inform $ text ""
                inform $ text "Der Ausdruck hat die Struktur"
                peng b
                inform $ text "und der Typcheck liefert folgende Aussage:"
                inform $ vcat output
            Left () -> do
                inform $ text "Nein."
                inform $ text ""
                inform $ text "Der Ausdruck hat die Struktur"
                peng b
                inform $ text "und der Typcheck liefert folgende Aussage:"
                mapM_ inform output

instance C.Measure TypeCheckBonn Config Exp where
    measure p i b = fromIntegral $ size b

make :: Make
make = direct TypeCheckBonn $
    TI { target = read "boolean"
       , signature = read "int a; boolean eq (int a, int b);"
       }
{-
    Config { feedback = ???
           , targetType = read "boolean"
           , declarations = read "int a; boolean eq (int a, int b);"
           }
-}

