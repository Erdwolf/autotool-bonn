{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckBonn.Central where

import Type.Data
import Type.Tree
import Type.Infer

import Autolib.Reporter.Type
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Identifier
import qualified Autolib.TES.In as T

import Data.Typeable
import Inter.Types
import qualified Challenger as C

data TypeCheckBonn = TypeCheckBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheckBonn where
    scoringOrder _ = Increasing


type IdentifierBonn = Identifier
{-
newtype IdentifierBonn = IdentifierBonn Identifier deriving ( Typeable, Eq, Ord )

instance ToDoc IdentifierBonn where
    toDoc (IdentifierBonn i) = toDoc i
instance Reader IdentifierBonn where
    reader = reader >>= return . IdentifierBonn

instance Size   IdentifierBonn where
instance ToTree IdentifierBonn where
instance Symbol IdentifierBonn where
-}


newtype ExpBonn = ExpBonn (Term Identifier IdentifierBonn) deriving ( Typeable )

instance ToDoc ExpBonn where
    toDoc (ExpBonn e) = toDoc e

instance Reader ExpBonn where
    reader = do
        e <- T.treader $ T.Config { T.reserved_symbols = [  (mkunary "&")
                                                         ,  (mkunary "*")
                                                         ]
                                  , T.allow_new_symbols = True
                                  }
        return (ExpBonn e)


instance C.Partial TypeCheckBonn (TI IdentifierBonn) ExpBonn where

    describe p i = vcat
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc (target i)
        , text "unter Verwendung von"
        , nest 4 $ toDoc (signature i)
        ]

    initial p i = read "f(a,g(b))"

    total p i (ExpBonn b) = do
        inform $ vcat
               [ text "Die Baumstruktur dieses Ausdrucks ist"
               , nest 4 $ draw b
               ]
        peng b
        t <- infer (signature i) b
        assert ( t == target i )
               $ text "ist das der geforderte Typ?"

instance C.Measure TypeCheckBonn (TI IdentifierBonn) ExpBonn where
    measure p i (ExpBonn b) = fromIntegral $ size b

make :: Make
make = direct TypeCheckBonn $
    TI { target = read "boolean"
       , signature = read "int a; boolean eq (int a, int b);" :: Signature IdentifierBonn
       }

