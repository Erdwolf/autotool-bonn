{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckBonn.Central where

import Type.Data
import Type.Tree
import Type.Infer

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

data TypeCheckBonn = TypeCheckBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheckBonn where
    scoringOrder _ = Increasing


--type IdentifierBonn = Identifier

newtype IdentifierBonn = IdentifierBonn { unBonn :: Identifier }
    deriving ( Typeable, Eq, Ord )

instance Show IdentifierBonn where
    show = show . unBonn
instance ToDoc IdentifierBonn where
    toDoc = toDoc . unBonn
instance Reader IdentifierBonn where
    reader = reader >>= return . IdentifierBonn

instance Hash IdentifierBonn where
    hash = hash . unBonn
instance Size IdentifierBonn where
    size = size . unBonn
instance Symbol IdentifierBonn where
    arity = arity . unBonn
    set_arity a = IdentifierBonn . set_arity a . unBonn
    pool = map IdentifierBonn pool
    stringify = stringify . map unBonn

    precedence (IdentifierBonn i) | show i `elem` ["&","*"] = Just 3
    precedence _ = Nothing



newtype ExpBonn = ExpBonn (Term IdentifierBonn IdentifierBonn) deriving ( Typeable )

instance ToDoc ExpBonn where
    toDoc (ExpBonn e) = toDoc e

instance Reader ExpBonn where
    reader = do
        e <- T.treader $ T.Config { T.reserved_symbols = [ IdentifierBonn (mkunary "&")
                                                         , IdentifierBonn (mkunary "*")
                                                         ]
                                  , T.allow_new_symbols = True
                                  }
        return (ExpBonn e)


instance C.Partial TypeCheckBonn (TI IdentifierBonn) ExpBonn where

    describe p i = vcat
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc (target i)
        , text "unter Verwendung von:"
        , text ""
        , nest 4 $ toDoc (signature i)
        ]

    initial p i = read "f(a,g(b))"

    total p i (ExpBonn b) = do
        inform $ text "Die Baumstruktur dieses Ausdrucks ist:"
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

