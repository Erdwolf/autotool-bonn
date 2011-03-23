{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Type.Poly.Check where


import Type.Poly.Data
import Type.Poly.Tree
import Type.Poly.Infer

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Size

import Autolib.TES.Identifier

import Inter.Types
import qualified Challenger as C

import Data.Typeable
import qualified Tree as T

data TypePolyCheck = TypePolyCheck deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypePolyCheck where
    scoringOrder _ = Increasing

instance C.Partial TypePolyCheck TI Expression where

    describe p i = vcat
        [ text "Gesucht ist ein Ausdruck vom Typ" 
               <+> protect (toDoc (target i))
	, text "in der Signatur"
	, nest 4 $ protect $ toDoc (signature i)
	]

    initial p i = read "f(a(),M.<Foo>g(b()))"

    total p i b = do
        inform $ vcat [ text "Die Baumstruktur des Ausdrucks"
                      , nest 4 $ protect $ toDoc b
                      , text "ist"
                      ]
	peng b
        t <- infer (signature i) b
        assert ( t == target i )
               $ text "ist das der geforderte Typ?"

instance C.Measure TypePolyCheck TI Expression where
    measure p i b = fromIntegral $ size b

make :: Make
make = direct TypePolyCheck $
    TI { target = read "boolean"
       , signature = read "static int a(); static <T>boolean eq (T a, T b);"
       }
