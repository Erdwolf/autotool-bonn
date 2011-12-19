{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Type.Check where

--  $Id$

import Type.Data
import Type.Tree
import Type.Infer

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Size

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Identifier

import Data.Typeable
import Inter.Types
import qualified Challenger as C

data TypeCheck = TypeCheck deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheck where
    scoringOrder _ = Increasing

instance C.Partial TypeCheck (TI Identifier) Exp where

    describe p i = vcat
        [ text "Gesucht ist ein Ausdruck vom Typ" <+> toDoc (target i)
	, text "in der Signatur"
	, nest 4 $ toDoc (signature i)
	]

    initial p i = read "f(a,g(b))"

    total p i b = do
        inform $ vcat 
	       [ text "Die Baumstruktur dieses Ausdrucks ist"
	       , nest 4 $ draw b
	       ]
	peng b
        t <- infer (signature i) b
        assert ( t == target i )
               $ text "ist das der geforderte Typ?"

instance C.Measure TypeCheck (TI Identifier) Exp where
    measure p i b = fromIntegral $ size b

make :: Make
make = direct TypeCheck $
    TI { target = read "boolean"
       , signature = read "int a; static boolean eq (int a, int b);"
       }

