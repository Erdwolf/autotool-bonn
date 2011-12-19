{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
module TypeCheckBonn.Central where

import Type.Data hiding (Exp)
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

data TypeCheckBonn = TypeCheckBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TypeCheckBonn where
    scoringOrder _ = Increasing

instance C.Partial TypeCheckBonn TI Exp where

    describe p i = vcat
        [ text "Bilden Sie einen Ausdruck vom Typ" <+> toDoc (target i)
	, text "unter Verwendung von"
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

instance C.Measure TypeCheckBonn TI Exp where
    measure p i b = fromIntegral $ size b

make :: Make
make = direct TypeCheckBonn $
    TI { target = read "boolean"
       , signature = read "int a; boolean eq (int a, int b);"
       }

