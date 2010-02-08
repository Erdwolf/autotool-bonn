{-# language DeriveDataTypeable, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Program.Cexp.Interface where

import qualified Program.Cexp.Type as T
import qualified Program.Cexp.Annotated as A

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter 

import Inter.Types
import Inter.Quiz
import Data.Typeable

data Cexp = Cexp deriving Typeable

instance OrderScore Cexp where
    scoringOrder _ = None -- ?

$(derives [makeToDoc,makeReader] [''Cexp])

-- Note: Order of components (first Integer, then T.Exp)
-- is important for parsing. T.Exp first would not work
-- since the final "," will be parsed as part of the T.Exp.
-- Fixme: "--" operator may be confused with comment?

instance Partial Cexp ( Integer, T.Exp ) A.Annotated where

    describe _ ( v, x ) = vcat
        [ text "Gesucht ist eine Auswertungsreihenfolge, durch welche der Ausdruck"
        , nest 4 $ toDoc x
        , text "den Wert" <+> toDoc v <+> text "bekommt."
        , text "TODO: more text"
        ]

    initial _ ( v, x ) = A.start x

    partial _ ( v, x ) a = A.same_skeleton (x, a)

    total _ ( v, x ) a = do
        A.execute a
        A.check a 
        when ( A.rvalue a /= Just v ) 
             $ reject $ text "Wert stimmt nicht mit Ziel überein."

make_fixed :: Make
make_fixed = direct Cexp T.example