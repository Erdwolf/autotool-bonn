module Specify.Inter where

import Specify.Config
import Specify.Definition
import Specify.Check

import Autolib.Reporter
import Autolib.ToDoc

import qualified Challenger as C
import Inter.Types
import Data.Typeable
import Data.List ( partition )
import Data.Maybe ( isJust )

data Specify = Specify deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Specify Config Program where
    describe s c = vcat
        [ text "Deklarieren Sie Funktionen mit den folgenden Eigenschaften."
	, text "wobei die Variablen über alle natürlichen Zahlen laufen:"
	, nest 4 $ toDoc $ constraints c
        , text "Sie können arithmetische (+,-,*,/,%), logische (&&,||,!),"
        , text "relationale (<,<=,==,>=,>,!=) Operatoren sowie if-then-else verwenden."
        , text "Ihre Deklarationen dürfen nicht rekursiv sein."
        , text "Der Ausdruck ? bezeichnet einen (noch) nicht definierten Wert."
	]
    initial s c = Specify.Definition.example
    total s c p = do
        results <- Specify.Check.full ( constraints c ) p ( checks_per_constraint c )
        let ( def, undef ) = partition ( \ (mx, doc ) -> isJust mx ) results
            ( ok, no ) = partition ( \ (mx, doc) -> mx == Just True ) def
        when ( not $ null no ) $ reject $ vcat
             [ text "nicht erfüllte Bedingungen:"
             , nest 4 $ vcat $ take 5 $ map snd no
             ]
        when ( not $ null undef ) $ reject $ vcat
             [ text "undefinierte Bedingungen:"
             , nest 4 $ vcat $ take 5 $ map snd undef
             ]
        return ()

make :: Make
make = direct Specify Specify.Config.example

