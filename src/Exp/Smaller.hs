module Exp.Smaller where

--  $Id$

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

import Autolib.Exp.Example
import Autolib.NFA.Eq
import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.Exp.Sanity
import Autolib.Size
import Autolib.Dot ( peng )

import Exp.Property
import Exp.Test
import Exp.Quiz
import NFA.Roll
import Convert.Input

import Autolib.Set
import Autolib.Informed

data Exp_Smaller = Exp_Smaller
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Exp_Smaller 
                   ( RX Char , [ Property Char ] ) 
		   ( RX Char ) 
  where

    report Exp_Smaller ( rx, props ) = do
        inform $ vcat
            [ text "Gesucht ist ein regulärer Ausdruck,"
	    , text "der die selbe Sprache wie der Ausdruck"
	    , nest 4 $ toDoc rx
	    , text "der Größe"
	    , nest 4 $ toDoc $ size rx
	    , text "erzeugt, aber kleiner ist und die Eigenschaften"
	    , nest 4 $ toDoc props
	    , text "hat!"
            ]

    initial Exp_Smaller ( rx, props ) = 
        let [ alpha ] = do Alphabet a <- props ; return a
	in  Autolib.Exp.Example.example alpha

    partial Exp_Smaller ( rx, props ) exp = do
-- als property definiert
--        sanity_keys ( mkSet [ "Eps", "Empty" ] ) exp
        inform $ text "Sind alle Eigenschaften erfüllt?"
        nested 4 $ mapM_ ( flip test exp ) props

        inform $ text "Ist Ihr Ausdruck kleiner?"

        when ( size exp >= size rx ) $ reject $ vcat 
	     [ text "Nein. Die Größe Ihres Ausdrucks ist"
	     , nest 4 $ toDoc $ size exp
	     , text "darf aber höchstens"
	     , nest 4 $ toDoc $ pred $ size rx
	     , text "sein." 
	     ]

        inform $ text "Ja."

    total Exp_Smaller ( rx, props ) exp = do
        inform $ text "Erzeugt Ihr Ausdruck die richtige Sprache?"
        let [ alpha ] = do Alphabet a <- props ; return a
        flag <- nested 4 
             $ equ ( informed ( text "Sprache der Aufgabenstellung" ) 
		   $ inter (std_sigma $ setToList alpha) rx
		   ) 
                   ( informed ( text "Sprache Ihres Ausdrucks" ) 
                   $ inter (std_sigma $ setToList alpha) exp 
		   )
        when (not flag) $ reject $ text ""


instance C.Measure Exp_Smaller
                   ( RX Char , [ Property Char ] ) 
		   ( RX Char ) 
  where
    measure _ _ exp = fromIntegral $ size exp

make :: Make
make = direct Exp_Smaller
     ( read "(a+b+ab+ba)^*" :: RX Char
     , [ Exp.Property.Simple , Exp.Property.Alphabet (mkSet "ab") ] :: [ Property Char ]
     )

