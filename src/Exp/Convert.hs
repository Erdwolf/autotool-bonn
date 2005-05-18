module Exp.Convert where

--  $Id$

import Convert.Type

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

data Convert_To_Exp = Convert_To_Exp 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Convert_To_Exp 
                   ( Convert , [ Property Char ] ) 
		   ( RX Char ) 
  where

    report Convert_To_Exp ( from, props ) = do
        inform $ vcat
            [ text "Gesucht ist ein regul�rer Ausdruck,"
	    , text "der die Sprache"
	    , nest 4 $ form from
	    , text "erzeugt und folgende Eigenschaften hat:"
	    , nest 4 $ toDoc props
            ]
        case ( name from , input from ) of
	     ( Nothing , NFA aut ) -> do
                 inform $ text "Das ist der Automat:"
		 peng aut
             _ -> return ()

    initial Convert_To_Exp ( from, props ) = 
        let [ alpha ] = do Alphabet a <- props ; return a
	in  Autolib.Exp.Example.example alpha

    partial Convert_To_Exp ( from, props ) exp = do
        sanity_keys ( mkSet [ "Eps", "Empty", "Sigma", "All" ] ) exp
        inform $ text "Sind alle Eigenschaften erf�llt?"
        nested 4 $ mapM_ ( flip test exp ) props

    total Convert_To_Exp ( from, props ) exp = do
        inform $ text "Erzeugt Ihr Ausdruck die richtige Sprache?"
        let [ alpha ] = do Alphabet a <- props ; return a
        flag <- nested 4 
             $ equ ( informed ( text "Sprache der Aufgabenstellung" ) $ eval alpha from ) 
                   ( informed ( text "Sprache Ihres Ausdrucks" ) 
                   $ inter (std_sigma $ setToList alpha) exp 
		   )
        when (not flag) $ reject $ text ""


instance C.Measure Convert_To_Exp 
                   ( Convert, [ Property Char ] ) 
		   ( RX Char ) 
  where
    measure _ _ exp = fromIntegral $ size exp

make :: Make
make = direct Convert_To_Exp 
     ( Convert { name = Just ["W�rter �ber {a,b}, die mit  a  beginnen und mit  b  enden"]
	       , input = Exp $ read "a (a+b)^* b"
	       }
     , Exp.Property.example
     )



instance Generator Convert_To_Exp ( Quiz Char ) 
           ( Convert, [ Property Char ] ) where
    generator p quiz key = do
        aut <- roll $ generate quiz
        return ( Convert { name = Nothing
		   , input = NFA aut 
		   }
	       , solve quiz
	       )

instance Project  Convert_To_Exp 
                  ( Convert, [ Property Char ] ) 
                  ( Convert, [ Property Char ] )  where
    project p x = x


qmake :: Make
qmake = quiz Convert_To_Exp Exp.Quiz.example
