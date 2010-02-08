module NFA.Convert where

import Convert.Type

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Data.List ( nub )
import Autolib.Reporter

import Autolib.NFA ( NFA )
import Autolib.NFA.Example
import Autolib.NFA.Eq
import Autolib.NFA.Restrict
import Autolib.Size
import Autolib.Set
import Autolib.Dot ( peng )
import Autolib.Informed

import NFA.Property
import NFA.Test
import NFA.Quiz

import Autolib.NFA.Type
import qualified Exp.Roll
import qualified NFA.Roll
import Convert.Input


data Convert_To_NFA = Convert_To_NFA 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Convert_To_NFA where
    scoringOrder _ = Increasing

instance C.Verify Convert_To_NFA ( Convert , [ Property Char ] ) where
    verify p ( from, props ) = do
        inform $ text "verifiziere die Quelle der Konversion:"
        nested 4 $ Convert.Input.verify_source $ input from
        inform $ text "verifiziere die Parameter der Konversion:"
        nested 4 $ case ( do Alphabet a <- props ; return a ) of
            [ alpha ] -> return ()
            xs -> reject $ vcat
                [ text "es soll genau einen Alphabet-Parameter geben,"
                , text "aber es gibt:" <+> toDoc xs
                ]

instance C.Partial Convert_To_NFA 
                   ( Convert , [ Property Char ] ) 
		   ( NFA Char Int ) 
  where

    describe Convert_To_NFA ( from, props ) = vcat
        [ text "Gesucht ist ein endlicher Automat,"
	, text "der die Sprache"
	, nest 4 $ form from
	, text "akzeptiert und folgende Eigenschaften hat:"
	, nest 4 $ toDoc props
        ]

    initial Convert_To_NFA ( from, props ) = 
        let alpha = case do Alphabet a <- props ; return a of
		  [ alpha ] -> alpha
		  xs -> error $ "alpha:" ++ show xs
	in  Autolib.NFA.Example.example_sigma alpha

    partial Convert_To_NFA ( from, props ) aut = do
        let [ alpha ] = do Alphabet a <- props ; return a
        inform $ text "Das ist Ihr Automat:"
	peng $ aut
        inform $ text "Sind alle Eigenschaften erfüllt?"
        nested 4 $ mapM_ ( flip test aut ) 
		 $ nub $ NFA.Property.Sane : props

    total Convert_To_NFA ( from, props ) aut = do
        inform $ text "Akzeptiert der Automat die richtige Sprache?"
        let [ alpha ] = do Alphabet a <- props ; return a
        flag <- nested 4 
             $ equ ( informed ( text "Sprache der Aufgabenstellung" ) $ eval alpha from ) 
                   ( informed ( text "Sprache Ihres Automaten" ) aut )
        when (not flag) $ reject $ text ""


instance C.Measure Convert_To_NFA 
                   ( Convert, [ Property Char ] ) 
		   ( NFA Char Int ) 
  where
    measure _ _ aut = fromIntegral $ size aut

make :: Make
make = direct Convert_To_NFA 
     ( Convert { name = Nothing
	       , input = Exp $ read "a (a+b)^* b"
	       }
     , NFA.Property.example
     )


instance Generator Convert_To_NFA ( Quiz Char ) 
           ( Convert, [ Property Char ] ) where
    generator p quiz key = do
        i <- case NFA.Quiz.generate quiz of
	    From_Exp props -> do
	        ( exp, aut) <- Exp.Roll.roll props
		return $ Convert.Input.Exp exp
	    From_NFA props -> do
	        aut <- NFA.Roll.roll props
		return $ Convert.Input.NFA aut
        return ( Convert { name = Nothing
		   , input = i
		   }
	       , solve quiz
	       )

instance Project  Convert_To_NFA 
                  ( Convert, [ Property Char ] ) 
                  ( Convert, [ Property Char ] )  where
    project p x = x


qmake :: Make
qmake = quiz Convert_To_NFA NFA.Quiz.example
