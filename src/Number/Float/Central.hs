module Number.Float.Central 

( fixed_from_float
, quiz_from_float
) 

where

--  $Id$

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types
import Inter.Quiz
import Data.Typeable
import Data.Ratio
import Autolib.Size
import Autolib.Xml
import System.Random

import Number.Wert
import Number.Float.Data
import Number.Float.Config
import Number.Float.Roll

-------------------------------------------------------------------------------

data From_Float = From_Float deriving ( Show, Typeable )

instance Partial From_Float Zahl Rational where

    describe From_Float z = vcat
	   [ text "Welche rationale Zahl wird durch die Gleitkommazahl"
           , nest 4 $ toDoc z
           , text "bezeichnet?"
	   ]

    initial From_Float z = 
        - 314 % 217

    partial From_Float z r = do
         return ()

    total From_Float z r = do
         assert ( (wert z :: Rational) == r )
                $ text "Stimmen die Bedeutungen der Zahlen sollen überein?"


-- das ist nicht sehr sinnvoll,
-- mir fällt keine aufgabe ein, bei der man das braucht
instance Size Rational where
    size r = fromIntegral $ length $ show r

instance Container Rational (Integer, Integer) where
    label _ = "Rational"
    pack r = ( numerator r, denominator r )
    unpack ( n, d) = n % d

-------------------------------------------------------------------------------

fixed_from_float :: Make
fixed_from_float = direct From_Float Number.Float.Data.example

quiz_from_float :: Make
quiz_from_float = quiz From_Float Number.Float.Config.example

instance Generator From_Float Config Zahl where
    generator _ p key = roll p

instance Project From_Float Zahl Zahl where
    project From_Float z = z




