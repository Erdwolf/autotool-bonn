-- | Korrekturfunktion für Faktorisierung

-- joe@informatik.uni-leipzig.de
-- benutzt code für challenger/PCProblem
-- von Markus Kreuz  mai99byv@studserv.uni-leipzig.de

module Faktor.Times (
      make_fixed 
     , make_quiz
    ) where



import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana
import Autolib.Size

import Inter.Types
import Inter.Quiz

import Faktor.Times.Param

import System.Random
import Data.Typeable

-------------------------------------------------------------------------------

data Times = Times deriving ( Eq, Show, Read, Typeable )

instance OrderScore Times where
    scoringOrder _ = None

instance Size Integer where size _ = 1

instance Partial Times [ Integer ] Integer where

    describe Times xs = vcat
	   [ text "Gesucht ist das Produkt der Zahlen " 
           , nest 4 $ toDoc xs 
           ]

    initial Times xs = sum xs

    total Times xs y = do
        let p = product xs
        when (y /= p) $ reject $ fsep
     	                  [ text "Das Produkt der Zahlen"
			  , toDoc xs, text "ist nicht", toDoc y
			  ]

make_fixed :: Make
make_fixed = direct Times 
    [ 222222222 :: Integer , 44444444444444, 555555555555555 ]

-------------------------------------------------------------------------------

make_quiz :: Make
make_quiz = quiz Times Faktor.Times.Param.example

roll :: Param -> IO [ Integer ]
roll p = sequence $ do
    i <- [ 1 .. anzahl p ]
    return $ do
        cs <- sequence $ replicate ( stellen p ) $ randomRIO ( 0, 9 )
        return $ foldl ( \ a b -> 10 * a + b ) 0 cs

instance Generator Times Param [ Integer ] where
    generator _ conf key = do
       xs <- roll conf
       return xs

instance Project Times [ Integer ][ Integer ] where
    project _ = id


