module Hanoi.Semantik where

-- $Id$

import Hanoi.Type
import Hanoi.Move

import Data.FiniteMap
import Reporter
import ToDoc

import qualified Challenger as C
import Inter.Types

data Hanoi = Hanoi deriving ( Eq, Ord, Show, Read )

data HI = HI { start :: Hof
	     , ziel  :: Hof
	     }
    deriving ( Show )

instance C.Partial Hanoi HI [ Zug ] where
    initial p i = [ (A, B) ]

    partial p i b = return ()
	-- evtl. hier testen, ob alle türme erlaubt sind

    total   p i b = do
        hof <- moves ( start i ) b
        inform $ vcat [ text "Sie erreichen diese Situation:"
		      , nest 4 $ toDoc hof
		      ]
	assert ( hof == ziel i ) $ text "Aufgabe gelöst?"
	return ()

data Conf = Conf { hoch :: Int
		 , turms :: Int
		 }
     deriving ( Show, Read )

make :: Conf -> HI
make conf = 
    let ts = take ( turms conf ) [ A .. ]
        leer = listToFM $ do t <- ts ; return ( t, [] )
        ss = [ 1 .. fromIntegral $ hoch conf ]
    in  HI { start = addToFM leer A ss
	   , ziel  = addToFM leer B ss
	   }

hanoi :: Conf -> Var Hanoi HI [ Zug ]
hanoi conf =
    Var { problem = Hanoi
        , aufgabe = "T" ++ show ( turms conf )
        , version = "H" ++ show ( hoch conf )
        , key = \ matrikel -> do
              return matrikel
        , gen = \ key -> do
              return $ do
	          let hi = make conf
                  inform $ vcat
                     [ text "Türme von Hanoi. Finden Sie eine Zugfolge von"
		     , nest 4 $ toDoc ( start hi )
		     , text "nach"
		     , nest 4 $ toDoc ( ziel hi )
		     ]
	          return hi
        }

