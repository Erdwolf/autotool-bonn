-- {-# language OverlappingInstances,IncoherentInstances #-}

module Graph.Hamilton.Plain where

--  $Id$

import Graph.Util
import Autolib.Graph.Kneser

import Autolib.Dot ( peng, Layout_Program (..) )

import Inter.Types

import Autolib.Reporter.Set
import Autolib.Set
import Autolib.Graph.Ops ( restrict )

import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable

data Hamilton = Hamilton 
   deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Hamilton where
    scoringOrder _ = None -- ?

instance ( GraphC a, Show a )
    => C.Partial Hamilton (Graph a) [a] where

    report _ g = do
        inform $ vcat
	       [ text "Gesucht ist ein Hamiltonkreis in"
	       , nest 4 $ toDoc g
	       ]
	peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"8,8\"" ]
		 , show_labels = True
		 }
    initial _ g = 
        shuffle $ lknoten g

    partial _ g xs = do
        nodes_in_graph g xs
        nodes_form_cycle g xs

    total _ g xs = do
        cycle_is_total g xs

instance ( GraphC a, Show a )
    => C.Verify Hamilton (Graph a) where
    verify _ g = do
        validate g
        inform $ text "Warnung: Existenz eines Hamiltonkreises wurde nicht überprüft!"

shuffle xs = do
    let ( pre, post ) = splitAt ( length xs `div` 2 ) xs
    (x,y) <- zip pre ( reverse post )
    [ x, y ]
    
nodes_in_graph g xs =
    subeq ( text "Knoten der Einsendung" , mkSet xs  )
          ( text "Knoten des Graphen"    , knoten g  )
   
nodes_form_cycle g xs = do
    inform $ text "Knotenfolge bildet Kreis?"
    sequence $ do
        (x, y) <- zip xs ( rotate 1 xs )
        let k = kante x y
        return $ assert ( elementOf k $ kanten g )
	       $ toDoc k
    inform $ text "ja."

rotate k xs = 
    let ( pre, post ) = splitAt k xs
    in  post ++ pre

cycle_is_total g xs = do
    subeq ( text "Knoten des Graphen"    , knoten g  )
          ( text "Knoten der Einsendung" , mkSet xs  )
          
instance ( GraphC a, Show a )
    => C.Measure Hamilton (Graph a) [a] where
    measure _ _ _ = 0

make :: Make
make = direct Hamilton
     $ let p = petersen
           x : xs = lknoten p
       in  restrict ( mkSet xs ) p

