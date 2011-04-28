{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Graph.Nachbar where

--  $Id$

import Graph.Util
import Graph.Iso

import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Kneser ( petersen )

import Inter.Types
import Autolib.Reporter
import Autolib.Util.Teilfolgen
import qualified Challenger as C

import Autolib.Dot ( peng )

import Data.Typeable
import Data.List ( partition )

data Nachbar = Nachbar deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Nachbar () ( Graph Int ) where

    describe p _ = vcat
        [ text "Gesucht ist ein Graph G mit der Eigenschaft:"
	, nest 4 $ vcat [ text "Für alle Knoten x, y gilt:"
			, text "Wenn x und y nicht benachbart sind,"
			, text "dann besteht ihre gemeinsame Nachbarschaft"
			, text "aus genau einem Knoten."
			]
	, text $ "Der Graph soll keine der folgenden Formen haben:"
	, nest 4 $ vcat [ text "C_5"
			, text "Petersen-Graph"
			, text "es gibt einen Knoten, der zu allen anderen benachbart ist"
			, text "K_x mit Dach"
			]
	]

    initial p _ = circle [ 1 .. 6 ]

    partial p _ g = do
        inform $ vcat [ text "Der Graph ist" , nest 4 $ toDoc g ]
	peng g
	validate g
        check_not_iso (circle [1 :: Int .. 5]) g
	check_not_iso petersen          g
        check_not_dominated             g
        nicht $ kx_mit_dach           g
    
    total p _ g = do
        check_nach g

{-
make :: Make
make = direct Nachbar ()
-}

----------------------------------------------------------------------------

check_nach :: GraphC a
	   => Graph a 
	   -> Reporter ()
check_nach g = silent $ mapM_ ( check_paar g ) $ paare $ lknoten g

paare :: [a] -> [(a,a)]
paare (x : ys) = map ( (,) x ) ys ++ paare ys
paare _ = []

check_paar :: GraphC a
	   => Graph a 
	   -> (a, a) 
	   -> Reporter ()
check_paar g (x, y) = when ( not $ kante x y `elementOf` kanten g ) $ do
    let n = intersect (nachbarn g x) (nachbarn g y)
    when (1 /= cardinality n) $ reject $ fsep
         [ text "die Knoten", toDoc x, text "und", toDoc y
	 , text "sind nicht benachbart"
	 , text "und haben die gemeinsamen Nachbarn", toDoc n
         ]
    
check_not_dominated g = do
    let dom = do 
	   x <- lknoten g
	   guard $ degree g x == pred (cardinality $ knoten g)
           return x
    when ( not $ null dom ) $ reject $ vcat
         [ text "Diese Knoten sind zu allen anderen benachbart:"
	 , nest 4 $ toDoc dom
	 ]

kx_mit_dach g = do
    inform $ text "hat der Graph die Form  K_x mit Dach?"
    let (x, r) = divMod (cardinality $ knoten g) 2
    assert ( 1 == r ) 
           $ text "ist die Knotenanzahl ungerade?"
    inform $ fsep [ text "vielleicht für x = ", toDoc x ]
    let xs = sfilter ( \ v -> 2 == degree g v ) $ knoten g
    assert ( x == cardinality xs ) 
           $ text "gibt es genau x Knoten vom Grad 2?"
    assert ( is_independent g xs )
           $ text "bilden diese eine unabhängige Menge?"
    let g' = restrict (minusSet (knoten g) xs) g
    inform $ text "G' := G ohne diese Menge."
    let (ys, zs) = Data.List.partition ( \ v -> 0 < degree g' v ) $ lknoten g'
    assert ( 1 == length zs )
           $ text "gibt es in G' genau eine Knoten vom Grad 0?"
    assert ( is_clique g' $ mkSet ys )
           $ text "bilden die anderen Knoten in G' eine Clique?"

---------------------------------------------------------------

-- | dreht ergebnis (ablehnen\/akzeptieren) um
-- TODO: move to Reporter.Type
nicht :: Reporter () -> Reporter ()
nicht r = do
    mx <- wrap r
    case mx of
	 Nothing -> return ()
         Just _  -> reject $ text "(nicht)"
    

