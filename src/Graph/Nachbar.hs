module Graph.Nachbar where

--  $Id$

import Graph.Util
import Graph.Iso
import Autolib.Graph.Basic
import Autolib.Graph.Kneser ( petersen )

import Inter.Types
import Autolib.Reporter
import qualified Challenger as C

import Data.Typeable

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
			]
	]

    initial p _ = circle [ 1 .. 6 ]

    partial p _ g = do
	validate g
        check_not_dominated             g
        check_not_iso (circle [1 :: Int .. 5]) g
	check_not_iso petersen          g
    
    total p _ g = do
        inform $ vcat [ text "Der Graph ist" , nest 4 $ toDoc g ]
        check_nach g

make :: Make
make = direct Nachbar ()

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
