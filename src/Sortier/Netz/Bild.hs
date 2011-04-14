{-# LANGUAGE FlexibleInstances #-}
module Sortier.Netz.Bild where


import Sortier.Netz.Type
import Sortier.Netz.Example
import Autolib.Util.Bild
import Data.Array

-- | bestimmt (greedy) die schicht in der zeichnung
levelled :: Netz -> [ ( Comp, Int, Int ) ]
levelled n = helped ( array ( low n, high n ) $ do
		          i <- [ low n .. high n ]
		          return ( i, 0 )
		    )
		    ( comps n )
		    0

-- | in  a ! x  steht die schicht des vorigen komparators auf linie x
-- ausgabe: (comp, alte nummer, level)
helped :: Array Int Int -> [ Comp ] -> Int 
       -> [ ( Comp, Int, Int ) ]
helped a [] n = []
helped a ( (x,y) : rest ) n =
    let this = succ $ maximum $ do
		    z <- [ min x y .. max x y ]
		    return $ a ! z
        b    = a // do 
		    z <- [ min x y .. max x y ]
		    return ( z, this )
    in  ((x,y), n, this) : helped b rest ( succ n )

instance ToBild Netz where toBild n = paint (n, [])
instance ToBild ( Netz, States ) where toBild ( n, sts ) = paint (n, sts )


paint :: ( Netz , States ) -> Bild
paint ( n, sts ) = 
    let xscale = 6 ; yscale = 2
        (u, o) = (yscale * low n, yscale * high n)
        cnts = levelled n

	-- die 1 , um leere listen zu retten
	ts = 1 : do ( c, n, t ) <- cnts ; return t

	( l, r ) = ( xscale * pred ( minimum ts )
		   , xscale * succ ( maximum ts )
		   )

        linien = do -- durchgehende leitungen
            z <- [ low n .. high n ]
	    t <- [ l .. r ]
	    return ((t, yscale * z), '-' )
	knoten =  do
            ((x,y), n, t) <- cnts
	    s <- [x,y]
	    return ((xscale * t, yscale * s), 'o')
        kanten = do -- für komparatoren
            ((x,y), n, t) <- cnts
 	    let c = if x < y then 'v' else '^'
	    z <- [ succ $ yscale * min x y .. pred $ yscale * max x y ]
	    return ((xscale * t,z), c)
	marken = do -- zahlen an komparatoren
            ((x,y), i, t) <- cnts
	    z <- [x, y]
	    let vor  = ( sts !!      i ) !! ( z - low n )
	        nach = ( sts !! succ i ) !! ( z - low n )
            (d , m) <- [ (-1, vor), (1, nach) ]
	    return ( ( xscale * t + ( xscale * d ) `div` 2, yscale * z )
		   , head $ show m
		   )
	    
    in  accumArray (flip const) ' ' ((l,u), (r,o)) 
	    $ linien ++ knoten ++ kanten 
	      ++ if null sts then [] else marken

