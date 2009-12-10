{-# LANGUAGE TemplateHaskell #-}

module Rushhour.Data where

--   $Id$

import Autolib.TES.Identifier
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash
import Autolib.FiniteMap

import Data.Typeable
import Data.Array

import Control.Monad ( guard )

data Rushhour = Rushhour deriving ( Typeable )
data Rushhour_Inverse = Rushhour_Inverse deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Rushhour])
$(derives [makeReader, makeToDoc] [''Rushhour_Inverse])

type Position = ( Int, Int )

data Orientation = Vertical | Horizontal 
     deriving ( Eq, Ord, Typeable, Enum, Bounded, Ix )

$(derives [makeReader, makeToDoc] [''Orientation])

offset :: Orientation -> Position
offset Vertical = ( 0, 1 )
offset Horizontal = ( 1, 0 )

data Car = Car { orientation :: Orientation
               , extension :: Int
		   , position :: Position
		   }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Car])

type Zug = ( Identifier, Int )

instance Size Zug where size _ = 1

data Instance =
     Instance { width  :: Int -- ^ range is [ - width .. width ]
	      , height :: Int -- ^  range ist [ - height .. height ]
	      ,  cars :: FiniteMap Identifier Car
	      , target :: Identifier -- ^ free this car (in positive direction)
	      }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

-- | Problem 9 of Railroad Rushhour (Binary Arts)
example :: Instance
example = Instance
    { width = 3, height = 3, target = read "X"
    , cars = listToFM
        [ ( read "A", Car { orientation = Vertical, extension = 2, position = ( -3, 2 ) } )
        , ( read "B", Car { orientation = Horizontal, extension = 2, position = ( 2, 3 ) } )
        , ( read "C", Car { orientation = Vertical, extension = 2, position = ( -3, 0 ) } )
        , ( read "D", Car { orientation = Vertical, extension = 2, position = ( -3, -2 ) } )
        , ( read "E", Car { orientation = Vertical, extension = 2, position = ( 0, -2 ) } )
        , ( read "F", Car { orientation = Horizontal, extension = 2, position = ( -3, -3 ) } )
        , ( read "G", Car { orientation = Horizontal, extension = 2, position = ( -1, -3 ) } )
        , ( read "H", Car { orientation = Horizontal, extension = 2, position = ( 1, -3 ) } )
        , ( read "O", Car { orientation = Horizontal, extension = 3, position = ( -2, 3 ) } )
        , ( read "P", Car { orientation = Vertical, extension = 3, position = ( 0, 0 ) } )
        , ( read "Q", Car { orientation = Vertical, extension = 3, position = ( 3, 0 ) } )
        , ( read "R", Car { orientation = Vertical, extension = 3, position = ( 3, -3 ) } )
        , ( read "X", Car { orientation = Horizontal, extension = 2, position = ( -2, 0 ) } )
        ]
    }

occupied :: Instance -> Array Position [ Identifier ]
occupied i = 
    let w = width i ; h = height i
	bnd = (( negate w, negate h),(w,h))
    in  accumArray ( \ old new -> new : old ) [] bnd $ do
	    ( name, car ) <-  fmToList $ cars i
	    let (x,y) = position car
		(dx, dy) = offset $ orientation car
	    d <- [ 0 .. extension car - 1 ]
	    return ( (x+d*dx, y+d*dy), name )

-- | list of maximal extension  of a fresh car beginning here
-- return only extensions >= first param
-- omit cases that block the target car (same direction, nearer to exit)
spaces :: Int
       -> Instance 
       -> [ ( Position , Orientation , Int ) ]
spaces atleast i = do
    let Just t = lookupFM ( cars i ) ( target i )
    let occ = occupied i
        ( ul, or ) = bounds occ
        bnd = ((ul, minBound), (or, maxBound ))
        ok q = inRange ( bounds occ ) q && null ( occ ! q )
    ( p @ (x,y) , o ) <- range bnd
    let ( dx, dy ) = offset o
    guard $ not 
          ( orientation t == o && dominates ( position t ) p )
    let e = length $ takeWhile ok $ do
                    d <- [ 0 .. ]
                    let q = ( x + d*dx, y + d*dy )
                    return q
    guard $ e > atleast
    return ( p, o, e - 1 )

dominates (a,b) (c,d) = 
    (a == c && b < d) || (a < c && b == d)


collisions :: Instance -> [ Doc ]
collisions i = do
    (p, names) <- assocs $ occupied i
    guard $ length names > 1
    return $ hsep [ text "Kollision"
		  , text "zwischen", toDoc names
		  , text "auf Position" , toDoc p
		  ]

consistent :: Instance -> Bool
consistent = null . collisions

instance Nice Instance where nice = present

present :: Instance -> Doc
present i = 
    let w = width i + 1 ; h = height i + 1
	bnd = ((negate $ 2 * w, negate h), (2 * w, h))
	a = accumArray ( \ old new -> new ) ' ' bnd []
	left_right = do
	    x <- [ negate $ 2 * w, 2 * w ]
	    y <- [ negate h .. h ]
	    return ( (x,y), '*' )
        top_bottom = do
	    x <- [ negate $ 2 * w .. 2 * w ]
	    y <- [ negate h , h ]
	    return ( (x,y), '*' )
        core = do
	    ((x,y), c ) <- assocs $ occupied i 
	    return ( (2 * x, y)
		   , case c of
		        [] -> '.'
			[n] -> head $ show n
			_ -> '?'
		   )
	hole = do
	    ( name, car ) <- fmToList $ cars i
	    guard $ name == target i
	    let (x,y) = position car
                (dx, dy ) = offset $ orientation car
	    return ((2 * (x + dx * (w-x)), y + dy * (h-y)), ' ')
        b = a // left_right // top_bottom // core // hole
    in  vcat $ do
	    let ((l,u),(r,o)) = bounds b
	    y <- reverse [ u .. o ]
	    return $ text $ do
	        x <- [ l .. r ]
		return $ b ! (x,y)


-- local variables:
-- mode: haskell
-- end:
