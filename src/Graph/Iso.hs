--  $Id$

module Graph.Iso where

import Graph.Util
import Autolib.Util.Splits

import Autolib.ToDoc
import Autolib.Graph.Basic
import Data.List ( partition )
import Control.Monad ( guard )

check_not_iso :: ( GraphC a, GraphC b , ToDoc (FiniteMap a b) )
     => Graph a 
     -> Graph b 
     -> Reporter ()
check_not_iso g h = do
    let idoc x = nest 4 $ info x <+> equals <+> toDoc x
    inform $ vcat
	   [ text "Der Graph" , idoc g, text "und der Graph", idoc h
	   , text "sollen nicht isomorph sein."
	   ]
    case find g h of
        []     -> inform $ text "OK"
        f : _  -> reject $ vcat 
		         [ text "es gibt doch eine Isomorphie:"
			 , nest 4 $ toDoc f
			 ]

iso ::  ( GraphC a, GraphC b )
     => Graph a 
     -> Graph b 
     -> Bool
iso g h = not $ null $ find g h

find :: ( GraphC a, GraphC b )
     => Graph a 
     -> Graph b 
     -> [ FiniteMap a b ]
find g h = do
    guard $ cardinality (knoten g) == cardinality (knoten h)
    guard $ cardinality (kanten g) == cardinality (kanten h)
    f <- find_with g h [] (lknoten g) (lknoten h)
    return $ listToFM f

find_with :: ( GraphC a, GraphC b )
     => Graph a 
     -> Graph b
     -> [(a,b)]
     -> [a] 
     -> [b]
     -> [[ (a,b)]]
find_with g h done [] [] = return done
find_with g h done (x : xs) yys = do
    ( pre, y : post ) <- splits yys
    let ys = pre ++ post
    let nx = setToList $ nachbarn g x
        ny = setToList $ nachbarn h y
    guard $ length nx == length ny
    let ( rx, lx ) = Data.List.partition (`elem` xs) nx
        ( ry, ly ) = Data.List.partition (`elem` ys) ny
    guard $ length rx == length ry
    guard $ length lx == length ly
    guard $ and $ do
        (a, b) <- done
        return $    ( kante a x `elementOf` kanten g )
	         == ( kante b y `elementOf` kanten h )
    find_with g h ((x,y) : done) xs ys



--------------------------------------------------------------------------

check_iso :: ( GraphC a, GraphC b, ToDoc  (FiniteMap a b)  )
	  => FiniteMap a b
	  -> Graph a
	  -> Graph b
	  -> Reporter ()
check_iso f g h = do
    inform $ vcat [ text "Sie behaupten diese Isomorphie" 
		  , nest 4 $ text "f" <+> equals <+> toDoc f
		  , text "von"
		  , nest 4 $ info g
		  , text "nach"
		  , nest 4 $ info h
		  ]
    equal_set ( text "V" <> parens ( info g ) , knoten g )
              ( text "domain f"               , mkSet $ keysFM f )
    equal_set ( text "V" <> parens ( info h ) , knoten h )
              ( text "range f"                , mkSet $ eltsFM f )
    fromto ( text "f" , f ) g h
    let invert fm = listToFM $ map ( \ (x,y) -> (y,x) ) $ fmToList fm
    fromto ( text "f^-", invert f ) h g        
    inform $ text "Das ist wirklich eine Isomorphie."

fromto :: ( GraphC a, GraphC b )
       => ( Doc, FiniteMap a b )
       -> Graph a
       -> Graph b
       -> Reporter ()
fromto ( doc, f ) g h = do
    inform $ vcat
	   [ text "bildet" <+> doc 
	   , text "jede Kante von" <+> info g
	   , text "auf eine Kante von" <+> info h
	   , text "ab?"
	   ]
    let fun x = lookupWithDefaultFM f (error "Selfcom.fromto") x
    let wrong = do
            k <- lkanten g
	    let k' = kante (fun $ von k) (fun $ nach k)
            guard $ not $ k' `elementOf` kanten h
	    return $ fsep [ text "das Bild", toDoc k' , text "von", toDoc k
			  , text "in", text "E" <> parens (info g)
			  , text "ist nicht in", text "E" <> parens (info h)
			  ]
    if not $ null wrong 
       then reject $ text "nein:" <+> vcat wrong
       else return ()
