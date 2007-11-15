module NFA.Compress.Look where

import NFA.Compress.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Set

import Data.Ix ( inRange )

up :: Compressed 
   -> Int -- ^ row
   -> Int -- ^ column
   -> Reporter Int
up c x y = do
    z <- ups emptySet c x y
    inform $ text "Resultat" <+> toDoc z
    return z

ups seen c x y = do
    inform $ hsep [ text "lookup" , toDoc (x,y) ]
    when ( ( x,y) `elementOf` seen ) $ reject 
	 $ text "terminiert nicht (dieses Argumentpaar kam bereits vor)"
    nested 4 $ do
        b <- get "base" ( base c ) x
        let a = b + y
        h <- get "check" ( chck c ) a
        if h == x
           then get "next" ( next c ) a
           else do
               d <- get "default" ( dflt c ) x
	       ups ( addToSet seen (x,y) ) c d y

get tag xs k = do
    let pre =  hsep [ text tag , brackets ( toDoc k ) ]
    when ( not $ inRange (0, length xs - 1) k ) 
	 $ reject $ pre <+> text "Indexfehler"
    let x = xs !! k
    inform $ pre <+> equals <+> toDoc x
    return x

