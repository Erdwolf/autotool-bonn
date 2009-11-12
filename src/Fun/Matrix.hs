{-# LANGUAGE TemplateHaskell #-}

module Fun.Matrix where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Array
import Data.List

data ( Reader a, ToDoc a ) => Matrix a =
     Matrix { width :: Integer
	    , height :: Integer
	    , contents :: [[a]]
	    }

examplematrix n = Matrix
    { width = n
    , height = n
    , contents = [ [ abs (x-y) | y <- [ 1 .. n ] ] | x <- [ 1 .. n ] ]
    }

instance ( Reader a, ToDoc a ) => ToDoc ( Matrix a ) where
    toDocPrec d m = docParen ( d >= 10 ) $
        text "Matrix" </> dutch_record
	    [ text "width" <+> equals <+> toDocPrec 0 ( width m )
	    , text "height" <+> equals <+> toDocPrec 0 ( height m )
	    , text "contents" <+> equals 
	        <+> dutch_matrix ( map ( map toDoc ) $ contents m ) 
	    ]

dutch_matrix = dutch_vertical_list . map dutch_horizontal_list
dutch_vertical_list = dutch_combined_list vcat
dutch_horizontal_list = dutch_combined_list hsep
dutch_combined_list com xs = 
    let helper op [] = [ text "]" ]
	helper op (x : xs) = ( op <+> x ) : helper comma xs
    in  com $ helper ( text "[" ) xs


$(derives [makeReader] [''Matrix])
