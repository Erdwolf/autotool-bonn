{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}

{-# LANGUAGE TemplateHaskell #-}
module Fun.Direct.Config where

import Fun.Type
import Fun.Table

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Typeable
import Data.Array
import Data.List

data Primrec_2D = Primrec_2D deriving ( Typeable )

data Config = 
     Config { table :: Matrix Integer
	    , properties :: [ Property ]
	    }
    deriving Typeable

example :: Config
example = Config
    { Fun.Direct.Config.table = examplematrix 10
    , properties = [ Builtins [] ]
    }

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

mkmatrix :: Tafel2 -> Matrix Integer
mkmatrix t = 
    let ( (0,0), (h,w)) = bounds $ unTafel2 t
    in  Matrix { width = w + 1
	       , height = h + 1
	       , contents = [ [ unTafel2 t ! (x,y) | y <- [ 0 .. w ] ]
			    | x <- [ 0 .. h ]
			    ]
	       }

mktafel2 :: Matrix Integer -> Reporter Tafel2 
mktafel2 m = do
    when ( genericLength ( contents m ) /= height m ) $ reject
	 $ text "Der Inhalt der Matrix hat nicht die Höhe" 
         <+> toDoc ( height m )
    sequence $ do
        ( k, xs) <- zip [ 0 :: Int .. ] $ contents m
        return $ when ( genericLength xs /= width m ) $ reject
	     $ text "Die Zeile" <+> toDoc k 
	     <+> text "hat nicht die Breite" <+> toDoc ( width m )
    return $ Tafel2 $ listArray ((0,0), (height m - 1, width m - 1 ))
		    $ concat $ contents m

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


$(derives [makeReader, makeToDoc] [''Primrec_2D])
-- {-! for Primrec_2D derive: Reader, ToDoc !-}

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc !-}

$(derives [makeReader] [''Matrix])
-- {-! for Matrix derive: Reader !-}

-- local variables:
-- mode: haskell
-- end:
