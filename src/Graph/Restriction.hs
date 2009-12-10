{-# LANGUAGE TemplateHaskell #-}

module Graph.Restriction where

import Graph.Util
import Autolib.Graph.Ops (complement)
import Condition
import Suggest

import Autolib.Size
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Typeable

data Restriction 
     = Min_Size Int
     | Max_Size Int
     | Max_Degree Int
     | Min_Degree Int
     | Connected
     | Complement_Connected
     | Regular
     | Irregular
   deriving ( Typeable, Eq, Ord )

$(derives [makeReader, makeToDoc] [''Restriction])

instance Suggest Restriction where
    suggest = [ Min_Size 8, Max_Size 12, Max_Degree 5
	      , Connected , Complement_Connected, Irregular
	      ]

instance GraphC a => Condition Restriction ( Graph a ) where
    condition r g = case r of
        Min_Size m -> do
	    let s = size g
	    when ( s < m ) $ reject $ fsep
		 [ text "Größe" <+> parens ( toDoc s )
		 , text "kleiner als Schranke" <+> parens ( toDoc m )
		 ]
        Max_Size m -> do
	    let s = size g
	    when ( s > m ) $ reject $ fsep
		 [ text "Größe" <+> parens ( toDoc s )
		 , text "größer als Schranke" <+> parens ( toDoc m )
		 ]
        Max_Degree m -> do
	    let degs = do x <- lknoten g ; return $ degree g x
		top  = maximum degs
	    when ( not ( null degs ) && top > m ) $ reject $ fsep	
		 [ text "Maximalgrad" <+> parens ( toDoc top )
		 , text "größer als Schranke" <+> parens ( toDoc m )
		 ]
        Min_Degree m -> do
	    let degs = do x <- lknoten g ; return $ degree g x
		top  = minimum degs
	    when ( not ( null degs ) && top < m ) $ reject $ fsep	
		 [ text "Minimalgrad" <+> parens ( toDoc top )
		 , text "kleiner als Schranke" <+> parens ( toDoc m )
		 ]
	Connected -> do
	    when ( not $ is_connected g ) $ reject $ fsep
		 [ text "ist nicht zusammenhängend" ]
	Complement_Connected -> do
	    let h = complement g
	    when ( not $ is_connected h ) $ reject $ fsep
		 [ text "Komplement ist nicht zusammenhängend" ]
	Regular -> do
	    check_reg g
	Irregular -> do
	    case result $ check_reg g of
		 Nothing -> return ()
		 Just _  -> reject $ text "ist regulär"

-- local variables:
-- mode: haskell
-- end:
