module Machine.Clock.Test

--   $Id$

( clock_test
)

where

import Machine.Class
import Machine.Fun

-- import Machine.Akzeptieren
-- import Machine.History

import qualified Machine.Clock.Type as C

import Control.Monad ( guard )
import Reporter hiding ( output )
import ToDoc
import Reader
import Data.FiniteMap
import Data.Set
import Size

clock_test ::  ( Encode dat, Machine m dat conf )
	        => C.Type m
		-> m
		-> Reporter Int
clock_test i m = do
    let cut = C.cut i
	inputs = C.args i
	fun = C.fun i
    let check ein conf = do
	    let a' = fun ein
	    let a = depth m conf
	    inform $ vcat
		   [ text "Endkonfiguration wird in Schritt"
		   <+> toDoc a <+> text "erreicht,"
		   , text "gefordert war", toDoc a'
		   ]
	    return $ a == fromIntegral a' 
    inner_fun_test cut inputs ( encode . \ n -> [n] ) check m	    

  
