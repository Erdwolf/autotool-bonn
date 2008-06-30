module Robots3.Form where

import Robots3.Data
import Robots3.Config

import Autolib.ToDoc

form :: Config -> Doc
form c = 
    let ((u,l),(o,r)) = bounds c
        grid = text 
	    $ "\\psgrid" ++ "[subgriddiv=1,griddots=10]"
	robs = vcat $ do
	    r <- robots c
            return $ rform r
	tags = vcat $ do
	    t <- goals c
	    return $ gform t
    in  vcat [ text $ "\\pspicture*" ++ show (u,l) ++ show (o+1,r+1) 
	     , grid, robs, tags
	     , text $ "\\endpspicture" 
	     ]

rform r = 
    let p = position r
	a = x p ; b = y p
    in  vcat
	[ text 
		   $ "\\psframe[framearc=.2,fillstyle=solid,fillcolor=lightgray]"
	             ++ concat ( map show [(a,b),(a+1,b+1)] )
	, text $ "\\rput" ++ show (fromIntegral a + 0.5, fromIntegral b + 0.5)
	       ++ "{" ++ name r ++ "}"
	]

gform p = 
    let a = fromIntegral $ x p ; b = fromIntegral $ y p
	m = (a+0.5,b+0.5)
    in  vcat
	[ text 
		   $ "\\pscircle[]"
	             ++ show m  ++ "{" ++ show 0.5 ++ "}"
	, text $ "\\rput" ++ show m ++ "{" ++ "*" ++ "}"
	]


        
       