module Control.Time.CGI where

import Control.Time.Typ
import Inter.DateTime
import Gateway.CGI

import Data.Ix

defaults :: IO (Time,Time)
defaults = do
    ( lo, hi ) <- Inter.DateTime.defaults
    return ( read lo, read hi ) -- ARGH

edit :: String -> Maybe Time -> Form IO Time
edit title mt = do
    jetzt <- fmap read $ io $ Inter.DateTime.now
    let t = case mt of
         Just t -> t
	 Nothing -> jetzt
    
    let def n opts = ( Just $ length $ takeWhile ( \(_,x) -> x /= n ) opts 
		     , opts 
		     )
        filled ff cs = drop ( length cs ) ff ++ cs
        numbers bnd = do n <- range bnd ; return ( filled "00" $ show n, n )
	months = zip [ "Januar", "Februar", "März", "April"
		     , "Mai", "Juni", "Juli", "August"
		     , "September", "Oktober", "November", "Dezember"  
		     ] 
		     [ 1.. ]
    [ y, m, d, h, i, s ]  <- selectors title
        [ def ( year t   ) $ numbers ( 2000, 2010 )
	, def ( month t  ) $ months 
	, def ( day t    ) $ numbers ( 1, 31 )
	, def ( hour t   ) $ numbers ( 0, 23 )
	, def ( minute t ) $ numbers ( 0, 59 )
	, def ( second t ) $ numbers ( 0, 59 )
	]
    return $  Time
	    { year = y, month = m, day = d
	    , hour = h, minute = i, second = s
	    }
    
	