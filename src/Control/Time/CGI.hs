module Control.Time.CGI where

import Control.Time.Typ
import Inter.DateTime
import Gateway.CGI

import Data.Ix

edit :: String -> Maybe Time -> Form IO Time
edit title mt = do
    t <- io $ case mt of
         Just t -> return t
	 Nothing -> fmap read $ Inter.DateTime.now
    
    let def n opts = ( Just $ length $ takeWhile ( \(_,x) -> x /= n ) opts 
		     , opts 
		     )
        numbers bnd = do n <- range bnd ; return ( show n, n )
	months = zip [ "Januar", "Februar", "März", "April"
		     , "Mai", "Juni", "Juli", "August"
		     , "September", "Oktober", "November", "Dezember"  
		     ] 
		     [ 1.. ]
    [ y, m, d, h, i, s ]  <- selectors title
        [ def ( year t   ) $ numbers ( 2000, 2010 )
	, def ( day t    ) $ numbers ( 1, 31 )
	, def ( month t  ) $ months 
	, def ( hour t   ) $ numbers ( 0, 23 )
	, def ( minute t ) $ numbers ( 0, 59 )
	, def ( second t ) $ numbers ( 0, 59 )
	]
    return $ Time
	    { year = y, month = m, day = d
	    , hour = h, minute = i, second = s
	    }
    
	