module Grammatik.Links_Ableitung where

-- $Id$

import Grammatik.Type
import Grammatik.Ableitung

import Schichten
-- import Util.BFS
-- import Util.Mehrfache
import Util.Doppler
import Util.Wort

import Control.Monad (guard)
import Data.List (intersperse)
import Sets


-- nur die erste (d. h. am weitesten linke) Ersetzung ausführen
-- das hat eigentlich nur für CF-Grammatiken Sinn,
-- das prüfen wir hier jedoch nicht

links_schritt :: Config
	-> Grammatik -> Ableitung -> Set Ableitung 
links_schritt conf g a = unionManySets 
			   $ take 1 
			   $ filter (not . isEmptySet)
			   $ do
    let w = car a
    (vorn, hinten) <- zerlegungen w
    return $ mkSet $ do
        (links, rechts) <- rules g
	let (mitte, rest) = splitAt (length links) hinten
	guard $ mitte == links    
	let w' = vorn ++ rechts ++ rest
	guard $ length w' <= max_length conf
	return $ cons w' a

links_ableitungen :: Config -> Grammatik -> [ Ableitung ]
links_ableitungen conf g = do
    ws <- take ( max_depth conf )
	$ takeWhile ( \ s -> cardinality s < max_width conf )
	$ schichten ( links_schritt conf g ) 
		    ( cons [ startsymbol g ] nil )
    w <- setToList ws
    guard $ and [ x `elementOf` terminale g | x <- car w ]
    return w

mehrdeutige_links_ableitungen 
    ::  Config -> Grammatik -> [ (Ableitung, Ableitung) ]
mehrdeutige_links_ableitungen conf g =
    doppler  ( max_width conf * max_depth conf )
             ( setToList . links_schritt conf g )
	     ( cons [ startsymbol g ] nil )

g :: Grammatik
g = Grammatik 
    { terminale = mkSet "ab"
    , nichtterminale = mkSet "S" 
    , startsymbol = 'S'
    , regeln = mkSet [ ( "S", ""), ( "S", "aSbS") , ( "S", "bSaS")]
    }
