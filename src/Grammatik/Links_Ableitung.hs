module Grammatik.Links_Ableitung where

-- $Id$

import Grammatik.Type
import Grammatik.Ableitung

import Schichten
import Util.BFS
import Util.Mehrfache
import Util.Wort

import Control.Monad (guard)
import Data.List (intersperse)
import Sets


-- nur die erste (d. h. am weitesten linke) Ersetzung ausführen
-- das hat eigentlich nur für CF-Grammatiken Sinn,
-- das prüfen wir hier jedoch nicht

links_schritt :: Maybe Int
	-> Grammatik -> Ableitung -> Set Ableitung 
links_schritt schranke g a = unionManySets 
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
	guard $ case schranke of Nothing -> True; Just n -> length w' <= n
	return $ cons w' a

links_ableitungen :: Maybe Int -> Grammatik -> [ Ableitung ]
links_ableitungen schranke g = do
    ws <- schichten (links_schritt schranke g) 
		    (cons [ startsymbol g ] nil)
    w <- setToList ws
    guard $ and [ x `elementOf` terminale g | x <- car w ]
    return w

alle_links_ableitungen :: Maybe Int -> Grammatik -> [ Ableitung ]
alle_links_ableitungen schranke g = do
    ws <- schichten (links_schritt schranke g) 
		    (cons [ startsymbol g ] nil)
    w <- setToList ws
    -- guard $ and [ x `elem` terminale g | x <- car w ]
    return w

mehrdeutige_links_ableitungen 
    ::  Maybe Int -> Grammatik -> [ (Ableitung, Ableitung) ]
mehrdeutige_links_ableitungen schranke g =
    --    mehrfacheBy car $ links_ableitungen schranke g
    meetings ( setToList . links_schritt schranke g )
	     ( cons [ startsymbol g ] nil )

g :: Grammatik
g = Grammatik 
    { terminale = mkSet "ab"
    , nichtterminale = mkSet "S" 
    , startsymbol = 'S'
    , regeln = mkSet [ ( "S", ""), ( "S", "aSbS") , ( "S", "bSaS")]
    }
