module Code.Hamming.Check where

--  $Id$


import Code.Hamming.Data 

import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( tails )
import Control.Monad ( guard )

-- | alle (xs !! i, xs !! j) für i < j
pairs :: [a] -> [(a,a)]
pairs xs = do
    u : rest <- tails xs
    v <- rest
    return (u, v)

-- | falls Code nicht leer und alles gleich lang,
-- dann diese Länge.
equal_length :: Code -> Reporter Int
equal_length code = do
    when ( null code ) $ reject $ text "Ihr Code ist leer."
    inform $ text "Haben alle Code-Wörter die gleiche Länge?"
    let wrong = do
            (u, v) <- pairs code
	    guard $ Prelude.length u /= Prelude.length v
	    return (u, v)
    let l = Prelude.length $ head code
    if ( null wrong )
       then inform $ text "Ja:" <+> toDoc l
       else reject $ text "Nein:" <+> toDoc ( head wrong )
    return $ l

-- | Hamming-Abstand zwischen zwei Wörtern
-- Vorsicht: schneidet auf gemeinsame Länge
dist :: Eq a 
	 => [a] -> [a] -> Int
dist u v = Prelude.length 
	     $ filter (uncurry (/=))
	     $ zip u v

-- | kleinster Hamming-Abstand zwischen zwei Wörtern
-- Vorsicht: stürzt ab für leeren code
minimum_distance :: Ord a 
      => [[a]] 
      -> ( Int, ([a], [a]) )
minimum_distance code = minimum $ do
    p @ (u, v) <- pairs code
    return ( dist u v, p )

    
