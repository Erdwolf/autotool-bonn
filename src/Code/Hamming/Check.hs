module Code.Hamming.Check where

--  $Id$


import Code.Hamming.Data 

import Reporter
import ToDoc

import Data.List ( tails )
import Control.Monad ( guard )

-- | alle (xs !! i, xs !! j) f�r i < j
pairs :: [a] -> [(a,a)]
pairs xs = do
    u : rest <- tails xs
    v <- rest
    return (u, v)

-- | falls Code nicht leer und alles gleich lang,
-- dann diese L�nge.
equal_length :: Code -> Reporter Int
equal_length code = do
    when ( null code ) $ reject $ text "Ihr Code ist leer."
    inform $ text "Haben alle Code-W�rter die gleiche L�nge?"
    let wrong = do
            (u, v) <- pairs code
	    guard $ Prelude.length u /= Prelude.length v
	    return (u, v)
    let l = Prelude.length $ head code
    if ( null wrong )
       then inform $ text "Ja:" <+> toDoc l
       else reject $ text "Nein:" <+> toDoc ( head wrong )
    return $ l

-- | Hamming-Abstand zwischen zwei W�rtern
-- Vorsicht: schneidet auf gemeinsame L�nge
dist :: Eq a 
	 => [a] -> [a] -> Int
dist u v = Prelude.length 
	     $ filter (uncurry (/=))
	     $ zip u v

-- | kleinster Hamming-Abstand zwischen zwei W�rtern
-- Vorsicht: st�rzt ab f�r leeren code
minimum_distance :: Ord a 
      => [[a]] 
      -> ( Int, ([a], [a]) )
minimum_distance code = minimum $ do
    p @ (u, v) <- pairs code
    return ( dist u v, p )

    
