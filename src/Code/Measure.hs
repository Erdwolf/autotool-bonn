module Code.Measure where

--  $Id$

import Code.Type

-- | measure a code, given a frequency distribution
measure :: Ord a
	=> Frequency a
	-> Code a b
	-> Int
measure freq code = sum $ do
    x <- keysFM freq
    let cs = lookupWithDefaultFM code ( error "Code.Measure" ) x
    return $ length cs


