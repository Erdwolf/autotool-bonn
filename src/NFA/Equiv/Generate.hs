module NFA.Equiv.Generate where

--  $Id$

import Autolib.NFA.Type hiding ( alphabet )

import NFA.Equiv.Core
import NFA.Equiv.Conf

import Autolib.NFA.Det (det0)
import Autolib.NFA.Normalize
import Autolib.NFA.Some

import Autolib.Util.Zufall
import Autolib.Size


roll :: Conf
     -> IO ( NFA Char Int , [Klassen Int] )
roll conf = do
    let sigma = alphabet conf
    repeat_until
            ( do a <- nontrivial sigma $ nfa_size conf
		 let d = normalize $ det0 a
		 return ( d, zerlege sigma d )
	    ) ( \ (d, xsss) -> 
		   length xsss > 2 -- wenigstens zwei schritte
		&& size d <= max_dfa_size conf
		&& min_dfa_size conf <= size d
	        && ( (1 <) $ length -- wenigstens zwei klassen
	           $ filter (>1)  -- mit wenigstens zwei elementen
		   $ map cardinality $ setToList $ last xsss 
	    ) )
	    



