module NFA.Synchronize where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Det
import Autolib.NFA.Minimize
import Autolib.NFA.Normalize
import Autolib.NFA.Minus
import Autolib.NFA.Shuffle
import Autolib.NFA.Ops
import Autolib.NFA.Basic
import Autolib.NFA.Shortest

import Autolib.NFA.Dot

import Autolib.ToDoc
import Autolib.Letters
import Autolib.Symbol

-- | input: complete and deterministic automaton
-- output: automaton that accepts all synchronizing words

synchro :: NFAC c a 
	=> NFA c a
	-> NFA c (Set a)
synchro a = 
    let b = det0 $ a { starts = states a }
    in  b { finals = sfilter ( \ s -> 1 == cardinality s ) 
		   $ states b
	  }

-- | factor-minimal words
facmin :: NFAC c a 
       => NFA c a
       -> NFA c Int
facmin a =
    let s = normalize a
	sig = sigma $ setToList $ letters s
    in  minus s
	      ( normalize
	      $ Autolib.NFA.Ops.union ( dot sig s ) 
				     ( dot s sig ) )

-- | minimal words w.r.t. embedding
-- (by Kruskal's theorem, this is a finite set)
-- hint: get list of these words with 'NFA.Shortest.accepted'
embmin :: NFAC c a
       => NFA c a
       -> NFA c Int
embmin a = 
    let s = minimize $ normalize a
	sig = sigma $ setToList $ letters s
        sigplus = dot sig ( star sig )
    in  minus s ( minimize $ normalize $ shuffle s sigplus )

-- | "bad" examples

bad :: Int -> NFA Char Int
bad n = NFA
      { nfa_info = funni "bad" [ toDoc n ]  
      , states = mkSet [ 0 .. pred n ]
      , starts = emptySet
      , finals = emptySet
      , trans  = collect $
           do i <- [ 0 .. pred n ]
              return (i, 'a' , if 1 == i then 0 else i)
        ++ do i <- [ 0 .. pred n ]
	      return (i, 'b', pred i `mod` n)
      }
