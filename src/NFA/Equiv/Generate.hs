module NFA.Equiv.Generate where

-- $Id$

import NFA.Type

import NFA.Det (det0)
import NFA.Normalize

import NFA.Equiv.Challenger
import NFA.Equiv.Core

import NFA.Some

import Util.Zufall
import Util.Cache
import Util.Seed
import Util.Datei

import Set
import Size
import ToDoc
import Reporter

import Inter.Types
import Informed

data Conf = Conf { alphabet :: Set Char
		 , nfa_size :: Int
		 , max_dfa_size :: Int
		 }
     deriving ( Eq, Ord, Show, Read )

--------------------------------------------------------------------------

roll :: Conf
    -> IO ( NFA Char Int )
roll conf = do
    let sigma = alphabet conf
    ( d, xsss ) <- repeat_until
            ( do a <- nontrivial sigma $ nfa_size conf
		 let d = normalize $ det0 a
		 return ( d, zerlege sigma d )
	    ) ( \ (d, xsss) -> 
		   length xsss > 2 -- wenigstens zwei schritte
		&& size d < max_dfa_size conf
	        && ( (1 <) $ length -- wenigstens zwei klassen
	           $ filter (>1)  -- mit wenigstens zwei elementen
		   $ map cardinality $ setToList $ last xsss 
	    ) )
    return $ informed ( toDoc d ) d

------------------------------------------------------------------------

this :: Conf -> Var Equiv (NFA Char Int) [[Trenner Char Int]]
this conf = x where x = Var  { problem = Equiv
      , aufgabe = "E"
      , version = "Quiz"
      , key	= \ matrikel -> return matrikel
      , gen	= \ key -> do
            seed $ read key
	    i <- cache (  Datei { pfad = [ "autotool", "cache"
                                              , aufgabe x, version x
                                              ]
                                     , name = key ++ ".cache"
                                     , relativzahl = error "NFA.Equiv.Generate.relativzahl"
                                     }
                            ) ( roll conf )
	    return $ return i
      }
	    



