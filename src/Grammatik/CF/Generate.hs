module Grammatik.CF.Generate where

--  $Id$

import qualified Grammatik.Type as G

import Grammatik.CF.Finite
import Grammatik.CF.Language

import Util.Wort
import Util.Zufall

import Sets
import Data.List ( inits )


data Config = Config
	    { terminale	:: Set Char
	    , nichtterminale :: Set Char
	    , start :: Char
	    , condition :: G.Grammatik -> Bool
	    , min_num_regeln :: Int
	    , max_num_regeln :: Int
	    , min_length_lhs :: Int
	    , max_length_lhs :: Int
	    }

ex :: Config
ex = Config
   { terminale = mkSet "ab"
   , nichtterminale = mkSet "STU"
   , start = 'S'
   , min_num_regeln = 2
   , max_num_regeln = 6
   , condition = not . finite
   , min_length_lhs = 1
   , max_length_lhs = 3
   }

-- | solange Regeln hinzufügen, bis condition wahr.
roll :: Config -> IO ( Maybe G.Grammatik )
roll conf = do
    let vars = setToList ( nichtterminale conf )
	alpha = setToList $ union ( terminale conf ) ( nichtterminale conf )
	lhsss = do n <- [ min_length_lhs conf .. max_length_lhs conf ]
		   return $ alle alpha n
    rs <- sequence $ replicate ( max_num_regeln conf ) $ rule vars lhsss
    let gs = do 
	   rules <- drop (min_num_regeln conf ) $ inits rs
	   return $ G.Grammatik
		  { G.terminale = terminale conf
		  , G.nichtterminale = nichtterminale conf
		  , G.startsymbol = start conf
		  , G.regeln = mkSet rules
		  }
    case filter ( condition conf ) gs of
	 []       -> return Nothing
	 (g : _ ) -> return $ Just g

-- | eine neue regel würfeln
rule :: [ Char ] -> [[String]] -> IO ( String, String )
rule vars lhsss = do
     lhss <- eins lhsss
     lhs <- eins lhss
     v <- eins vars
     return ( [ v ] , lhs )
