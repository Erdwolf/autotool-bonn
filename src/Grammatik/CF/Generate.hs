module Grammatik.CF.Generate where

--  $Id$

import qualified Grammatik.Type as G

import Grammatik.CF.Finite
import Grammatik.CF.Language
import Grammatik.CF.Create
import Language.Type ( present, samples )

import Grammatik.Reduziert 
import Util.Wort
import Util.Zufall

import Sets
import Reporter
import Data.List ( inits )
import Data.Maybe ( isJust )


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

nontrivial :: G.Grammatik -> Bool
nontrivial g =  and
      [ G.regeln (reduktion g) == G.regeln g 
      , not $ finite g 
      ]

expl :: Config
expl = Config
   { terminale = mkSet "ab"
   , nichtterminale = mkSet "ST"
   , start = 'S'
   , min_num_regeln = 4
   , max_num_regeln = 10
   , condition = nontrivial
   , min_length_lhs = 1
   , max_length_lhs = 3
   }

-- | solange rollen bis erfolg
throw :: Config -> IO G.Grammatik
throw conf = do
    Just g <- repeat_until ( roll conf ) 
			   ( \ mg -> isJust mg ) 
    return g

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

test :: IO ()
test = do
    mg <- roll expl
    case mg of
	 Nothing -> do
		 putStr "* " 
		 test
	 Just g -> do
	      print g
	      let l = Grammatik.CF.Language.make "Test" g
	      -- let ws = create g 10 
	      ws <- samples l 20 0
	      print $ take 100 ws

